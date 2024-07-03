use std::{collections::HashSet, rc::Rc};

use windows::{
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect,
    },
    Win32::{
        Foundation::HWND,
        Graphics::DirectWrite::{IDWriteTextFormat, DWRITE_FONT_WEIGHT_NORMAL},
    },
    UI::{
        Color,
        Composition::{CompositionSurfaceBrush, InsetClip, SpriteVisual, VisualCollection},
    },
};

use crate::{
    app_subsystem_instances::AppSubsystemInstances,
    new_cyclic_shared_mut, new_shared_mut,
    observable::{ObservationDisconnector, ValueChangedEventHandlerHashKey},
    uikit::{self, HitTestTree, InputContext, InputEventHandler, MountableView, ViewContext},
    utils::RectExtensions,
    winapi_extras::VisualExtensions,
    AppWindow, SharedMut, WeakMut,
};

pub struct FloatSliderView {
    root: SpriteVisual,
    gauge_clip: InsetClip,
    value_label_format: IDWriteTextFormat,
    value_label: SpriteVisual,
    value_label_brush: CompositionSurfaceBrush,
    ht: HitTestTree,
    rendered_dpi: f32,
    current_value: f32,
    max_value: f32,
    drag_base_x: f32,
    value_change_event_handlers: HashSet<ValueChangedEventHandlerHashKey<f32>>,
}
impl FloatSliderView {
    pub const BORDER_RECT_ROUNDING: f32 = 6.0;

    pub fn new(
        view_ctx: &(impl ViewContext + ?Sized),
        init_value: f32,
        max_value: f32,
    ) -> windows::core::Result<SharedMut<Self>> {
        let root = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        root.set_properties()
            .size(Vector2 { X: 128.0, Y: 16.0 })?
            .brush(
                &AppSubsystemInstances::get()
                    .ui_common_objects
                    .slider_base_brush,
            )?;

        let rate = (init_value / max_value).clamp(0.0, 1.0);
        let gauge = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        let gauge_base_brush = AppSubsystemInstances::get()
            .compositor
            .CreateColorBrushWithColor(Color {
                A: 128,
                R: 255,
                G: 255,
                B: 128,
            })?;
        let gauge_masked_brush = AppSubsystemInstances::get().compositor.CreateMaskBrush()?;
        gauge_masked_brush.SetSource(&gauge_base_brush)?;
        gauge_masked_brush.SetMask(
            &AppSubsystemInstances::get()
                .ui_common_objects
                .slider_base_brush,
        )?;
        gauge
            .set_properties()
            .expand_to_parent()?
            .brush(&gauge_masked_brush)?;
        let gauge_clip = AppSubsystemInstances::get()
            .compositor
            .CreateInsetClipWithInsets(0.0, 0.0, 128.0 * (1.0 - rate), 0.0)?;
        gauge.SetClip(&gauge_clip)?;
        root.Children()?.InsertAtTop(&gauge)?;

        let value_text_fmt = AppSubsystemInstances::get()
            .text_format_stock
            .borrow_mut()
            .get("system-ui", 10.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let value_init_surface = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .get(
                &value_text_fmt,
                view_ctx.current_dpi(),
                format!("{:.1}", init_value),
            )?;
        let value_label = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        let value_label_brush = AppSubsystemInstances::get()
            .compositor
            .CreateSurfaceBrushWithSurface(&value_init_surface.surface)?;
        value_label
            .set_properties()
            .brush(&value_label_brush)?
            .size(value_init_surface.visual_size())?
            .offset(Vector3 {
                X: 8.0,
                Y: 0.0,
                Z: 0.0,
            })?
            .relative_offset_adjustment(Vector3 {
                X: 0.0,
                Y: 0.5,
                Z: 0.0,
            })?
            .anchor_point(Vector2 { X: 0.0, Y: 0.5 })?;
        root.Children()?.InsertAtTop(&value_label)?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(wthis.clone()),
                Rect::from_size(128.0, 16.0),
                Rect::empty(),
            );

            Self {
                root,
                gauge_clip,
                value_label_format: value_text_fmt,
                value_label,
                value_label_brush,
                ht,
                rendered_dpi: view_ctx.current_dpi(),
                current_value: init_value,
                max_value,
                drag_base_x: 0.0,
                value_change_event_handlers: HashSet::new(),
            }
        }))
    }

    // TODO: これもうちょっと柔軟に書けるようにしたい（どっちか決め打ちで相対値みたいなのはやめたい）
    pub fn reposition_xrel(&self, x_rel: f32, y: f32) -> windows::core::Result<()> {
        self.root.SetOffset(Vector3 {
            X: 0.0,
            Y: y,
            Z: 0.0,
        })?;
        self.root.SetRelativeOffsetAdjustment(Vector3 {
            X: x_rel,
            Y: 0.0,
            Z: 0.0,
        })?;
        self.ht.set_top(y);
        self.ht.set_relative_left(x_rel, 0.0);

        Ok(())
    }

    pub fn observe_value_changes(
        this: &SharedMut<Self>,
        handler: impl FnMut(&dyn ViewContext, f32) + 'static,
        view_context: &dyn ViewContext,
        request_current_value: bool,
    ) -> impl ObservationDisconnector {
        let key = ValueChangedEventHandlerHashKey(new_shared_mut(handler));
        this.borrow_mut()
            .value_change_event_handlers
            .insert(key.clone());
        if request_current_value {
            (&mut *key.0.borrow_mut())(view_context, this.borrow().current_value);
        }

        FloatSliderValueChangedObservationDisconnector {
            view_ref: Rc::downgrade(this),
            key,
        }
    }

    fn notify_current_value(&self, view_context: &dyn ViewContext) {
        for e in self.value_change_event_handlers.iter() {
            (&mut *e.0.borrow_mut())(view_context, self.current_value);
        }
    }

    fn update_rate(&self) -> windows::core::Result<()> {
        let rate = self.current_value / self.max_value;

        self.gauge_clip
            .SetRightInset(self.ht.rect().Width * (1.0 - rate))?;
        Ok(())
    }

    fn update_value_label(&self) -> windows::core::Result<()> {
        let label_text = format!("{:.1}", self.current_value);
        let label_text_u16 = label_text.encode_utf16().collect::<Vec<_>>();
        let label_text_layout = unsafe {
            AppSubsystemInstances::get()
                .dwrite_factory
                .CreateTextLayout(
                    &label_text_u16,
                    &self.value_label_format,
                    core::f32::MAX,
                    core::f32::MAX,
                )?
        };
        let label_surface = AppSubsystemInstances::get()
            .text_surface_stock
            .borrow_mut()
            .create_text_surface(&label_text_layout, self.rendered_dpi)?;

        self.value_label_brush.SetSurface(&label_surface.surface)?;
        self.value_label.SetSize(label_surface.visual_size())?;

        Ok(())
    }
}
impl MountableView for FloatSliderView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &HitTestTree,
        _view_context: &dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        onto_ht.add_child(&self.ht);

        Ok(())
    }

    fn unmount(&self, _view_context: &dyn ViewContext) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        self.ht.unmount();

        Ok(())
    }
}
impl InputEventHandler for WeakMut<FloatSliderView> {
    fn hover_cursor(&self) -> uikit::CursorStyle {
        uikit::CursorStyle::SizeEW
    }

    fn on_pointer_down(&self, _x: f32, _y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        ctx.capture_mouse();
        let component_global_x = this.borrow().ht.global_rect().X;
        this.borrow_mut().drag_base_x = component_global_x;
    }

    fn on_drag_move(&self, x: f32, _y: f32, window: HWND, mut ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        let d = x - this.borrow().drag_base_x;
        let max_value = this.borrow().max_value;
        let component_width = this.borrow().ht.rect().Width;
        this.borrow_mut().current_value = d * max_value / component_width;
        this.borrow()
            .update_rate()
            .expect("Failed to update gauge rate");
        this.borrow()
            .update_value_label()
            .expect("Failed to update value label");
        this.borrow().notify_current_value(&mut ctx);
    }

    fn on_pointer_up(&self, _x: f32, _y: f32, ctx: &mut dyn InputContext) {
        ctx.release_mouse_capture();
    }
}

struct FloatSliderValueChangedObservationDisconnector {
    view_ref: WeakMut<FloatSliderView>,
    key: ValueChangedEventHandlerHashKey<f32>,
}
impl ObservationDisconnector for FloatSliderValueChangedObservationDisconnector {
    fn disconnect(&self) {
        let Some(view) = self.view_ref.upgrade() else {
            return;
        };

        view.borrow_mut()
            .value_change_event_handlers
            .remove(&self.key);
    }
}
