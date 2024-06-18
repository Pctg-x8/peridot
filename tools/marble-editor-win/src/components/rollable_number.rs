use std::{collections::HashSet, rc::Rc};

use windows::{
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect,
    },
    Win32::{
        Foundation::{HWND, POINT},
        Graphics::DirectWrite::{IDWriteTextFormat, DWRITE_FONT_WEIGHT_NORMAL},
        UI::WindowsAndMessaging::{SetCursorPos, ShowCursor},
    },
    UI::{
        Color,
        Composition::{CompositionSurfaceBrush, SpriteVisual, VisualCollection},
    },
};

use crate::{
    new_cyclic_shared_mut, new_shared_mut,
    observable::{ObservationDisconnector, ValueChangedEventHandlerHashKey},
    uikit::{self, HitTestTree, InputContext, InputEventHandler, MountableView, ViewContext},
    winapi_extras::{VectorScalarConstructor, VisualExtensions},
    AppWindow, SharedMut, WeakMut,
};

pub struct RollableNumberView {
    root: SpriteVisual,
    label_fmt: IDWriteTextFormat,
    label: SpriteVisual,
    label_brush: CompositionSurfaceBrush,
    ht: SharedMut<HitTestTree>,
    current_value: f32,
    drag_point: peridot_math::Vector2F32,
    drag_base_value: f32,
    value_change_event_handlers: HashSet<ValueChangedEventHandlerHashKey<f32>>,
}
impl RollableNumberView {
    pub fn new(
        view_ctx: &impl ViewContext,
        init_value: f32,
    ) -> windows::core::Result<SharedMut<Self>> {
        let label_fmt = view_ctx
            .app_subsystems()
            .borrow_mut()
            .text_format_stock
            .get("system-ui", 12.0, DWRITE_FONT_WEIGHT_NORMAL)?;
        let label_surface = view_ctx
            .app_subsystems()
            .borrow_mut()
            .text_surface_stock
            .get(
                &label_fmt,
                view_ctx.current_dpi(),
                format!("{init_value:.1}"),
            )?;
        let label_brush = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSurfaceBrushWithSurface(&label_surface.surface)?;

        let border_color_brush = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateColorBrushWithColor(Color {
                A: 64,
                R: 224,
                G: 224,
                B: 224,
            })?;
        let border_brush = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateNineGridBrush()?;
        border_brush.SetSource(&border_color_brush)?;
        border_brush.SetInsets(1.0)?;
        border_brush.SetIsCenterHollow(true)?;

        let root = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteVisual()?;
        root.set_properties()
            .brush(&border_brush)?
            .size(Vector2 { X: 64.0, Y: 16.0 })?;

        let label = view_ctx
            .app_subsystems()
            .borrow()
            .compositor
            .CreateSpriteVisual()?;
        label
            .set_properties()
            .center_point(Vector3::scalar(0.5))?
            .anchor_point(Vector2::scalar(0.5))?
            .relative_offset_adjustment(Vector3::scalar(0.5))?
            .size(label_surface.visual_size())?
            .brush(&label_brush)?;
        root.Children()?.InsertAtTop(&label)?;

        Ok(new_cyclic_shared_mut(move |wthis| {
            let ht = HitTestTree::new(
                Some(&Rc::new(wthis.clone())),
                view_ctx.hittest_context().new_id(),
                Rect {
                    X: 0.0,
                    Y: 0.0,
                    Width: 64.0,
                    Height: 16.0,
                },
            );

            Self {
                root,
                label_fmt,
                label,
                label_brush,
                ht,
                current_value: init_value,
                drag_point: peridot_math::Vector2(0.0, 0.0),
                drag_base_value: 0.0,
                value_change_event_handlers: HashSet::new(),
            }
        }))
    }

    pub fn current_value(&self) -> f32 {
        self.current_value
    }

    pub fn observe_value_changes(
        this: &SharedMut<Self>,
        handler: impl FnMut(&dyn ViewContext, f32) + 'static,
        view_context: &dyn ViewContext,
        requires_current_value: bool,
    ) -> impl ObservationDisconnector {
        let key = ValueChangedEventHandlerHashKey(new_shared_mut(handler));
        this.borrow_mut()
            .value_change_event_handlers
            .insert(key.clone());
        if requires_current_value {
            let cv = this.borrow().current_value;
            (key.0.borrow_mut())(view_context, cv);
        }

        RollableNumberValueChangedObservationDisconnector {
            view_ref: Rc::downgrade(this),
            key,
        }
    }

    fn notify_value_changes(&self, view_ctx: &impl ViewContext) {
        let c = self.current_value;

        for e in &self.value_change_event_handlers {
            (e.0.borrow_mut())(view_ctx, c);
        }
    }

    pub fn set_position(&self, x_rel: f32, x_offs: f32, y: f32) -> windows::core::Result<()> {
        self.root.SetOffset(Vector3 {
            X: x_offs,
            Y: y,
            Z: 0.0,
        })?;
        self.root.SetRelativeOffsetAdjustment(Vector3 {
            X: x_rel,
            Y: 0.0,
            Z: 0.0,
        })?;
        self.ht.borrow_mut().set_top(y);
        self.ht.borrow_mut().set_relative_left(x_rel, x_offs);

        Ok(())
    }
    pub fn set_relative_width(&self, rel: f32, offs: f32) -> windows::core::Result<()> {
        self.root.SetSize(Vector2 { X: offs, Y: 16.0 })?;
        self.root
            .SetRelativeSizeAdjustment(Vector2 { X: rel, Y: 0.0 })?;
        self.ht.borrow_mut().set_width(0.0);
        self.ht.borrow_mut().set_relative_width(rel, offs);

        Ok(())
    }

    fn update_label(&self, view_ctx: &impl ViewContext) -> windows::core::Result<()> {
        let label_text = format!("{:.1}", self.current_value);
        let label_text_u16 = label_text.encode_utf16().collect::<Vec<_>>();
        let label_layout = unsafe {
            view_ctx
                .app_subsystems()
                .borrow_mut()
                .dwrite_factory
                .CreateTextLayout(
                    &label_text_u16,
                    &self.label_fmt,
                    std::f32::MAX,
                    std::f32::MAX,
                )?
        };
        let label_surface = view_ctx
            .app_subsystems()
            .borrow_mut()
            .text_surface_stock
            .create_text_surface(&label_layout, view_ctx.current_dpi())?;

        self.label_brush.SetSurface(&label_surface.surface)?;
        self.label.SetSize(label_surface.visual_size())?;

        Ok(())
    }
}
impl MountableView for RollableNumberView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.root)?;
        HitTestTree::add_child(onto_ht, self.ht.clone());

        Ok(())
    }

    fn unmount(&self) -> windows::core::Result<()> {
        self.root.Parent()?.Children()?.Remove(&self.root)?;
        self.ht.borrow_mut().unmount();

        Ok(())
    }
}
impl InputEventHandler for WeakMut<RollableNumberView> {
    fn hover_cursor(&self) -> uikit::CursorStyle {
        uikit::CursorStyle::SizeNS
    }

    fn on_pointer_down(&self, x: f32, y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        let current = this.borrow().current_value;
        this.borrow_mut().drag_point = peridot_math::Vector2(x, y);
        this.borrow_mut().drag_base_value = current;
        ctx.capture_mouse();
        unsafe {
            ShowCursor(false);
        }
    }

    fn on_drag_move(&self, x: f32, y: f32, window: HWND, mut ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        let app_window = AppWindow::wrap(window);
        let mut points = [POINT {
            x: this.borrow().drag_point.0 as _,
            y: this.borrow().drag_point.1 as _,
        }];
        app_window.map_points_to_desktop(&mut points);
        unsafe {
            SetCursorPos(points[0].x, points[0].y).expect("Failed to hold cursor");
        }

        let d = peridot_math::Vector2(x, y) - this.borrow().drag_point;
        const SENSITIVITY: f32 = 0.1;
        let new_value = this.borrow().current_value - d.1 * SENSITIVITY;
        this.borrow_mut().current_value = new_value;
        this.borrow()
            .update_label(&ctx)
            .expect("Failed to update view");
        this.borrow().notify_value_changes(&mut ctx);
    }

    fn on_pointer_up(&self, _x: f32, _y: f32, ctx: &mut dyn InputContext) {
        ctx.release_mouse_capture();
        unsafe {
            ShowCursor(true);
        }
    }
}

pub struct RollableNumberValueChangedObservationDisconnector {
    view_ref: WeakMut<RollableNumberView>,
    key: ValueChangedEventHandlerHashKey<f32>,
}
impl ObservationDisconnector for RollableNumberValueChangedObservationDisconnector {
    fn disconnect(&self) {
        let Some(view) = self.view_ref.upgrade() else {
            return;
        };

        view.borrow_mut()
            .value_change_event_handlers
            .remove(&self.key);
    }
}
