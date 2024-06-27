use windows::{
    core::h,
    Foundation::{Numerics::Vector3, Rect},
    Win32::Foundation::HWND,
    UI::{
        Color,
        Composition::{ScalarKeyFrameAnimation, SpriteVisual, VisualCollection},
    },
};

use crate::{
    app_subsystem_instances::AppSubsystemInstances,
    empty_weak_mut, new_cyclic_shared_mut,
    uikit::{
        CursorStyle, HitTestTree, InputContext, InputEventHandler, MountableView, ViewContext,
    },
    utils::RectExtensions,
    winapi_extras::{
        timespan_ms, KeyFrameAnimationExtension, KeyFrameAnimationPropertySetterExtension,
        VisualExtensions,
    },
    AppWindow, DockDirection, PaneDockLayer, SharedMut, WeakMut,
};

#[derive(Clone, Copy)]
pub enum SplitDirection {
    Horizontal,
    Vertical,
}

pub struct PaneSplitterView {
    visual: SpriteVisual,
    hover_animation: ScalarKeyFrameAnimation,
    hover_end_animation: ScalarKeyFrameAnimation,
    ht: SharedMut<HitTestTree>,
    dir: SplitDirection,
    controlling_dock_layer: WeakMut<PaneDockLayer>,
    drag_start_values: Option<(f32, f32, f32)>,
}
impl PaneSplitterView {
    const SURFACE_COLOR: Color = Color {
        A: 32,
        R: 255,
        G: 255,
        B: 255,
    };

    pub fn new(
        ctx: &(impl ViewContext + ?Sized),
        dir: SplitDirection,
    ) -> windows::core::Result<SharedMut<Self>> {
        let visual = AppSubsystemInstances::get()
            .compositor
            .CreateSpriteVisual()?;
        visual.SetBrush(
            &AppSubsystemInstances::get()
                .compositor
                .CreateColorBrushWithColor(Self::SURFACE_COLOR)?,
        )?;
        visual.SetOpacity(0.0)?;

        let linear_easing = AppSubsystemInstances::get()
            .compositor
            .CreateLinearEasingFunction()?;

        let hover_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        hover_animation
            .keyframe(0.0, 0.0)?
            .interpolate(1.0, 1.0, &linear_easing)?
            .set_properties()
            .duration(timespan_ms(100))?;

        let hover_end_animation = AppSubsystemInstances::get()
            .compositor
            .CreateScalarKeyFrameAnimation()?;
        hover_end_animation
            .keyframe(0.0, 1.0)?
            .interpolate(1.0, 0.0, &linear_easing)?
            .set_properties()
            .duration(timespan_ms(100))?;

        Ok(new_cyclic_shared_mut(|wthis| {
            let ht = HitTestTree::new(
                Some(wthis.clone()),
                ctx.hittest_context().new_id(),
                Rect::from_size(1.0, 1.0),
                Rect::empty(),
            );

            Self {
                visual,
                hover_animation,
                hover_end_animation,
                ht,
                dir,
                controlling_dock_layer: empty_weak_mut(),
                drag_start_values: None,
            }
        }))
    }

    pub fn bind_dock_layer(&mut self, layer: &WeakMut<PaneDockLayer>) {
        self.controlling_dock_layer = layer.clone();
    }

    pub fn set_offset(&self, left: f32, top: f32) -> windows::core::Result<()> {
        self.visual.SetOffset(Vector3 {
            X: left,
            Y: top,
            Z: 0.0,
        })?;
        self.ht.borrow_mut().set_offset(left, top);

        Ok(())
    }
    pub fn set_rect(&self, rect: Rect) -> windows::core::Result<()> {
        self.visual.set_properties().rect(&rect)?;
        self.ht
            .borrow_mut()
            .set_rect(rect.X, rect.Y, rect.Width, rect.Height);

        Ok(())
    }
}
impl MountableView for PaneSplitterView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
        _view_context: &dyn ViewContext,
    ) -> windows::core::Result<()> {
        onto.InsertAtTop(&self.visual)?;
        HitTestTree::add_child(onto_ht, self.ht.clone());

        Ok(())
    }

    fn unmount(&self, _view_context: &dyn ViewContext) -> windows::core::Result<()> {
        self.visual.Parent()?.Children()?.Remove(&self.visual)?;
        self.ht.borrow_mut().unmount();

        Ok(())
    }
}
impl InputEventHandler for WeakMut<PaneSplitterView> {
    fn hover_cursor(&self) -> CursorStyle {
        match self.upgrade().map(|x| x.borrow().dir) {
            Some(SplitDirection::Horizontal) => CursorStyle::SizeNS,
            Some(SplitDirection::Vertical) => CursorStyle::SizeEW,
            None => CursorStyle::Arrow,
        }
    }

    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .visual
            .StartAnimation(h!("Opacity"), &this.borrow().hover_animation)
            .expect("Failed to start hover animation");
    }

    fn on_pointer_leave(&self, _view_ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow()
            .visual
            .StartAnimation(h!("Opacity"), &this.borrow().hover_end_animation)
            .expect("Failed to start hover end animation");
    }

    fn on_pointer_down(&self, x: f32, y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(target_dock) = this.borrow().controlling_dock_layer.upgrade() else {
            return;
        };

        this.borrow_mut().drag_start_values = Some((x, y, target_dock.borrow().dock_size()));
        ctx.capture_mouse();
    }

    fn on_drag_move(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };
        let Some(target_dock) = this.borrow().controlling_dock_layer.upgrade() else {
            return;
        };
        let Some((bx, by, bs)) = this.borrow().drag_start_values else {
            return;
        };

        let app_window = AppWindow::wrap(window);
        let new_size = match &*target_dock.borrow() {
            PaneDockLayer::EmptyRoot(_, _) => return,
            PaneDockLayer::Docked {
                direction: DockDirection::Left,
                ..
            } => bs + app_window.pixels_to_dip(x - bx),
            PaneDockLayer::Docked {
                direction: DockDirection::Top,
                ..
            } => bs + app_window.pixels_to_dip(y - by),
            PaneDockLayer::Docked {
                direction: DockDirection::Right,
                ..
            } => bs - app_window.pixels_to_dip(x - bx),
            PaneDockLayer::Docked {
                direction: DockDirection::Bottom,
                ..
            } => bs - app_window.pixels_to_dip(y - by),
            PaneDockLayer::Fill { .. } => return,
        }
        .max(1.0);
        let (x, y) = target_dock
            .borrow_mut()
            .set_dock_size(new_size, &ctx.make_resize_context())
            .expect("Failed to resize pane");
        this.borrow_mut()
            .set_offset(x, y)
            .expect("Failed to set splitter position");
    }

    fn on_pointer_up(&self, _x: f32, _y: f32, ctx: &mut dyn InputContext) {
        let Some(this) = self.upgrade() else {
            return;
        };

        this.borrow_mut().drag_start_values = None;
        ctx.release_mouse_capture();
    }
}
