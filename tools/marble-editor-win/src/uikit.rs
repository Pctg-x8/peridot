use windows::{
    Win32::{
        Foundation::HWND, Graphics::DirectWrite::IDWriteTextFormat,
        UI::WindowsAndMessaging::HTCLIENT,
    },
    UI::Composition::{
        CompositionColorBrush, CompositionLinearGradientBrush, CompositionNineGridBrush,
        ScalarKeyFrameAnimation, VisualCollection,
    },
};

use crate::SharedMut;

mod input;
pub use self::input::*;

pub struct UICommonObjects {
    pub tab_base_brush: CompositionColorBrush,
    pub tab_active_overlay_brush: CompositionLinearGradientBrush,
    pub tab_title_font: IDWriteTextFormat,
    pub tab_active_title_font: IDWriteTextFormat,
    pub tab_hover_animation: ScalarKeyFrameAnimation,
    pub tab_hover_end_animation: ScalarKeyFrameAnimation,
    pub tab_active_overlay_enter_animation: ScalarKeyFrameAnimation,
    pub tab_active_overlay_leave_animation: ScalarKeyFrameAnimation,
    pub slider_base_brush: CompositionNineGridBrush,
}

pub struct ResizeContext {
    pub current_dpi: f32,
}

pub trait ViewContext {
    fn hittest_context(&self) -> &HitTestTreeContext;
    fn current_dpi(&self) -> f32;
}
pub trait InputContext: ViewContext {
    fn make_resize_context(&self) -> ResizeContext;

    fn capture_mouse(&mut self);
    fn release_mouse_capture(&mut self);
}

impl<T: ViewContext + ?Sized> ViewContext for &'_ T {
    fn hittest_context(&self) -> &HitTestTreeContext {
        T::hittest_context(*self)
    }

    fn current_dpi(&self) -> f32 {
        T::current_dpi(*self)
    }
}
impl<T: ViewContext + ?Sized> ViewContext for &'_ mut T {
    fn hittest_context(&self) -> &HitTestTreeContext {
        T::hittest_context(*self)
    }

    fn current_dpi(&self) -> f32 {
        T::current_dpi(*self)
    }
}
impl<T: InputContext + ?Sized> InputContext for &'_ mut T {
    fn make_resize_context(&self) -> ResizeContext {
        T::make_resize_context(*self)
    }

    fn capture_mouse(&mut self) {
        T::capture_mouse(*self)
    }

    fn release_mouse_capture(&mut self) {
        T::release_mouse_capture(*self)
    }
}

pub struct ViewContext1<'r> {
    pub hittest_context: &'r HitTestTreeContext,
    pub current_dpi: f32,
}
impl ViewContext for ViewContext1<'_> {
    fn hittest_context(&self) -> &HitTestTreeContext {
        self.hittest_context
    }

    fn current_dpi(&self) -> f32 {
        self.current_dpi
    }
}

pub enum CursorStyle {
    Arrow,
    SizeNS,
    SizeEW,
}

pub trait InputEventHandler {
    fn hover_cursor(&self) -> CursorStyle {
        CursorStyle::Arrow
    }

    fn nc_hittest(&self) -> u32 {
        HTCLIENT
    }

    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {}
    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {}
    fn on_pointer_down(&self, _x: f32, _y: f32, _ctx: &mut dyn InputContext) {}
    fn on_pointer_up(&self, _x: f32, _y: f32, _ctx: &mut dyn InputContext) {}
    fn on_click(&self, _window: HWND, _ctx: &mut dyn InputContext) {}
    fn on_begin_drag(&self, _x: f32, _y: f32, _window: HWND, _ctx: &mut dyn InputContext) {}
    fn on_drag_move(&self, _x: f32, _y: f32, _window: HWND, _ctx: &mut dyn InputContext) {}
    fn on_end_drag(&self, _x: f32, _y: f32, _window: HWND, _ctx: &mut dyn InputContext) {}
}
impl<T: InputEventHandler + ?Sized> InputEventHandler for std::rc::Rc<T> {
    #[inline(always)]
    fn hover_cursor(&self) -> CursorStyle {
        T::hover_cursor(&*self)
    }

    #[inline(always)]
    fn nc_hittest(&self) -> u32 {
        T::nc_hittest(&*self)
    }

    #[inline(always)]
    fn on_pointer_enter(&self, ctx: &mut dyn InputContext) {
        T::on_pointer_enter(&*self, ctx)
    }

    #[inline(always)]
    fn on_pointer_leave(&self, ctx: &mut dyn InputContext) {
        T::on_pointer_leave(&*self, ctx)
    }

    #[inline(always)]
    fn on_pointer_down(&self, x: f32, y: f32, ctx: &mut dyn InputContext) {
        T::on_pointer_down(&*self, x, y, ctx)
    }

    #[inline(always)]
    fn on_pointer_up(&self, x: f32, y: f32, ctx: &mut dyn InputContext) {
        T::on_pointer_up(&*self, x, y, ctx)
    }

    #[inline(always)]
    fn on_click(&self, window: HWND, ctx: &mut dyn InputContext) {
        T::on_click(&*self, window, ctx)
    }

    #[inline(always)]
    fn on_begin_drag(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        T::on_begin_drag(&*self, x, y, window, ctx)
    }

    #[inline(always)]
    fn on_drag_move(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        T::on_drag_move(&*self, x, y, window, ctx)
    }

    #[inline(always)]
    fn on_end_drag(&self, x: f32, y: f32, window: HWND, ctx: &mut dyn InputContext) {
        T::on_end_drag(&*self, x, y, window, ctx)
    }
}
impl InputEventHandler for () {}

pub trait MountableView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &SharedMut<HitTestTree>,
        view_context: &dyn ViewContext,
    ) -> windows::core::Result<()>;
    fn unmount(&self, view_context: &dyn ViewContext) -> windows::core::Result<()>;
}
