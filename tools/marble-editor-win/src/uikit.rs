use windows::{
    Win32::{Foundation::HWND, Graphics::DirectWrite::IDWriteTextFormat},
    UI::Composition::{
        CompositionColorBrush, CompositionLinearGradientBrush, ScalarKeyFrameAnimation,
    },
};

use crate::object_cache::{TextFormatStock, TextSurfaceStock};

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
}

pub trait ViewContext {
    fn compositor(&self) -> &windows::UI::Composition::Compositor;
    fn common(&self) -> &UICommonObjects;
    fn text_format_stock_mut(&mut self) -> &mut TextFormatStock;
    fn text_surface_stock_mut(&mut self) -> &mut TextSurfaceStock;
    fn hittest_context_mut(&mut self) -> &mut HitTestTreeContext;
}
pub trait InputContext: ViewContext {
    fn capture_mouse(&mut self);
    fn release_mouse_capture(&mut self);
}

impl<T: ViewContext + ?Sized> ViewContext for &'_ mut T {
    fn compositor(&self) -> &windows::UI::Composition::Compositor {
        T::compositor(*self)
    }

    fn common(&self) -> &UICommonObjects {
        T::common(*self)
    }

    fn text_format_stock_mut(&mut self) -> &mut TextFormatStock {
        T::text_format_stock_mut(*self)
    }

    fn text_surface_stock_mut(&mut self) -> &mut TextSurfaceStock {
        T::text_surface_stock_mut(*self)
    }

    fn hittest_context_mut(&mut self) -> &mut HitTestTreeContext {
        T::hittest_context_mut(*self)
    }
}
impl<T: InputContext + ?Sized> InputContext for &'_ mut T {
    fn capture_mouse(&mut self) {
        T::capture_mouse(*self)
    }

    fn release_mouse_capture(&mut self) {
        T::release_mouse_capture(*self)
    }
}

pub struct ViewContext1<'r> {
    pub compositor: &'r windows::UI::Composition::Compositor,
    pub common: &'r UICommonObjects,
    pub text_format_stock: &'r mut TextFormatStock,
    pub text_surface_stock: &'r mut TextSurfaceStock,
    pub hittest_context: &'r mut HitTestTreeContext,
}
impl ViewContext for ViewContext1<'_> {
    fn compositor(&self) -> &windows::UI::Composition::Compositor {
        self.compositor
    }

    fn common(&self) -> &UICommonObjects {
        self.common
    }

    fn text_format_stock_mut(&mut self) -> &mut TextFormatStock {
        self.text_format_stock
    }

    fn text_surface_stock_mut(&mut self) -> &mut TextSurfaceStock {
        self.text_surface_stock
    }

    fn hittest_context_mut(&mut self) -> &mut HitTestTreeContext {
        self.hittest_context
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

    fn on_pointer_enter(&self, _ctx: &mut dyn InputContext) {}
    fn on_pointer_leave(&self, _ctx: &mut dyn InputContext) {}
    fn on_pointer_down(&self, _x: f32, _y: f32, _ctx: &mut dyn InputContext) {}
    fn on_pointer_up(&self, _x: f32, _y: f32, _ctx: &mut dyn InputContext) {}
    fn on_click(&self, _ctx: &mut dyn InputContext) {}
    fn on_begin_drag(&self, _x: f32, _y: f32, _window: HWND, _ctx: &mut dyn InputContext) {}
    fn on_drag_move(&self, _x: f32, _y: f32, _window: HWND, _ctx: &mut dyn InputContext) {}
    fn on_end_drag(&self, _x: f32, _y: f32, _window: HWND, _ctx: &mut dyn InputContext) {}
}
impl<T: InputEventHandler> InputEventHandler for std::rc::Rc<T> {
    #[inline(always)]
    fn hover_cursor(&self) -> CursorStyle {
        T::hover_cursor(&*self)
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
    fn on_click(&self, ctx: &mut dyn InputContext) {
        T::on_click(&*self, ctx)
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
