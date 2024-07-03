use windows::{
    Win32::{
        Foundation::HWND, Graphics::DirectWrite::IDWriteTextFormat,
        UI::WindowsAndMessaging::HTCLIENT,
    },
    UI::Composition::{
        CompositionColorBrush, CompositionEasingFunction, CompositionLinearGradientBrush,
        CompositionNineGridBrush, ScalarKeyFrameAnimation, VisualCollection,
    },
};

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
    pub menu_item_back_mask_brush: CompositionNineGridBrush,
    pub menu_item_enter_opacity_easing_fn: CompositionEasingFunction,
    pub menu_item_enter_offset_easing_fn: CompositionEasingFunction,
}

pub struct ResizeContext {
    pub current_dpi: f32,
}

pub trait ViewContext {
    fn current_dpi(&self) -> f32;
}

impl<T: ViewContext + ?Sized> ViewContext for &'_ T {
    fn current_dpi(&self) -> f32 {
        T::current_dpi(*self)
    }
}
impl<T: ViewContext + ?Sized> ViewContext for &'_ mut T {
    fn current_dpi(&self) -> f32 {
        T::current_dpi(*self)
    }
}

pub struct ViewContext1 {
    pub current_dpi: f32,
}
impl ViewContext for ViewContext1 {
    fn current_dpi(&self) -> f32 {
        self.current_dpi
    }
}

pub enum CursorStyle {
    Arrow,
    SizeNS,
    SizeEW,
}

pub trait MountableView {
    fn mount(
        &self,
        onto: &VisualCollection,
        onto_ht: &HitTestTree,
        view_context: &dyn ViewContext,
    ) -> windows::core::Result<()>;
    fn unmount(&self, view_context: &dyn ViewContext) -> windows::core::Result<()>;
}
