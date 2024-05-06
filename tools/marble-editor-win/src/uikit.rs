use windows::{
    Win32::{Foundation::HWND, Graphics::DirectWrite::IDWriteTextFormat},
    UI::Composition::{
        CompositionColorBrush, CompositionLinearGradientBrush, ScalarKeyFrameAnimation,
    },
};

use crate::{
    object_cache::{TextFormatStock, TextSurfaceStock},
    HitTestTree, HitTestTreeContext, PaneGroupDockingManager,
};

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

pub struct ViewContext<'r> {
    pub compositor: &'r windows::UI::Composition::Compositor,
    pub common: &'r UICommonObjects,
    pub text_format_stock: &'r mut TextFormatStock,
    pub text_surface_stock: &'r mut TextSurfaceStock,
    pub hittest_tree_parent: &'r core::cell::RefCell<HitTestTree>,
    pub hittest_context: &'r mut HitTestTreeContext,
}
impl<'r> ViewContext<'r> {
    pub fn on_new_hittest_tree<'r1>(
        &'r1 mut self,
        new_parent: &'r1 core::cell::RefCell<HitTestTree>,
    ) -> ViewContext<'r1> {
        ViewContext {
            compositor: &self.compositor,
            common: &self.common,
            text_format_stock: &mut self.text_format_stock,
            text_surface_stock: &mut self.text_surface_stock,
            hittest_tree_parent: new_parent,
            hittest_context: &mut self.hittest_context,
        }
    }

    pub fn hittest_tree_parent_mut(&self) -> core::cell::RefMut<HitTestTree> {
        self.hittest_tree_parent.borrow_mut()
    }
}

pub trait InputEventHandler {
    fn on_pointer_enter(&self, _view_ctx: &mut ViewContext) {}
    fn on_pointer_leave(&self, _view_ctx: &mut ViewContext) {}
    fn on_click(&self, _view_ctx: &mut ViewContext) {}
    fn on_begin_drag(
        &self,
        _x: f32,
        _y: f32,
        _window: HWND,
        _view_ctx: &mut ViewContext,
        _pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
    }
    fn on_drag_move(
        &self,
        _x: f32,
        _y: f32,
        _window: HWND,
        _view_ctx: &mut ViewContext,
        _pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
    }
    fn on_end_drag(
        &self,
        _window: HWND,
        _view_ctx: &mut ViewContext,
        _pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
    }
}
impl<T: InputEventHandler> InputEventHandler for std::rc::Rc<T> {
    #[inline(always)]
    fn on_pointer_enter(&self, view_ctx: &mut ViewContext) {
        T::on_pointer_enter(&*self, view_ctx)
    }

    #[inline(always)]
    fn on_pointer_leave(&self, view_ctx: &mut ViewContext) {
        T::on_pointer_leave(&*self, view_ctx)
    }

    #[inline(always)]
    fn on_click(&self, view_ctx: &mut ViewContext) {
        T::on_click(&*self, view_ctx)
    }

    #[inline(always)]
    fn on_begin_drag(
        &self,
        x: f32,
        y: f32,
        window: HWND,
        view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        T::on_begin_drag(&*self, x, y, window, view_ctx, pane_group_docking_manager)
    }

    #[inline(always)]
    fn on_drag_move(
        &self,
        x: f32,
        y: f32,
        window: HWND,
        view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        T::on_drag_move(&*self, x, y, window, view_ctx, pane_group_docking_manager)
    }

    #[inline(always)]
    fn on_end_drag(
        &self,
        window: HWND,
        view_ctx: &mut ViewContext,
        pane_group_docking_manager: &core::cell::RefCell<PaneGroupDockingManager>,
    ) {
        T::on_end_drag(&*self, window, view_ctx, pane_group_docking_manager)
    }
}
impl InputEventHandler for () {}
