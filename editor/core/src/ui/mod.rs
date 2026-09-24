use std::collections::HashSet;

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    input::hittest::HitTestTreeRef,
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View, ViewConstructor,
        ViewLayoutStateStore, ViewRenderElements,
    },
};

pub mod app_menu_bar;
pub mod dock;
pub mod pane;
pub mod window_footer;
pub mod window_header;

pub struct PerWindowData {
    pub screen_reposition_interests: HashSet<HitTestTreeRef>,
    pub root_view: TypedViewIdentifier<WindowRootView>,
    pub header: window_header::Component,
    pub appmenu: Option<TypedViewIdentifier<app_menu_bar::View>>,
    pub footer: Option<TypedViewIdentifier<window_footer::View>>,
    pub docking_manager: dock::WindowDockingManager,
}
impl PerWindowData {
    pub fn compute_content_area(&self, surface_size: Size<LogicalUnit>) -> Rect<LogicalUnit> {
        let top_offset = if self.appmenu.is_some() {
            window_header::View::THICKNESS + app_menu_bar::View::HEIGHT
        } else {
            window_header::View::THICKNESS
        };
        let bottom_offset = if self.footer.is_some() {
            window_footer::View::THICKNESS
        } else {
            0.0
        };

        Rect::from_lt_size(
            Point::new_logical(0.0, top_offset),
            Size::new_logical(
                surface_size.width,
                surface_size.height - top_offset - bottom_offset,
            ),
        )
    }

    pub fn compute_content_left_top(&self) -> Point<LogicalUnit> {
        let top_offset = if self.appmenu.is_some() {
            window_header::View::THICKNESS + app_menu_bar::View::HEIGHT
        } else {
            window_header::View::THICKNESS
        };

        Point::new_logical(0.0, top_offset)
    }
}

pub struct WindowRootViewInit;
impl ViewConstructor for WindowRootViewInit {
    type ConcreteView = WindowRootView;

    #[inline(always)]
    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        WindowRootView {}
    }
}

/// 全てのウィンドウの最上位にいるView
pub struct WindowRootView {}
impl View for WindowRootView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        _ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        ViewRenderElements::EMPTY
    }

    fn teardown(&mut self, _ctx: &mut TeardownContext) {}

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}
