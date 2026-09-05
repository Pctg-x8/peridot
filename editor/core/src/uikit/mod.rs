//! Non-Application related common ui kits

use crate::{
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, ViewConstructor,
        ViewLayoutStateStore, ViewRenderElements,
    },
    utils::{LogicalUnit, Rect, Size},
};

/// なにもしないView(他のViewをいれるためだけに使う)
pub struct ContainerView;
impl crate::uicore::View for ContainerView {
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

pub struct ContainerViewInit;
impl ViewConstructor for ContainerViewInit {
    type ConcreteView = ContainerView;

    #[inline(always)]
    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        ContainerView
    }
}

mod dialog;

pub use self::dialog::*;

mod label;
pub use self::label::*;

mod button;
pub use self::button::*;

mod menu;
pub use self::menu::{
    CommandView as MenuItemCommandView, CommonResources as MenuItemCommonResources,
    DELAYED_ACTION_TIMEOUT_MS as MENU_DELAYED_ACTION_TIMEOUT_MS, EventHandler as MenuEventHandler,
    MenuCommandSelectionHandler, MenuItem, MenuItemInteractableElement, MenuItemLayout,
    SubMenuView as MenuItemSubMenuView,
};

mod text_input;
pub use self::text_input::*;

mod scroll;
pub use self::scroll::*;

pub mod dropdown_box;

pub mod checkbox;
pub use self::checkbox::*;

mod radio;
pub use self::radio::*;
