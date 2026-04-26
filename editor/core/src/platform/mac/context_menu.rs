use crate::{
    SyncEvent, SystemLink,
    input::{
        KeyboardFocusTokenRegistry, PerWindowKeyboardFocusState,
        hittest::{HitTestTreeManager, HitTestTreeRef},
    },
    rendering::composite::{CompositeTree, CompositeTreeRef},
    uikit::{MenuItemView, MountTarget},
    utils::{LogicalUnit, PixelsUnit, Point, Size},
};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Handle(*mut super::bridge::WindowLink);
unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
impl MountTarget for Handle {
    fn ct_root(&self) -> CompositeTreeRef {
        unimplemented!("Handle::ct_root")
    }

    fn ht_root(&self) -> HitTestTreeRef {
        unimplemented!("Handle::ht_root")
    }
}
impl Handle {
    pub fn close(
        self,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        unimplemented!("Handle::close")
    }

    pub fn logical_size(&self) -> Size<LogicalUnit> {
        unimplemented!("Handle::logical_size")
    }

    pub fn pixels_size(&self) -> Size<PixelsUnit> {
        unimplemented!("Handle::pixels_size")
    }

    pub fn render_scale(&self) -> f32 {
        unimplemented!("Handle::render_scale")
    }

    pub fn rescale<E>(&self, scale: f32, composite_tree: &mut CompositeTree<E>) {
        unimplemented!("Handle::rescale")
    }

    pub fn take_latest_ui_scale_change(&self) -> Option<f32> {
        unimplemented!("Handle::take_latest_ui_scale_change")
    }

    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        unimplemented!("Handle::keyboard_focus_state_mut")
    }

    pub fn view(&self, index: usize) -> Option<MenuItemView> {
        unimplemented!("Handle::view")
    }

    pub fn submenu_pop_position(&self, index: usize) -> Option<Point<LogicalUnit>> {
        unimplemented!("Handle::submenu_pop_position")
    }
}

pub struct SharedState {}
impl SharedState {
    pub fn reserve_delayed_action(&self) {
        unimplemented!("SharedState::reserve_delayed_action")
    }

    pub fn unreserve_delayed_action(&self) {
        unimplemented!("SharedState::unreserve_delayed_action")
    }
}
