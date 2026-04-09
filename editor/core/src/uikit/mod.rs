//! Non-Application related common ui kits

use crate::{
    SyncEvent, SystemLink, WindowHandle,
    input::{
        KeyboardFocusTokenRegistry,
        hittest::{HitTestTreeManager, HitTestTreeRef},
    },
    rendering::composite::{CompositeTree, CompositeTreeRef},
};

pub struct MountContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub current_sec: f32,
}

pub struct ViewInitContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub system_link: &'a SystemLink<'a>,
    pub ui_scale_factor: f32,
}
impl<'a, 'h> core::ops::Deref for ViewInitContext<'a, 'h> {
    type Target = MountContext<'a, 'h>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.mount_context
    }
}
impl<'a, 'h> core::ops::DerefMut for ViewInitContext<'a, 'h> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.mount_context
    }
}

pub trait MountTarget {
    fn ct_root(&self) -> CompositeTreeRef;
    fn ht_root(&self) -> HitTestTreeRef;
}
impl<T> MountTarget for &'_ T
where
    T: MountTarget + ?Sized,
{
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        T::ct_root(self)
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        T::ht_root(self)
    }
}
impl MountTarget for WindowHandle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.composite_root()
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root()
    }
}

pub struct RawMountTarget {
    pub ct_root: CompositeTreeRef,
    pub ht_root: HitTestTreeRef,
}
impl MountTarget for RawMountTarget {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root
    }
}
impl RawMountTarget {
    #[inline(always)]
    pub fn from_typed(target: &(impl MountTarget + ?Sized)) -> Self {
        Self {
            ct_root: target.ct_root(),
            ht_root: target.ht_root(),
        }
    }
}

pub struct Positioning {
    pub parent_anchor: [f32; 2],
    pub anchor: [f32; 2],
    pub offset: [f32; 2],
}

mod popup;
pub use self::popup::{
    OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup, PopupID, PopupManager,
};

mod button;
pub use self::button::SimpleButtonView;

mod menu;
pub use self::menu::{
    BaseSurfaceEventHandler as MenuBaseSurfaceEventHandler, CommandView as MenuItemCommandView,
    CommonResources as MenuItemCommonResources, HeadingView as MenuItemHeadingView, MenuItem,
    MenuItemLayout, MenuItemView, SeparatorView as MenuItemSeparatorView,
    SubMenuView as MenuItemSubMenuView,
};
