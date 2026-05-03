//! Non-Application related common ui kits

use std::{
    collections::BTreeSet,
    num::NonZeroUsize,
    rc::{Rc, Weak},
};

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
    pub keyboard_focus_registry: &'a mut KeyboardFocusTokenRegistry,
    pub current_sec: f32,
}

pub struct ViewInitContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub view_registry: &'a mut ViewRegistry,
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

pub struct ViewUpdateContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<SyncEvent>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
    pub keyboard_focus_registry: &'a KeyboardFocusTokenRegistry,
    pub system_link: &'a SystemLink<'a>,
    pub current_sec: f32,
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

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ViewIdentifier(NonZeroUsize);
impl core::fmt::Debug for ViewIdentifier {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ViewID#{}", self.0)
    }
}

pub struct ViewRegistry {
    last_free_identifier: NonZeroUsize,
    free_identifier: BTreeSet<NonZeroUsize>,
    event_handlers: Vec<Weak<dyn ViewEventHandler>>,
}
impl ViewRegistry {
    pub fn new() -> Self {
        Self {
            last_free_identifier: unsafe { NonZeroUsize::new_unchecked(1) },
            free_identifier: BTreeSet::new(),
            event_handlers: Vec::new(),
        }
    }

    pub fn alloc(&mut self) -> ViewIdentifier {
        if let Some(id) = self.free_identifier.pop_first() {
            return ViewIdentifier(id);
        }

        let r = ViewIdentifier(self.last_free_identifier);
        self.last_free_identifier
            .checked_add(1)
            .expect("too many views!");
        self.event_handlers
            .push(Weak::<EmptyViewEventHandler>::new());
        return r;
    }

    pub fn free(&mut self, id: ViewIdentifier) {
        if id.0.get() + 1 == self.last_free_identifier.get() {
            // returned last identifier
            self.last_free_identifier =
                unsafe { NonZeroUsize::new_unchecked(self.last_free_identifier.get() - 1) };
            self.event_handlers.pop();
            return;
        }

        self.free_identifier.insert(id.0);
        self.event_handlers[id.0.get() - 1] = Weak::<EmptyViewEventHandler>::new();
    }

    pub fn set_event_handler(
        &mut self,
        id: ViewIdentifier,
        handler: &Rc<impl ViewEventHandler + 'static>,
    ) {
        self.event_handlers[id.0.get() - 1] = Rc::downgrade(handler) as _;
    }

    pub fn call_update(&self, id: ViewIdentifier, context: &mut ViewUpdateContext) {
        let Some(eh) = self.event_handlers[id.0.get() - 1].upgrade() else {
            return;
        };

        eh.update(context);
    }
}

pub trait ViewEventHandler {
    #[allow(unused_variables)]
    fn update(&self, context: &mut ViewUpdateContext) {}
}

struct EmptyViewEventHandler;
impl ViewEventHandler for EmptyViewEventHandler {}

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

mod text_input;
pub use self::text_input::TextInputView;
