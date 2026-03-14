use crate::{Event, input::hittest::HitTestTreeManager, rendering::composite::CompositeTree};

pub struct MountContext<'a, 'h> {
    pub composite_tree: &'a mut CompositeTree<Event>,
    pub ht_manager: &'a mut HitTestTreeManager<'h>,
}

pub struct ViewInitContext<'a, 'h> {
    pub mount_context: MountContext<'a, 'h>,
    pub ui_scale_factor: f32,
}
impl<'a, 'h> ViewInitContext<'a, 'h> {
    pub const fn as_mount(&mut self) -> &mut MountContext<'a, 'h> {
        &mut self.mount_context
    }
}

pub mod window_header;
