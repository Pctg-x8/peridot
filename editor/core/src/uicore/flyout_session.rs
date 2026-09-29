use core::pin::Pin;

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    CoreLoop, FlyoutSurfaceHandle, SyncEvent, SystemLink, WindowHandle, create_flyout_surface,
    input::hittest::HitTestTreeManager,
    rendering::composite::CompositeTree,
    uicore::{
        TeardownContext, ViewDestructionContext, ViewGroupRelationStore, ViewIdentifier,
        ViewIdentifierAllocator, ViewImmediateRenderable, ViewInitContext, ViewInstanceStore,
        ViewLayoutStateStore, ViewRenderStateStore, ViewTreeRelationStore,
    },
};

pub trait FlyoutSurfacePresenter {
    fn root_view_id(&self) -> ViewIdentifier;

    #[allow(unused_variables)]
    fn rescale(
        &self,
        new_scale: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &HitTestTreeManager,
        system_link: &SystemLink,
    ) {
    }

    #[allow(unused_variables)]
    fn teardown(&self, ctx: &mut TeardownContext) {}
}

pub trait FlyoutSurfacePresenterConstructor {
    fn size(&self) -> Size<LogicalUnit>;
    fn create(&self, view_init_context: &mut ViewInitContext) -> Box<dyn FlyoutSurfacePresenter>;
}

pub struct CustomFlyoutViewOpenRequest {
    pub parent: WindowHandle,
    pub pos: Point<LogicalUnit>,
    pub content_ctor: Box<dyn FlyoutSurfacePresenterConstructor>,
}

pub struct CustomViewFlyoutSurface {
    native_surface: FlyoutSurfaceHandle,
    content: Box<dyn FlyoutSurfacePresenter>,
}
pub struct CustomViewFlyoutSession {
    parent: WindowHandle,
    opening_surface: CustomViewFlyoutSurface,
}
impl CustomViewFlyoutSession {
    pub fn begin(
        parent: WindowHandle,
        pos: Point<LogicalUnit>,
        content_ctor: Box<dyn FlyoutSurfacePresenterConstructor>,
        mut cl: Pin<&mut CoreLoop<'_>>,
    ) -> Self {
        let surface = create_flyout_surface(parent, pos, content_ctor.size(), cl.as_mut());

        let cl = unsafe { cl.get_unchecked_mut() };
        let mut view_init_ctx = ViewInitContext {
            composite_tree: &mut cl.composite_tree,
            ht_manager: &mut cl.ht_manager,
            current_sec: cl.global_time_base.elapsed().as_secs_f32(),
            keyboard_focus_registry: &mut cl.keyboard_focus_registry,
            view_allocator: &mut cl.view_allocator,
            view_instance_store: &mut cl.view_instance_store,
            view_tree_relation_store: &mut cl.view_tree_relation_store,
            view_group_relation_store: &mut cl.view_group_relation_store,
            view_layout_state_store: &mut cl.view_layout_state_store,
            view_render_state_store: &mut cl.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut cl.view_feedback_registry_delayed_ops,
            system_link: &cl.syslink,
            main_thread_texture_id_issuer: &mut cl.texture_id_issuer,
            application: &cl.application,
        };
        let content = content_ctor.create(&mut view_init_ctx);
        view_init_ctx.render_view_with_base(
            content.root_view_id(),
            &surface,
            surface.keyboard_focus_state().root_group(),
            Rect::from_lt_size(Point::new_logical(0.0, 0.0), content_ctor.size()),
        );

        Self {
            parent,
            opening_surface: CustomViewFlyoutSurface {
                native_surface: surface,
                content,
            },
        }
    }

    #[inline(always)]
    pub fn is_child_of(&self, parent: WindowHandle) -> bool {
        self.parent == parent
    }

    pub fn rescale(
        &self,
        new_scale: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &HitTestTreeManager,
        system_link: &SystemLink,
    ) {
        self.opening_surface
            .content
            .rescale(new_scale, composite_tree, ht_manager, system_link);
    }

    pub fn terminate<'a, 'h: 'a>(self, env: &mut FlyoutSurfaceSessionTerminateContext) {
        self.opening_surface
            .content
            .teardown(&mut env.teardown_context);
        env.destruct_view_recursive_untyped(self.opening_surface.content.root_view_id());
        self.opening_surface.native_surface.close(
            env.syslink,
            env.teardown_context.composite_tree,
            env.teardown_context.ht_manager,
            env.teardown_context.keyboard_focus_registry,
        );
    }
}

pub struct FlyoutSurfaceSessionTerminateContext<'a, 'sys> {
    pub syslink: &'a SystemLink<'sys>,
    pub view_allocator: &'a mut ViewIdentifierAllocator,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_tree_relation_store: &'a mut ViewTreeRelationStore,
    pub view_group_relation_store: &'a mut ViewGroupRelationStore,
    pub view_layout_state_store: &'a mut ViewLayoutStateStore,
    pub view_render_state_store: &'a mut ViewRenderStateStore,
    pub teardown_context: TeardownContext<'a>,
}
impl ViewDestructionContext for FlyoutSurfaceSessionTerminateContext<'_, '_> {
    #[inline(always)]
    fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier) {
        super::destruct_view_recursive(
            target,
            &mut self.teardown_context,
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        );
    }
}
