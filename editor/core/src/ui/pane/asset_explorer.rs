use std::{cell::RefCell, rc::Rc};

use crate::{
    Event, SystemLink,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, PointerActionArgs, PointerButtonActionArgs,
        },
    },
    model::{Application, ApplicationMutation},
    rendering::{
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture,
        Normalized2DStaticMeshTextureLazyInit, RenderMessageSender,
        composite::{
            AnimatableColor, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeTexture, CompositeTree, CompositeTreeRef, CornerRadius, TextureMappingMode,
            TextureType,
        },
        text::{FontID, TextLayout},
    },
    ui::dock::PaneContentPresenter,
    uikit::{
        ContainerView, ContainerViewInit, MeasureContext, MenuCommandSelectionHandler, MenuItem,
        RenderContext, ScrollContainer, ScrollContainerInit, TeardownContext, TypedViewIdentifier,
        View, ViewConstructor, ViewFeedbackContext, ViewFeedbackHandler, ViewFeedbackRegisterable,
        ViewIdentifier, ViewInitContext, ViewInstanceQueryable, ViewInstanceQueryableMut,
        ViewLayoutChild, ViewLayoutFlowAlignment, ViewLayoutFlowBasis, ViewLayoutFlowDirection,
        ViewLayoutFlowJustify, ViewLayoutOverflow, ViewLayoutStateStore, ViewRegisterable,
        ViewRenderElements, ViewRenderer, ViewSize,
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct Presenter {
    root_view: TypedViewIdentifier<ContainerView>,
    eh: Rc<EventHandler>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("AssetExplorer");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let eh = Rc::new_cyclic(|eh| {
            let path_navigator_view = ctx.construct_view(PathNavigatorViewInit, |_| []);
            let file_list_view = ctx.construct_view(FileListViewInit, |_| []);

            let l = ctx
                .view_layout_mut(path_navigator_view)
                .expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.height = ViewSize::Fixed(24.0);

            let l = ctx.view_layout_mut(file_list_view).expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.height = ViewSize::FillAvailable;

            let file_list_container_view = ctx
                .construct_view(ScrollContainerInit::new(file_list_view), |_| {
                    [file_list_view.into_untyped()]
                });
            let l = ctx
                .view_layout_mut(file_list_container_view)
                .expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.height = ViewSize::FillAvailable;
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);

            EventHandler {
                path_navigator_view,
                file_list_view,
                file_list_container_view,
            }
        });
        ctx.subscribe_view_feedback::<crate::model::asset_explorer::ViewFeedbackCurrentDirectoryChanged>(&eh);

        let root_view = ctx.construct_view(ContainerViewInit, |_| {
            [
                eh.path_navigator_view.into_untyped(),
                eh.file_list_container_view.into_untyped(),
            ]
        });
        {
            let l = ctx.view_layout_mut(root_view).expect("query failed");
            l.child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment: ViewLayoutFlowAlignment::Start,
                justify: ViewLayoutFlowJustify::Start,
                overflow: ViewLayoutOverflow::Overflow,
                gap: 2.0,
            };
        }

        Self { root_view, eh }
    }
}
impl PaneContentPresenter for Presenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Asset Explorer".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view.into_untyped()
    }

    fn resize(
        &self,
        new_size: &Size<LogicalUnit>,
        context: &mut crate::ui::dock::PaneContentResizeContext,
    ) {
        let l = context
            .view_layout_mut(self.root_view)
            .expect("query failed");
        l.width = ViewSize::Fixed(new_size.width);
        l.height = ViewSize::Fixed(new_size.height);

        let tile_view_height = context
            .view_instance(self.eh.file_list_view)
            .expect("query failed")
            .compute_tiled_view_height(new_size.width, None);
        let l = context
            .view_layout_mut(self.eh.file_list_view)
            .expect("query failed");
        l.width = ViewSize::Fixed(new_size.width);
        l.height = ViewSize::Fixed(tile_view_height);

        context.schedule_view_render(self.root_view);
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<crate::model::asset_explorer::ViewFeedbackCurrentDirectoryChanged>(&self.eh);
    }
}

struct EventHandler {
    path_navigator_view: TypedViewIdentifier<PathNavigatorView>,
    file_list_view: TypedViewIdentifier<FileListView>,
    file_list_container_view: TypedViewIdentifier<ScrollContainer>,
}
impl ViewFeedbackHandler<crate::model::asset_explorer::ViewFeedbackCurrentDirectoryChanged>
    for EventHandler
{
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &crate::model::asset_explorer::ViewFeedbackCurrentDirectoryChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        context
            .view_instance_mut(self.path_navigator_view)
            .expect("query failed")
            .revalidate = true;
        context.schedule_view_render(self.path_navigator_view);

        context
            .view_instance_mut(self.file_list_view)
            .expect("query failed")
            .revalidate_elements = true;

        let ViewSize::Fixed(w) = context
            .view_layout_mut(self.file_list_view)
            .expect("query failed")
            .width
        else {
            unreachable!();
        };
        let tile_view_height = context
            .view_instance(self.file_list_view)
            .expect("query failed")
            .compute_tiled_view_height(w, Some((context.system_link, context.application)));
        context
            .view_layout_mut(self.file_list_view)
            .expect("query failed")
            .height = ViewSize::Fixed(tile_view_height);

        // スクロール範囲の再計算が必要なのでScrollContainerから再レンダリングする
        context.schedule_view_render(self.file_list_container_view);
    }
}

struct PathNavigatorViewInit;
impl ViewConstructor for PathNavigatorViewInit {
    type ConcreteView = PathNavigatorView;

    #[inline(always)]
    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        PathNavigatorView {
            entity: None,
            revalidate: false,
        }
    }
}

struct PathNavigatorView {
    entity: Option<Rc<PathNavigatorViewEntity>>,
    revalidate: bool,
}
impl View for PathNavigatorView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .rect_imm(layout_rect.clone())
                    .apply();
                ctx.ht_manager
                    .mod_chain(e.ht_root)
                    .rect(layout_rect.clone());

                if core::mem::replace(&mut self.revalidate, false) {
                    // should retrieve from the model
                    let mut label_elements = e.breadcumb_labels.borrow_mut();
                    let mut arrow_elements = e.breadcumb_arrows.borrow_mut();
                    for e in label_elements.drain(..) {
                        ctx.composite_tree.free_all(e.ct_root);
                        ctx.ht_manager.free_all(e.ht_root);
                    }
                    for e in arrow_elements.drain(..) {
                        ctx.composite_tree.free_all(e.ct_root);
                        ctx.ht_manager.free_all(e.ht_root);
                    }

                    let labels = crate::model::asset_explorer::breadcumb_elements(ctx.application);
                    label_elements.reserve(labels.len());
                    arrow_elements.reserve(labels.len() - 1);
                    let mut left_cursor = 0.0;
                    for label in labels {
                        if !label_elements.is_empty() {
                            let arrow = PathNavigatorBreadcumbArrowSubView::new(
                                left_cursor,
                                ctx.composite_tree,
                                ctx.ht_manager,
                                ctx.main_thread_texture_id_issuer,
                                ctx.system_link.rt_sender(),
                            );
                            left_cursor += PathNavigatorBreadcumbArrowSubView::LIT_SIZE;
                            ctx.composite_tree.add_child(e.ct_root, arrow.ct_root);
                            ctx.ht_manager.add_child(e.ht_root, arrow.ht_root);
                            ctx.ht_manager.set_action_handler(arrow.ht_root, e);
                            arrow_elements.push(arrow);
                        }

                        let element = PathNavigatorBreadcumbLabelSubView::new(
                            label.clone(),
                            left_cursor,
                            ctx.composite_tree,
                            ctx.ht_manager,
                            ctx.system_link,
                        );
                        left_cursor += element.size.width as f32;
                        ctx.composite_tree.add_child(e.ct_root, element.ct_root);
                        ctx.ht_manager.add_child(e.ht_root, element.ht_root);
                        ctx.ht_manager.set_action_handler(element.ht_root, e);
                        label_elements.push(element);
                    }
                }

                e
            }
            None => {
                let ct_root = CompositeRect::build()
                    .rect_imm(layout_rect.clone())
                    .create(ctx.composite_tree);
                let ct_bottom_border = CompositeRect::build()
                    .anchor_parent_bottom()
                    .expand_width()
                    .size_imm(0.0, 1.0)
                    .offset_imm(0.0, -0.5)
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.25])
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .rect(layout_rect)
                    .create(ctx.ht_manager);

                ctx.composite_tree.add_child(ct_root, ct_bottom_border);

                let labels = crate::model::asset_explorer::breadcumb_elements(ctx.application);
                let mut breadcumb_labels = Vec::with_capacity(labels.len());
                let mut breadcumb_arrows = Vec::with_capacity(labels.len() - 1);
                let mut left_cursor = 0.0;
                for label in labels {
                    if !breadcumb_labels.is_empty() {
                        let arrow = PathNavigatorBreadcumbArrowSubView::new(
                            left_cursor,
                            ctx.composite_tree,
                            ctx.ht_manager,
                            ctx.main_thread_texture_id_issuer,
                            ctx.system_link.rt_sender(),
                        );
                        left_cursor += PathNavigatorBreadcumbArrowSubView::LIT_SIZE;
                        ctx.composite_tree.add_child(ct_root, arrow.ct_root);
                        ctx.ht_manager.add_child(ht_root, arrow.ht_root);
                        breadcumb_arrows.push(arrow);
                    }

                    let element = PathNavigatorBreadcumbLabelSubView::new(
                        label.clone(),
                        left_cursor,
                        ctx.composite_tree,
                        ctx.ht_manager,
                        ctx.system_link,
                    );
                    left_cursor += element.size.width as f32;
                    ctx.composite_tree.add_child(ct_root, element.ct_root);
                    ctx.ht_manager.add_child(ht_root, element.ht_root);
                    breadcumb_labels.push(element);
                }

                let entity = Rc::new(PathNavigatorViewEntity {
                    ct_root,
                    ht_root,
                    breadcumb_labels: RefCell::new(breadcumb_labels),
                    breadcumb_arrows: RefCell::new(breadcumb_arrows),
                });
                for e in entity.breadcumb_labels.borrow().iter() {
                    ctx.ht_manager.set_action_handler(e.ht_root, &entity);
                }
                for e in entity.breadcumb_arrows.borrow().iter() {
                    ctx.ht_manager.set_action_handler(e.ht_root, &entity);
                }

                &*self.entity.insert(entity)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

struct PathNavigatorViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    breadcumb_labels: RefCell<Vec<PathNavigatorBreadcumbLabelSubView>>,
    breadcumb_arrows: RefCell<Vec<PathNavigatorBreadcumbArrowSubView>>,
}
impl HitTestTreeActionHandler for PathNavigatorViewEntity {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for e in self.breadcumb_labels.borrow().iter() {
            if e.ht_root == sender {
                context
                    .composite_tree
                    .begin_mod_chain(e.ct_root)
                    .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                        from_value: [1.0, 1.0, 1.0, 0.0],
                        to_value: [1.0, 1.0, 1.0, 0.25],
                        curve: AnimationCurve::Linear,
                        event_on_complete: None,
                        sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                    }))
                    .apply();

                return EventContinueControl::STOP_PROPAGATION;
            }
        }
        for e in self.breadcumb_arrows.borrow().iter() {
            if e.ht_root == sender {
                context
                    .composite_tree
                    .begin_mod_chain(e.ct_root)
                    .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                        from_value: [1.0, 1.0, 1.0, 0.0],
                        to_value: [1.0, 1.0, 1.0, 0.25],
                        curve: AnimationCurve::Linear,
                        event_on_complete: None,
                        sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                    }))
                    .apply();

                return EventContinueControl::STOP_PROPAGATION;
            }
        }

        EventContinueControl::empty()
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for e in self.breadcumb_labels.borrow().iter() {
            if e.ht_root == sender {
                context
                    .composite_tree
                    .begin_mod_chain(e.ct_root)
                    .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                        from_value: [1.0, 1.0, 1.0, 0.25],
                        to_value: [1.0, 1.0, 1.0, 0.0],
                        curve: AnimationCurve::Linear,
                        event_on_complete: None,
                        sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                    }))
                    .apply();

                return EventContinueControl::STOP_PROPAGATION;
            }
        }
        for e in self.breadcumb_arrows.borrow().iter() {
            if e.ht_root == sender {
                context
                    .composite_tree
                    .begin_mod_chain(e.ct_root)
                    .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                        from_value: [1.0, 1.0, 1.0, 0.25],
                        to_value: [1.0, 1.0, 1.0, 0.0],
                        curve: AnimationCurve::Linear,
                        event_on_complete: None,
                        sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                    }))
                    .apply();

                return EventContinueControl::STOP_PROPAGATION;
            }
        }

        EventContinueControl::empty()
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        for (n, e) in self.breadcumb_labels.borrow().iter().enumerate() {
            if e.ht_root == sender {
                crate::model::asset_explorer::move_dir_by_breadcumb_index(context, n);
                return EventContinueControl::STOP_PROPAGATION;
            }
        }
        for (n, e) in self.breadcumb_arrows.borrow().iter().enumerate() {
            if e.ht_root == sender {
                let next_dir_names =
                    crate::model::asset_explorer::breadcumb_next_directory_list(context, n)
                        .collect::<Vec<_>>();
                tracing::debug!(?next_dir_names);
                let (x, y, w, h, _) = context.ht_manager.compute_global_rect_autoroot(sender);
                context.system_link.dispatch_event(Event::MenuOpen {
                    parent: context
                        .ht_manager
                        .query_root_window(sender)
                        .expect("not mounted?"),
                    items: next_dir_names
                        .iter()
                        .enumerate()
                        .map(|(i, x)| MenuItem::Command {
                            label: x.clone(),
                            command_id: i as _,
                        })
                        .collect(),
                    command_handler: (Box::new(PathNavigatorBreadcumbArrowMenuCommandHandler {
                        index: n,
                        dir_names: next_dir_names,
                    })
                        as Box<dyn MenuCommandSelectionHandler>)
                        .into(),
                    surface_pos: Point::new_logical(x, y + h),
                });

                return EventContinueControl::STOP_PROPAGATION;
            }
        }

        EventContinueControl::empty()
    }
}

struct PathNavigatorBreadcumbArrowMenuCommandHandler {
    index: usize,
    dir_names: Vec<String>,
}
impl MenuCommandSelectionHandler for PathNavigatorBreadcumbArrowMenuCommandHandler {
    fn on_select_command(&mut self, command_id: u64, context: &mut ApplicationMutation) {
        crate::model::asset_explorer::move_dir_by_breadcumb_index_and_next_directory(
            context,
            self.index,
            core::mem::replace(&mut self.dir_names[command_id as usize], String::new()),
        );
    }
}

static RIGHT_ARROW_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        width: 6.0,
        height: 8.0,
        vertices: &[
            [0.0, 0.0],
            [0.5, 0.0],
            [0.5, 0.5],
            [1.0, 0.5],
            [0.0, 1.0],
            [0.5, 1.0],
        ],
        indices: &[0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, 5],
    });
struct PathNavigatorBreadcumbArrowSubView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl PathNavigatorBreadcumbArrowSubView {
    const LIT_SIZE: f32 = 12.0;

    pub fn new<E>(
        left: f32,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        mt_tex_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let icon = RIGHT_ARROW_ICON.get(mt_tex_issuer, rt_sender);
        let ct_root = CompositeRect::build()
            .size_imm(Self::LIT_SIZE, Self::LIT_SIZE)
            .relative_offset_adjustment(0.0, 0.5)
            .offset_imm(left, -Self::LIT_SIZE * 0.5)
            .composite_fill_color_imm([1.0, 1.0, 1.0, 0.0])
            .corner_radius(CornerRadius::all(Self::LIT_SIZE * 0.5))
            .create(composite_tree);
        let ct_icon = CompositeRect::build()
            .size_imm(6.0, 8.0)
            .centering()
            .composite(CompositeMode::ColorTint(
                AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                CompositeTexture {
                    id: icon,
                    r#type: TextureType::Mask,
                    mapping: TextureMappingMode::Stretch,
                    slice_borders: [0.0, 0.0, 0.0, 0.0],
                },
            ))
            .create(composite_tree);
        let ht_root = HitTestTreeData::build()
            .interactive_defaults()
            .rect(Rect::from_lt_size(
                Point::new_logical(left, 0.0),
                Size::new_logical(Self::LIT_SIZE, 0.0),
            ))
            .expand_height()
            .create(ht_manager);

        composite_tree.add_child(ct_root, ct_icon);

        Self { ct_root, ht_root }
    }
}

struct PathNavigatorBreadcumbLabelSubView {
    size: Size<LogicalUnit>,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl PathNavigatorBreadcumbLabelSubView {
    const ROUNDING: f32 = 4.0;
    const MARGIN_X: f32 = 12.0;
    const MARGIN_Y: f32 = 2.0;

    pub fn new<E>(
        label: String,
        left: f32,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        syslink: &SystemLink,
    ) -> Self {
        let label_size = TextLayout::new_single(
            &label,
            FontID::UIDefault,
            syslink.font_set(),
            CompositeRectTextHorizontalAlignment::Start,
            None,
            None,
        )
        .size();
        let geometry = Rect::from_lt_size(
            Point::new_logical(left, -label_size.height * 0.5 - Self::MARGIN_Y),
            Size::new_logical(
                label_size.width + Self::MARGIN_X * 2.0,
                label_size.height + Self::MARGIN_Y * 2.0,
            ),
        );

        let ct_root = CompositeRect::build()
            .rect_imm(geometry.clone())
            .relative_offset_adjustment(0.0, 0.5)
            .text(
                CompositeRectText::build()
                    .run(CompositeRectTextRun::build(label).color_imm([1.0, 1.0, 1.0, 1.0]))
                    .horizontal_middle()
                    .vertical_middle(),
            )
            .corner_radius(CornerRadius::all(Self::ROUNDING))
            .composite_fill_color_imm([1.0, 1.0, 1.0, 0.0])
            .create(composite_tree);
        let ht_root = HitTestTreeData::build()
            .interactive_defaults()
            .rect(Rect::from_lt_size(
                Point::new_logical(left, 0.0),
                Size::new_logical(geometry.width, 0.0),
            ))
            .expand_height()
            .create(ht_manager);

        Self {
            size: geometry.size(),
            ct_root,
            ht_root,
        }
    }
}

struct FileListViewInit;
impl ViewConstructor for FileListViewInit {
    type ConcreteView = FileListView;

    #[inline(always)]
    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        FileListView {
            entity: None,
            revalidate_elements: false,
        }
    }
}

struct FileListView {
    entity: Option<Rc<FileListViewEntity>>,
    revalidate_elements: bool,
}
impl FileListView {
    fn compute_tiled_view_height(
        &self,
        available_width: f32,
        revalidate_ctx: Option<(&SystemLink, &Application)>,
    ) -> f32 {
        let Some(ref entity) = self.entity else {
            // not mounted yet
            return 0.0;
        };

        let mut left_offset = 0.0;
        let mut line_max_height = 0.0f32;
        let mut top_offset = 0.0;
        if let Some((syslink, app)) = revalidate_ctx {
            for e in crate::model::asset_explorer::current_dir_entries(app) {
                if left_offset + TiledElementSubView::ITEM_WIDTH >= available_width {
                    // wrap
                    left_offset = 0.0;
                    top_offset += core::mem::replace(&mut line_max_height, 0.0);
                }

                left_offset += TiledElementSubView::ITEM_WIDTH;
                line_max_height = line_max_height.max(TiledElementSubView::offline_compute_height(
                    &e.name, syslink,
                ));
            }
        } else {
            for e in entity.elements.borrow().iter() {
                if left_offset + TiledElementSubView::ITEM_WIDTH >= available_width {
                    // wrap
                    left_offset = 0.0;
                    top_offset += core::mem::replace(&mut line_max_height, 0.0);
                }

                left_offset += TiledElementSubView::ITEM_WIDTH;
                line_max_height = line_max_height.max(e.height);
            }
        }

        top_offset + line_max_height
    }
}
impl View for FileListView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref entity) => {
                ctx.composite_tree
                    .begin_mod_chain(entity.ct_root)
                    .rect_imm(layout_rect.clone())
                    .apply();
                ctx.ht_manager
                    .mod_chain(entity.ht_root)
                    .rect(layout_rect.clone());

                if core::mem::replace(&mut self.revalidate_elements, false) {
                    // revalidate elements
                    // TODO: 最適化とか仮想化はあとまわし
                    for e in entity.elements.borrow_mut().drain(..) {
                        ctx.composite_tree.free_all(e.ct_root);
                        ctx.ht_manager.free_all(e.ht_root);
                    }

                    let mut left_offset = 0.0;
                    let mut line_max_height = 0.0;
                    let mut top_offset = 0.0;
                    entity.elements.borrow_mut().extend(
                        crate::model::asset_explorer::current_dir_entries(ctx.application).map(
                            |e| {
                                if left_offset + TiledElementSubView::ITEM_WIDTH
                                    >= layout_rect.width
                                {
                                    // wrap
                                    left_offset = 0.0;
                                    top_offset += core::mem::replace(&mut line_max_height, 0.0);
                                }

                                let element = TiledElementSubView::new(
                                    ctx.composite_tree,
                                    ctx.ht_manager,
                                    ctx.system_link,
                                    e,
                                    Point::new_logical(left_offset, top_offset),
                                );
                                ctx.composite_tree
                                    .add_child(entity.ct_root, element.ct_root);
                                ctx.ht_manager.add_child(entity.ht_root, element.ht_root);
                                ctx.ht_manager.set_action_handler(element.ht_root, entity);

                                left_offset += TiledElementSubView::ITEM_WIDTH;
                                line_max_height = line_max_height.max(element.height);

                                element
                            },
                        ),
                    );
                } else {
                    // relayout only
                    let mut left_offset = 0.0;
                    let mut line_max_height = 0.0;
                    let mut top_offset = 0.0;
                    for e in entity.elements.borrow().iter() {
                        if left_offset + TiledElementSubView::ITEM_WIDTH >= layout_rect.width {
                            // wrap
                            left_offset = 0.0;
                            top_offset += core::mem::replace(&mut line_max_height, 0.0);
                        }

                        e.set_offset(
                            Point::new_logical(left_offset, top_offset),
                            ctx.composite_tree,
                            ctx.ht_manager,
                        );

                        left_offset += TiledElementSubView::ITEM_WIDTH;
                        line_max_height = line_max_height.max(e.height);
                    }
                }

                entity
            }
            None => {
                let ct_root = CompositeRect::build()
                    .rect_imm(layout_rect.clone())
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .rect(layout_rect.clone())
                    .create(ctx.ht_manager);

                let mut left_offset = 0.0;
                let mut line_max_height = 0.0;
                let mut top_offset = 0.0;
                let elements = crate::model::asset_explorer::current_dir_entries(ctx.application)
                    .map(|e| {
                        if left_offset + TiledElementSubView::ITEM_WIDTH >= layout_rect.width {
                            // wrap
                            left_offset = 0.0;
                            top_offset += core::mem::replace(&mut line_max_height, 0.0);
                        }

                        let element = TiledElementSubView::new(
                            ctx.composite_tree,
                            ctx.ht_manager,
                            ctx.system_link,
                            e,
                            Point::new_logical(left_offset, top_offset),
                        );
                        ctx.composite_tree.add_child(ct_root, element.ct_root);
                        ctx.ht_manager.add_child(ht_root, element.ht_root);

                        left_offset += TiledElementSubView::ITEM_WIDTH;
                        line_max_height = line_max_height.max(element.height);

                        element
                    })
                    .collect::<Vec<_>>();

                let entity = Rc::new(FileListViewEntity {
                    ct_root,
                    ht_root,
                    elements: RefCell::new(elements),
                });
                for e in entity.elements.borrow().iter() {
                    ctx.ht_manager.set_action_handler(e.ht_root, &entity);
                }

                &*self.entity.insert(entity)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

struct FileListViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    elements: RefCell<Vec<TiledElementSubView>>,
}
impl HitTestTreeActionHandler for FileListViewEntity {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for e in self.elements.borrow().iter() {
            if e.ht_root == sender {
                e.lit(context.composite_tree, context.current_sec);
                break;
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for e in self.elements.borrow().iter() {
            if e.ht_root == sender {
                e.unlit(context.composite_tree, context.current_sec);
                break;
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &crate::input::hittest::PointerButtonActionArgs,
    ) -> EventContinueControl {
        for e in self.elements.borrow().iter() {
            if e.ht_root == sender {
                crate::model::asset_explorer::interact(context, &e.model);
                break;
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }
}

struct TiledElementSubView {
    height: f32,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    model: crate::model::asset_explorer::FileEntry,
}
impl TiledElementSubView {
    const MARGIN: f32 = 8.0;
    const ICON_TEXT_MARGIN: f32 = 2.0;
    const TEXT_WIDTH_MAX: f32 = 64.0;
    const ITEM_WIDTH: f32 = Self::TEXT_WIDTH_MAX + Self::MARGIN * 2.0;

    fn offline_compute_height(label: &str, syslink: &SystemLink) -> f32 {
        32.0 + Self::MARGIN * 2.0
            + TextLayout::new_single(
                label,
                FontID::UIDefault,
                syslink.font_set(),
                CompositeRectTextHorizontalAlignment::Start,
                Some(Self::TEXT_WIDTH_MAX),
                Some(2),
            )
            .height()
            + Self::ICON_TEXT_MARGIN
    }

    pub fn new<E>(
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        syslink: &SystemLink,
        model: crate::model::asset_explorer::FileEntry,
        left_top: Point<LogicalUnit>,
    ) -> Self {
        let label_metric = TextLayout::new_single(
            &model.name,
            FontID::UIDefault,
            syslink.font_set(),
            CompositeRectTextHorizontalAlignment::Middle,
            Some(Self::TEXT_WIDTH_MAX),
            Some(2),
        )
        .size();

        let ct_root = CompositeRect::build()
            .offset_imm(left_top.x, left_top.y)
            .size_imm(
                Self::ITEM_WIDTH,
                32.0 + Self::MARGIN * 2.0 + label_metric.height + Self::ICON_TEXT_MARGIN,
            )
            .composite_fill_color_imm([0.0; 4])
            .border(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.0]),
                ..Default::default()
            })
            .corner_radius(CornerRadius::all(4.0))
            .create(composite_tree);
        let ct_icon = CompositeRect::build()
            .composite_fill_color_imm([1.0, 1.0, 1.0, 0.5])
            .size_imm(32.0, 32.0)
            .relative_offset_adjustment(0.5, 0.0)
            .offset_imm(-16.0, Self::MARGIN)
            .create(composite_tree);
        let ct_label = CompositeRect::build()
            .text(
                CompositeRectText::build()
                    .run(
                        CompositeRectTextRun::build(model.name.clone())
                            .color_imm([1.0, 1.0, 1.0, 1.0]),
                    )
                    .horizontal_middle()
                    .allow_wrapping()
                    .limit_lines(2),
            )
            .size_imm(Self::TEXT_WIDTH_MAX, 0.0)
            .offset_imm(Self::MARGIN, Self::MARGIN + 32.0 + Self::ICON_TEXT_MARGIN)
            .create(composite_tree);
        let ht_root = ht_manager.create(HitTestTreeData {
            left: left_top.x,
            top: left_top.y,
            width: Self::ITEM_WIDTH,
            height: 32.0 + Self::MARGIN * 2.0 + label_metric.height + Self::ICON_TEXT_MARGIN,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        composite_tree.add_child(ct_root, ct_icon);
        composite_tree.add_child(ct_root, ct_label);

        Self {
            height: 32.0 + Self::MARGIN * 2.0 + label_metric.height + Self::ICON_TEXT_MARGIN,
            ct_root,
            ht_root,
            model,
        }
    }

    fn set_offset<E>(
        &self,
        left_top: Point<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) where
        E: PartialEq,
    {
        composite_tree
            .begin_mod_chain(self.ct_root)
            .offset_imm(left_top.x, left_top.y)
            .apply();
        ht_manager
            .mod_chain(self.ht_root)
            .left(left_top.x)
            .top(left_top.y);
    }

    fn lit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        composite_tree
            .begin_mod_chain(self.ct_root)
            .border_color(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.25],
                curve: AnimationCurve::Linear,
                event_on_complete: None,
                sec_duration: (current_sec..current_sec + 0.1).into(),
            })
            .apply();
    }

    fn unlit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        composite_tree
            .begin_mod_chain(self.ct_root)
            .border_color(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.25],
                to_value: [1.0, 1.0, 1.0, 0.0],
                curve: AnimationCurve::Linear,
                event_on_complete: None,
                sec_duration: (current_sec..current_sec + 0.1).into(),
            })
            .apply();
    }
}
