use std::{cell::RefCell, rc::Rc};

use crate::{
    SystemLink,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, PointerActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimationCurve, Border, CompositeRect, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun, CompositeTree,
            CompositeTreeRef, CornerRadius,
        },
        text::{FontID, TextLayout},
    },
    ui::dock::PaneContentPresenter,
    uikit::{
        ContainerView, ContainerViewInit, MeasureContext, RenderContext, ScrollContainerInit,
        TeardownContext, TextInputView, TextInputViewIO, TextInputViewInit, TypedViewIdentifier,
        View, ViewConstructor, ViewFeedbackContext, ViewFeedbackHandler, ViewFeedbackRegisterable,
        ViewIdentifier, ViewInitContext, ViewInstanceQueryableMut, ViewLayoutChild,
        ViewLayoutFlowAlignment, ViewLayoutFlowBasis, ViewLayoutFlowDirection,
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
            let path_input_view = ctx.construct_view(TextInputViewInit::new(eh.clone()), |_| []);
            let file_list_view = ctx.construct_view(FileListViewInit, |_| []);
            ctx.view_instance_mut(path_input_view)
                .expect("query failed")
                .revalidate();

            let l = ctx.view_layout_mut(path_input_view).expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.height = ViewSize::Fixed(20.0);

            let l = ctx.view_layout_mut(file_list_view).expect("query failed");
            l.width = ViewSize::FillAvailable;
            l.height = ViewSize::FillAvailable;
            l.flow_basis = ViewLayoutFlowBasis::Flexible(1.0);

            EventHandler {
                path_input_view,
                file_list_view,
            }
        });
        ctx.subscribe_view_feedback::<crate::model::asset_explorer::ViewFeedbackCurrentDirectoryChanged>(&eh);

        let root_view = ctx.construct_view(ContainerViewInit, |ctx| {
            [
                eh.path_input_view.into_untyped(),
                ctx.construct_view(ScrollContainerInit::new(eh.file_list_view), |_| {
                    [eh.file_list_view.into_untyped()]
                })
                .into_untyped(),
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
        context.schedule_view_render(self.root_view);
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<crate::model::asset_explorer::ViewFeedbackCurrentDirectoryChanged>(&self.eh);
    }
}

struct EventHandler {
    path_input_view: TypedViewIdentifier<TextInputView>,
    file_list_view: TypedViewIdentifier<FileListView>,
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
            .view_instance_mut(self.path_input_view)
            .expect("query failed")
            .revalidate();
        context.schedule_view_render(self.path_input_view);

        context
            .view_instance_mut(self.file_list_view)
            .expect("query failed")
            .revalidate_elements = true;
        context.schedule_view_render(self.file_list_view);
    }
}
impl TextInputViewIO for EventHandler {
    fn text(&self, requester: ViewIdentifier, app: &crate::model::Application) -> String {
        if requester == self.path_input_view {
            return crate::model::asset_explorer::current_path(app)
                .display()
                .to_string();
        }

        String::new()
    }

    fn set_text(
        &self,
        sender: ViewIdentifier,
        app: &mut crate::model::ApplicationMutation,
        text: String,
    ) {
        if sender == self.path_input_view {
            tracing::debug!(text, "todo: set_text path_input_view");
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

                // TODO: relayoutの結果を自身のサイズそしてレイアウトシステムに反映する必要がある ただしrenderのタイミングでサイズいじることはできないのでlayoutフェーズで自身のサイズ計算のときに計算を差し込める必要がある

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
        Size::new_logical(10.0, 10.0)
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
                crate::model::asset_explorer::interact(context, &e.entry_type);
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
    entry_type: crate::model::asset_explorer::FileEntryType,
}
impl TiledElementSubView {
    const MARGIN: f32 = 8.0;
    const ICON_TEXT_MARGIN: f32 = 2.0;
    const TEXT_WIDTH_MAX: f32 = 64.0;
    const ITEM_WIDTH: f32 = Self::TEXT_WIDTH_MAX + Self::MARGIN * 2.0;

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
                    .run(CompositeRectTextRun::build(model.name).color_imm([1.0, 1.0, 1.0, 1.0]))
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
            entry_type: model.r#type,
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
