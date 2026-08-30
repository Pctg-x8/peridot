use std::rc::Rc;

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
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View, ViewConstructor,
        ViewIdentifier, ViewInitContext, ViewLayoutStateStore, ViewRegisterable,
        ViewRenderElements,
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct Presenter {
    root_view_id: TypedViewIdentifier<FileListView>,
}
impl Presenter {
    pub const ID: &str = internal_pane_identifier!("AssetExplorer");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        Self {
            root_view_id: ctx.construct_view2(FileListViewInit, |_| []),
        }
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
        self.root_view_id.into_untyped()
    }
}

struct FileListViewInit;
impl ViewConstructor for FileListViewInit {
    type ConcreteView = FileListView;

    #[inline(always)]
    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        FileListView { entity: None }
    }
}

struct FileListView {
    entity: Option<Rc<FileListViewEntity>>,
}
impl View for FileListView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                let ct_root = CompositeRect::build()
                    .expand_full()
                    .create(ctx.composite_tree);
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height_adjustment_factor: 1.0,
                    ..Default::default()
                });

                let items = std::fs::read_dir(std::env::current_dir().expect("current_dir"))
                    .expect("read_dir")
                    .map(|e| {
                        let e = e.expect("read_dir.iter");

                        e.file_name().into_string().expect("invalid file name str")
                    })
                    .collect::<Vec<_>>();

                let mut elements = Vec::with_capacity(items.len());
                let mut left_offset = 0.0;
                let mut top_offset = 0.0;
                for x in &items {
                    let element = TiledElementSubView::new(
                        ctx.composite_tree,
                        ctx.ht_manager,
                        ctx.system_link,
                        x.into(),
                        Point::new_logical(left_offset, top_offset),
                    );
                    ctx.composite_tree.add_child(ct_root, element.ct_root);
                    ctx.ht_manager.add_child(ht_root, element.ht_root);
                    elements.push(element);

                    left_offset += TiledElementSubView::ITEM_WIDTH;
                }

                let entity = Rc::new(FileListViewEntity {
                    ct_root,
                    ht_root,
                    elements,
                });
                for e in entity.elements.iter() {
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
    elements: Vec<TiledElementSubView>,
}
impl HitTestTreeActionHandler for FileListViewEntity {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for e in self.elements.iter() {
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
        for e in self.elements.iter() {
            if e.ht_root == sender {
                e.unlit(context.composite_tree, context.current_sec);
                break;
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }
}

struct TiledElementSubView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
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
        label: String,
        left_top: Point<LogicalUnit>,
    ) -> Self {
        let label_metric = TextLayout::new_single(
            &label,
            FontID::UIDefault,
            syslink.font_set(),
            CompositeRectTextHorizontalAlignment::Middle,
            Some(Self::TEXT_WIDTH_MAX),
            Some(2),
        )
        .size();

        let ct_root = CompositeRect::build()
            .use_ui_scale()
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
            .use_ui_scale()
            .composite_fill_color_imm([1.0, 1.0, 1.0, 0.5])
            .size_imm(32.0, 32.0)
            .relative_offset_adjustment(0.5, 0.0)
            .offset_imm(-16.0, Self::MARGIN)
            .create(composite_tree);
        let ct_label = CompositeRect::build()
            .text(
                CompositeRectText::build()
                    .run(CompositeRectTextRun::build(label).color_imm([1.0, 1.0, 1.0, 1.0]))
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

        Self { ct_root, ht_root }
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
