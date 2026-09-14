use core::cell::Cell;
use std::rc::Rc;

use peridot_math::Zero;
use shared::{LogicalUnit, Point, Rect, SafeF32, Size};

use crate::{
    CustomFlyoutViewOpenRequest, Event, FlyoutSurfacePresenter, FlyoutSurfacePresenterConstructor,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    model::{Application, ApplicationMutation},
    rendering::{
        Normalized2DStaticMeshTexture, Normalized2DStaticMeshTextureLazyInit,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectScaleFactor, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTexture, CompositeTree, CompositeTreeRef,
            CornerRadius, FloatAnimationTemplate, TextureMappingMode, TextureType,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, ViewConstructor,
        ViewIdentifier, ViewInitContext, ViewInstanceQueryableMut, ViewLayoutStateStore,
        ViewRegisterable, ViewRenderElements, ViewRenderer,
    },
};

const ARROW_PRESS_Y_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: -8.0,
    to_value: -7.0,
    curve: AnimationCurve::EASE_OUT,
    duration: 0.1,
};
const ARROW_RELEASE_Y_ANIM: FloatAnimationTemplate =
    ARROW_PRESS_Y_ANIM.flip(AnimationCurve::EASE_OUT);

static DOWN_ARROW_ICON: Normalized2DStaticMeshTextureLazyInit =
    Normalized2DStaticMeshTextureLazyInit::new(Normalized2DStaticMeshTexture {
        vertices: &[[0.25, 0.375], [0.75, 0.375], [0.5, 0.625]],
        indices: &[0, 1, 2],
        width: 16.0,
        height: 16.0,
    });

pub trait IO {
    fn selected_index(&self, requester: ViewIdentifier, application: &Application) -> usize;
    fn on_selected_index_change(
        &self,
        sender: ViewIdentifier,
        index: usize,
        application: &mut ApplicationMutation,
    );
}

pub struct View {
    id: TypedViewIdentifier<Self>,
    io: std::rc::Weak<dyn IO>,
    entity: Option<Rc<EventHandler>>,
    items: Vec<String>,
    should_revalidate_next_render: bool,
}
impl View {
    pub fn new(
        id: TypedViewIdentifier<Self>,
        io: std::rc::Weak<impl IO + 'static>,
        items: Vec<String>,
    ) -> Self {
        Self {
            id,
            io: io as _,
            entity: None,
            items,
            should_revalidate_next_render: false,
        }
    }

    pub fn revalidate(&mut self) {
        self.should_revalidate_next_render = true;
    }
}
impl crate::uicore::View for View {
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
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(e.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(e.ht_root).height = layout_rect.height;

                if core::mem::replace(&mut self.should_revalidate_next_render, false)
                    && !self.items.is_empty()
                {
                    // revalidate
                    let current_index =
                        e.io.upgrade()
                            .expect("DropdownBox has defunct")
                            .selected_index(e.id.into_untyped(), ctx.application);
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_text)
                        .text_run(
                            CompositeRectTextRun::build(self.items[current_index].clone())
                                .color_imm([1.0, 1.0, 1.0, 1.0]),
                        )
                        .apply();
                }

                e
            }
            None => {
                // first render
                let current_index = self
                    .io
                    .upgrade()
                    .expect("DropdownBox has defunct")
                    .selected_index(self.id.into_untyped(), ctx.application);
                self.should_revalidate_next_render = false;

                let down_arrow_icon = DOWN_ARROW_ICON.get(
                    ctx.main_thread_texture_id_issuer,
                    ctx.system_link.rt_sender(),
                );

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(layout_rect.left),
                        AnimatableFloat::Value(layout_rect.top),
                    ],
                    size: [
                        AnimatableFloat::Value(layout_rect.width),
                        AnimatableFloat::Value(layout_rect.height),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.0,
                    ])),
                    corner_radius: CornerRadius::all(4.0),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_text_clip = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    size: [AnimatableFloat::Value(-12.0), AnimatableFloat::Value(0.0)],
                    clip_child: Some(ClipConfig {
                        left_softness: SafeF32::ZERO,
                        right_softness: unsafe { SafeF32::new_unchecked(12.0) },
                        top_softness: SafeF32::ZERO,
                        bottom_softness: SafeF32::ZERO,
                    }),
                    ..Default::default()
                });
                let ct_text = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: if self.items.is_empty() {
                                String::new()
                            } else {
                                self.items[current_index].clone()
                            },
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
                        offset: [4.0, 0.0],
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_down_arrow = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(-20.0), AnimatableFloat::Value(-8.0)],
                    relative_offset_adjustment: [1.0, 0.5],
                    size: [
                        AnimatableFloat::Value(DOWN_ARROW_ICON.width()),
                        AnimatableFloat::Value(DOWN_ARROW_ICON.height()),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::ColorTint(
                        AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        CompositeTexture {
                            id: down_arrow_icon,
                            r#type: TextureType::Mask,
                            mapping: TextureMappingMode::Stretch,
                            slice_borders: [0.0; 4],
                        },
                    ),
                    ..Default::default()
                });
                ctx.composite_tree.add_child(ct_text_clip, ct_text);
                ctx.composite_tree.add_child(ct_root, ct_text_clip);
                ctx.composite_tree.add_child(ct_root, ct_down_arrow);

                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: layout_rect.left,
                    top: layout_rect.top,
                    width: layout_rect.width,
                    height: layout_rect.height,
                    ..Default::default()
                });

                let eh = Rc::new_cyclic(|w| EventHandler {
                    id: self.id,
                    this_weakref: w.clone(),
                    io: self.io.clone(),
                    ct_root,
                    ct_text,
                    ct_down_arrow,
                    ht_root,
                    items: self.items.clone(),
                    current_selected: core::cell::Cell::new(0),
                    control_width: Cell::new(128.0),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                &*self.entity.insert(eh)
            }
        };

        e.control_width.set(layout_rect.width);

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(e.ct_root);
        ctx.ht_manager.free_all(e.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        let content_size = self
            .items
            .iter()
            .map(|t| {
                TextLayout::new_single(
                    t,
                    FontID::UIDefault,
                    ctx.system_link.font_set(),
                    CompositeRectTextHorizontalAlignment::Start,
                    None,
                    None,
                )
                .size()
            })
            .fold(Size::new_logical(8.0, 24.0), |a, b| {
                Size::new_logical(a.width.max(b.width), a.height.max(b.height))
            });

        // space for arrow icon
        Size::new_logical(content_size.width + 24.0, content_size.height)
    }
}

pub struct EventHandler {
    this_weakref: std::rc::Weak<EventHandler>,
    id: TypedViewIdentifier<View>,
    io: std::rc::Weak<dyn IO>,
    ct_root: CompositeTreeRef,
    ct_text: CompositeTreeRef,
    ct_down_arrow: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    items: Vec<String>,
    current_selected: core::cell::Cell<usize>,
    control_width: Cell<f32>,
}
impl HitTestTreeActionHandler for EventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_root)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.0625],
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_root)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0625],
                to_value: [1.0, 1.0, 1.0, 0.0],
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_down_arrow)
            .y_animated_from_template(&ARROW_PRESS_Y_ANIM, context.current_sec)
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_down_arrow)
            .y_animated_from_template(&ARROW_RELEASE_Y_ANIM, context.current_sec)
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let w = context
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted");
        let (x, y) = context.ht_manager.translate_tree_local_to_root(
            self.ht_root,
            0.0,
            24.0,
            w.client_size().width,
            w.client_size().height,
        );

        context.request_open_custom_flyout_view(CustomFlyoutViewOpenRequest {
            parent: w,
            pos: Point::new_logical(x, y),
            content_ctor: Box::new(FlyoutContentPresenterInit {
                layout: MenuLayout::new(
                    self.items
                        .iter()
                        .enumerate()
                        .map(|(n, c)| MenuItem {
                            content: c.into(),
                            id: n,
                        })
                        .collect(),
                    context.system_link.font_set(),
                    self.control_width.get(),
                ),
                event_handler: self.this_weakref.clone(),
            }),
        });

        EventContinueControl::STOP_PROPAGATION
    }
}
impl EventHandler {
    pub fn set_selection_id(
        &self,
        id: usize,
        application: &mut ApplicationMutation,
        env: &mut (impl ViewInstanceQueryableMut + ViewRenderer + ?Sized),
    ) {
        self.current_selected.set(id);
        self.io
            .upgrade()
            .expect("DropdownBox has defunct")
            .on_selected_index_change(self.id.into_untyped(), id, application);
        env.view_instance_mut(self.id)
            .expect("query failed")
            .revalidate();
        env.schedule_view_render(self.id);
    }
}

#[derive(Debug, Clone)]
pub struct MenuItem {
    pub content: String,
    pub id: usize,
}

#[derive(Clone)]
pub struct MenuLayout {
    items: Vec<MenuItem>,
    required_width: f32,
}
impl MenuLayout {
    #[inline(always)]
    pub fn new(items: Vec<MenuItem>, font_set: &FontSet, min_width: f32) -> Self {
        let mut width = 0.0f32;
        for v in items.iter() {
            width = width.max(
                TextLayout::measure_visual_width(&v.content, FontID::UIDefault, font_set)
                    + 4.0
                    + 4.0,
            );
        }

        Self {
            items,
            required_width: width.max(min_width),
        }
    }

    #[inline(always)]
    pub const fn height(&self) -> f32 {
        self.items.len() as f32 * MenuItemSubView::ITEM_HEIGHT
    }

    #[inline(always)]
    pub const fn required_width(&self) -> f32 {
        self.required_width
    }
}

pub struct FlyoutContentPresenterInit {
    pub layout: MenuLayout,
    pub event_handler: std::rc::Weak<EventHandler>,
}
impl FlyoutSurfacePresenterConstructor for FlyoutContentPresenterInit {
    fn create(&self, view_init_context: &mut ViewInitContext) -> Box<dyn FlyoutSurfacePresenter> {
        let root_view = view_init_context.construct_view(
            MenuFlyoutViewInit {
                layout: self.layout.clone(),
                event_handler: self.event_handler.clone(),
            },
            |_| [],
        );

        Box::new(FlyoutContentPresenter { root_view })
    }

    fn size(&self) -> Size<LogicalUnit> {
        Size::new_logical(self.layout.required_width(), self.layout.height())
    }
}

pub struct FlyoutContentPresenter {
    root_view: TypedViewIdentifier<MenuFlyoutView>,
}
impl FlyoutSurfacePresenter for FlyoutContentPresenter {
    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view.into_untyped()
    }
}

struct MenuFlyoutViewInit {
    layout: MenuLayout,
    event_handler: std::rc::Weak<EventHandler>,
}
impl ViewConstructor for MenuFlyoutViewInit {
    type ConcreteView = MenuFlyoutView;

    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        MenuFlyoutView {
            layout: self.layout,
            event_handler: self.event_handler,
            entity: None,
        }
    }
}

pub struct MenuFlyoutView {
    layout: MenuLayout,
    event_handler: std::rc::Weak<EventHandler>,
    entity: Option<Rc<MenuFlyoutViewEntity>>,
}
impl crate::uicore::View for MenuFlyoutView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .expand_full()
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .expand_full()
                    .create(ctx.ht_manager);

                let mut items = Vec::with_capacity(self.layout.items.len());
                for (n, x) in self.layout.items.iter().enumerate() {
                    let v = MenuItemSubView::new(
                        x.clone(),
                        n as f32 * MenuItemSubView::ITEM_HEIGHT,
                        ctx.composite_tree,
                        ctx.ht_manager,
                    );
                    ctx.composite_tree.add_child(ct_root, v.ct_root);
                    ctx.ht_manager.add_child(ht_root, v.ht_root);

                    items.push(v);
                }

                let entity = Rc::new(MenuFlyoutViewEntity {
                    ct_root,
                    ht_root,
                    items,
                    receiver: self.event_handler.clone(),
                });
                for v in entity.items.iter() {
                    ctx.ht_manager.set_action_handler(v.ht_root, &entity);
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
        let Some(e) = self.entity.take() else {
            return;
        };

        ctx.composite_tree.free_all(e.ct_root);
        ctx.ht_manager.free_all(e.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

struct MenuFlyoutViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    items: Vec<MenuItemSubView>,
    receiver: std::rc::Weak<EventHandler>,
}
impl HitTestTreeActionHandler for MenuFlyoutViewEntity {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for v in self.items.iter() {
            if v.ht_root != sender {
                continue;
            }

            context
                .composite_tree
                .begin_mod_chain(v.ct_root)
                .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.0],
                    to_value: [1.0, 1.0, 1.0, 0.125],
                    sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                }))
                .apply();
            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        for v in self.items.iter() {
            if v.ht_root != sender {
                continue;
            }

            context
                .composite_tree
                .begin_mod_chain(v.ct_root)
                .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.125],
                    to_value: [1.0, 1.0, 1.0, 0.0],
                    sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                }))
                .apply();
            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        for v in self.items.iter() {
            if v.ht_root != sender {
                continue;
            }

            context
                .system_link
                .dispatch_event(Event::DropdownMenuSelectItem {
                    id: v.id,
                    receiver: self.receiver.clone(),
                });
            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::STOP_PROPAGATION
    }
}

pub struct MenuItemSubView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    id: usize,
}
impl MenuItemSubView {
    const ITEM_HEIGHT: f32 = 24.0;

    pub fn new<E>(
        item: MenuItem,
        y_pos: f32,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) -> Self {
        let ct_root = composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(y_pos)],
            relative_size_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(Self::ITEM_HEIGHT),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: item.content,
                    color: AnimatableColor::Value([1.0; 4]),
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                offset: [4.0, 0.0],
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ht_manager.create(HitTestTreeData {
            top: y_pos,
            width_adjustment_factor: 1.0,
            height: Self::ITEM_HEIGHT,
            ..Default::default()
        });

        Self {
            ct_root,
            ht_root,
            id: item.id,
        }
    }
}
