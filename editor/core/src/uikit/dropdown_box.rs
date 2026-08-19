use std::rc::{Rc, Weak};

use crate::{
    Event,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef, PointerActionArgs,
            PointerButtonActionArgs,
        },
    },
    model::{Application, ApplicationMutation},
    rendering::{
        Normalized2DStaticMeshTexture, Normalized2DStaticMeshTextureLazyInit,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectScaleFactor, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTexture, CompositeTreeRef, CornerRadius,
            FloatAnimationTemplate, TextureMappingMode, TextureType,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{
        MountContext, MountTarget, RenderContext, TeardownContext, TypedViewIdentifier,
        ViewIdentifier, ViewInitContext, ViewInstanceQueryableMut, ViewLayoutStateStore,
        ViewRenderer,
    },
    utils::{LogicalUnit, Point, Rect, SafeF32, Size},
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
impl super::View for View {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> super::ViewRenderElements {
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
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                &*self.entity.insert(eh)
            }
        };

        super::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..super::ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(e.ct_root);
        ctx.mount_context.ht_manager.free_all(e.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
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

        context.system_link.dispatch_event(Event::DropdownMenuOpen {
            parent: w,
            surface_pos: Point::new_logical(x, y),
            min_width: 128.0,
            items: self
                .items
                .iter()
                .enumerate()
                .map(|(n, c)| MenuItem {
                    content: c.into(),
                    id: n,
                })
                .collect(),
            selection_receiver: self.this_weakref.clone(),
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

pub struct MenuLayout {
    items: Vec<MenuItem>,
    required_width: f32,
}
impl MenuLayout {
    #[inline(always)]
    pub fn new(items: Vec<MenuItem>, font_set: &FontSet) -> Self {
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
            required_width: width,
        }
    }

    #[inline(always)]
    pub const fn height(&self) -> f32 {
        self.items.len() as f32 * MenuItemView::ITEM_HEIGHT
    }

    #[inline(always)]
    pub const fn required_width(&self) -> f32 {
        self.required_width
    }

    #[inline(always)]
    pub fn instantiate_all(
        self,
        view_init_context: &mut ViewInitContext,
        selection_receiver: Weak<EventHandler>,
        mut post_instantiate_action: impl FnMut(&mut MenuItemView, &mut ViewInitContext),
    ) -> impl Iterator<Item = MenuItemView> {
        self.items.into_iter().enumerate().map(move |(n, v)| {
            let mut v = MenuItemView::new(
                view_init_context,
                selection_receiver.clone(),
                v,
                n as f32 * MenuItemView::ITEM_HEIGHT,
            );
            post_instantiate_action(&mut v, view_init_context);
            v
        })
    }
}

pub struct MenuItemView {
    eh: Rc<MenuItemEventHandler>,
}
impl MenuItemView {
    const ITEM_HEIGHT: f32 = 24.0;

    pub fn new(
        ctx: &mut ViewInitContext,
        selection_receiver: Weak<EventHandler>,
        item: MenuItem,
        y_pos: f32,
    ) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
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
        let ht_root = ctx.mount_context.ht_manager.create(HitTestTreeData {
            top: y_pos,
            width_adjustment_factor: 1.0,
            height: Self::ITEM_HEIGHT,
            ..Default::default()
        });

        let eh = Rc::new(MenuItemEventHandler {
            ct_root,
            ht_root,
            id: item.id,
            receiver: selection_receiver,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }
}

pub struct MenuItemEventHandler {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    id: usize,
    receiver: std::rc::Weak<EventHandler>,
}
impl HitTestTreeActionHandler for MenuItemEventHandler {
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
                to_value: [1.0, 1.0, 1.0, 0.125],
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
                from_value: [1.0, 1.0, 1.0, 0.125],
                to_value: [1.0, 1.0, 1.0, 0.0],
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        context
            .system_link
            .dispatch_event(Event::DropdownMenuSelectItem {
                id: self.id,
                receiver: self.receiver.clone(),
            });

        EventContinueControl::STOP_PROPAGATION
    }
}
