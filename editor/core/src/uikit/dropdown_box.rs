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
    rendering::{
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture, RenderMessage,
        RenderMessageSender, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectScaleFactor, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTexture, CompositeTree, CompositeTreeRef,
            CornerRadius, FloatAnimationTemplate, TextureMappingMode, TextureType,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{
        MountContext, MountTarget, RenderContext, TeardownContext, ViewElementSize,
        ViewInitContext, ViewPlacement,
    },
    utils::{Point, SafeF32, Size, UnsafeMainThreadOnlyOnceCell},
};

const ARROW_PRESS_Y_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: -8.0,
    to_value: -7.0,
    curve: AnimationCurve::EASE_OUT,
    duration: 0.1,
};
const ARROW_RELEASE_Y_ANIM: FloatAnimationTemplate =
    ARROW_PRESS_Y_ANIM.flip(AnimationCurve::EASE_OUT);

struct SharedResources {
    down_arrow_tex: TextureID,
}
impl SharedResources {
    const DOWN_ARROW: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
        vertices: &[[0.25, 0.375], [0.75, 0.375], [0.5, 0.625]],
        indices: &[0, 1, 2],
        width: 16.0,
        height: 16.0,
    };

    fn new(id_issuer: &mut MainThreadTextureIDIssuer, rt_sender: &RenderMessageSender) -> Self {
        let down_arrow_tex = id_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: down_arrow_tex,
                data: Self::DOWN_ARROW,
            })
            .expect("rt_sender.send");

        Self { down_arrow_tex }
    }
}

static SHARED_RESOURCES: UnsafeMainThreadOnlyOnceCell<SharedResources> =
    UnsafeMainThreadOnlyOnceCell(core::cell::OnceCell::new());

pub struct View {
    entity: Option<Rc<EventHandler>>,
    placement: ViewPlacement,
    items: Vec<String>,
}
impl View {
    pub fn new(placement: ViewPlacement, items: Vec<String>) -> Self {
        Self {
            entity: None,
            placement,
            items,
        }
    }

    pub fn render(&mut self, ctx: &mut RenderContext, parent: &(impl MountTarget + ?Sized)) {
        match self.entity {
            Some(_) => {
                // TODO: reflect changes
            }
            None => {
                // first render
                let shared_res = SHARED_RESOURCES.0.get_or_init(|| {
                    SharedResources::new(
                        ctx.main_thread_texture_id_issuer,
                        ctx.system_link.rt_sender(),
                    )
                });

                let size = match self.placement.size {
                    ViewElementSize::Fixed(s) => s,
                    ViewElementSize::Automatic => {
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
                };
                let offset = Point::new_logical(
                    self.placement.location.offset.x
                        - size.width * self.placement.location.anchor[0],
                    self.placement.location.offset.y
                        - size.height * self.placement.location.anchor[1],
                );
                let relative_offset = self.placement.location.parent_anchor.clone();

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(offset.x),
                        AnimatableFloat::Value(offset.y),
                    ],
                    relative_offset_adjustment: relative_offset.clone(),
                    size: [
                        AnimatableFloat::Value(size.width),
                        AnimatableFloat::Value(size.height),
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
                                ""
                            } else {
                                self.items[0].as_str()
                            }
                            .into(),
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
                        AnimatableFloat::Value(SharedResources::DOWN_ARROW.width),
                        AnimatableFloat::Value(SharedResources::DOWN_ARROW.height),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::ColorTint(
                        AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        CompositeTexture {
                            id: shared_res.down_arrow_tex,
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
                    left: offset.x,
                    top: offset.y,
                    left_adjustment_factor: relative_offset[0],
                    top_adjustment_factor: relative_offset[1],
                    width: size.width,
                    height: size.height,
                    ..Default::default()
                });

                let eh = Rc::new_cyclic(|w| EventHandler {
                    this_weakref: w.clone(),
                    ct_root,
                    ct_text,
                    ct_down_arrow,
                    ht_root,
                    items: self.items.clone(),
                    current_selected: core::cell::Cell::new(0),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                ctx.composite_tree.add_child(parent.ct_root(), eh.ct_root);
                ctx.ht_manager.add_child(parent.ht_root(), eh.ht_root);

                self.entity = Some(eh);
            }
        }
    }

    pub fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(e.ct_root);
        ctx.mount_context.ht_manager.free_all(e.ht_root);
    }
}

pub struct EventHandler {
    this_weakref: std::rc::Weak<EventHandler>,
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
    pub fn set_selection_id<E>(&self, id: usize, composite_tree: &mut CompositeTree<E>) {
        self.current_selected.set(id);
        self.update_text(composite_tree);
    }

    fn update_text<E>(&self, composite_tree: &mut CompositeTree<E>) {
        let content = if self.items.is_empty() {
            ""
        } else {
            self.items[self.current_selected.get()].as_str()
        };
        composite_tree
            .get_mut(self.ct_text)
            .text
            .as_mut()
            .expect("no text set?")
            .runs[0]
            .content = content.into();
        composite_tree.mark_text_layout_dirty(self.ct_text);
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
