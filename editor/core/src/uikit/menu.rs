use std::rc::Rc;

use crate::{
    Event, FlyoutSurfaceHandle,
    input::hittest::{CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef},
    rendering::{
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture, RenderMessage,
        RenderMessageSender, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTexture,
            CompositeTree, CompositeTreeRef, FloatAnimationTemplate, Gradient, GradientRef,
            TextureMappingMode, TextureType,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{MountTarget, ViewInitContext},
    utils::{LogicalUnit, SafeF32, Size},
};

pub const DELAYED_ACTION_TIMEOUT_MS: u32 = 400;

#[derive(Clone, Debug)]
pub enum MenuItem {
    Heading { label: String },
    Command { label: String, command_id: u64 },
    SubMenu { label: String, items: Vec<MenuItem> },
    Separator,
}

pub enum MenuItemInteractableElement {
    Command(CommandView),
    SubMenu(SubMenuView),
}
impl MenuItemInteractableElement {
    pub fn lit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        match self {
            MenuItemInteractableElement::Command(x) => {
                x.event_handler.lit(composite_tree, current_sec)
            }
            MenuItemInteractableElement::SubMenu(x) => {
                x.event_handler.lit(composite_tree, current_sec)
            }
        }
    }

    pub fn unlit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        match self {
            MenuItemInteractableElement::Command(x) => {
                x.event_handler.unlit(composite_tree, current_sec)
            }
            MenuItemInteractableElement::SubMenu(x) => {
                x.event_handler.unlit(composite_tree, current_sec)
            }
        }
    }
}

pub struct MenuItemLayout {
    pub item: MenuItem,
    pub placement_y: f32,
    pub height: f32,
    pub required_width: SafeF32,
}
impl MenuItemLayout {
    pub fn build(items: impl Iterator<Item = MenuItem>, font_set: &FontSet) -> Vec<Self> {
        items
            .scan(0.0, |top, item| {
                let (pre_spacing, view_height, post_spacing, required_width);
                match item {
                    MenuItem::Heading { ref label } => {
                        pre_spacing = 0.0;
                        view_height = ITEM_HEIGHT;
                        post_spacing = 0.0;
                        required_width = SafeF32::new(
                            TextLayout::measure_visual_width(label, FontID::UIDefault, font_set)
                                + TEXT_INLINE_MARGIN * 2.0,
                        )
                        .expect("invalid width measured");
                    }
                    MenuItem::Command { ref label, .. } => {
                        pre_spacing = 0.0;
                        view_height = ITEM_HEIGHT;
                        post_spacing = 0.0;
                        required_width = SafeF32::new(
                            TextLayout::measure_visual_width(label, FontID::UIDefault, font_set)
                                + TEXT_INLINE_MARGIN * 2.0,
                        )
                        .expect("invalid width measured");
                    }
                    MenuItem::SubMenu { ref label, .. } => {
                        pre_spacing = 0.0;
                        view_height = ITEM_HEIGHT;
                        post_spacing = 0.0;
                        required_width = SafeF32::new(
                            TextLayout::measure_visual_width(label, FontID::UIDefault, font_set)
                                + TEXT_INLINE_MARGIN * 2.0
                                + SubMenuView::ARROW.width,
                        )
                        .expect("invalid width measured")
                            + LR_TEXT_MINIMUM_MARGIN;
                    }
                    MenuItem::Separator => {
                        pre_spacing = 2.0;
                        view_height = 1.0;
                        post_spacing = 2.0;
                        required_width = MINIMUM_WIDTH;
                    }
                }

                let placement_y = *top + pre_spacing;
                *top += pre_spacing + view_height + post_spacing;
                Some(Self {
                    item,
                    placement_y,
                    height: view_height + post_spacing,
                    required_width,
                })
            })
            .collect()
    }

    pub fn height<'x>(layout: impl Iterator<Item = &'x Self>) -> SafeF32 {
        layout
            .filter_map(|x| SafeF32::new(x.placement_y + x.height))
            .max()
            .unwrap_or(unsafe { SafeF32::new_unchecked(0.0) })
            .max(MINIMUM_HEIGHT)
    }

    pub fn min_width<'x>(layout: impl Iterator<Item = &'x Self>) -> SafeF32 {
        layout
            .map(|x| x.required_width)
            .max()
            .unwrap_or(unsafe { SafeF32::new_unchecked(0.0) })
            .max(MINIMUM_WIDTH)
    }

    pub fn instantiate(
        layout: impl Iterator<Item = Self>,
        depth: usize,
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
        onto: &FlyoutSurfaceHandle,
    ) -> Vec<Option<MenuItemInteractableElement>> {
        layout
            .enumerate()
            .scan(0.0, |ad_accum, (index, x)| match x.item {
                MenuItem::Heading { label } => {
                    create_heading_visual(label, x.placement_y, onto.ct_root(), ctx.composite_tree);
                    Some(None)
                }
                MenuItem::Command { label, command_id } => {
                    let ad = *ad_accum;
                    *ad_accum += ANIMATION_DELAY_PER_ELEMENT;

                    Some(Some(MenuItemInteractableElement::Command(
                        CommandView::new(
                            ctx,
                            common_res,
                            label,
                            command_id,
                            depth,
                            index,
                            ad,
                            x.placement_y,
                            (onto.ct_root(), onto.ht_root()),
                        ),
                    )))
                }
                MenuItem::SubMenu { label, .. } => {
                    let ad = *ad_accum;
                    *ad_accum += ANIMATION_DELAY_PER_ELEMENT;

                    Some(Some(MenuItemInteractableElement::SubMenu(
                        SubMenuView::new(
                            ctx,
                            common_res,
                            label,
                            depth,
                            index,
                            ad,
                            x.placement_y,
                            (onto.ct_root(), onto.ht_root()),
                        ),
                    )))
                }
                MenuItem::Separator => {
                    create_separator_visual(x.placement_y, onto.ct_root(), ctx.composite_tree);
                    Some(None)
                }
            })
            .collect()
    }
}

pub struct CommonResources {
    light_gradient: GradientRef,
    tid_submenu_arrow: TextureID,
}
impl CommonResources {
    pub fn new<E>(
        composite_tree: &mut CompositeTree<E>,
        tid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let light_gradient = composite_tree.create_gradient(Gradient::Radial {
            start_color: [0.75, 1.0, 1.5, 1.0],
            end_color: [0.25, 0.5, 1.0, 0.0],
            center_relative: [0.5, 0.9],
            radius: [0.5, 0.1],
        });
        let tid_submenu_arrow = tid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: tid_submenu_arrow,
                data: SubMenuView::ARROW,
            })
            .expect("rt_sender.send");

        Self {
            light_gradient,
            tid_submenu_arrow,
        }
    }

    pub const fn composite_mode_light<E>(&self) -> CompositeMode<E> {
        CompositeMode::FillRadialGradient(self.light_gradient)
    }
}

const ITEM_HEIGHT: f32 = 20.0;
const TEXT_INLINE_MARGIN: f32 = 8.0;
const MINIMUM_WIDTH: SafeF32 = unsafe { SafeF32::new_unchecked(16.0) };
const MINIMUM_HEIGHT: SafeF32 = unsafe { SafeF32::new_unchecked(8.0) };
const LR_TEXT_MINIMUM_MARGIN: SafeF32 = unsafe { SafeF32::new_unchecked(16.0) };

const ANIMATION_DELAY_PER_ELEMENT: f32 = 0.025;
const INTRO_X_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: 4.0,
    to_value: 0.0,
    curve: AnimationCurve::EASE_OUT,
    duration: 0.125,
};
const INTRO_OPACITY_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: 0.0,
    to_value: 1.0,
    curve: AnimationCurve::Linear,
    duration: 0.125,
};

const LIT_OPACITY_ANIM: FloatAnimationTemplate = FloatAnimationTemplate {
    from_value: 0.0,
    to_value: 1.0,
    curve: AnimationCurve::Linear,
    duration: 0.1,
};
const UNLIT_OPACITY_ANIM: FloatAnimationTemplate = LIT_OPACITY_ANIM.flip(AnimationCurve::Linear);

pub fn create_heading_visual<E>(
    label: String,
    placement_y: f32,
    onto: CompositeTreeRef,
    composite_tree: &mut CompositeTree<E>,
) {
    let ct_root = CompositeRect::build()
        .use_ui_scale()
        .expand_width()
        .size_imm(0.0, ITEM_HEIGHT)
        .offset_imm(0.0, placement_y)
        .composite_fill_color_imm([0.0, 0.0, 0.0, 0.9])
        .text(
            CompositeRectText::build()
                .run(CompositeRectTextRun::build(label).color_imm([1.0, 1.0, 1.0, 1.0]))
                .horizontal_middle()
                .vertical_middle(),
        )
        .create(composite_tree);

    composite_tree.add_child(onto, ct_root);
}

pub struct CommandView {
    event_handler: Rc<CommandViewEventHandler>,
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
    ct_label: CompositeTreeRef,
}
impl CommandView {
    pub fn new(
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
        label: String,
        command_id: u64,
        depth: usize,
        index: usize,
        animation_delay: f32,
        placement_y: f32,
        onto: (CompositeTreeRef, HitTestTreeRef),
    ) -> Self {
        let animation_base_time = ctx.current_sec + animation_delay;

        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height: ITEM_HEIGHT,
            top: placement_y,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });
        let ct_root = CompositeRect::build()
            .use_ui_scale()
            .expand_width()
            .size_imm(0.0, ITEM_HEIGHT)
            .offset(
                AnimatableFloat::from_template(&INTRO_X_ANIM, animation_base_time),
                AnimatableFloat::Value(placement_y),
            )
            .opacity_anim(&INTRO_OPACITY_ANIM, animation_base_time)
            .create(ctx.composite_tree);
        let ct_label = CompositeRect::build()
            .use_ui_scale()
            .expand_full()
            .size_imm(-TEXT_INLINE_MARGIN * 2.0, 0.0)
            .offset_imm(TEXT_INLINE_MARGIN, 0.0)
            .text(
                CompositeRectText::build()
                    .run(CompositeRectTextRun::build(label).color_imm([1.0, 1.0, 1.0, 1.0]))
                    .vertical_middle(),
            )
            .create(ctx.composite_tree);
        let ct_light = CompositeRect::build()
            .expand_full()
            .composite(common_res.composite_mode_light())
            .opacity_imm(0.0)
            .create(ctx.composite_tree);
        ctx.composite_tree.add_child(ct_root, ct_light);
        ctx.composite_tree.add_child(ct_root, ct_label);
        let eh = std::rc::Rc::new(CommandViewEventHandler {
            ct_light,
            command_id,
            depth,
            index,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        ctx.composite_tree.add_child(onto.0, ct_root);
        ctx.ht_manager.add_child(onto.1, ht_root);

        Self {
            event_handler: eh,
            ht_root,
            ct_root,
            ct_label,
        }
    }
}

pub struct SubMenuView {
    event_handler: Rc<SubMenuViewEventHandler>,
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
    ct_label: CompositeTreeRef,
    ct_arrow: CompositeTreeRef,
    pub placement_y: f32,
}
impl SubMenuView {
    const ICON_SIZE: Size<LogicalUnit> = Size::new_logical(6.0, 8.0);
    const ARROW: Normalized2DStaticMeshTexture = Normalized2DStaticMeshTexture {
        vertices: &[
            [0.0, 0.0],
            [0.0 + 1.5 / Self::ICON_SIZE.width, 0.0],
            [1.0 - 1.5 / Self::ICON_SIZE.width, 0.5],
            [1.0, 0.5],
            [0.0, 1.0],
            [0.0 + 1.5 / Self::ICON_SIZE.width, 1.0],
        ],
        indices: &[0, 1, 2, 2, 1, 3, 2, 4, 5, 5, 3, 2],
        width: Self::ICON_SIZE.width,
        height: Self::ICON_SIZE.height,
    };

    pub fn new(
        ctx: &mut ViewInitContext,
        common_res: &CommonResources,
        label: String,
        depth: usize,
        index: usize,
        animation_delay: f32,
        placement_y: f32,
        onto: (CompositeTreeRef, HitTestTreeRef),
    ) -> Self {
        let animation_base_time = ctx.current_sec + animation_delay;

        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height: ITEM_HEIGHT,
            top: placement_y,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });
        let ct_root = CompositeRect::build()
            .use_ui_scale()
            .expand_width()
            .size_imm(0.0, ITEM_HEIGHT)
            .offset(
                AnimatableFloat::from_template(&INTRO_X_ANIM, animation_base_time),
                AnimatableFloat::Value(placement_y),
            )
            .opacity_anim(&INTRO_OPACITY_ANIM, animation_base_time)
            .create(ctx.composite_tree);
        let ct_label = CompositeRect::build()
            .use_ui_scale()
            .expand_full()
            .size_imm(-TEXT_INLINE_MARGIN * 2.0, 0.0)
            .offset_imm(TEXT_INLINE_MARGIN, 0.0)
            .text(
                CompositeRectText::build()
                    .run(CompositeRectTextRun::build(label).color_imm([1.0, 1.0, 1.0, 1.0]))
                    .vertical_middle(),
            )
            .create(ctx.composite_tree);
        let ct_arrow = CompositeRect::build()
            .use_ui_scale()
            .relative_offset_adjustment(1.0, 0.5)
            .offset_imm(
                -Self::ICON_SIZE.width - TEXT_INLINE_MARGIN,
                -Self::ICON_SIZE.height * 0.5,
            )
            .size_imm(Self::ICON_SIZE.width, Self::ICON_SIZE.height)
            .composite(CompositeMode::ColorTint(
                AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                CompositeTexture {
                    id: common_res.tid_submenu_arrow,
                    r#type: TextureType::Mask,
                    mapping: TextureMappingMode::Stretch,
                    slice_borders: [0.0; 4],
                },
            ))
            .create(ctx.composite_tree);
        let ct_light = CompositeRect::build()
            .expand_full()
            .composite(common_res.composite_mode_light())
            .opacity_imm(0.0)
            .create(ctx.composite_tree);
        ctx.composite_tree.add_child(ct_root, ct_light);
        ctx.composite_tree.add_child(ct_root, ct_label);
        ctx.composite_tree.add_child(ct_root, ct_arrow);
        let eh = std::rc::Rc::new(SubMenuViewEventHandler {
            ct_light,
            depth,
            index,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        ctx.composite_tree.add_child(onto.0, ct_root);
        ctx.ht_manager.add_child(onto.1, ht_root);

        Self {
            event_handler: eh,
            ht_root,
            ct_root,
            ct_label,
            ct_arrow,
            placement_y,
        }
    }
}

pub fn create_separator_visual<E>(
    placement_y: f32,
    onto: CompositeTreeRef,
    composite_tree: &mut CompositeTree<E>,
) {
    let ct_root = CompositeRect::build()
        .use_ui_scale()
        .expand_width()
        .size_imm(0.0, 1.0)
        .offset_imm(0.0, placement_y)
        .composite_fill_color_imm([1.0, 1.0, 1.0, 0.5])
        .create(composite_tree);

    composite_tree.add_child(onto, ct_root);
}

pub struct BaseSurfaceEventHandler {
    depth: usize,
}
impl BaseSurfaceEventHandler {
    pub fn new(depth: usize) -> Self {
        Self { depth }
    }
}
impl HitTestTreeActionHandler for BaseSurfaceEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context
            .system_link
            .dispatch_event(Event::MenuDeselectItem { depth: self.depth });

        crate::input::EventContinueControl::STOP_PROPAGATION
    }
}

struct CommandViewEventHandler {
    ct_light: CompositeTreeRef,
    depth: usize,
    index: usize,
    command_id: u64,
}
impl HitTestTreeActionHandler for CommandViewEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context.system_link.dispatch_event(Event::MenuSelectItem {
            depth: self.depth,
            index: self.index,
        });

        crate::input::EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerButtonActionArgs,
    ) -> crate::input::EventContinueControl {
        context
            .system_link
            .dispatch_event(Event::MenuSelectCommand {
                id: self.command_id,
            });

        crate::input::EventContinueControl::STOP_PROPAGATION
    }
}
impl CommandViewEventHandler {
    pub fn lit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        composite_tree
            .begin_mod_chain(self.ct_light)
            .opacity_animated_from_template(&LIT_OPACITY_ANIM, current_sec)
            .apply();
    }

    pub fn unlit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        composite_tree
            .begin_mod_chain(self.ct_light)
            .opacity_animated_from_template(&UNLIT_OPACITY_ANIM, current_sec)
            .apply();
    }
}

struct SubMenuViewEventHandler {
    ct_light: CompositeTreeRef,
    depth: usize,
    index: usize,
}
impl HitTestTreeActionHandler for SubMenuViewEventHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut crate::input::InputEventContext,
        _args: &crate::input::hittest::PointerActionArgs,
    ) -> crate::input::EventContinueControl {
        context.system_link.dispatch_event(Event::MenuSelectItem {
            depth: self.depth,
            index: self.index,
        });

        crate::input::EventContinueControl::STOP_PROPAGATION
    }
}
impl SubMenuViewEventHandler {
    pub fn lit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        composite_tree
            .begin_mod_chain(self.ct_light)
            .opacity_animated_from_template(&LIT_OPACITY_ANIM, current_sec)
            .apply();
    }

    pub fn unlit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        composite_tree
            .begin_mod_chain(self.ct_light)
            .opacity_animated_from_template(&UNLIT_OPACITY_ANIM, current_sec)
            .apply();
    }
}
