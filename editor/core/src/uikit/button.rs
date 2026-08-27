use core::cell::Cell;
use std::rc::Rc;

use crate::{
    Event, SyncEvent, SystemLink, WindowHandle,
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputEventHandler,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTree,
            CompositeTreeRef, CornerRadius,
        },
        text::{FontID, TextLayout},
    },
    uikit::{
        RenderContext, TeardownContext, View, ViewConstructor, ViewLayoutStateStore,
        ViewRenderElements,
    },
    utils::{LogicalUnit, Rect, Size, range_helper::range_from_len},
};

pub trait SimpleButtonEventHandler {
    fn on_click_event(&self, window: WindowHandle) -> Event;
}

pub struct SimpleButtonConstantEventHandler(pub Event);
impl SimpleButtonEventHandler for SimpleButtonConstantEventHandler {
    fn on_click_event(&self, _window: WindowHandle) -> Event {
        self.0.clone()
    }
}

pub struct SimpleButtonViewInit {
    pub label: String,
    pub event_handler: Option<Box<dyn SimpleButtonEventHandler>>,
}
impl Default for SimpleButtonViewInit {
    #[inline(always)]
    fn default() -> Self {
        Self {
            label: String::new(),
            event_handler: None,
        }
    }
}
impl ViewConstructor for SimpleButtonViewInit {
    type ConcreteView = SimpleButtonView;

    #[inline(always)]
    fn construct(self, _id: super::TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        SimpleButtonView::new(self)
    }
}

pub struct SimpleButtonView {
    entity: Option<Rc<SimpleButtonActionHandler>>,
    label: String,
    event_handler: Option<Box<dyn SimpleButtonEventHandler>>,
    interactive_changes: Option<bool>,
}
impl SimpleButtonView {
    const ROUNDING: f32 = 8.0;

    pub fn new(init: SimpleButtonViewInit) -> Self {
        Self {
            entity: None,
            label: init.label,
            event_handler: init.event_handler,
            interactive_changes: None,
        }
    }

    pub fn set_interactive(&mut self, interactive: bool) {
        self.interactive_changes = Some(interactive);
    }
}
impl View for SimpleButtonView {
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

                if let Some(interactive) = self.interactive_changes.take() {
                    ctx.ht_manager.get_data_mut(e.ht_root).active = interactive;
                    e.interactive.set(interactive);
                }

                // TODO: reflect other changes
                e
            }
            None => {
                // first render
                let kf_token = ctx.keyboard_focus_registry.acquire_token();

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [
                        AnimatableFloat::Value(layout_rect.width),
                        AnimatableFloat::Value(layout_rect.height),
                    ],
                    offset: [
                        AnimatableFloat::Value(layout_rect.left),
                        AnimatableFloat::Value(layout_rect.top),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.0,
                    ])),
                    corner_radius: CornerRadius::all(Self::ROUNDING),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            font_id: FontID::UIDefault,
                            content: self.label.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            spacing_inline_start: 0.0,
                        }],
                        horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_focus = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(3.0), AnimatableFloat::Value(3.0)],
                    size: [AnimatableFloat::Value(-6.0), AnimatableFloat::Value(-6.0)],
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0; 4])),
                    corner_radius: CornerRadius::all(6.0),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                        break_pattern: [2.0, 2.0],
                    }),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width: layout_rect.width,
                    height: layout_rect.height,
                    left: layout_rect.left,
                    top: layout_rect.top,
                    cursor_shape: CursorShape::Pointer,
                    keyboard_focus: Some(kf_token),
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_focus);

                let action_handler = Rc::new(SimpleButtonActionHandler {
                    kf_token,
                    ht_root,
                    ct_root,
                    ct_focus,
                    event_handler: self.event_handler.take(),
                    state: Cell::new(ButtonState::None),
                    interactive: Cell::new(true),
                });
                ctx.ht_manager.set_action_handler(ht_root, &action_handler);
                ctx.keyboard_focus_registry
                    .set_event_handler(kf_token, &action_handler);

                if let Some(interactive) = self.interactive_changes.take() {
                    ctx.ht_manager.get_data_mut(ht_root).active = interactive;
                    action_handler.interactive.set(interactive);
                }

                &*self.entity.insert(action_handler)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            keyboard_focus: Some(e.kf_token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        if let Some(entity) = self.entity.take() {
            // some rendered
            ctx.ht_manager.free_all(entity.ht_root);
            ctx.composite_tree.free_all(entity.ct_root);
            ctx.keyboard_focus_registry.release_token(entity.kf_token);
        }
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
        let label_size = TextLayout::new_single(
            &self.label,
            FontID::UIDefault,
            ctx.system_link.font_set(),
            CompositeRectTextHorizontalAlignment::Start,
            None,
            None,
        )
        .size();
        // consider rounding pads
        Size::new_logical(
            label_size.width + Self::ROUNDING * 2.0,
            label_size.height + Self::ROUNDING,
        )
    }
}

struct SimpleButtonActionHandler {
    kf_token: FocusTargetToken,
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
    ct_focus: CompositeTreeRef,
    event_handler: Option<Box<dyn SimpleButtonEventHandler>>,
    state: Cell<ButtonState>,
    interactive: Cell<bool>,
}
impl KeyInputEventHandler for SimpleButtonActionHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        context.composite_tree.get_mut(self.ct_focus).opacity = AnimatableFloat::Value(1.0);
        context.composite_tree.mark_dirty(self.ct_focus);
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        context.composite_tree.get_mut(self.ct_focus).opacity = AnimatableFloat::Value(0.0);
        context.composite_tree.mark_dirty(self.ct_focus);
    }

    fn keydown(
        &self,
        context: &mut InputEventContext,
        code: crate::input::KeyInputCode,
        _modifier: crate::input::ModifierKey,
    ) {
        if !self.interactive.get() {
            // interaction disabled
            return;
        }

        if code == crate::input::KeyInputCode::Enter {
            // hit enter
            self.perform_click_action(
                context.system_link,
                context
                    .ht_manager
                    .query_root_window(self.ht_root)
                    .expect("not mounted"),
            );
        }
    }
}
impl HitTestTreeActionHandler for SimpleButtonActionHandler {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.transit(
            ButtonState::Hovering,
            context.composite_tree,
            context.current_sec,
        );

        EventContinueControl::empty()
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.transit(
            ButtonState::None,
            context.composite_tree,
            context.current_sec,
        );

        EventContinueControl::empty()
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.transit(
            ButtonState::Pressing,
            context.composite_tree,
            context.current_sec,
        );

        EventContinueControl::empty()
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.transit(
            ButtonState::Hovering,
            context.composite_tree,
            context.current_sec,
        );

        EventContinueControl::empty()
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.perform_click_action(
            context.system_link,
            context
                .ht_manager
                .query_root_window(sender)
                .expect("not mounted"),
        );

        EventContinueControl::empty()
    }
}
impl SimpleButtonActionHandler {
    const fn alpha(state: ButtonState) -> f32 {
        match state {
            ButtonState::None => 0.0,
            ButtonState::Hovering => 0.125,
            ButtonState::Pressing => 0.25,
        }
    }

    fn transit(
        &self,
        new_state: ButtonState,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        let before = Self::alpha(self.state.get());
        let after = Self::alpha(new_state);

        if before != after {
            // transit occured
            composite_tree.get_mut(self.ct_root).composite_mode =
                CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, before],
                    to_value: [1.0, 1.0, 1.0, after],
                    curve: AnimationCurve::Linear,
                    sec_duration: range_from_len(current_sec, 0.05),
                    event_on_complete: None,
                });
            composite_tree.mark_dirty(self.ct_root);
        }

        self.state.set(new_state);
    }

    fn perform_click_action(&self, syslink: &SystemLink, window: WindowHandle) {
        if let Some(ref c) = self.event_handler {
            syslink.dispatch_event(c.on_click_event(window));
        }
    }
}

#[derive(Clone, Copy)]
enum ButtonState {
    None,
    Hovering,
    Pressing,
}
