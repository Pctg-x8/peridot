use std::rc::Rc;

use crate::{
    Event, SyncEvent, SystemLink,
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputEventHandler,
        KeyboardFocusGroupRef, KeyboardFocusTokenRegistry,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTree,
            CompositeTreeRef, CornerRadius,
        },
        text::FontID,
    },
    uikit::{MountContext, MountTarget, Positioning, ViewInitContext},
    utils::{LogicalUnit, Size},
};

pub struct SimpleButtonView {
    ht_root: HitTestTreeRef,
    size: Size<LogicalUnit>,
    action_handler: Rc<SimpleButtonActionHandler>,
    kf_token: FocusTargetToken,
}
impl SimpleButtonView {
    pub fn new(
        ctx: &mut ViewInitContext,
        init_label: String,
        size: Size<LogicalUnit>,
        click_event: Option<Event>,
    ) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(size.width),
                AnimatableFloat::Value(size.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            corner_radius: CornerRadius::all(8.0),
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: init_label,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    spacing_inline_start: 0.0,
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_focus = ctx.mount_context.composite_tree.create(CompositeRect {
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
        let kf = ctx.keyboard_focus_registry.acquire_token();
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width: size.width,
            height: size.height,
            cursor_shape: CursorShape::Pointer,
            keyboard_focus: Some(kf),
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_focus);

        let action_handler = Rc::new(SimpleButtonActionHandler {
            ct_root,
            ct_focus,
            click_event,
            state: core::cell::Cell::new(ButtonState::None),
        });
        ctx.ht_manager.set_action_handler(ht_root, &action_handler);
        ctx.keyboard_focus_registry
            .set_event_handler(kf, &action_handler);

        Self {
            ht_root,
            size,
            action_handler,
            kf_token: kf,
        }
    }

    pub fn terminate(&mut self, ctx: &mut MountContext) {
        ctx.ht_manager.free_all(self.ht_root);
        ctx.composite_tree.free_all(self.action_handler.ct_root);
        ctx.keyboard_focus_registry.release_token(self.kf_token);
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(parent.ct_root(), self.action_handler.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.ht_root);
    }

    pub fn unmount(&self, ctx: &mut MountContext) {
        ctx.ht_manager.remove_child(self.ht_root);
        ctx.composite_tree.remove_child(self.action_handler.ct_root);
    }

    pub fn set_keyboard_focus_group(
        &self,
        group: KeyboardFocusGroupRef,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        keyboard_focus_registry.join_group(group, self.kf_token);
    }

    pub fn locate(
        &self,
        pos: &Positioning,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        let ht = ht_manager.get_data_mut(self.ht_root);
        let ct = composite_tree.get_mut(self.action_handler.ct_root);

        ht.left_adjustment_factor = pos.parent_anchor[0];
        ht.top_adjustment_factor = pos.parent_anchor[1];
        ht.left = pos.offset[0] - self.size.width * pos.anchor[0];
        ht.top = pos.offset[1] - self.size.height * pos.anchor[1];
        ct.relative_offset_adjustment = [pos.parent_anchor[0], pos.parent_anchor[1]];
        ct.offset = [
            AnimatableFloat::Value(pos.offset[0] - self.size.width * pos.anchor[0]),
            AnimatableFloat::Value(pos.offset[1] - self.size.height * pos.anchor[1]),
        ];

        composite_tree.mark_dirty(self.action_handler.ct_root);
    }

    pub fn set_interactive(&self, interactive: bool, ht_manager: &mut HitTestTreeManager) {
        ht_manager.get_data_mut(self.ht_root).active = interactive;
    }
}

struct SimpleButtonActionHandler {
    ct_root: CompositeTreeRef,
    ct_focus: CompositeTreeRef,
    click_event: Option<Event>,
    state: core::cell::Cell<ButtonState>,
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
        if code == crate::input::KeyInputCode::Enter {
            // hit enter
            self.perform_click_action(context.system_link);
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
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.perform_click_action(context.system_link);

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
                    start_sec: current_sec,
                    end_sec: current_sec + 0.05,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                });
            composite_tree.mark_dirty(self.ct_root);
        }

        self.state.set(new_state);
    }

    fn perform_click_action(&self, syslink: &SystemLink) {
        if let Some(ref c) = self.click_event {
            syslink.dispatch_event(c.clone());
        }
    }
}

#[derive(Clone, Copy)]
enum ButtonState {
    None,
    Hovering,
    Pressing,
}
