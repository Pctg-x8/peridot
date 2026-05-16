use std::rc::Rc;

use bitflags::bitflags;

#[cfg(target_os = "macos")]
use crate::{Event, LogicFiberEventDispatcher, input::hittest::HitTestTreeManager};
use crate::{
    LogicFiberEventDispatcher, SystemLink, WindowHandle,
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputCode,
        KeyInputEventHandler, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, ModifierKey,
        PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, HitTestTreeScreenRepositionHandler, PointerActionArgs,
            PointerButtonActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTree,
            CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{
        MountContext, MountTarget, ViewEventHandler, ViewIdentifier, ViewInitContext,
        ViewUpdateContext,
    },
    utils::{LogicalUnit, Point, SafeF32},
};

pub struct TextInputView {
    ct_text_clip: CompositeTreeRef,
    eh: Rc<TextInputViewEventHandler>,
}
impl TextInputView {
    pub fn new(ctx: &mut ViewInitContext, pos: Point<LogicalUnit>) -> Self {
        let kf_token = ctx.keyboard_focus_registry.acquire_token();
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(20.0)],
            offset: [AnimatableFloat::Value(pos.x), AnimatableFloat::Value(pos.y)],
            has_bitmap: true,
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_text_clip = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [
                AnimatableFloat::Value(128.0 - 4.0),
                AnimatableFloat::Value(20.0 - 4.0),
            ],
            offset: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(2.0)],
            clip_child: Some(ClipConfig {
                left_softness: unsafe { SafeF32::new_unchecked(1.0) },
                right_softness: unsafe { SafeF32::new_unchecked(1.0) },
                top_softness: unsafe { SafeF32::new_unchecked(1.0) },
                bottom_softness: unsafe { SafeF32::new_unchecked(1.0) },
            }),
            ..Default::default()
        });
        let ct_text = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            has_bitmap: true,
            ..Default::default()
        });
        let ct_cursor = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(16.0)],
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_preedit_underline = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(1.0), AnimatableFloat::Value(1.0)],
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(14.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_selection_bg = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(16.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.2, 0.4, 1.0, 0.25])),
            ..Default::default()
        });
        let ht_root = ctx.mount_context.ht_manager.create(HitTestTreeData {
            width: 128.0,
            height: 20.0,
            left: pos.x,
            top: pos.y,
            cursor_shape: CursorShape::IBeam,
            keyboard_focus: Some(kf_token),
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_text_clip, ct_selection_bg);
        ctx.composite_tree.add_child(ct_text_clip, ct_text);
        ctx.composite_tree.add_child(ct_text_clip, ct_cursor);
        ctx.composite_tree
            .add_child(ct_text_clip, ct_preedit_underline);
        ctx.composite_tree.add_child(ct_root, ct_text_clip);

        let eh = Rc::new(TextInputViewEventHandler {
            id: ctx.view_registry.alloc(),
            token: kf_token,
            ct_root,
            ct_text,
            ct_cursor,
            ct_preedit_underline,
            ct_selection_bg,
            ht_root,
            render_scale: core::cell::Cell::new(ctx.ui_scale_factor),
            content_h_offset: core::cell::Cell::new(0.0),
            content_visible_width: 128.0 - 4.0,
            content: core::cell::RefCell::new("aaa".into()),
            cursor_pos_bytes: core::cell::Cell::new(0),
            preedit_range_start_bytes: core::cell::Cell::new(0),
            preedit_range_end_bytes: core::cell::Cell::new(0),
            selection_begin_bytes: core::cell::Cell::new(0),
            #[cfg(windows)]
            native_text_input_context: crate::platform::windows::NativeTextInputContext::new(
                ctx.system_link,
            ),
            #[cfg(target_os = "macos")]
            ht_manager_ptr: core::ptr::from_mut(ctx.ht_manager).cast(),
            pending_update_mask: core::cell::Cell::new(TextInputViewUpdateMask::empty()),
            event_dispatcher: ctx.system_link.event_dispatcher,
        });
        ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);
        ctx.ht_manager.set_action_handler(ht_root, &eh);
        ctx.view_registry.set_event_handler(eh.id, &eh);
        #[cfg(windows)]
        ctx.ht_manager
            .set_native_text_deferrable_event_handler(ht_root, &eh);
        #[cfg(windows)]
        eh.native_text_input_context
            .bind_action(ctx.system_link, &eh, ht_root);

        ctx.ht_manager.set_screen_reposition_handler(ht_root, &eh);

        eh.update_text(ctx.composite_tree);

        Self { ct_text_clip, eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(parent.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.eh.ht_root);

        #[cfg(windows)]
        unsafe {
            ctx.ht_manager
                .query_root_window(parent.ht_root())
                .expect("no root window")
                .extra_data_mut::<crate::PerWindowData>()
                .screen_reposition_interests
                .insert(self.eh.ht_root);
        }
    }

    pub fn set_keyboard_focus_group(
        &self,
        group: KeyboardFocusGroupRef,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        keyboard_focus_registry.join_group(group, self.eh.token);
    }

    pub fn rescale<E>(
        &self,
        ct: &mut CompositeTree<E>,
        syslink: &SystemLink,
        ht_manager: &HitTestTreeManager,
        new_scale: f32,
    ) {
        self.eh.render_scale.set(new_scale);

        ct.get_mut(self.eh.ct_root).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_root);
        ct.get_mut(self.eh.ct_text).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_text);
        ct.get_mut(self.eh.ct_cursor).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_cursor);
        ct.get_mut(self.eh.ct_preedit_underline).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_preedit_underline);
        ct.get_mut(self.ct_text_clip).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.ct_text_clip);
        ct.get_mut(self.eh.ct_selection_bg).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_selection_bg);

        // update for cursors
        self.eh.update_views(
            TextInputViewUpdateMask::CURSOR | TextInputViewUpdateMask::PREEDIT,
            ct,
            syslink,
            ht_manager,
            0.0, // not interested in scale change
        );
    }
}

struct TextInputViewEventHandler {
    id: ViewIdentifier,
    token: FocusTargetToken,
    ct_root: CompositeTreeRef,
    ct_text: CompositeTreeRef,
    ct_cursor: CompositeTreeRef,
    ct_preedit_underline: CompositeTreeRef,
    ct_selection_bg: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    render_scale: core::cell::Cell<f32>,
    content_h_offset: core::cell::Cell<f32>,
    content_visible_width: f32,
    content: core::cell::RefCell<String>,
    cursor_pos_bytes: core::cell::Cell<usize>,
    preedit_range_start_bytes: core::cell::Cell<usize>,
    preedit_range_end_bytes: core::cell::Cell<usize>,
    selection_begin_bytes: core::cell::Cell<usize>,
    #[cfg(windows)]
    native_text_input_context: crate::platform::windows::NativeTextInputContext,
    #[cfg(target_os = "macos")]
    ht_manager_ptr: *const HitTestTreeManager<'static>,
    pending_update_mask: core::cell::Cell<TextInputViewUpdateMask>,
    event_dispatcher: *mut LogicFiberEventDispatcher,
}
impl ViewEventHandler for TextInputViewEventHandler {
    fn update(&self, context: &mut ViewUpdateContext) {
        self.update_views(
            self.pending_update_mask
                .replace(TextInputViewUpdateMask::empty()),
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }
}
impl KeyInputEventHandler for TextInputViewEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        tracing::debug!("text input focus taken");

        #[cfg(windows)]
        self.native_text_input_context.notify_focus_enter();
        #[cfg(target_os = "macos")]
        context
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted")
            .begin_text_input(core::ptr::from_ref(self).cast_mut());

        self.update_views(
            TextInputViewUpdateMask::FOCUS,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        tracing::debug!("text input focus released");

        #[cfg(windows)]
        self.native_text_input_context.notify_focus_leave();
        #[cfg(target_os = "macos")]
        context
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted")
            .end_text_input();

        // clear selection
        let update_mask = self.deselect();

        self.update_views(
            update_mask | TextInputViewUpdateMask::FOCUS,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        tracing::debug!(?code, "keydown");

        let view_update_mask = match code {
            // cursor operations
            KeyInputCode::LeftArrow => {
                self.move_cursor_to_left(modifier.contains(ModifierKey::SHIFT))
            }
            KeyInputCode::RightArrow => {
                self.move_cursor_to_right(modifier.contains(ModifierKey::SHIFT))
            }
            KeyInputCode::Home => self.jump_to_beginning_of_line(),
            KeyInputCode::End => self.jump_to_end_of_line(),
            // TODO: insert mode
            KeyInputCode::Insert => TextInputViewUpdateMask::empty(),
            // deletions
            KeyInputCode::Backspace if !self.has_selection() => self.delete_prev_char(),
            KeyInputCode::Backspace => self.delete_selection(),
            KeyInputCode::Delete if !self.has_selection() => self.delete_next_char(),
            KeyInputCode::Delete => self.delete_selection(),
            // non-control chars
            KeyInputCode::Character(c) if !c.is_control() && !self.has_selection() => {
                self.insert_char_at_cursor(c)
            }
            KeyInputCode::Character(c) if !c.is_control() => self.replace_selection_by_char(c),
            // ignore enter key
            KeyInputCode::Enter => TextInputViewUpdateMask::empty(),
            _ => TextInputViewUpdateMask::empty(),
        };

        self.update_views(
            view_update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: &str,
        new_preedit_string: &str,
    ) {
        let selection_range = self.selection_range();
        if !selection_range.is_empty() {
            // remove selection first
            self.content
                .borrow_mut()
                .replace_range(selection_range.clone(), "");
            self.cursor_pos_bytes.set(selection_range.start);
            self.selection_begin_bytes.set(selection_range.start);
        }

        // TODO: waylandのText Input v3はこの順序で処理しろと書いてある https://wayland.app/protocols/text-input-unstable-v3#zwp_text_input_v3:event:done
        // 他PFではどうなのかは不明
        let has_preedit_text =
            self.preedit_range_start_bytes.get() != self.preedit_range_end_bytes.get();

        if has_preedit_text {
            if !new_preedit_string.is_empty() {
                // replace preedit
                self.content.borrow_mut().replace_range(
                    self.preedit_range_start_bytes.get()..self.preedit_range_end_bytes.get(),
                    new_preedit_string,
                );
                self.preedit_range_start_bytes
                    .set(self.preedit_range_start_bytes.get());
                self.preedit_range_end_bytes
                    .set(self.preedit_range_start_bytes.get() + new_preedit_string.len());
                self.cursor_pos_bytes
                    .set(self.preedit_range_end_bytes.get());
            } else {
                // clear preedit
                self.content.borrow_mut().replace_range(
                    self.preedit_range_start_bytes.get()..self.preedit_range_end_bytes.get(),
                    "",
                );
                self.preedit_range_start_bytes
                    .set(self.preedit_range_start_bytes.get());
                self.preedit_range_end_bytes
                    .set(self.preedit_range_start_bytes.get());
                self.cursor_pos_bytes
                    .set(self.preedit_range_start_bytes.get());
            }
        }

        if !new_committed_string.is_empty() {
            // insert committed
            self.content
                .borrow_mut()
                .insert_str(self.cursor_pos_bytes.get(), new_committed_string);
            self.cursor_pos_bytes
                .set(self.cursor_pos_bytes.get() + new_committed_string.len());
        }

        if !has_preedit_text && !new_preedit_string.is_empty() {
            // insert preedit
            self.content
                .borrow_mut()
                .insert_str(self.cursor_pos_bytes.get(), new_preedit_string);
            self.preedit_range_start_bytes
                .set(self.cursor_pos_bytes.get());
            self.preedit_range_end_bytes
                .set(self.cursor_pos_bytes.get() + new_preedit_string.len());
            self.cursor_pos_bytes
                .set(self.preedit_range_end_bytes.get());
        }

        // no selection in editing
        self.selection_begin_bytes.set(self.cursor_pos_bytes.get());

        self.update_views(
            TextInputViewUpdateMask::TEXT
                | TextInputViewUpdateMask::CURSOR
                | TextInputViewUpdateMask::PREEDIT,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }
}
impl HitTestTreeActionHandler for TextInputViewEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let (local_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        let bytes = TextLayout::find_nearest_bytes(
            local_x - 2.0 - self.content_h_offset.get(),
            &self.content.borrow(),
            FontID::UIDefault,
            context.system_link.font_set(),
        );

        // PointerDownのときは範囲選択なし状態
        let update_mask = self.move_cursor(bytes);

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        let (local_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        let bytes = TextLayout::find_nearest_bytes(
            local_x - 2.0 - self.content_h_offset.get(),
            &self.content.borrow(),
            FontID::UIDefault,
            context.system_link.font_set(),
        );

        let update_mask = self.move_cursor_keep_selection(bytes);

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let (local_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        let bytes = TextLayout::find_nearest_bytes(
            local_x - 2.0 - self.content_h_offset.get(),
            &self.content.borrow(),
            FontID::UIDefault,
            context.system_link.font_set(),
        );

        let update_mask = self.move_cursor_keep_selection(bytes);

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::RELEASE_CAPTURE_ELEMENT
    }

    fn on_double_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let selection_range =
            self.select_word_at(&*self.content.borrow(), self.cursor_pos_bytes.get());
        let update_mask = self.select_range(selection_range);

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        EventContinueControl::STOP_PROPAGATION
    }
}
impl HitTestTreeScreenRepositionHandler for TextInputViewEventHandler {
    fn on_screen_reposition_required(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _window_screen_pos: Point<PointerInputUnit>,
    ) {
        #[cfg(windows)]
        {
            self.native_text_input_context.notify_layout_changed();
        }
    }
}
impl TextInputViewEventHandler {
    fn update_focus<E>(
        &self,
        window: WindowHandle,
        composite_tree: &mut CompositeTree<E>,
        current_sec: f32,
    ) {
        let has_focus = window.keyboard_focus_state().has_focus(&self.token);

        if has_focus {
            composite_tree.get_mut(self.ct_root).border = Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.5],
                    to_value: [1.0, 1.0, 1.0, 1.0],
                    start_sec: current_sec,
                    end_sec: current_sec + 0.1,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                },
                ..Default::default()
            });
            composite_tree.get_mut(self.ct_cursor).opacity = AnimatableFloat::Value(1.0);
        } else {
            composite_tree.get_mut(self.ct_root).border = Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 1.0],
                    to_value: [1.0, 1.0, 1.0, 0.5],
                    start_sec: current_sec,
                    end_sec: current_sec + 0.1,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                },
                ..Default::default()
            });
            composite_tree.get_mut(self.ct_cursor).opacity = AnimatableFloat::Value(0.0);
        }

        composite_tree.mark_dirty(self.ct_root);
        composite_tree.mark_dirty(self.ct_cursor);
    }

    fn update_text<E>(&self, composite_tree: &mut CompositeTree<E>) {
        composite_tree.get_mut(self.ct_text).text = Some(CompositeRectText {
            runs: vec![CompositeRectTextRun {
                font_id: FontID::UIDefault,
                content: self.content.borrow().clone(),
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }],
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
            vertical_alignment: CompositeRectTextVerticalAlignment::Start,
            ..Default::default()
        });
        composite_tree.mark_text_layout_dirty(self.ct_text);
    }

    fn update_cursor_position<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
        ht_manager: &HitTestTreeManager,
    ) {
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[..self.cursor_pos_bytes.get()],
            FontID::UIDefault,
            system_link.font_set(),
        );

        let mut text_scroll_occured = false;
        let cursor_rect = composite_tree.get_mut(self.ct_cursor);
        let mut cursor_display_x = tw + self.content_h_offset.get();
        if cursor_display_x < 0.0 {
            // 範囲外になる(左すぎ cursor_display_xが0になるようにスクロール量を調整)
            self.content_h_offset
                .set(self.content_h_offset.get() - cursor_display_x);
            text_scroll_occured = true;
            cursor_display_x = 0.0;
        } else if self.content_visible_width - 2.0 < cursor_display_x {
            // 範囲外になる(右すぎ cursor_display_xがcontent_visible_widthになるようにスクロール量を調整)
            self.content_h_offset.set(
                self.content_h_offset.get()
                    - (cursor_display_x - (self.content_visible_width - 2.0)),
            );
            text_scroll_occured = true;
            cursor_display_x = self.content_visible_width - 2.0;
        }
        cursor_rect.offset[0] = AnimatableFloat::Value(cursor_display_x);

        #[cfg(feature = "wayland")]
        let (sx, sy) = ht_manager.translate_tree_local_to_root_autoroot(
            self.ht_root,
            2.0 + cursor_display_x,
            2.0,
        );
        #[cfg(feature = "wayland")]
        system_link.set_ime_cursor_rect(crate::utils::Rect::from_lt_size(
            Point::new_logical(sx, sy),
            crate::utils::Size::new_logical(2.0, 16.0),
        ));
        #[cfg(feature = "wayland")]
        system_link.ime_set_surrounding_text(
            &self.content.borrow(),
            self.cursor_pos_bytes.get(),
            self.selection_begin_bytes.get(),
        );
        #[cfg(feature = "wayland")]
        system_link.ime_commit();

        composite_tree.mark_dirty(self.ct_cursor);

        if text_scroll_occured {
            composite_tree.get_mut(self.ct_text).offset[0] =
                AnimatableFloat::Value(self.content_h_offset.get());
            composite_tree.mark_dirty(self.ct_text);
            self.update_preedit_underline(composite_tree, system_link);
            self.update_selection(composite_tree, system_link);
        }
    }

    fn update_preedit_underline<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
    ) {
        let preedit_range =
            self.preedit_range_start_bytes.get()..self.preedit_range_end_bytes.get();
        if preedit_range.is_empty() {
            // no preedit
            composite_tree.get_mut(self.ct_preedit_underline).opacity = AnimatableFloat::Value(0.0);
            composite_tree.mark_dirty(self.ct_preedit_underline);
            return;
        }

        let o = TextLayout::measure_total_advances(
            &self.content.borrow()[..preedit_range.start],
            FontID::UIDefault,
            system_link.font_set(),
        );
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[preedit_range],
            FontID::UIDefault,
            system_link.font_set(),
        );

        let underline_rect = composite_tree.get_mut(self.ct_preedit_underline);
        underline_rect.offset[0] = AnimatableFloat::Value(o + self.content_h_offset.get());
        underline_rect.size[0] = AnimatableFloat::Value(tw);
        underline_rect.opacity = AnimatableFloat::Value(1.0);

        composite_tree.mark_dirty(self.ct_preedit_underline);
    }

    fn update_selection<E>(&self, composite_tree: &mut CompositeTree<E>, system_link: &SystemLink) {
        let selection_range = self.selection_range();
        if selection_range.is_empty() {
            // no selection
            composite_tree.get_mut(self.ct_selection_bg).size[0] = AnimatableFloat::Value(0.0);
            composite_tree.mark_dirty(self.ct_selection_bg);
            return;
        }

        let o = TextLayout::measure_total_advances(
            &self.content.borrow()[..selection_range.start],
            FontID::UIDefault,
            system_link.font_set(),
        );
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[..selection_range.end],
            FontID::UIDefault,
            system_link.font_set(),
        );

        let ct = composite_tree.get_mut(self.ct_selection_bg);
        ct.offset[0] = AnimatableFloat::Value(o + self.content_h_offset.get());
        ct.size[0] = AnimatableFloat::Value(tw - o);

        composite_tree.mark_dirty(self.ct_selection_bg);
    }

    fn update_views<E>(
        &self,
        mask: TextInputViewUpdateMask,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
        ht_manager: &HitTestTreeManager,
        current_sec: f32,
    ) {
        if mask.contains(TextInputViewUpdateMask::TEXT) {
            // needs update text
            self.update_text(composite_tree);
        }
        if mask.contains(TextInputViewUpdateMask::CURSOR) {
            // needs update cursor position and selection highlight
            self.update_cursor_position(composite_tree, system_link, ht_manager);
            self.update_selection(composite_tree, system_link);
        }
        if mask.contains(TextInputViewUpdateMask::PREEDIT) {
            self.update_preedit_underline(composite_tree, system_link);
        }
        if mask.contains(TextInputViewUpdateMask::FOCUS) {
            self.update_focus(
                ht_manager
                    .query_root_window(self.ht_root)
                    .expect("not mounted"),
                composite_tree,
                current_sec,
            );
        }

        if mask.intersects(TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR) {
            // どっちにも影響する
            self.sync_selection_native();
        }
    }

    fn sync_selection_native(&self) {
        #[cfg(windows)]
        let selection_begin_bytes = self.selection_begin_bytes.get();
        #[cfg(windows)]
        let cursor_pos_bytes = self.cursor_pos_bytes.get();
        #[cfg(windows)]
        let selection_begin_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < selection_begin_bytes)
            .count();
        #[cfg(windows)]
        let cursor_pos_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < cursor_pos_bytes)
            .count();
        #[cfg(windows)]
        self.native_text_input_context.notify_selection_changed(
            selection_begin_acp.min(cursor_pos_acp) as _,
            selection_begin_acp.max(cursor_pos_acp) as _,
        );
    }

    fn compute_prev_char_pos_bytes(content: &str, current_pos_bytes: usize) -> usize {
        let mut new_cursor_pos = current_pos_bytes.saturating_sub(1);
        while new_cursor_pos > 0 && !content.is_char_boundary(new_cursor_pos) {
            new_cursor_pos -= 1;
        }

        new_cursor_pos
    }

    fn compute_next_char_pos_bytes(content: &str, current_pos_bytes: usize) -> usize {
        let mut new_cursor_pos = current_pos_bytes.saturating_add(1).min(content.len());
        while new_cursor_pos < content.len() && !content.is_char_boundary(new_cursor_pos) {
            new_cursor_pos += 1;
        }

        new_cursor_pos
    }

    #[inline(always)]
    fn has_selection(&self) -> bool {
        self.selection_begin_bytes.get() != self.cursor_pos_bytes.get()
    }

    fn selection_range(&self) -> core::ops::Range<usize> {
        match (
            self.cursor_pos_bytes.get(),
            self.selection_begin_bytes.get(),
        ) {
            (a, b) if a <= b => a..b,
            (a, b) => b..a,
        }
    }

    fn move_cursor(&self, pos_bytes: usize) -> TextInputViewUpdateMask {
        self.cursor_pos_bytes.set(pos_bytes);
        self.selection_begin_bytes.set(pos_bytes);

        TextInputViewUpdateMask::CURSOR
    }

    fn move_cursor_keep_selection(&self, pos_bytes: usize) -> TextInputViewUpdateMask {
        self.cursor_pos_bytes.set(pos_bytes);

        TextInputViewUpdateMask::CURSOR
    }

    fn deselect(&self) -> TextInputViewUpdateMask {
        self.selection_begin_bytes.set(self.cursor_pos_bytes.get());

        TextInputViewUpdateMask::CURSOR
    }

    fn select_range(&self, range: core::ops::Range<usize>) -> TextInputViewUpdateMask {
        self.selection_begin_bytes.set(range.start);
        self.cursor_pos_bytes.set(range.end);

        TextInputViewUpdateMask::CURSOR
    }

    fn insert_char_at_cursor(&self, c: char) -> TextInputViewUpdateMask {
        self.content
            .borrow_mut()
            .insert(self.cursor_pos_bytes.get(), c);
        self.cursor_pos_bytes.update(|x| x + c.len_utf8());
        let deselect_updates = self.deselect();

        TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR | deselect_updates
    }

    fn replace_selection_by_char(&self, c: char) -> TextInputViewUpdateMask {
        let selection_range = self.selection_range();
        assert!(!selection_range.is_empty(), "replacing empty selection");

        self.content
            .borrow_mut()
            .replace_range(selection_range.clone(), &c.to_string());
        self.cursor_pos_bytes
            .set(selection_range.start + c.len_utf8());
        let deselect_updates = self.deselect();

        TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR | deselect_updates
    }

    fn delete_prev_char(&self) -> TextInputViewUpdateMask {
        let remove_from = self.cursor_pos_bytes.get();
        let remove_to = Self::compute_prev_char_pos_bytes(&*self.content.borrow(), remove_from);
        if remove_from == remove_to {
            // no deletion
            return TextInputViewUpdateMask::empty();
        }

        // remove_to < remove_from
        self.content
            .borrow_mut()
            .replace_range(remove_to..remove_from, "");
        self.cursor_pos_bytes.set(remove_to);
        self.selection_begin_bytes.set(remove_to);

        TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR
    }

    fn delete_next_char(&self) -> TextInputViewUpdateMask {
        let remove_from = self.cursor_pos_bytes.get();
        let remove_to = remove_from
            + self.content.borrow()[remove_from..]
                .chars()
                .next()
                .map_or(0, |x| x.len_utf8());

        if remove_from == remove_to {
            // no deletion
            return TextInputViewUpdateMask::empty();
        }

        self.content
            .borrow_mut()
            .replace_range(remove_from..remove_to, "");
        // no cursor updates here

        TextInputViewUpdateMask::TEXT
    }

    fn delete_selection(&self) -> TextInputViewUpdateMask {
        let selection_range = self.selection_range();
        if selection_range.is_empty() {
            // no selection
            return TextInputViewUpdateMask::empty();
        }

        self.content
            .borrow_mut()
            .replace_range(selection_range.clone(), "");
        self.cursor_pos_bytes.set(selection_range.start);
        self.selection_begin_bytes.set(selection_range.start);

        TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR
    }

    fn jump_to_beginning_of_line(&self) -> TextInputViewUpdateMask {
        // TODO: multiline
        self.move_cursor(0)
    }

    fn jump_to_end_of_line(&self) -> TextInputViewUpdateMask {
        // TODO: multiline
        self.move_cursor(self.content.borrow().len())
    }

    fn move_cursor_to_left(&self, with_selection: bool) -> TextInputViewUpdateMask {
        if with_selection && !self.has_selection() {
            // 初めて選択状態になった
            self.selection_begin_bytes.set(self.cursor_pos_bytes.get());
        }

        let new_cursor_pos =
            Self::compute_prev_char_pos_bytes(&*self.content.borrow(), self.cursor_pos_bytes.get());

        if with_selection {
            self.move_cursor_keep_selection(new_cursor_pos)
        } else {
            self.move_cursor(new_cursor_pos)
        }
    }

    fn move_cursor_to_right(&self, with_selection: bool) -> TextInputViewUpdateMask {
        if with_selection && !self.has_selection() {
            // 初めて選択状態になった
            self.selection_begin_bytes.set(self.cursor_pos_bytes.get());
        }

        let new_cursor_pos =
            Self::compute_next_char_pos_bytes(&*self.content.borrow(), self.cursor_pos_bytes.get());

        if with_selection {
            self.move_cursor_keep_selection(new_cursor_pos)
        } else {
            self.move_cursor(new_cursor_pos)
        }
    }

    fn select_word_at(&self, content: &str, cursor_pos_bytes: usize) -> core::ops::Range<usize> {
        if content.is_empty() {
            return 0..0;
        }

        #[cfg(windows)]
        {
            let user_language = windows::System::UserProfile::GlobalizationPreferences::Languages()
                .expect("globalization_preferences.languages")
                .First()
                .expect("vector_view.first")
                .Current()
                .expect("iterator.current");
            let word_segmenter =
                windows::Data::Text::WordsSegmenter::CreateWithLanguage(&user_language)
                    .expect("words_segmenter.create");

            let start_index = content
                .char_indices()
                .take_while(|&(i, _)| i < cursor_pos_bytes)
                .count()
                .min(content.chars().count() - 1);
            let ws = word_segmenter
                .GetTokenAt(
                    &windows_core::HSTRING::from_wide(&{
                        let mut u16s = Vec::new();
                        for c in content.chars() {
                            let mut b = [0; 2];
                            u16s.extend_from_slice(c.encode_utf16(&mut b));
                        }
                        u16s
                    }),
                    start_index as _,
                )
                .expect("word_segmenter.get_token_at");
            let text_segment = ws
                .SourceTextSegment()
                .expect("word_segment.source_text_segment");

            let start = content
                .chars()
                .take(text_segment.StartPosition as _)
                .map(|c| c.len_utf8())
                .sum();
            let end = content
                .chars()
                .take((text_segment.StartPosition + text_segment.Length) as _)
                .map(|c| c.len_utf8())
                .sum();

            start..end
        }

        #[cfg(target_os = "macos")]
        {
            let mut start = core::mem::MaybeUninit::uninit();
            let mut end = core::mem::MaybeUninit::uninit();

            let at_utf16 = content[..cursor_pos_bytes]
                .encode_utf16()
                .count()
                .min(content.encode_utf16().count() - 1);
            unsafe {
                crate::platform::mac::bridge::ni_query_range_for_word_at(
                    content.as_ptr(),
                    content.len() as _,
                    at_utf16 as _,
                    start.as_mut_ptr(),
                    end.as_mut_ptr(),
                );
            }

            let start_bytes = unsafe {
                std::char::decode_utf16(content.encode_utf16().take(start.assume_init() as _))
                    .map(|x| x.expect("invalid char?").len_utf8())
                    .sum()
            };
            let end_bytes = unsafe {
                std::char::decode_utf16(content.encode_utf16().take(end.assume_init() as _))
                    .map(|x| x.expect("invalid char?").len_utf8())
                    .sum()
            };

            start_bytes..end_bytes
        }

        #[cfg(not(any(windows, target_os = "macos")))]
        {
            // UnicodeSegmentation+BudouX fallback
            use unicode_segmentation::UnicodeSegmentation;

            let mut words = Vec::new();
            let content = self.content.borrow();
            let mut chars = content.chars();
            let mut is_budou_cluster = false;
            let mut same_cluster_range = 0..0;
            let mut cb = 0;
            while let Some(c) = chars.next() {
                let is_budou_cluster_c = peridot_tp_unicode_properties::script::is_hiragana(c)
                    || peridot_tp_unicode_properties::script::is_katakana(c)
                    || peridot_tp_unicode_properties::script::is_han(c)
                    || peridot_tp_unicode_properties::script::is_thai(c)
                    // 一部Commonにあるらしいので特別対応
                    || c as u32 == 0x30fc || c as u32 == 0xff70;
                if is_budou_cluster_c != is_budou_cluster {
                    if !same_cluster_range.is_empty() {
                        if !is_budou_cluster {
                            words.extend(
                                content[same_cluster_range.clone()]
                                    .split_word_bounds()
                                    .map(|x| x.to_owned()),
                            )
                        } else {
                            words.extend(
                                peridot_tp_budoux::parse(
                                    &peridot_tp_budoux::embedded::ja_knbc::MODEL,
                                    &content[same_cluster_range.clone()],
                                )
                                .into_iter()
                                .map(|x| x.to_owned()),
                            )
                        }
                    }

                    is_budou_cluster = is_budou_cluster_c;
                    same_cluster_range = cb..cb;
                }

                same_cluster_range.end += c.len_utf8();
                cb += c.len_utf8();
            }
            if !same_cluster_range.is_empty() {
                if !is_budou_cluster {
                    words.extend(
                        content[same_cluster_range.clone()]
                            .split_word_bounds()
                            .map(|x| x.to_owned()),
                    )
                } else {
                    words.extend(
                        peridot_tp_budoux::parse(
                            &peridot_tp_budoux::embedded::ja_knbc::MODEL,
                            &content[same_cluster_range.clone()],
                        )
                        .into_iter()
                        .map(|x| x.to_owned()),
                    )
                }
            }

            tracing::debug!(?words, "double click");

            // TODO: LTR前提 最適化はあとで
            let mut measure_range = 0..0;
            let mut select_range = 0..content.len();
            for w in words {
                let starting_byte = measure_range.end;
                measure_range.end += w.len();

                select_range = starting_byte..measure_range.end;
                if select_range.contains(&cursor_pos_bytes) {
                    // ここで確定
                    break;
                }
            }

            select_range
        }
    }
}
#[cfg(windows)]
impl crate::platform::windows::TextProvider for TextInputViewEventHandler {
    fn text(
        &self,
        range: windows::UI::Text::Core::CoreTextRange,
    ) -> windows_core::Result<windows_core::HSTRING> {
        let mut u16s = Vec::with_capacity((range.EndCaretPosition - range.StartCaretPosition) as _);
        for c in self
            .content
            .borrow()
            .chars()
            .skip(range.StartCaretPosition as _)
            .take((range.EndCaretPosition - range.StartCaretPosition) as _)
        {
            let mut buf = [0; 2];
            u16s.extend_from_slice(c.encode_utf16(&mut buf));
        }

        Ok(windows_core::HSTRING::from_wide(&u16s))
    }

    fn selection(
        &self,
        req: &windows::UI::Text::Core::CoreTextSelectionRequest,
    ) -> windows_core::Result<()> {
        let selection_begin_bytes = self.selection_begin_bytes.get();
        let cursor_pos_bytes = self.cursor_pos_bytes.get();
        let selection_begin_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < selection_begin_bytes)
            .count();
        let cursor_pos_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < cursor_pos_bytes)
            .count();

        req.SetSelection(windows::UI::Text::Core::CoreTextRange {
            StartCaretPosition: selection_begin_acp.min(cursor_pos_acp) as _,
            EndCaretPosition: selection_begin_acp.max(cursor_pos_acp) as _,
        })
    }
}
#[cfg(windows)]
impl crate::platform::windows::CoreTextDeferrableEventHandler for TextInputViewEventHandler {
    fn layout(
        &self,
        ctx: &mut InputEventContext,
        req: &windows::UI::Text::Core::CoreTextLayoutRequest,
    ) -> windows_core::Result<()> {
        let range = req.Range()?;
        tracing::trace!(
            req.range = ?range,
            "edit_context.layout_requested"
        );

        let start_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let end_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        let r = ctx.ht_manager.compute_screen_rect_pixels_with_insets(
            self.ht_root,
            Point::new_logical(2.0, 2.0),
            Point::new_logical(2.0, 2.0),
        );
        let o = TextLayout::measure_total_advances(
            &self.content.borrow()[..start_bytes],
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );
        let w = TextLayout::measure_total_advances(
            &self.content.borrow()[start_bytes..end_bytes],
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );

        req.LayoutBounds()?
            .SetTextBounds(windows::Foundation::Rect {
                X: r.left as f32 + (o + self.content_h_offset.get()) * self.render_scale.get(),
                Y: r.top as _,
                Width: w * self.render_scale.get(),
                Height: r.height as _,
            })
    }

    fn text_updating(
        &self,
        ctx: &mut InputEventContext,
        e: &windows::UI::Text::Core::CoreTextTextUpdatingEventArgs,
    ) -> windows_core::Result<()> {
        let range = e.Range()?;
        let text = e.Text()?.to_string_lossy();
        let new_selection = e.NewSelection()?;
        tracing::trace!(
            ?new_selection,
            ?range,
            ?text,
            current = &self.content.borrow() as &str,
            "edit_context.text_updating"
        );

        let replace_start_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let replace_end_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        self.content
            .borrow_mut()
            .replace_range(replace_start_bytes..replace_end_bytes, &text);

        let new_cursor_start_bytes = self
            .content
            .borrow()
            .chars()
            .take(new_selection.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let new_cursor_end_bytes = self
            .content
            .borrow()
            .chars()
            .take(new_selection.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        let update_mask = self.select_range(new_cursor_start_bytes..new_cursor_end_bytes)
            | TextInputViewUpdateMask::TEXT;

        self.update_views(
            update_mask,
            ctx.composite_tree,
            ctx.system_link,
            ctx.ht_manager,
            ctx.current_sec,
        );

        e.SetResult(windows::UI::Text::Core::CoreTextTextUpdatingResult::Succeeded)?;
        Ok(())
    }

    fn format_updating(
        &self,
        ctx: &mut InputEventContext,
        e: &windows::UI::Text::Core::CoreTextFormatUpdatingEventArgs,
    ) -> windows_core::Result<()> {
        let underline_type = e.UnderlineType()?.Value()?;
        let range = e.Range()?;
        let reason = e.Reason()?;
        tracing::trace!(
            background_color = ?e.BackgroundColor(),
            ?range,
            ?reason,
            text_color = ?e.TextColor(),
            underline_color = ?e.UnderlineColor(),
            ?underline_type,
            "edit_context.format_updating"
        );

        // TODO: Windowsの場合は複数下線要素ができる場合がある（部分的に変換する場合など）
        if underline_type == windows::UI::Text::UnderlineType::None {
            self.preedit_range_start_bytes.set(0);
            self.preedit_range_end_bytes.set(0);
        } else {
            self.preedit_range_start_bytes.set(
                self.content
                    .borrow()
                    .chars()
                    .take(range.StartCaretPosition as _)
                    .map(|x| x.len_utf8())
                    .sum(),
            );
            self.preedit_range_end_bytes.set(
                self.content
                    .borrow()
                    .chars()
                    .take(range.EndCaretPosition as _)
                    .map(|x| x.len_utf8())
                    .sum(),
            );
        }

        self.update_preedit_underline(ctx.composite_tree, ctx.system_link);
        Ok(())
    }
}
#[cfg(target_os = "macos")]
impl crate::platform::mac::bridge::TextInputClientForwarding for TextInputViewEventHandler {
    fn has_marked_text(&self) -> bool {
        tracing::debug!(
            start = self.preedit_range_start_bytes.get(),
            end = self.preedit_range_end_bytes.get(),
            "hasMarkedText"
        );
        self.preedit_range_start_bytes.get() != self.preedit_range_end_bytes.get()
    }

    fn marked_range(&self, out_location: *mut i64, out_length: *mut i64) -> bool {
        let start = self.preedit_range_start_bytes.get();
        let end = self.preedit_range_end_bytes.get();
        tracing::debug!(start, end, "markedRange");

        if start == end {
            return false;
        }

        let startc = self.content.borrow()[..start].chars().count();
        let endc = self.content.borrow()[..end].chars().count();

        unsafe {
            out_location.write(startc as _);
            out_length.write((endc - startc) as _);
        }
        true
    }

    fn selected_range(&self, out_location: *mut i64, out_length: *mut i64) {
        let r = self.selection_range();

        let startc = self.content.borrow()[..r.start].chars().count();
        let endc = self.content.borrow()[..r.end].chars().count();

        unsafe {
            out_location.write(startc as _);
            out_length.write((endc - startc) as _);
        }
    }

    fn set_marked_text(
        &self,
        text: &core::ffi::CStr,
        new_selection_location: i64,
        new_selection_length: i64,
        replacement_location: i64,
        replacement_length: i64,
    ) {
        tracing::debug!(
            ?text,
            new_selection_location,
            new_selection_length,
            replacement_location,
            replacement_length,
            "set marked text"
        );

        // なんかreplacement系の範囲が信用できなさそうなので自前でどこを書き換えるか判定する
        let preedit_start = self.preedit_range_start_bytes.get();
        let preedit_end = self.preedit_range_end_bytes.get();
        if preedit_start == preedit_end {
            // non-preedit state
            let r = self.selection_range();
            let text = text.to_str().expect("invalid input str");
            let mut content = self.content.borrow_mut();
            content.replace_range(r.clone(), text);
            drop(content);

            self.preedit_range_start_bytes.set(r.start);
            self.preedit_range_end_bytes.set(r.start + text.len());
            self.cursor_pos_bytes.set(r.start + text.len());
            self.selection_begin_bytes.set(r.start + text.len());

            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &*self.event_dispatcher }.dispatch(Event::UpdateView { id: self.id });
        } else {
            let text = text.to_str().expect("invalid input str");
            let mut content = self.content.borrow_mut();
            content.replace_range(preedit_start..preedit_end, text);
            drop(content);

            self.preedit_range_end_bytes.set(preedit_start + text.len());
            self.cursor_pos_bytes.set(preedit_start + text.len());
            self.selection_begin_bytes.set(preedit_start + text.len());

            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &*self.event_dispatcher }.dispatch(Event::UpdateView { id: self.id });
        }
    }

    fn insert_text(
        &self,
        text: &core::ffi::CStr,
        replacement_location: i64,
        replacement_length: i64,
    ) {
        tracing::debug!(
            ?text,
            replacement_location,
            replacement_length,
            "insert text"
        );

        // なんかreplacement系の範囲が信用できなさそうなので自前でどこを書き換えるか判定する
        let preedit_start = self.preedit_range_start_bytes.get();
        let preedit_end = self.preedit_range_end_bytes.get();
        if preedit_start == preedit_end {
            // non-preedit state
            let r = self.selection_range();
            let text = text.to_str().expect("invalid input str");
            let mut content = self.content.borrow_mut();
            content.replace_range(r.clone(), text);
            drop(content);

            self.preedit_range_start_bytes.set(r.start);
            self.preedit_range_end_bytes.set(r.start);
            self.cursor_pos_bytes.set(r.start + text.len());
            self.selection_begin_bytes.set(r.start + text.len());

            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &*self.event_dispatcher }.dispatch(Event::UpdateView { id: self.id });
        } else {
            let text = text.to_str().expect("invalid input str");
            let mut content = self.content.borrow_mut();
            content.replace_range(preedit_start..preedit_end, text);
            drop(content);

            self.preedit_range_end_bytes.set(preedit_start);
            self.cursor_pos_bytes.set(preedit_start + text.len());
            self.selection_begin_bytes.set(preedit_start + text.len());

            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &*self.event_dispatcher }.dispatch(Event::UpdateView { id: self.id });
        }
    }

    fn substring(
        &self,
        location: Option<i64>,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        out_chars: *mut *const core::ffi::c_char,
        out_len: *mut u64,
    ) {
        let location = location.unwrap_or(0);
        let length = length.min(self.content.borrow().len() as i64);

        let loc = self
            .content
            .borrow()
            .chars()
            .take(location as _)
            .map(|x| x.len_utf8())
            .sum();
        let endloc = self
            .content
            .borrow()
            .chars()
            .take((location + length) as _)
            .map(|x| x.len_utf8())
            .sum::<usize>();

        unsafe {
            out_chars.write(self.content.borrow().as_ptr().add(loc).cast());
            out_len.write((endloc - loc) as _);
        }

        if !actual_location.is_null() {
            unsafe {
                actual_location.write(location);
            }
        }
        if !actual_length.is_null() {
            unsafe {
                actual_length.write(length);
            }
        }
    }

    #[tracing::instrument(skip(self))]
    fn first_rect(
        &self,
        location: i64,
        length: i64,
        actual_location: *mut i64,
        actual_length: *mut i64,
        surface_x: *mut f32,
        surface_y: *mut f32,
        width: *mut f32,
        height: *mut f32,
    ) {
        tracing::debug!(location, length, "first rect");

        let endloc = self
            .content
            .borrow()
            .chars()
            .take((location + length) as _)
            .map(|x| x.len_utf8())
            .sum();

        let window = unsafe { &*self.ht_manager_ptr }
            .query_root_window(self.ht_root)
            .expect("not mounted");
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[..endloc],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            1.0, // no scaling for this measure
        );

        if !actual_location.is_null() {
            unsafe {
                actual_location.write(location);
            }
        }
        if !actual_length.is_null() {
            unsafe {
                actual_length.write(length);
            }
        }

        let (sx, sy, _, sh, _) =
            unsafe { &*self.ht_manager_ptr }.compute_global_rect_autoroot(self.ht_root);

        unsafe {
            surface_x.write(sx + tw);
            surface_y.write(sy);
            width.write(0.0);
            height.write(sh);
        }
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct TextInputViewUpdateMask : u32 {
        const TEXT = 1 << 0;
        const CURSOR = 1 << 1;
        const PREEDIT = 1 << 2;
        const FOCUS = 1 << 3;
    }
}
