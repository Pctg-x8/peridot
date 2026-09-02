use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use bitflags::bitflags;

use crate::{
    LogicFiberEventDispatcher, SystemLink,
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputCode,
        KeyInputEventHandler, ModifierKey, PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, HitTestTreeScreenRepositionHandler, PointerActionArgs,
            PointerButtonActionArgs, ScrollWheelActionArgs, ScrollWheelActionResponse,
        },
    },
    model::{Application, ApplicationMutation},
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectScaleFactor, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{
        RenderContext, TypedViewIdentifier, View, ViewConstructor, ViewIdentifier,
        ViewLayoutStateStore, ViewRenderElements, ViewRenderQueue, ViewRenderer,
    },
    utils::{
        LogicalUnit, Point, Rect, SafeF32, Size,
        text::{next_char_byte, prev_char_byte},
    },
};

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct SingleLineTextDirtyFlags : u8 {
        const CONTENT = 1 << 0;
        const CURSOR = 1 << 1;
        const PREEDIT = 1 << 2;
    }
}

pub struct SingleLineTextEditState {
    content: String,
    cursor_pos_byte: usize,
    selection_anchor_byte: Option<usize>,
    preedit_start_byte: usize,
    preedit_end_byte: usize,
}
impl SingleLineTextEditState {
    pub fn new(init_content: String) -> Self {
        Self {
            content: init_content,
            cursor_pos_byte: 0,
            selection_anchor_byte: None,
            preedit_start_byte: 0,
            preedit_end_byte: 0,
        }
    }

    pub const fn is_compositioning(&self) -> bool {
        self.preedit_start_byte != self.preedit_end_byte
    }

    pub fn selection_range(&self) -> core::range::Range<usize> {
        let range_anchor = self.selection_anchor_byte.unwrap_or(self.cursor_pos_byte);
        let range_target = self.cursor_pos_byte;

        (range_anchor.min(range_target)..range_anchor.max(range_target)).into()
    }

    #[cfg(windows)]
    pub fn selection_range_win32_acp(&self) -> core::range::Range<i32> {
        let cursor_acp = self
            .content
            .char_indices()
            .take_while(|&(i, _)| i < self.cursor_pos_byte)
            .count() as i32;
        let selection_anchor_acp = self.selection_anchor_byte.map_or(cursor_acp, |n| {
            self.content
                .char_indices()
                .take_while(|&(i, _)| i < n)
                .count() as i32
        });

        (cursor_acp.min(selection_anchor_acp)..cursor_acp.max(selection_anchor_acp)).into()
    }

    pub fn set_content(&mut self, content: String) -> SingleLineTextDirtyFlags {
        assert!(
            !self.is_compositioning(),
            "setting new text while compositioning is not allowed"
        );

        self.content = content;
        self.cursor_pos_byte = self.cursor_pos_byte.min(self.content.len());
        self.selection_anchor_byte = None;

        SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR
    }

    pub fn cursor_move_at(&mut self, at: usize, select: bool) -> SingleLineTextDirtyFlags {
        if select && self.selection_anchor_byte.is_none() {
            // first time selection
            self.selection_anchor_byte = Some(self.cursor_pos_byte);
        }
        if !select {
            // deselect
            self.selection_anchor_byte = None;
        }

        self.cursor_pos_byte = at;
        SingleLineTextDirtyFlags::CURSOR
    }

    pub fn cursor_move_left(&mut self, select: bool) -> SingleLineTextDirtyFlags {
        self.cursor_move_at(prev_char_byte(&self.content, self.cursor_pos_byte), select)
    }

    pub fn cursor_move_right(&mut self, select: bool) -> SingleLineTextDirtyFlags {
        self.cursor_move_at(next_char_byte(&self.content, self.cursor_pos_byte), select)
    }

    pub fn cursor_move_to_home(&mut self, select: bool) -> SingleLineTextDirtyFlags {
        self.cursor_move_at(0, select)
    }

    pub fn cursor_move_to_end(&mut self, select: bool) -> SingleLineTextDirtyFlags {
        self.cursor_move_at(self.content.len(), select)
    }

    pub fn delete_backward(&mut self) -> SingleLineTextDirtyFlags {
        let range_anchor = self.selection_anchor_byte.unwrap_or(self.cursor_pos_byte);
        let range_target = self.cursor_pos_byte;
        if range_anchor == range_target {
            // no selection: simply delete the previous character
            let delete_range = core::range::Range::from(
                prev_char_byte(&self.content, self.cursor_pos_byte)..self.cursor_pos_byte,
            );
            if delete_range.is_empty() {
                // no char will be deleted
                return SingleLineTextDirtyFlags::empty();
            }

            self.content.replace_range(delete_range, "");
            self.cursor_pos_byte = delete_range.start;
            return SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR;
        }

        let range = core::range::Range::from(
            range_anchor.min(range_target)..range_anchor.max(range_target),
        );
        self.content.replace_range(range, "");
        self.selection_anchor_byte = None;
        self.cursor_pos_byte = range.start;
        SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR
    }

    pub fn delete_forward(&mut self) -> SingleLineTextDirtyFlags {
        let range_anchor = self.selection_anchor_byte.unwrap_or(self.cursor_pos_byte);
        let range_target = self.cursor_pos_byte;
        if range_anchor == range_target {
            // no selection: simply delete the previous character
            let delete_range = core::range::Range::from(
                self.cursor_pos_byte..next_char_byte(&self.content, self.cursor_pos_byte),
            );
            if delete_range.is_empty() {
                // no char will be deleted
                return SingleLineTextDirtyFlags::empty();
            }

            self.content.replace_range(delete_range, "");
            // カーソルは同じ位置にとどまる
            return SingleLineTextDirtyFlags::CONTENT;
        }

        let range = core::range::Range::from(
            range_anchor.min(range_target)..range_anchor.max(range_target),
        );
        self.content.replace_range(range, "");
        self.selection_anchor_byte = None;
        self.cursor_pos_byte = range.start;
        SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR
    }

    pub fn insert_char(&mut self, ch: char) -> SingleLineTextDirtyFlags {
        let mut dirty_flags = SingleLineTextDirtyFlags::empty();

        if self.preedit_start_byte != self.preedit_end_byte {
            // preeditがある場合は先に消す
            // TODO: waylandはこの挙動で良さそうだけどほかも問題ないか？
            self.content
                .replace_range(self.preedit_start_byte..self.preedit_end_byte, "");
            self.preedit_end_byte = self.preedit_start_byte;

            dirty_flags |= SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::PREEDIT;
        }

        let range_anchor = self.selection_anchor_byte.unwrap_or(self.cursor_pos_byte);
        let range_target = self.cursor_pos_byte;
        if range_anchor != range_target {
            // remove selection first
            let range = core::range::Range::from(
                range_anchor.min(range_target)..range_anchor.max(range_target),
            );
            self.content.replace_range(range, "");
            self.selection_anchor_byte = None;
            self.cursor_pos_byte = range.start;

            dirty_flags |= SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR;
        }

        self.content.insert(self.cursor_pos_byte, ch);
        self.cursor_pos_byte += ch.len_utf8();
        dirty_flags | SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR
    }

    pub fn select_word_at_cursor(&mut self) -> SingleLineTextDirtyFlags {
        if self.content.is_empty() {
            return self.cursor_move_to_home(false);
        }

        #[cfg(windows)]
        {
            let word_segment = crate::utils::platform::windows::find_word_segment(
                &self.content,
                self.cursor_pos_byte,
            );

            self.selection_anchor_byte = Some(word_segment.start);
            self.cursor_pos_byte = word_segment.end;
            return SingleLineTextDirtyFlags::CURSOR;
        }

        #[cfg(target_os = "macos")]
        {
            let at_utf16 = self.content[..self.cursor_pos_byte]
                .encode_utf16()
                .count()
                .min(self.content.encode_utf16().count() - 1);
            let word_range_utf16 =
                crate::platform::mac::query_range_for_word_at(&self.content, at_utf16);

            let start_bytes = std::char::decode_utf16(
                self.content
                    .encode_utf16()
                    .take(word_range_utf16.start as _),
            )
            .map(|x| x.expect("invalid char?").len_utf8())
            .sum();
            let end_bytes = std::char::decode_utf16(
                self.content.encode_utf16().take(word_range_utf16.end as _),
            )
            .map(|x| x.expect("invalid char?").len_utf8())
            .sum();

            self.selection_anchor_byte = Some(start_bytes);
            self.cursor_pos_byte = end_bytes;
            return SingleLineTextDirtyFlags::CURSOR;
        }

        #[cfg(not(any(windows, target_os = "macos")))]
        {
            // generic fallback
            let words = crate::utils::text::generic_word_segments(&self.content);
            tracing::debug!(?words, "double click");

            // TODO: 最適化はあとで
            let select_range = words
                .into_iter()
                // wordのbyte rangeを生成
                .scan(0, |range_start, w| {
                    let r = *range_start..(*range_start + w.len());
                    *range_start += w.len();
                    Some(r)
                })
                // cursor_pos_bytesを含むものを探す
                .find(|r| r.contains(&self.cursor_pos_byte))
                // なければ全体
                .unwrap_or(0..self.content.len());
            self.selection_anchor_byte = Some(select_range.start);
            self.cursor_pos_byte = select_range.end;
            return SingleLineTextDirtyFlags::CURSOR;
        }
    }

    pub fn select_all(&mut self) -> SingleLineTextDirtyFlags {
        self.selection_anchor_byte = Some(0);
        self.cursor_pos_byte = self.content.len();

        SingleLineTextDirtyFlags::CURSOR
    }

    pub fn deselect(&mut self) -> SingleLineTextDirtyFlags {
        self.selection_anchor_byte = None;
        SingleLineTextDirtyFlags::CURSOR
    }

    #[cfg(windows)]
    pub fn winct_update_text(
        &mut self,
        replace_range_acp: &windows::UI::Text::Core::CoreTextRange,
        replace_to: &str,
        new_selection_range_acp: &windows::UI::Text::Core::CoreTextRange,
    ) -> SingleLineTextDirtyFlags {
        let replace_start_bytes = self
            .content
            .chars()
            .take(replace_range_acp.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let replace_end_bytes = self
            .content
            .chars()
            .take(replace_range_acp.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        self.content
            .replace_range(replace_start_bytes..replace_end_bytes, replace_to);

        let new_cursor_start_bytes = self
            .content
            .chars()
            .take(new_selection_range_acp.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let new_cursor_end_bytes = self
            .content
            .chars()
            .take(new_selection_range_acp.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        if new_cursor_start_bytes == new_cursor_end_bytes {
            // no selection
            self.selection_anchor_byte = None;
            self.cursor_pos_byte = new_cursor_start_bytes;
        } else {
            self.selection_anchor_byte = Some(new_cursor_start_bytes);
            self.cursor_pos_byte = new_cursor_end_bytes;
        }

        SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR
    }

    #[cfg(windows)]
    pub fn winct_update_format(
        &mut self,
        underline_type: windows::UI::Text::UnderlineType,
        range: &windows::UI::Text::Core::CoreTextRange,
    ) -> SingleLineTextDirtyFlags {
        // TODO: Windowsの場合は複数下線要素ができる場合がある（部分的に変換する場合など）
        if underline_type == windows::UI::Text::UnderlineType::None {
            self.preedit_start_byte = 0;
            self.preedit_end_byte = 0;
        } else {
            self.preedit_start_byte = self
                .content
                .chars()
                .take(range.StartCaretPosition as _)
                .map(|x| x.len_utf8())
                .sum();
            self.preedit_end_byte = self
                .content
                .chars()
                .take(range.EndCaretPosition as _)
                .map(|x| x.len_utf8())
                .sum();
        }

        SingleLineTextDirtyFlags::PREEDIT
    }

    #[cfg(feature = "wayland")]
    pub fn wl_perform_ime_state_changes(
        &mut self,
        committed_string: Option<&str>,
        preedit_string: Option<&str>,
    ) -> SingleLineTextDirtyFlags {
        let mut update_mask = SingleLineTextDirtyFlags::empty();

        // remove selection first(this is overwriting op)
        if let Some(selection_anchor) = self.selection_anchor_byte.take() {
            self.content
                .replace_range(selection_anchor..self.cursor_pos_byte, "");
            self.cursor_pos_byte = selection_anchor;

            update_mask |= SingleLineTextDirtyFlags::CURSOR | SingleLineTextDirtyFlags::CONTENT;
        }

        // Note: waylandのText Input v3はこの順序で処理しろと書いてある https://wayland.app/protocols/text-input-unstable-v3#zwp_text_input_v3:event:done
        if self.preedit_start_byte != self.preedit_end_byte {
            // replace existing preedit with the cursor
            self.content
                .replace_range(self.preedit_start_byte..self.preedit_end_byte, "");
            self.cursor_pos_byte = self.preedit_start_byte;
            self.preedit_end_byte = self.preedit_start_byte;

            update_mask |= SingleLineTextDirtyFlags::CONTENT
                | SingleLineTextDirtyFlags::CURSOR
                | SingleLineTextDirtyFlags::PREEDIT;
        }

        // TODO: remove surrounding text
        if let Some(committed_string) = committed_string {
            // insert commit string with the cursor at its end
            self.content
                .insert_str(self.cursor_pos_byte, committed_string);
            self.cursor_pos_byte += committed_string.len();

            update_mask |= SingleLineTextDirtyFlags::CONTENT | SingleLineTextDirtyFlags::CURSOR;
        }

        // TODO: compute new surrounding text
        if let Some(preedit_string) = preedit_string {
            // insert new preedit text in cursor position
            self.content
                .insert_str(self.cursor_pos_byte, preedit_string);
            self.preedit_start_byte = self.cursor_pos_byte;
            self.preedit_end_byte = self.cursor_pos_byte + preedit_string.len();
            self.cursor_pos_byte = self.preedit_end_byte;

            update_mask |= SingleLineTextDirtyFlags::CONTENT
                | SingleLineTextDirtyFlags::CURSOR
                | SingleLineTextDirtyFlags::PREEDIT;
        }

        update_mask
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
impl TextInputViewUpdateMask {
    fn translate(v: SingleLineTextDirtyFlags) -> Self {
        let mut flags = Self::empty();
        if v.contains(SingleLineTextDirtyFlags::CONTENT) {
            flags |= Self::TEXT;
        }
        if v.contains(SingleLineTextDirtyFlags::CURSOR) {
            flags |= Self::CURSOR;
        }
        if v.contains(SingleLineTextDirtyFlags::PREEDIT) {
            flags |= Self::PREEDIT;
        }

        flags
    }
}

pub struct TextInputViewCore {
    ct_text_clip: CompositeTreeRef,
    eh: Rc<TextInputViewCoreEventHandler>,
}
impl TextInputViewCore {
    pub fn new(
        ctx: &mut RenderContext,
        rect: Rect<LogicalUnit>,
        parent_anchor: [f32; 2],
        size_anchor: [f32; 2],
        delegated_view_id: ViewIdentifier,
        ht_root: HitTestTreeRef,
    ) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(rect.width),
                AnimatableFloat::Value(rect.height),
            ],
            offset: [
                AnimatableFloat::Value(rect.left),
                AnimatableFloat::Value(rect.top),
            ],
            relative_offset_adjustment: parent_anchor,
            relative_size_adjustment: size_anchor,
            has_bitmap: true,
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_text_clip = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(rect.width - 4.0),
                AnimatableFloat::Value(rect.height - 4.0),
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
        let ct_text = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            ..Default::default()
        });
        let ct_cursor = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(16.0)],
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_preedit_underline = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [AnimatableFloat::Value(1.0), AnimatableFloat::Value(1.0)],
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(14.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_selection_bg = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(16.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.2, 0.4, 1.0, 0.25])),
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_text_clip, ct_selection_bg);
        ctx.composite_tree.add_child(ct_text_clip, ct_text);
        ctx.composite_tree.add_child(ct_text_clip, ct_cursor);
        ctx.composite_tree
            .add_child(ct_text_clip, ct_preedit_underline);
        ctx.composite_tree.add_child(ct_root, ct_text_clip);

        let eh = Rc::new(TextInputViewCoreEventHandler {
            ht_root,
            ct_root,
            ct_text,
            ct_cursor,
            ct_preedit_underline,
            ct_selection_bg,
            has_focus: core::cell::Cell::new(false),
            content_h_offset: core::cell::Cell::new(0.0),
            content_visible_width: core::cell::Cell::new(128.0 - 4.0),
            text_edit_state: RefCell::new(SingleLineTextEditState::new(String::new())),
            #[cfg(windows)]
            native_text_input_context: crate::platform::windows::NativeTextInputContext::new(
                ctx.system_link,
            ),
            #[cfg(target_os = "macos")]
            ht_manager_ptr: core::ptr::from_mut(ctx.ht_manager).cast(),
            #[cfg(target_os = "macos")]
            font_set_ptr: core::ptr::from_ref(ctx.system_link.font_set()),
            pending_update_mask: core::cell::Cell::new(TextInputViewUpdateMask::empty()),
            event_dispatcher: ctx.system_link.event_dispatcher,
            delegated_view_id,
        });
        ctx.ht_manager
            .set_screen_reposition_handler(eh.ht_root, &eh);
        #[cfg(windows)]
        ctx.ht_manager
            .set_native_text_deferrable_event_handler(eh.ht_root, &eh);
        #[cfg(windows)]
        eh.native_text_input_context
            .bind_action(ctx.system_link, &eh, eh.ht_root);

        eh.update_text(ctx.composite_tree);

        Self { ct_text_clip, eh }
    }

    pub const fn entity(&self) -> &std::rc::Rc<TextInputViewCoreEventHandler> {
        &self.eh
    }

    pub fn set_rect<E: PartialEq>(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
    ) {
        composite_tree
            .begin_mod_chain(self.eh.ct_root)
            .offset_imm(rect.left, rect.top)
            .size_imm(rect.width, rect.height)
            .apply();
        composite_tree
            .begin_mod_chain(self.ct_text_clip)
            .size_imm(rect.width - 4.0, rect.height - 4.0)
            .apply();
        self.eh.content_visible_width.set(rect.width);
    }
}

pub struct TextInputViewCoreEventHandler {
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
    ct_text: CompositeTreeRef,
    ct_cursor: CompositeTreeRef,
    ct_preedit_underline: CompositeTreeRef,
    ct_selection_bg: CompositeTreeRef,
    has_focus: core::cell::Cell<bool>,
    content_h_offset: core::cell::Cell<f32>,
    content_visible_width: core::cell::Cell<f32>,
    text_edit_state: RefCell<SingleLineTextEditState>,
    // content: core::cell::RefCell<String>,
    // cursor_pos_bytes: core::cell::Cell<usize>,
    // preedit_range_start_bytes: core::cell::Cell<usize>,
    // preedit_range_end_bytes: core::cell::Cell<usize>,
    // selection_begin_bytes: core::cell::Cell<usize>,
    #[cfg(windows)]
    native_text_input_context: crate::platform::windows::NativeTextInputContext,
    #[cfg(target_os = "macos")]
    ht_manager_ptr: *const HitTestTreeManager<'static>,
    #[cfg(target_os = "macos")]
    font_set_ptr: *const FontSet,
    pending_update_mask: core::cell::Cell<TextInputViewUpdateMask>,
    event_dispatcher: *mut LogicFiberEventDispatcher,
    delegated_view_id: ViewIdentifier,
}
impl HitTestTreeScreenRepositionHandler for TextInputViewCoreEventHandler {
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
// impl ViewEventHandler for TextInputViewCoreEventHandler {
//     #[inline(always)]
//     fn update(&self, context: &mut ViewUpdateContext) {
//         self.process_pending_updates_with_ht_mutation(
//             context.mount_context.composite_tree,
//             context.system_link,
//             context.mount_context.ht_manager,
//             context.mount_context.current_sec,
//         );
//     }
// }
impl KeyInputEventHandler for TextInputViewCoreEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        // HitTestTreeへの変更がはいるので遅延させる
        self.lazy_update_and_schedule(context.view_render_queue, |this| {
            this.set_focus(context.ht_manager)
        });
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        // HitTestTreeへの変更がはいるので遅延させる
        self.lazy_update_and_schedule(context.view_render_queue, |this| {
            this.release_focus(context.ht_manager)
        });
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        tracing::debug!(?code, "keydown");

        let update_mask = match code {
            // cursor operations
            KeyInputCode::LeftArrow => TextInputViewUpdateMask::translate(
                self.text_edit_state
                    .borrow_mut()
                    .cursor_move_left(modifier.contains(ModifierKey::SHIFT)),
            ),
            KeyInputCode::RightArrow => TextInputViewUpdateMask::translate(
                self.text_edit_state
                    .borrow_mut()
                    .cursor_move_right(modifier.contains(ModifierKey::SHIFT)),
            ),
            KeyInputCode::Home => TextInputViewUpdateMask::translate(
                self.text_edit_state
                    .borrow_mut()
                    .cursor_move_to_home(modifier.contains(ModifierKey::SHIFT)),
            ),
            KeyInputCode::End => TextInputViewUpdateMask::translate(
                self.text_edit_state
                    .borrow_mut()
                    .cursor_move_to_end(modifier.contains(ModifierKey::SHIFT)),
            ),
            // TODO: insert mode
            KeyInputCode::Insert => TextInputViewUpdateMask::empty(),
            // deletions
            KeyInputCode::Backspace => TextInputViewUpdateMask::translate(
                self.text_edit_state.borrow_mut().delete_backward(),
            ),
            KeyInputCode::Delete => TextInputViewUpdateMask::translate(
                self.text_edit_state.borrow_mut().delete_forward(),
            ),
            _ => TextInputViewUpdateMask::empty(),
        };

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    fn r#char(&self, context: &mut InputEventContext, ch: char, _modifier: ModifierKey) {
        tracing::debug!(%ch, "char");

        if ch.is_control() {
            return;
        }

        let update_mask =
            TextInputViewUpdateMask::translate(self.text_edit_state.borrow_mut().insert_char(ch));
        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    #[cfg(feature = "wayland")]
    #[tracing::instrument(skip(self, context))]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
    ) {
        tracing::trace!("ime_state_changes");

        let update_mask = TextInputViewUpdateMask::translate(
            self.text_edit_state
                .borrow_mut()
                .wl_perform_ime_state_changes(new_committed_string, new_preedit_string),
        );
        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }
}
impl HitTestTreeActionHandler for TextInputViewCoreEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        self.update_views(
            self.move_cursor_by_point(
                Point::new_logical(local_x, local_y),
                context.system_link.font_set(),
            ),
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
        let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        self.update_views(
            self.move_cursor_by_point_keep_selection(
                Point::new_logical(local_x, local_y),
                context.system_link.font_set(),
            ),
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
        let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        self.update_views(
            self.move_cursor_by_point_keep_selection(
                Point::new_logical(local_x, local_y),
                context.system_link.font_set(),
            ),
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
        let update_mask = TextInputViewUpdateMask::translate(
            self.text_edit_state.borrow_mut().select_word_at_cursor(),
        );

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
impl TextInputViewCoreEventHandler {
    pub const fn delegated_view_id(&self) -> ViewIdentifier {
        self.delegated_view_id
    }

    pub const fn ct_root(&self) -> CompositeTreeRef {
        self.ct_root
    }

    pub const fn ht_root(&self) -> HitTestTreeRef {
        self.ht_root
    }

    #[inline(always)]
    pub fn lazy_update(&self, op: impl FnOnce(&Self) -> TextInputViewUpdateMask) {
        let additional_flags = op(self);
        self.pending_update_mask.update(|x| x | additional_flags);
    }

    #[inline(always)]
    pub fn lazy_update_and_schedule(
        &self,
        view_render_queue: &mut ViewRenderQueue,
        op: impl FnOnce(&Self) -> TextInputViewUpdateMask,
    ) {
        self.lazy_update(op);
        view_render_queue.schedule(self.delegated_view_id);
    }

    #[inline(always)]
    pub fn perform_external_state_update(
        &self,
        mut updater: impl FnMut(&mut SingleLineTextEditState) -> SingleLineTextDirtyFlags,
    ) -> TextInputViewUpdateMask {
        TextInputViewUpdateMask::translate(updater(&mut self.text_edit_state.borrow_mut()))
    }

    pub fn set_focus(
        &self,
        #[allow(unused_variables)] ht_manager: &HitTestTreeManager,
    ) -> TextInputViewUpdateMask {
        tracing::debug!("text input focus taken");

        if self.has_focus.replace(true) {
            // already taking focus
            return TextInputViewUpdateMask::empty();
        }

        #[cfg(windows)]
        self.native_text_input_context.notify_focus_enter();
        #[cfg(target_os = "macos")]
        ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted")
            .begin_text_input(core::ptr::from_ref(self).cast_mut());

        TextInputViewUpdateMask::FOCUS
    }

    pub fn release_focus(
        &self,
        #[allow(unused_variables)] ht_manager: &HitTestTreeManager,
    ) -> TextInputViewUpdateMask {
        tracing::debug!("text input focus released");

        if !self.has_focus.replace(false) {
            // already losing focus
            return TextInputViewUpdateMask::empty();
        }

        #[cfg(windows)]
        self.native_text_input_context.notify_focus_leave();
        #[cfg(target_os = "macos")]
        ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted")
            .end_text_input();

        // clear selection
        let update_mask =
            TextInputViewUpdateMask::translate(self.text_edit_state.borrow_mut().deselect());

        update_mask | TextInputViewUpdateMask::FOCUS
    }

    #[inline(always)]
    pub fn set_content(&self, content: String) -> TextInputViewUpdateMask {
        TextInputViewUpdateMask::translate(self.text_edit_state.borrow_mut().set_content(content))
    }

    pub fn move_cursor_by_point(
        &self,
        point: Point<LogicalUnit>,
        font_set: &FontSet,
    ) -> TextInputViewUpdateMask {
        let bytes = TextLayout::find_nearest_bytes(
            point.x - 2.0 - self.content_h_offset.get(),
            0.0,
            &self.text_edit_state.borrow().content,
            FontID::UIDefault,
            font_set,
        );

        TextInputViewUpdateMask::translate(
            self.text_edit_state
                .borrow_mut()
                .cursor_move_at(bytes, false),
        )
    }

    pub fn move_cursor_by_point_keep_selection(
        &self,
        point: Point<LogicalUnit>,
        font_set: &FontSet,
    ) -> TextInputViewUpdateMask {
        let bytes = TextLayout::find_nearest_bytes(
            point.x - 2.0 - self.content_h_offset.get(),
            0.0,
            &self.text_edit_state.borrow().content,
            FontID::UIDefault,
            font_set,
        );

        TextInputViewUpdateMask::translate(
            self.text_edit_state
                .borrow_mut()
                .cursor_move_at(bytes, true),
        )
    }

    fn update_focus<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.has_focus.get() {
            composite_tree
                .begin_mod_chain(self.ct_root)
                .border_color(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.5],
                    to_value: [1.0, 1.0, 1.0, 1.0],
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                })
                .apply();
            composite_tree
                .begin_mod_chain(self.ct_cursor)
                .opacity_imm(1.0)
                .apply();
        } else {
            composite_tree
                .begin_mod_chain(self.ct_root)
                .border_color(AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 1.0],
                    to_value: [1.0, 1.0, 1.0, 0.5],
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                })
                .apply();
            composite_tree
                .begin_mod_chain(self.ct_cursor)
                .opacity_imm(0.0)
                .apply();
        }
    }

    fn update_text<E>(&self, composite_tree: &mut CompositeTree<E>) {
        composite_tree
            .begin_mod_chain(self.ct_text)
            .text_run(CompositeRectTextRun {
                content: self.text_edit_state.borrow().content.clone(),
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            })
            .apply();
    }

    fn update_cursor_position<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
    ) {
        let state = self.text_edit_state.borrow();
        let cursor_x_global = TextLayout::measure_total_advances(
            &state.content[..state.cursor_pos_byte],
            FontID::UIDefault,
            system_link.font_set(),
        );

        let mut text_scroll_occured = false;
        let mut cursor_x_display = cursor_x_global + self.content_h_offset.get();
        if cursor_x_display < 0.0 {
            // 範囲外になる(左すぎ cursor_display_xが0になるようにスクロール量を調整)
            self.content_h_offset
                .set(self.content_h_offset.get() - cursor_x_display);
            text_scroll_occured = true;
            cursor_x_display = 0.0;
        } else if self.content_visible_width.get() - 2.0 < cursor_x_display {
            // 範囲外になる(右すぎ cursor_display_xがcontent_visible_widthになるようにスクロール量を調整)
            self.content_h_offset.set(
                self.content_h_offset.get()
                    - (cursor_x_display - (self.content_visible_width.get() - 2.0)),
            );
            text_scroll_occured = true;
            cursor_x_display = self.content_visible_width.get() - 2.0;
        }

        composite_tree
            .begin_mod_chain(self.ct_cursor)
            .x_imm(cursor_x_display)
            .apply();

        if text_scroll_occured {
            composite_tree
                .begin_mod_chain(self.ct_text)
                .x_imm(self.content_h_offset.get())
                .apply();

            self.update_preedit_underline(composite_tree, system_link);
            self.update_selection(composite_tree, system_link);
        }
    }

    fn update_preedit_underline<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
    ) {
        let state = self.text_edit_state.borrow();
        if !state.is_compositioning() {
            // no preedit
            composite_tree
                .begin_mod_chain(self.ct_preedit_underline)
                .opacity_imm(0.0)
                .apply();

            return;
        }

        let x1 = TextLayout::measure_total_advances(
            &state.content[..state.preedit_start_byte],
            FontID::UIDefault,
            system_link.font_set(),
        );
        let x2 = TextLayout::measure_total_advances(
            &state.content[..state.preedit_end_byte],
            FontID::UIDefault,
            system_link.font_set(),
        );

        composite_tree
            .begin_mod_chain(self.ct_preedit_underline)
            .x_imm(x1 + self.content_h_offset.get())
            .width_imm(x2 - x1)
            .opacity_imm(1.0)
            .apply();
    }

    fn update_selection<E>(&self, composite_tree: &mut CompositeTree<E>, system_link: &SystemLink) {
        let state = self.text_edit_state.borrow();
        let selection_range = state.selection_range();
        if selection_range.is_empty() {
            // no selection
            composite_tree
                .begin_mod_chain(self.ct_selection_bg)
                .width_imm(0.0)
                .apply();

            return;
        }

        let x1 = TextLayout::measure_total_advances(
            &state.content[..selection_range.start],
            FontID::UIDefault,
            system_link.font_set(),
        );
        let x2 = TextLayout::measure_total_advances(
            &state.content[..selection_range.end],
            FontID::UIDefault,
            system_link.font_set(),
        );

        composite_tree
            .begin_mod_chain(self.ct_selection_bg)
            .x_imm(x1 + self.content_h_offset.get())
            .width_imm(x2 - x1)
            .apply();
    }

    #[tracing::instrument(skip(self, composite_tree, system_link, ht_manager))]
    pub fn process_pending_updates_with_ht_mutation<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        let update_mask = self
            .pending_update_mask
            .replace(TextInputViewUpdateMask::empty());
        self.update_views(
            update_mask,
            composite_tree,
            system_link,
            ht_manager,
            current_sec,
        );

        // if update_mask.contains(TextInputViewUpdateMask::FOCUS)
        //     && !self
        //         .creation_flags
        //         .contains(RawTextInputViewCreateFlags::NON_DELEGATED_HT)
        // {
        //     ht_manager.get_data_mut(self.ht_root).active = self.has_focus.get();
        // }
    }

    pub fn process_pending_updates<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
        ht_manager: &HitTestTreeManager,
        current_sec: f32,
    ) {
        self.update_views(
            self.pending_update_mask
                .replace(TextInputViewUpdateMask::empty()),
            composite_tree,
            system_link,
            ht_manager,
            current_sec,
        );
    }

    #[tracing::instrument(skip(self, composite_tree, system_link, ht_manager))]
    pub fn update_views<E>(
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
        if mask.intersects(TextInputViewUpdateMask::CURSOR | TextInputViewUpdateMask::TEXT) {
            // needs update cursor position and selection highlight
            self.update_cursor_position(composite_tree, system_link);
            self.update_selection(composite_tree, system_link);
        }
        if mask.contains(TextInputViewUpdateMask::PREEDIT) {
            self.update_preedit_underline(composite_tree, system_link);
        }
        if mask.contains(TextInputViewUpdateMask::FOCUS) {
            self.update_focus(composite_tree, current_sec);
        }

        if Self::should_sync_selection_native(mask) {
            self.sync_selection_native(ht_manager, system_link);
        }
    }

    fn should_sync_selection_native(
        #[allow(unused_variables)] mask: TextInputViewUpdateMask,
    ) -> bool {
        // どっちにも影響する
        #[cfg(windows)]
        return mask.intersects(TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR);

        // 両方に影響する
        #[cfg(feature = "wayland")]
        return mask.intersects(TextInputViewUpdateMask::TEXT | TextInputViewUpdateMask::CURSOR);

        // fallback for unsupported platforms
        #[allow(unreachable_code)]
        return false;
    }

    fn sync_selection_native(&self, ht_manager: &HitTestTreeManager, system_link: &SystemLink) {
        let state = self.text_edit_state.borrow();

        #[cfg(windows)]
        let selection_range_acp = state.selection_range_win32_acp();
        #[cfg(windows)]
        self.native_text_input_context
            .notify_selection_changed(selection_range_acp.start, selection_range_acp.end);

        #[cfg(feature = "wayland")]
        let cursor_display_x = TextLayout::measure_total_advances(
            &state.content[..state.cursor_pos_byte],
            FontID::UIDefault,
            system_link.font_set(),
        ) + self.content_h_offset.get();
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
            &state.content,
            state.cursor_pos_byte,
            state.selection_anchor_byte.unwrap_or(state.cursor_pos_byte),
        );
        #[cfg(feature = "wayland")]
        system_link.ime_commit();
    }
}
#[cfg(windows)]
impl crate::platform::windows::TextProvider for TextInputViewCoreEventHandler {
    fn text(
        &self,
        range: windows::UI::Text::Core::CoreTextRange,
    ) -> windows_core::Result<windows_core::HSTRING> {
        let mut u16s = Vec::with_capacity((range.EndCaretPosition - range.StartCaretPosition) as _);
        for c in self
            .text_edit_state
            .borrow()
            .content
            .chars()
            .skip(range.StartCaretPosition as _)
            .take((range.EndCaretPosition - range.StartCaretPosition) as _)
        {
            u16s.extend_from_slice(c.encode_utf16(&mut [0; 2]));
        }

        Ok(windows_core::HSTRING::from_wide(&u16s))
    }

    fn selection(
        &self,
        req: &windows::UI::Text::Core::CoreTextSelectionRequest,
    ) -> windows_core::Result<()> {
        let range = self.text_edit_state.borrow().selection_range_win32_acp();

        req.SetSelection(windows::UI::Text::Core::CoreTextRange {
            StartCaretPosition: range.start,
            EndCaretPosition: range.end,
        })
    }
}
#[cfg(windows)]
impl crate::platform::windows::CoreTextDeferrableEventHandler for TextInputViewCoreEventHandler {
    fn layout(
        &self,
        ctx: &mut InputEventContext,
        req: &windows::UI::Text::Core::CoreTextLayoutRequest,
    ) -> windows_core::Result<()> {
        use crate::utils::Size;

        let range = req.Range()?;
        tracing::trace!(
            req.range = ?range,
            "edit_context.layout_requested"
        );

        let state = self.text_edit_state.borrow();
        let start_bytes = state
            .content
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let end_bytes = state
            .content
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        let (r, render_scale) = ctx
            .ht_manager
            .compute_screen_rect_with_render_scale(self.ht_root);
        let r = Rect::from_lt_size(
            Point::new_logical(r.left + 2.0, r.top + 2.0).to_pixels_round(render_scale),
            Size::new_logical(r.width - 4.0, r.height - 4.0).to_pixels_ceil(render_scale),
        );
        let o = TextLayout::measure_total_advances(
            &state.content[..start_bytes],
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );
        let w = TextLayout::measure_total_advances(
            &state.content[start_bytes..end_bytes],
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );

        tracing::debug!(?r, "ScreenRect");

        req.LayoutBounds()?
            .SetTextBounds(windows::Foundation::Rect {
                X: r.left as f32 + (o + self.content_h_offset.get()) * render_scale,
                Y: r.top as _,
                Width: w * render_scale,
                Height: r.height as _,
            })?;
        req.LayoutBounds()?
            .SetControlBounds(windows::Foundation::Rect {
                X: r.left as _,
                Y: r.top as _,
                Width: r.width as _,
                Height: r.height as _,
            })?;
        Ok(())
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
            current = &self.text_edit_state.borrow().content,
            "edit_context.text_updating"
        );

        let update_mask = TextInputViewUpdateMask::translate(
            self.text_edit_state
                .borrow_mut()
                .winct_update_text(&range, &text, &new_selection),
        );

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

        let update_mask = TextInputViewUpdateMask::translate(
            self.text_edit_state
                .borrow_mut()
                .winct_update_format(underline_type, &range),
        );
        self.update_views(
            update_mask,
            ctx.composite_tree,
            ctx.system_link,
            ctx.ht_manager,
            ctx.current_sec,
        );
        Ok(())
    }
}
#[cfg(target_os = "macos")]
impl crate::platform::mac::bridge::TextInputClientForwarding for TextInputViewCoreEventHandler {
    fn has_marked_text(&self) -> bool {
        let state = self.text_edit_state.borrow();

        tracing::debug!(
            start = state.preedit_start_byte,
            end = state.preedit_end_byte,
            "hasMarkedText"
        );

        state.preedit_start_byte != state.preedit_end_byte
    }

    fn marked_range(&self, out_location: *mut i64, out_length: *mut i64) -> bool {
        let state = self.text_edit_state.borrow();

        let start = state.preedit_start_byte;
        let end = state.preedit_end_byte;
        tracing::debug!(start, end, "markedRange");

        if start == end {
            return false;
        }

        let startc = state.content[..start].chars().count();
        let endc = state.content[..end].chars().count();

        unsafe {
            out_location.write(startc as _);
            out_length.write((endc - startc) as _);
        }
        true
    }

    fn selected_range(&self, out_location: *mut i64, out_length: *mut i64) {
        let state = self.text_edit_state.borrow();
        let r = state.selection_range();

        let startc = state.content[..r.start].chars().count();
        let endc = state.content[..r.end].chars().count();

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
        let mut state = self.text_edit_state.borrow_mut();

        tracing::debug!(
            ?text,
            new_selection_location,
            new_selection_length,
            replacement_location,
            replacement_length,
            "set marked text"
        );

        // なんかreplacement系の範囲が信用できなさそうなので自前でどこを書き換えるか判定する
        // （数値の解釈の方法の違いか？）
        let preedit_start = state.preedit_start_byte;
        let preedit_end = state.preedit_end_byte;
        if preedit_start == preedit_end {
            // non-preedit state
            let r = state.selection_range();
            let text = text.to_str().expect("invalid input str");
            state.content.replace_range(r.clone(), text);
            state.deselect();

            state.preedit_start_byte = r.start;
            state.preedit_end_byte = r.start + text.len();
            state.cursor_pos_byte = r.start + text.len();

            drop(state);
            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &mut *self.event_dispatcher }.dispatch(crate::Event::ScheduleViewRenderExt {
                id: self.delegated_view_id,
            });
        } else {
            let text = text.to_str().expect("invalid input str");
            state
                .content
                .replace_range(preedit_start..preedit_end, text);

            state.preedit_end_byte = preedit_start + text.len();
            state.cursor_pos_byte = preedit_start + text.len();

            drop(state);
            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &mut *self.event_dispatcher }.dispatch(crate::Event::ScheduleViewRenderExt {
                id: self.delegated_view_id,
            });
        }
    }

    fn insert_text(
        &self,
        text: &core::ffi::CStr,
        replacement_location: i64,
        replacement_length: i64,
    ) {
        let mut state = self.text_edit_state.borrow_mut();

        tracing::debug!(
            ?text,
            replacement_location,
            replacement_length,
            "insert text"
        );

        // なんかreplacement系の範囲が信用できなさそうなので自前でどこを書き換えるか判定する
        let preedit_start = state.preedit_start_byte;
        let preedit_end = state.preedit_end_byte;
        if preedit_start == preedit_end {
            // non-preedit state
            let r = state.selection_range();
            let text = text.to_str().expect("invalid input str");
            state.content.replace_range(r.clone(), text);
            state.deselect();

            state.preedit_start_byte = 0;
            state.preedit_end_byte = 0;
            state.cursor_pos_byte = r.start + text.len();

            drop(state);
            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &mut *self.event_dispatcher }.dispatch(crate::Event::ScheduleViewRenderExt {
                id: self.delegated_view_id,
            });
        } else {
            let text = text.to_str().expect("invalid input str");
            state
                .content
                .replace_range(preedit_start..preedit_end, text);

            state.preedit_end_byte = preedit_start;
            state.cursor_pos_byte = preedit_start + text.len();

            drop(state);
            self.pending_update_mask.update(|x| {
                x | TextInputViewUpdateMask::TEXT
                    | TextInputViewUpdateMask::CURSOR
                    | TextInputViewUpdateMask::PREEDIT
            });
            unsafe { &mut *self.event_dispatcher }.dispatch(crate::Event::ScheduleViewRenderExt {
                id: self.delegated_view_id,
            });
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
        let state = self.text_edit_state.borrow();
        let location = location.unwrap_or(0);
        let length = length.min(state.content.len() as i64);

        let loc = state
            .content
            .chars()
            .take(location as _)
            .map(|x| x.len_utf8())
            .sum();
        let endloc = state
            .content
            .chars()
            .take((location + length) as _)
            .map(|x| x.len_utf8())
            .sum::<usize>();

        unsafe {
            out_chars.write(state.content.as_ptr().add(loc).cast());
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
        let state = self.text_edit_state.borrow();
        tracing::debug!(location, length, "first rect");

        let endloc = state
            .content
            .chars()
            .take((location + length) as _)
            .map(|x| x.len_utf8())
            .sum();

        let tw = TextLayout::measure_total_advances(
            &state.content[..endloc],
            FontID::UIDefault,
            unsafe { &*self.font_set_ptr },
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

pub trait TextInputViewIO {
    fn text(&self, requester: ViewIdentifier, app: &Application) -> String;
    fn set_text(&self, sender: ViewIdentifier, app: &mut ApplicationMutation, text: String);
}

pub struct TextInputViewInit<IO: TextInputViewIO> {
    pub io: std::rc::Weak<IO>,
}
impl<IO: TextInputViewIO + 'static> ViewConstructor for TextInputViewInit<IO> {
    type ConcreteView = TextInputView;

    fn construct(self, id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        TextInputView {
            id,
            io: self.io,
            eh: None,
            should_revalidate_on_next_render: false,
        }
    }
}
impl<IO: TextInputViewIO> TextInputViewInit<IO> {
    #[inline(always)]
    pub const fn new(io: std::rc::Weak<IO>) -> Self {
        Self { io }
    }
}

pub struct TextInputView {
    id: TypedViewIdentifier<Self>,
    io: std::rc::Weak<dyn TextInputViewIO>,
    eh: Option<Rc<TextInputViewEventHandler>>,
    should_revalidate_on_next_render: bool,
}
impl TextInputView {
    pub fn new(
        id: TypedViewIdentifier<Self>,
        io: std::rc::Weak<impl TextInputViewIO + 'static>,
    ) -> Self {
        Self {
            id,
            eh: None,
            io: io as _,
            should_revalidate_on_next_render: false,
        }
    }

    pub fn revalidate(&mut self) {
        self.should_revalidate_on_next_render = true;
    }
}
impl View for TextInputView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let eh = match self.eh {
            Some(ref eh) => {
                eh.core.set_rect(layout_rect.clone(), ctx.composite_tree);
                ctx.ht_manager.get_data_mut(eh.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(eh.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(eh.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(eh.ht_root).height = layout_rect.height;

                eh
            }
            None => {
                // first render
                let kf_token = ctx.keyboard_focus_registry.acquire_token();
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width: layout_rect.width,
                    height: layout_rect.height,
                    left: layout_rect.left,
                    top: layout_rect.top,
                    cursor_shape: CursorShape::IBeam,
                    keyboard_focus: Some(kf_token),
                    ..Default::default()
                });
                let eh = Rc::new(TextInputViewEventHandler {
                    io: self.io.clone(),
                    core: TextInputViewCore::new(
                        ctx,
                        layout_rect.clone(),
                        [0.0; 2],
                        [0.0; 2],
                        self.id.into_untyped(),
                        ht_root,
                    ),
                    id: self.id,
                    token: kf_token,
                    ht_root,
                });
                ctx.ht_manager.set_action_handler(eh.ht_root, &eh);
                ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);

                &*self.eh.insert(eh)
            }
        };

        if core::mem::replace(&mut self.should_revalidate_on_next_render, false) {
            eh.core.eh.update_views(
                eh.core.eh.perform_external_state_update(|st| {
                    st.set_content(
                        self.io
                            .upgrade()
                            .expect("TextInputView has defunct")
                            .text(self.id.into_untyped(), ctx.application),
                    )
                }),
                ctx.composite_tree,
                ctx.system_link,
                ctx.ht_manager,
                ctx.current_sec,
            );
        }

        eh.core.eh.process_pending_updates_with_ht_mutation(
            ctx.composite_tree,
            ctx.system_link,
            ctx.ht_manager,
            ctx.current_sec,
        );

        ViewRenderElements {
            composite_tree: Some(eh.core.eh.ct_root),
            hit_tree: Some(eh.ht_root),
            keyboard_focus: Some(eh.token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut super::TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.keyboard_focus_registry.release_token(entity.token);
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(32.0, 16.0)
    }
}

struct TextInputViewEventHandler {
    io: std::rc::Weak<dyn TextInputViewIO>,
    core: TextInputViewCore,
    id: TypedViewIdentifier<TextInputView>,
    token: FocusTargetToken,
    ht_root: HitTestTreeRef,
}
impl KeyInputEventHandler for TextInputViewEventHandler {
    #[inline(always)]
    fn focus_taken(&self, context: &mut InputEventContext) {
        self.core.eh.focus_taken(context)
    }

    #[inline(always)]
    fn focus_released(&self, context: &mut InputEventContext) {
        self.core.eh.focus_released(context);
    }

    fn r#char(&self, context: &mut InputEventContext, ch: char, modifier: ModifierKey) {
        let r = self.core.eh.r#char(context, ch, modifier);
        self.io
            .upgrade()
            .expect("TextInputView has defunct")
            .set_text(
                self.id.into_untyped(),
                &mut context.application,
                self.core.eh.text_edit_state.borrow().content.clone(),
            );
        r
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        let r = self.core.eh.keydown(context, code, modifier);
        self.io
            .upgrade()
            .expect("TextInputView has defunct")
            .set_text(
                self.id.into_untyped(),
                &mut context.application,
                self.core.eh.text_edit_state.borrow().content.clone(),
            );
        r
    }

    #[inline(always)]
    fn keyup(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        self.core.eh.keyup(context, code, modifier)
    }

    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
    ) {
        self.core
            .eh
            .ime_state_changes(context, new_committed_string, new_preedit_string);
        self.io
            .upgrade()
            .expect("TextInputView has defunct")
            .set_text(
                self.id.into_untyped(),
                &mut context.application,
                self.core.eh.text_edit_state.borrow().content.clone(),
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
        // forward first event
        self.core.eh.on_pointer_down(sender, context, args);

        // 下の要素にフォーカス処理がいかないようにする
        EventContinueControl::STOP_PROPAGATION
    }

    #[inline(always)]
    fn on_drag_start(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.core.eh.on_drag_start(sender, context, args)
    }

    #[inline(always)]
    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.core.eh.on_drag_move(sender, context, args)
    }

    #[inline(always)]
    fn on_drag_end(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.core.eh.on_drag_end(sender, context, args)
    }

    #[inline(always)]
    fn on_double_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.core.eh.on_double_click(sender, context, args)
    }
}

pub trait NumericInputViewIO: TextInputViewIO {
    fn set_delta(&self, sender: ViewIdentifier, app: &mut ApplicationMutation, delta: f32);
}

pub struct NumericInputViewInit<ValueIO: NumericInputViewIO + 'static> {
    pub value: std::rc::Weak<ValueIO>,
}
impl<ValueIO: NumericInputViewIO + 'static> Default for NumericInputViewInit<ValueIO> {
    fn default() -> Self {
        Self {
            value: std::rc::Weak::new(),
        }
    }
}
impl<ValueIO: NumericInputViewIO + 'static> ViewConstructor for NumericInputViewInit<ValueIO> {
    type ConcreteView = NumericInputView;

    #[inline(always)]
    fn construct(self, id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        NumericInputView {
            id,
            eh: None,
            value: self.value as _,
            should_revalidate_on_next_render: Cell::new(true),
        }
    }
}

pub struct NumericInputView {
    id: TypedViewIdentifier<Self>,
    eh: Option<Rc<NumericInputViewEventHandler>>,
    value: std::rc::Weak<dyn NumericInputViewIO>,
    should_revalidate_on_next_render: Cell<bool>,
}
impl NumericInputView {
    pub fn revalidate(&self) {
        self.should_revalidate_on_next_render.set(true);
    }
}
impl View for NumericInputView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let eh = match self.eh {
            Some(ref x) => {
                x.core.set_rect(layout_rect.clone(), ctx.composite_tree);
                ctx.ht_manager.get_data_mut(x.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(x.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(x.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(x.ht_root).height = layout_rect.height;

                ctx.ht_manager.get_data_mut(x.ht_root).cursor_shape = if x.key_input_enabled.get() {
                    CursorShape::IBeam
                } else {
                    CursorShape::ResizeVertical
                };

                x
            }
            None => {
                // first render
                let kf_token = ctx.keyboard_focus_registry.acquire_token();

                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width: layout_rect.width,
                    height: layout_rect.height,
                    left: layout_rect.left,
                    top: layout_rect.top,
                    cursor_shape: CursorShape::ResizeVertical,
                    keyboard_focus: Some(kf_token),
                    ..Default::default()
                });
                let eh = Rc::new(NumericInputViewEventHandler {
                    core: TextInputViewCore::new(
                        ctx,
                        layout_rect.clone(),
                        [0.0; 2],
                        [0.0; 2],
                        self.id.into_untyped(),
                        ht_root,
                    ),
                    value: self.value.clone(),
                    kf_token,
                    ht_root,
                    key_input_enabled: Cell::new(false),
                });
                ctx.ht_manager.set_action_handler(eh.ht_root, &eh);
                ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);

                &*self.eh.insert(eh)
            }
        };

        if self.should_revalidate_on_next_render.replace(false) {
            eh.core.eh.update_views(
                eh.core.eh.perform_external_state_update(|st| {
                    st.set_content(
                        eh.value
                            .upgrade()
                            .expect("NumericInputView has defunct")
                            .text(self.id.into_untyped(), ctx.application),
                    )
                }),
                ctx.composite_tree,
                ctx.system_link,
                ctx.ht_manager,
                ctx.current_sec,
            );
        }

        eh.core.eh.process_pending_updates_with_ht_mutation(
            ctx.composite_tree,
            ctx.system_link,
            ctx.ht_manager,
            ctx.current_sec,
        );

        ViewRenderElements {
            composite_tree: Some(eh.core.eh.ct_root),
            hit_tree: Some(eh.ht_root),
            keyboard_focus: Some(eh.kf_token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut super::TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.keyboard_focus_registry.release_token(entity.kf_token);
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(32.0, 16.0)
    }
}

struct NumericInputViewEventHandler {
    core: TextInputViewCore,
    value: std::rc::Weak<dyn NumericInputViewIO>,
    kf_token: FocusTargetToken,
    ht_root: HitTestTreeRef,
    key_input_enabled: Cell<bool>,
}
impl KeyInputEventHandler for NumericInputViewEventHandler {
    fn focus_released(&self, context: &mut InputEventContext) {
        self.confirm_direct_input(
            context.ht_manager,
            context.view_render_queue,
            &mut context.application,
        );
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        if code == KeyInputCode::Enter {
            // 確定or入力開始
            if self.key_input_enabled.get() {
                self.confirm_direct_input(
                    context.ht_manager,
                    context.view_render_queue,
                    &mut context.application,
                );
            } else {
                self.begin_direct_input(
                    context.ht_manager,
                    context.view_render_queue,
                    &context.application,
                );
            }

            return;
        }

        if code == KeyInputCode::Esc {
            // 入力キャンセル
            self.cancel_direct_input(
                context.ht_manager,
                context.view_render_queue,
                &context.application,
            );
            return;
        }

        // delegate
        self.core.eh.keydown(context, code, modifier);
    }

    #[inline(always)]
    fn r#char(&self, context: &mut InputEventContext, ch: char, modifier: ModifierKey) {
        // delegate
        self.core.eh.r#char(context, ch, modifier);
    }

    #[inline(always)]
    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
    ) {
        // delegate
        self.core
            .eh
            .ime_state_changes(context, new_committed_string, new_preedit_string);
    }
}
impl HitTestTreeActionHandler for NumericInputViewEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if self.key_input_enabled.get() {
            // delegate
            self.core.eh.on_pointer_down(sender, context, args);
        }

        // 下の要素にフォーカス処理がいかないようにする
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if self.key_input_enabled.get() {
            // delegate
            return self.core.eh.on_drag_start(_sender, _context, _args);
        }

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::GRAB_POINTER
    }

    fn grab_delta_move(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &crate::input::hittest::GrabDeltaMoveActionArgs,
    ) -> EventContinueControl {
        let value = self.value.upgrade().expect("NumericInputView has defunct");
        value.set_delta(
            self.core.eh.delegated_view_id,
            &mut context.application,
            -args.delta.y,
        );
        self.core.eh.update_views(
            self.core.eh.perform_external_state_update(|st| {
                st.set_content(value.text(self.core.eh.delegated_view_id, &context.application))
            }),
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if self.key_input_enabled.get() {
            // delegate
            return self.core.eh.on_drag_move(sender, context, args);
        }

        EventContinueControl::empty()
    }

    fn on_drag_end(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if self.key_input_enabled.get() {
            // delegate
            return self.core.eh.on_drag_end(_sender, _context, _args);
        }

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::RELEASE_CAPTURE_ELEMENT
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_scroll_wheel(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &ScrollWheelActionArgs,
    ) -> ScrollWheelActionResponse {
        let value = self.value.upgrade().expect("NumericInputView has defunct");
        value.set_delta(
            self.core.eh.delegated_view_id,
            &mut context.application,
            args.amount,
        );
        self.core.eh.update_views(
            self.core.eh.perform_external_state_update(|st| {
                st.set_content(value.text(self.core.eh.delegated_view_id, &context.application))
            }),
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        ScrollWheelActionResponse {
            continue_flags: EventContinueControl::STOP_PROPAGATION,
            left_amount: 0.0,
        }
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.begin_direct_input(
            context.ht_manager,
            context.view_render_queue,
            &context.application,
        );

        EventContinueControl::STOP_PROPAGATION
    }

    #[inline(always)]
    fn on_double_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.core.eh.on_double_click(sender, context, args)
    }
}
impl NumericInputViewEventHandler {
    fn begin_direct_input(
        &self,
        ht_manager: &HitTestTreeManager,
        view_render_queue: &mut ViewRenderQueue,
        application: &Application,
    ) {
        if self.key_input_enabled.replace(true) {
            // already enabled
            return;
        }

        // HitTestTreeへの変更がはいるので遅延させる(最初は全選択状態)
        self.core
            .eh
            .lazy_update_and_schedule(view_render_queue, |e| {
                let update_mask = e.perform_external_state_update(|st| {
                    st.set_content(
                        self.value
                            .upgrade()
                            .expect("NumericInputView has defunct")
                            .text(self.core.eh.delegated_view_id, application),
                    ) | st.select_all()
                });

                e.set_focus(ht_manager) | update_mask
            });
    }

    fn confirm_direct_input(
        &self,
        ht_manager: &HitTestTreeManager,
        view_render_queue: &mut ViewRenderQueue,
        application: &mut ApplicationMutation,
    ) {
        if !self.key_input_enabled.replace(false) {
            // already disabled
            return;
        }

        let value = self.value.upgrade().expect("NumericInputView has defunct");
        value.set_text(
            self.core.eh.delegated_view_id,
            application,
            self.core.eh.text_edit_state.borrow().content.clone(),
        );

        // HitTestTreeへの変更がはいるので遅延させる
        self.core
            .eh
            .lazy_update_and_schedule(view_render_queue, |e| {
                let mut update_mask = e.release_focus(ht_manager);
                update_mask |= e.set_content(value.text(e.delegated_view_id, application));
                update_mask |= e.perform_external_state_update(|st| st.cursor_move_to_home(false));

                update_mask
            });
    }

    fn cancel_direct_input(
        &self,
        ht_manager: &HitTestTreeManager,
        view_render_queue: &mut ViewRenderQueue,
        application: &Application,
    ) {
        self.key_input_enabled.set(false);

        // HitTestTreeへの変更がはいるので遅延させる
        self.core
            .eh
            .lazy_update_and_schedule(view_render_queue, |e| {
                let mut update_mask = e.release_focus(ht_manager);
                // キャンセル時はもとにもどす
                update_mask |= e.set_content(
                    self.value
                        .upgrade()
                        .expect("NumericInputView has defunct")
                        .text(e.delegated_view_id, application),
                );
                update_mask |= e.perform_external_state_update(|st| st.cursor_move_to_home(false));

                update_mask
            });
    }
}

pub struct MultilineTextInputView {
    id: TypedViewIdentifier<Self>,
    eh: Option<Rc<MultilineTextInputEventHandler>>,
}
impl MultilineTextInputView {
    pub fn new(id: TypedViewIdentifier<Self>) -> Self {
        Self { id, eh: None }
    }
}
impl View for MultilineTextInputView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let eh = match self.eh {
            Some(ref eh) => {
                ctx.composite_tree
                    .begin_mod_chain(eh.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();
                ctx.ht_manager.get_data_mut(eh.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(eh.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(eh.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(eh.ht_root).height = layout_rect.height;

                eh
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
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_text_clip = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [
                        AnimatableFloat::Value(layout_rect.width - 4.0),
                        AnimatableFloat::Value(layout_rect.height - 4.0),
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
                let ct_text = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    ..Default::default()
                });
                let ct_cursor = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(16.0)],
                    offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 1.0,
                    ])),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ct_preedit_underline = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [AnimatableFloat::Value(1.0), AnimatableFloat::Value(1.0)],
                    offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(14.0)],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 1.0,
                    ])),
                    opacity: AnimatableFloat::Value(0.0),
                    ..Default::default()
                });
                let ct_selection_base = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    ..Default::default()
                });

                ctx.composite_tree
                    .add_child(ct_text_clip, ct_selection_base);
                ctx.composite_tree.add_child(ct_text_clip, ct_text);
                ctx.composite_tree.add_child(ct_text_clip, ct_cursor);
                ctx.composite_tree
                    .add_child(ct_text_clip, ct_preedit_underline);
                ctx.composite_tree.add_child(ct_root, ct_text_clip);

                let eh = Rc::new(MultilineTextInputEventHandler {
                    view_id: self.id,
                    kf_token,
                    ht_root: ctx.ht_manager.create(HitTestTreeData {
                        left: layout_rect.left,
                        top: layout_rect.top,
                        width: layout_rect.width,
                        height: layout_rect.height,
                        cursor_shape: CursorShape::IBeam,
                        keyboard_focus: Some(kf_token),
                        ..Default::default()
                    }),
                    ct_root,
                    ct_text,
                    ct_cursor,
                    ct_preedit_underline,
                    ct_selection_base,
                    ct_selection_bgs: core::cell::RefCell::new(Vec::new()),
                    ct_text_clip,
                    has_focus: core::cell::Cell::new(false),
                    content_h_offset: core::cell::Cell::new(0.0),
                    content_v_offset: core::cell::Cell::new(0.0),
                    content_visible_width: layout_rect.width - 4.0,
                    content_visible_height: layout_rect.height - 4.0,
                    content: core::cell::RefCell::new(String::new()),
                    cursor_pos_bytes: core::cell::Cell::new(0),
                    preedit_range_start_bytes: core::cell::Cell::new(0),
                    preedit_range_end_bytes: core::cell::Cell::new(0),
                    selection_begin_bytes: core::cell::Cell::new(0),
                    #[cfg(windows)]
                    native_text_input_context:
                        crate::platform::windows::NativeTextInputContext::new(ctx.system_link),
                    #[cfg(target_os = "macos")]
                    ht_manager_ptr: core::ptr::from_mut(ctx.ht_manager).cast(),
                    #[cfg(target_os = "macos")]
                    font_set_ptr: core::ptr::from_ref(ctx.system_link.font_set()),
                    pending_update_mask: core::cell::Cell::new(TextInputViewUpdateMask::empty()),
                    event_dispatcher: ctx.system_link.event_dispatcher,
                });
                ctx.ht_manager.set_action_handler(eh.ht_root, &eh);
                ctx.ht_manager
                    .set_screen_reposition_handler(eh.ht_root, &eh);
                ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);
                #[cfg(windows)]
                ctx.ht_manager
                    .set_native_text_deferrable_event_handler(eh.ht_root, &eh);
                #[cfg(windows)]
                eh.native_text_input_context
                    .bind_action(ctx.system_link, &eh, eh.ht_root);

                eh.update_text(ctx.composite_tree);

                // TODO: これ見直したほうがよさそう(入力メソッドのポップをウィンドウ移動に追従させるために機構 ウィンドウ移動を毎回全Viewに流すと流石に重いと思うのでなんかいい感じに絞りたい)
                /*
                #[cfg(windows)]
                unsafe {
                    ctx.ht_manager
                        .query_root_window(parent.ht_root())
                        .expect("no root window")
                        .extra_data_mut::<crate::PerWindowData>()
                        .screen_reposition_interests
                        .insert(self.eh.ht_root);
                }*/

                &*self.eh.insert(eh)
            }
        };

        eh.process_pending_updates_with_ht_mutation(
            ctx.composite_tree,
            ctx.system_link,
            ctx.ht_manager,
            ctx.current_sec,
        );

        ViewRenderElements {
            composite_tree: Some(eh.ct_root),
            hit_tree: Some(eh.ht_root),
            keyboard_focus: Some(eh.kf_token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut super::TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.keyboard_focus_registry.release_token(entity.kf_token);
    }

    fn measure_preferred_content_size(&self, ctx: &mut super::MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(128.0, 96.0)
    }
}

struct MultilineTextInputEventHandler {
    view_id: TypedViewIdentifier<MultilineTextInputView>,
    kf_token: FocusTargetToken,
    ht_root: HitTestTreeRef,
    ct_root: CompositeTreeRef,
    ct_text: CompositeTreeRef,
    ct_cursor: CompositeTreeRef,
    ct_preedit_underline: CompositeTreeRef,
    ct_selection_base: CompositeTreeRef,
    ct_selection_bgs: core::cell::RefCell<Vec<CompositeTreeRef>>,
    ct_text_clip: CompositeTreeRef,
    has_focus: core::cell::Cell<bool>,
    content_h_offset: core::cell::Cell<f32>,
    content_v_offset: core::cell::Cell<f32>,
    content_visible_width: f32,
    content_visible_height: f32,
    content: core::cell::RefCell<String>,
    cursor_pos_bytes: core::cell::Cell<usize>,
    preedit_range_start_bytes: core::cell::Cell<usize>,
    preedit_range_end_bytes: core::cell::Cell<usize>,
    selection_begin_bytes: core::cell::Cell<usize>,
    #[cfg(windows)]
    native_text_input_context: crate::platform::windows::NativeTextInputContext,
    #[cfg(target_os = "macos")]
    ht_manager_ptr: *const HitTestTreeManager<'static>,
    #[cfg(target_os = "macos")]
    font_set_ptr: *const FontSet,
    pending_update_mask: core::cell::Cell<TextInputViewUpdateMask>,
    event_dispatcher: *mut LogicFiberEventDispatcher,
}
impl KeyInputEventHandler for MultilineTextInputEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        // HitTestTreeへの変更がはいるので遅延させる
        self.pending_update_mask
            .set(self.set_focus(context.ht_manager));
        context.schedule_view_render(self.view_id);
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        // HitTestTreeへの変更がはいるので遅延させる
        self.pending_update_mask
            .set(self.release_focus(context.ht_manager));
        context.schedule_view_render(self.view_id);
    }

    #[inline(always)]
    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        tracing::debug!(?code, "keydown");

        let update_mask = match code {
            // cursor operations
            KeyInputCode::LeftArrow => {
                self.move_cursor_to_left(modifier.contains(ModifierKey::SHIFT))
            }
            KeyInputCode::RightArrow => {
                self.move_cursor_to_right(modifier.contains(ModifierKey::SHIFT))
            }
            KeyInputCode::UpArrow => {
                self.move_cursor_to_up(modifier.contains(ModifierKey::SHIFT), context.system_link)
            }
            KeyInputCode::DownArrow => {
                self.move_cursor_to_down(modifier.contains(ModifierKey::SHIFT), context.system_link)
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
            KeyInputCode::Enter if !self.has_selection() => self.insert_char_at_cursor('\n'),
            KeyInputCode::Enter => self.replace_selection_by_char('\n'),
            // nop for chars(char callback will be process this)
            KeyInputCode::Character(_) => TextInputViewUpdateMask::empty(),
            _ => TextInputViewUpdateMask::empty(),
        };

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    #[inline(always)]
    fn r#char(&self, context: &mut InputEventContext, ch: char, modifier: ModifierKey) {
        tracing::debug!(%ch, "char");

        if ch.is_control() {
            return;
        }

        let update_mask = if self.has_selection() {
            self.replace_selection_by_char(ch)
        } else {
            self.insert_char_at_cursor(ch)
        };

        self.update_views(
            update_mask,
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );
    }

    #[inline(always)]
    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
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

        if has_preedit_text && let Some(new_preedit_string) = new_preedit_string {
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

        if let Some(new_committed_string) = new_committed_string
            && !new_committed_string.is_empty()
        {
            // insert committed
            self.content
                .borrow_mut()
                .insert_str(self.cursor_pos_bytes.get(), new_committed_string);
            self.cursor_pos_bytes
                .set(self.cursor_pos_bytes.get() + new_committed_string.len());
        }

        if !has_preedit_text
            && let Some(new_preedit_string) = new_preedit_string
            && !new_preedit_string.is_empty()
        {
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
impl HitTestTreeScreenRepositionHandler for MultilineTextInputEventHandler {
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
impl HitTestTreeActionHandler for MultilineTextInputEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        self.update_views(
            self.move_cursor_by_point(
                Point::new_logical(local_x, local_y),
                context.system_link.font_set(),
            ),
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
        let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        self.update_views(
            self.move_cursor_by_point_keep_selection(
                Point::new_logical(local_x, local_y),
                context.system_link.font_set(),
            ),
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
        let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        self.update_views(
            self.move_cursor_by_point_keep_selection(
                Point::new_logical(local_x, local_y),
                context.system_link.font_set(),
            ),
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
        self.update_views(
            self.select_word_at_cursor(),
            context.composite_tree,
            context.system_link,
            context.ht_manager,
            context.current_sec,
        );

        EventContinueControl::STOP_PROPAGATION
    }
}
impl MultilineTextInputEventHandler {
    pub fn set_focus(
        &self,
        #[allow(dead_code)] ht_manager: &HitTestTreeManager,
    ) -> TextInputViewUpdateMask {
        tracing::debug!("text input focus taken");

        if self.has_focus.replace(true) {
            // already taking focus
            return TextInputViewUpdateMask::empty();
        }

        #[cfg(windows)]
        self.native_text_input_context.notify_focus_enter();
        #[cfg(target_os = "macos")]
        ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted")
            .begin_text_input(core::ptr::from_ref(self).cast_mut());

        TextInputViewUpdateMask::FOCUS
    }

    pub fn release_focus(
        &self,
        #[allow(dead_code)] ht_manager: &HitTestTreeManager,
    ) -> TextInputViewUpdateMask {
        tracing::debug!("text input focus released");

        if !self.has_focus.replace(false) {
            // already losing focus
            return TextInputViewUpdateMask::empty();
        }

        #[cfg(windows)]
        self.native_text_input_context.notify_focus_leave();
        #[cfg(target_os = "macos")]
        ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted")
            .end_text_input();

        // clear selection
        let update_mask = self.deselect();

        update_mask | TextInputViewUpdateMask::FOCUS
    }

    pub fn move_cursor_by_point(
        &self,
        point: Point<LogicalUnit>,
        font_set: &FontSet,
    ) -> TextInputViewUpdateMask {
        let bytes = TextLayout::find_nearest_bytes(
            point.x - 2.0 - self.content_h_offset.get(),
            point.y - 2.0 - self.content_v_offset.get(),
            &self.content.borrow(),
            FontID::UIDefault,
            font_set,
        );

        self.move_cursor(bytes)
    }

    pub fn move_cursor_by_point_keep_selection(
        &self,
        point: Point<LogicalUnit>,
        font_set: &FontSet,
    ) -> TextInputViewUpdateMask {
        let bytes = TextLayout::find_nearest_bytes(
            point.x - 2.0 - self.content_h_offset.get(),
            point.y - 2.0 - self.content_v_offset.get(),
            &self.content.borrow(),
            FontID::UIDefault,
            font_set,
        );

        self.move_cursor_keep_selection(bytes)
    }

    pub fn select_word_at_cursor(&self) -> TextInputViewUpdateMask {
        let content = self.content.borrow();
        if content.is_empty() {
            return self.move_cursor(0);
        }

        let cursor_pos_bytes = self.cursor_pos_bytes.get();

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

            self.select_range(start..end)
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

            self.select_range(start_bytes..end_bytes)
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
                let is_budou_cluster_c = crate::utils::is_budou_cluster_char(c);
                if is_budou_cluster != is_budou_cluster_c {
                    // breaking method boundary
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

            return self.select_range(select_range);
        }
    }

    fn update_focus<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.has_focus.get() {
            composite_tree.get_mut(self.ct_root).border = Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.5],
                    to_value: [1.0, 1.0, 1.0, 1.0],
                    sec_duration: (current_sec..current_sec + 0.1).into(),
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
                    sec_duration: (current_sec..current_sec + 0.1).into(),
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
        #[allow(dead_code)] ht_manager: &HitTestTreeManager,
    ) {
        let cr = TextLayout::measure_cursor_rect(
            &self.content.borrow()[..self.cursor_pos_bytes.get()],
            FontID::UIDefault,
            system_link.font_set(),
        );
        tracing::debug!(?cr, "cursor rect");

        let mut text_scroll_occured = false;
        let cursor_rect = composite_tree.get_mut(self.ct_cursor);
        let mut cursor_display_x = cr.left + self.content_h_offset.get();
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
        let mut cursor_display_y = cr.top + self.content_v_offset.get();
        if cursor_display_y < 0.0 {
            // 範囲外になる(上すぎ cursor_display_yが0になるようにスクロール量を調整)
            self.content_v_offset
                .set(self.content_v_offset.get() - cursor_display_y);
            text_scroll_occured = true;
            cursor_display_y = 0.0;
        } else if self.content_visible_height - 2.0 < cursor_display_y + cr.height {
            // 範囲外になる(下すぎ cursor_display_yがcontent_visible_height - cr.heightになるようにスクロール量を調整)
            self.content_v_offset.set(
                self.content_v_offset.get()
                    - (cursor_display_y - (self.content_visible_height - 2.0 - cr.height)),
            );
            text_scroll_occured = true;
            cursor_display_y = self.content_visible_height - 2.0 - cr.height;
        }
        cursor_rect.offset[0] = AnimatableFloat::Value(cursor_display_x);
        cursor_rect.offset[1] = AnimatableFloat::Value(cursor_display_y);
        cursor_rect.size[1] = AnimatableFloat::Value(cr.height);

        #[cfg(feature = "wayland")]
        let (sx, sy) = ht_manager.translate_tree_local_to_root_autoroot(
            self.ht_root,
            2.0 + cursor_display_x,
            2.0 + cursor_display_y,
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
            composite_tree.get_mut(self.ct_text).offset[1] =
                AnimatableFloat::Value(self.content_v_offset.get());
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

        let rects = TextLayout::measure_line_rects(
            &self.content.borrow(),
            preedit_range,
            FontID::UIDefault,
            system_link.font_set(),
        );

        // TODO: multiline preedit?
        let underline_rect = composite_tree.get_mut(self.ct_preedit_underline);
        underline_rect.offset[0] =
            AnimatableFloat::Value(rects[0].left + self.content_h_offset.get());
        underline_rect.offset[1] =
            AnimatableFloat::Value(rects[0].bottom() + self.content_v_offset.get());
        underline_rect.size[0] = AnimatableFloat::Value(rects[0].width);
        underline_rect.opacity = AnimatableFloat::Value(1.0);

        composite_tree.mark_dirty(self.ct_preedit_underline);
    }

    fn update_selection<E>(&self, composite_tree: &mut CompositeTree<E>, system_link: &SystemLink) {
        let selection_range = self.selection_range();
        if selection_range.is_empty() {
            // no selection
            for x in self.ct_selection_bgs.borrow_mut().drain(..) {
                composite_tree.remove_child(x);
                composite_tree.free(x);
            }

            return;
        }

        let rects = TextLayout::measure_line_rects(
            &self.content.borrow(),
            selection_range.clone(),
            FontID::UIDefault,
            system_link.font_set(),
        );
        tracing::debug!(?selection_range, ?rects, "selection rects");

        // TODO: いったん雑に全部作り直す 最適化はあとで
        for x in self.ct_selection_bgs.borrow_mut().drain(..) {
            composite_tree.remove_child(x);
            composite_tree.free(x);
        }

        self.ct_selection_bgs.borrow_mut().reserve(rects.len());
        for r in rects {
            let ct = composite_tree.create(CompositeRect {
                scale_factor: CompositeRectScaleFactor::UI,
                offset: [
                    AnimatableFloat::Value(r.left + self.content_h_offset.get()),
                    AnimatableFloat::Value(r.top + self.content_v_offset.get()),
                ],
                size: [
                    AnimatableFloat::Value(r.width),
                    AnimatableFloat::Value(r.height),
                ],
                has_bitmap: true,
                composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                    0.25, 0.5, 1.0, 0.5,
                ])),
                ..Default::default()
            });
            composite_tree.add_child(self.ct_selection_base, ct);
            self.ct_selection_bgs.borrow_mut().push(ct);
        }
    }

    pub fn process_pending_updates_with_ht_mutation<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        let update_mask = self
            .pending_update_mask
            .replace(TextInputViewUpdateMask::empty());
        self.update_views(
            update_mask,
            composite_tree,
            system_link,
            ht_manager,
            current_sec,
        );

        // if update_mask.contains(TextInputViewUpdateMask::FOCUS)
        //     && !self
        //         .creation_flags
        //         .contains(RawTextInputViewCreateFlags::NON_DELEGATED_HT)
        // {
        //     ht_manager.get_data_mut(self.ht_root).active = self.has_focus.get();
        // }
    }

    pub fn process_pending_updates<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        system_link: &SystemLink,
        ht_manager: &HitTestTreeManager,
        current_sec: f32,
    ) {
        self.update_views(
            self.pending_update_mask
                .replace(TextInputViewUpdateMask::empty()),
            composite_tree,
            system_link,
            ht_manager,
            current_sec,
        );
    }

    pub fn update_views<E>(
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
        if mask.intersects(TextInputViewUpdateMask::CURSOR | TextInputViewUpdateMask::TEXT) {
            // needs update cursor position and selection highlight
            self.update_cursor_position(composite_tree, system_link, ht_manager);
            self.update_selection(composite_tree, system_link);
        }
        if mask.contains(TextInputViewUpdateMask::PREEDIT) {
            self.update_preedit_underline(composite_tree, system_link);
        }
        if mask.contains(TextInputViewUpdateMask::FOCUS) {
            self.update_focus(composite_tree, current_sec);
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

    #[inline(always)]
    pub fn select_all(&self) -> TextInputViewUpdateMask {
        self.select_range(0..self.content.borrow().len())
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
        let mut new_cursor_pos = self.cursor_pos_bytes.get();
        while new_cursor_pos > 0
            && !(self.content.borrow().is_char_boundary(new_cursor_pos - 1)
                && self.content.borrow().as_bytes()[new_cursor_pos - 1] == b'\n')
        {
            new_cursor_pos -= 1;
        }

        self.move_cursor(new_cursor_pos)
    }

    fn jump_to_end_of_line(&self) -> TextInputViewUpdateMask {
        let mut new_cursor_pos = self.cursor_pos_bytes.get();
        while new_cursor_pos < self.content.borrow().len()
            && !(self.content.borrow().is_char_boundary(new_cursor_pos)
                && self.content.borrow().as_bytes()[new_cursor_pos] == b'\n')
        {
            new_cursor_pos += 1;
        }

        self.move_cursor(new_cursor_pos)
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

    fn move_cursor_to_up(
        &self,
        with_selection: bool,
        system_link: &SystemLink,
    ) -> TextInputViewUpdateMask {
        // 雑にカーソル座標のちょっと上をみてみる
        // TODO: 必要なら最適化
        let cr = TextLayout::measure_cursor_rect(
            &self.content.borrow()[..self.cursor_pos_bytes.get()],
            FontID::UIDefault,
            system_link.font_set(),
        );
        let bytes = TextLayout::find_nearest_bytes(
            cr.left,
            cr.top - 1.0,
            &self.content.borrow(),
            FontID::UIDefault,
            system_link.font_set(),
        );

        if with_selection {
            self.move_cursor_keep_selection(bytes)
        } else {
            self.move_cursor(bytes)
        }
    }

    fn move_cursor_to_down(
        &self,
        with_selection: bool,
        system_link: &SystemLink,
    ) -> TextInputViewUpdateMask {
        // 雑にカーソル座標のちょっと下をみてみる
        // TODO: 必要なら最適化
        let cr = TextLayout::measure_cursor_rect(
            &self.content.borrow()[..self.cursor_pos_bytes.get()],
            FontID::UIDefault,
            system_link.font_set(),
        );
        let bytes = TextLayout::find_nearest_bytes(
            cr.left,
            cr.top + cr.height + 1.0,
            &self.content.borrow(),
            FontID::UIDefault,
            system_link.font_set(),
        );

        if with_selection {
            self.move_cursor_keep_selection(bytes)
        } else {
            self.move_cursor(bytes)
        }
    }
}
#[cfg(windows)]
impl crate::platform::windows::TextProvider for MultilineTextInputEventHandler {
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
            u16s.extend_from_slice(c.encode_utf16(&mut [0; 2]));
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
impl crate::platform::windows::CoreTextDeferrableEventHandler for MultilineTextInputEventHandler {
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

        let content = self.content.borrow();
        let start_bytes = content
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let end_bytes = content
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        let r = ctx.ht_manager.compute_screen_rect_pixels_with_insets(
            self.ht_root,
            Point::new_logical(2.0, 2.0),
            Point::new_logical(2.0, 2.0),
        );
        let rects = TextLayout::measure_line_rects(
            &content,
            start_bytes..end_bytes,
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );

        let x_min = rects
            .iter()
            .map(|x| x.left + self.content_h_offset.get())
            .min_by(f32::total_cmp)
            .unwrap_or(0.0);
        let y_min = rects
            .iter()
            .map(|x| x.top + self.content_v_offset.get())
            .min_by(f32::total_cmp)
            .unwrap_or(0.0);
        let x_max = rects
            .iter()
            .map(|x| x.right() + self.content_h_offset.get())
            .max_by(f32::total_cmp)
            .unwrap_or(0.0);
        let y_max = rects
            .iter()
            .map(|x| x.bottom() + self.content_v_offset.get())
            .max_by(f32::total_cmp)
            .unwrap_or(0.0);

        // TODO: RenderScale(DPI)どうやってとるか
        let render_scale = 1.0;
        req.LayoutBounds()?
            .SetTextBounds(windows::Foundation::Rect {
                X: r.left as f32 + x_min * render_scale,
                Y: r.top as f32 + y_min * render_scale,
                Width: (x_max - x_min) * render_scale,
                Height: (y_max - y_min) * render_scale,
            })?;
        req.LayoutBounds()?
            .SetControlBounds(windows::Foundation::Rect {
                X: r.left as _,
                Y: r.top as _,
                Width: r.width as _,
                Height: r.height as _,
            })?;

        Ok(())
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
        let mut content = self.content.borrow_mut();

        let replace_start_bytes = content
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let replace_end_bytes = content
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        content.replace_range(replace_start_bytes..replace_end_bytes, &text);

        let new_cursor_start_bytes = content
            .chars()
            .take(new_selection.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let new_cursor_end_bytes = content
            .chars()
            .take(new_selection.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        drop(content);
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
impl crate::platform::mac::bridge::TextInputClientForwarding for MultilineTextInputEventHandler {
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
            unsafe { &mut *self.event_dispatcher }
                .dispatch(crate::Event::ScheduleViewRenderExt { id: self.view_id });
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
            unsafe { &mut *self.event_dispatcher }
                .dispatch(crate::Event::ScheduleViewRenderExt { id: self.view_id });
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
            unsafe { &mut *self.event_dispatcher }
                .dispatch(crate::Event::ScheduleViewRenderExt { id: self.view_id });
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
            unsafe { &mut *self.event_dispatcher }
                .dispatch(crate::Event::ScheduleViewRenderExt { id: self.view_id });
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
            unsafe { &*self.font_set_ptr },
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
