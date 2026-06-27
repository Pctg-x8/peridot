use crate::{
    Event, SyncEvent, WindowHandle,
    input::{KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, hittest::HitTestTreeManager},
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, CompositeRect, CompositeRectScaleFactor,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{
        MountContext, MountTarget, OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup,
        PopupID, Positioning, RawMountTarget, SimpleButtonConstantEventHandler, SimpleButtonView,
        ViewInitContext,
    },
    utils::Size,
};

pub struct AlertDialogPresenter {
    id: PopupID,
    mask: OverlayPopupBasicMaskView,
    frame: OverlayPopupBasicFrameView,
    ct_message: CompositeTreeRef,
    confirm_button: SimpleButtonView,
}
impl AlertDialogPresenter {
    const AROUND_PADDING: f32 = 16.0;
    const MESSAGE_BUTTON_SPACING: f32 = 12.0;

    pub fn new(
        ctx: &mut ViewInitContext,
        popup_id: PopupID,
        message: String,
        owner_window: WindowHandle,
    ) -> Self {
        let tl = TextLayout::new_single(
            &message,
            FontID::UIDefault,
            ctx.system_link.font_set(),
            CompositeRectTextHorizontalAlignment::Middle,
            Some(owner_window.client_size().width * 0.8),
        );
        let text_width = tl
            .visual_width(ctx.system_link.font_set())
            .max(64.0)
            .min(owner_window.client_size().width * 0.8);

        let mask = OverlayPopupBasicMaskView::new(ctx);
        let frame = OverlayPopupBasicFrameView::new(
            ctx,
            Size::new_logical(
                text_width + Self::AROUND_PADDING * 2.0,
                tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
            ),
        );
        let confirm_button = SimpleButtonView::new(
            ctx,
            "OK".into(),
            Size::new_logical(64.0, 24.0),
            Some(Box::new(SimpleButtonConstantEventHandler(
                Event::PopupClose { id: popup_id },
            ))),
        );
        let ct_message = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(text_width),
                AnimatableFloat::Value(16.0),
            ],
            relative_offset_adjustment: [0.5, 0.0],
            offset: [
                AnimatableFloat::Value(-text_width * 0.5),
                AnimatableFloat::Value(Self::AROUND_PADDING),
            ],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: message,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    spacing_inline_start: 0.0,
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                vertical_alignment: CompositeRectTextVerticalAlignment::Start,
                allow_wrapping: true,
                ..Default::default()
            }),
            ..Default::default()
        });

        confirm_button.locate(
            &Positioning {
                parent_anchor: [0.5, 1.0],
                anchor: [0.5, 1.0],
                offset: [0.0, -Self::AROUND_PADDING],
            },
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        ctx.composite_tree.add_child(frame.ct_root(), ct_message);
        confirm_button.mount(ctx, &frame);
        frame.mount(ctx, &mask);

        Self {
            id: popup_id,
            mask,
            frame,
            ct_message,
            confirm_button,
        }
    }
}
impl Popup for AlertDialogPresenter {
    fn mount(&self, ctx: &mut MountContext, parent: &RawMountTarget) {
        self.mask.mount(ctx, parent);
        self.mask
            .play_open_animation(ctx.composite_tree, ctx.current_sec);
        self.frame
            .play_open_animation(ctx.composite_tree, ctx.current_sec);
    }

    fn set_keyboard_focus_group(
        &self,
        group: KeyboardFocusGroupRef,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        self.confirm_button
            .set_keyboard_focus_group(group, keyboard_focus_registry);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        self.mask.unmount(ctx);
    }

    fn close(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        // disable button interaction while animating
        self.confirm_button.set_interactive(false, ht_manager);

        self.mask.play_close_animation(composite_tree, current_sec);
        self.frame.play_close_animation(
            composite_tree,
            current_sec,
            SyncEvent::PopupUnmount { id: self.id },
        );
    }

    fn terminate(&mut self, ctx: &mut MountContext) {
        self.confirm_button.unmount(ctx);
        self.confirm_button.terminate(ctx);

        ctx.composite_tree.free_all(self.mask.ct_root());
        ctx.ht_manager.free_all(self.mask.ht_root());
    }
}
