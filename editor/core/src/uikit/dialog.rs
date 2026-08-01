use crate::{
    Event, SyncEvent, WindowHandle,
    input::{KeyboardFocusGroupRef, hittest::HitTestTreeManager},
    rendering::{
        composite::{CompositeRectTextHorizontalAlignment, CompositeTree},
        text::{FontID, TextLayout},
    },
    uikit::{
        OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup, PopupID, RawMountTarget,
        RenderContext, SimpleButtonConstantEventHandler, SimpleButtonView, StaticTextView,
        TeardownContext, ViewElementSize, ViewInitContext, ViewLocation, ViewPlacement,
    },
    utils::{Point, Size},
};

pub struct AlertDialogPresenter {
    id: PopupID,
    mask: OverlayPopupBasicMaskView,
    frame: OverlayPopupBasicFrameView,
    msg: StaticTextView,
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

        let mask = OverlayPopupBasicMaskView::new();
        let frame = OverlayPopupBasicFrameView::new(Size::new_logical(
            text_width + Self::AROUND_PADDING * 2.0,
            tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
        ));

        let confirm_button = SimpleButtonView::new(
            ctx,
            "OK".into(),
            ViewPlacement {
                location: ViewLocation {
                    parent_anchor: [0.5, 1.0],
                    anchor: [0.5, 1.0],
                    offset: Point::new_logical(0.0, -Self::AROUND_PADDING),
                },
                size: ViewElementSize::Fixed(Size::new_logical(64.0, 24.0)),
            },
            Some(Box::new(SimpleButtonConstantEventHandler(
                Event::PopupClose { id: popup_id },
            ))),
        );

        let mut msg = StaticTextView::new(
            message,
            ViewPlacement {
                location: ViewLocation {
                    offset: Point::new_logical(0.0, Self::AROUND_PADDING),
                    anchor: [0.5, 0.0],
                    parent_anchor: [0.5, 0.0],
                },
                size: ViewElementSize::Fixed(Size::new_logical(text_width, 16.0)),
            },
        );
        msg.allow_wrapping();
        msg.set_horizontal_alignment(CompositeRectTextHorizontalAlignment::Middle);

        Self {
            id: popup_id,
            mask,
            frame,
            msg,
            confirm_button,
        }
    }
}
impl Popup for AlertDialogPresenter {
    fn render(
        &mut self,
        ctx: &mut RenderContext,
        parent: &RawMountTarget,
        keyboard_focus_group: KeyboardFocusGroupRef,
    ) {
        let mask = self.mask.render(ctx, parent);
        let frame = self.frame.render(ctx, &mask);
        self.msg.render(ctx, &frame);
        self.confirm_button
            .render(ctx, &frame, keyboard_focus_group);
    }

    fn close(
        &mut self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        _ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        // disable button interaction while animating
        self.confirm_button.set_interactive(false);

        self.mask.play_close_animation(composite_tree, current_sec);
        self.frame.play_close_animation(
            composite_tree,
            current_sec,
            SyncEvent::PopupUnmount { id: self.id },
        );
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        self.confirm_button.teardown(ctx);
        self.msg.teardown(ctx);
        self.frame.teardown(ctx);
        self.mask.teardown(ctx);
    }
}
