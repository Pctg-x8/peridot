use shared::Size;

use crate::{
    SyncEvent, WindowHandle,
    rendering::{
        composite::CompositeRectTextHorizontalAlignment,
        text::{FontID, TextLayout},
    },
    uicore::{
        OverlayPopupBasicFrameView, OverlayPopupBasicFrameViewInit, OverlayPopupBasicMaskView,
        OverlayPopupBasicMaskViewInit, Popup, PopupCloseContext, PopupID, TeardownContext,
        TypedViewIdentifier, ViewIdentifier, ViewInitContext, ViewInstanceQueryableMut,
        ViewLayoutChild, ViewLayoutFlowAlignment, ViewLayoutFlowDirection, ViewLayoutFlowJustify,
        ViewLayoutOverflow, ViewRegisterable, ViewRelationControllable, ViewSize,
    },
    uikit::{SimpleButtonEventHandler, SimpleButtonView, SimpleButtonViewInit, StaticTextViewInit},
};

pub struct AlertDialogPresenter {
    id: PopupID,
    mask: TypedViewIdentifier<OverlayPopupBasicMaskView>,
    frame: TypedViewIdentifier<OverlayPopupBasicFrameView>,
    confirm_button: TypedViewIdentifier<SimpleButtonView>,
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
            None,
        );
        let text_width = tl
            .visual_width(ctx.system_link.font_set())
            .max(64.0)
            .min(owner_window.client_size().width * 0.8);

        let mask = ctx.construct_view(OverlayPopupBasicMaskViewInit, |_| []);
        let frame = ctx.construct_view(
            OverlayPopupBasicFrameViewInit {
                size: Size::new_logical(
                    text_width + Self::AROUND_PADDING * 2.0,
                    tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
                ),
            },
            |_| [],
        );
        {
            let frame = ctx.view_layout_mut(frame).expect("query failed");
            frame.padding.set_all(16.0);
            frame.child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment: ViewLayoutFlowAlignment::Center,
                justify: ViewLayoutFlowJustify::Start,
                overflow: ViewLayoutOverflow::Overflow,
                gap: 16.0,
            };
        }

        let msg = ctx.construct_view(
            StaticTextViewInit {
                content: message,
                allow_wrapping: true,
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_layout_mut(msg).expect("query failed").width = ViewSize::Fixed(text_width);

        let confirm_button = ctx.construct_view(
            SimpleButtonViewInit {
                label: "OK".into(),
                event_handler: Some(Box::new(EventHandler { popup_id })),
            },
            |_| [],
        );
        {
            let confirm_button = ctx.view_layout_mut(confirm_button).expect("query failed");
            confirm_button.width = ViewSize::Fixed(64.0);
            confirm_button.height = ViewSize::Fixed(24.0);
        }

        ctx.view_set_parent(msg, frame);
        ctx.view_set_parent(confirm_button, frame);
        ctx.view_set_parent(frame, mask);

        Self {
            id: popup_id,
            mask,
            frame,
            confirm_button,
        }
    }
}
impl Popup for AlertDialogPresenter {
    fn root_view_id(&self) -> ViewIdentifier {
        self.mask.into_untyped()
    }

    fn close(&mut self, context: &mut PopupCloseContext) {
        // disable button interaction while animating
        context
            .view_instance_mut(self.confirm_button)
            .expect("query failed")
            .set_interactive(false);

        context
            .view_instance_mut(self.mask)
            .expect("query failed")
            .play_close_animation();
        context
            .view_instance_mut(self.frame)
            .expect("query failed")
            .play_close_animation(SyncEvent::PopupUnmount { id: self.id });
    }

    #[allow(unused_variables)]
    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct EventHandler {
    popup_id: PopupID,
}
impl SimpleButtonEventHandler for EventHandler {
    fn on_click(
        &self,
        _sender: TypedViewIdentifier<SimpleButtonView>,
        _window: WindowHandle,
        ctx: &mut crate::input::InputEventContext,
    ) {
        ctx.close_popup(self.popup_id);
    }
}
