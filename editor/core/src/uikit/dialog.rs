use crate::{
    Event, SyncEvent, WindowHandle,
    input::hittest::HitTestTreeManager,
    rendering::{
        composite::{CompositeRectTextHorizontalAlignment, CompositeTree},
        text::{FontID, TextLayout},
    },
    uicore::{
        OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup, PopupCloseContext, PopupID,
        TeardownContext, TypedViewIdentifier, ViewIdentifier, ViewInitContext,
        ViewInstanceQueryable, ViewInstanceQueryableMut, ViewLayoutChild, ViewLayoutFlowAlignment,
        ViewLayoutFlowDirection, ViewLayoutFlowJustify, ViewLayoutOverflow, ViewRegisterable,
        ViewRelationControllable, ViewSize,
    },
    uikit::{
        SimpleButtonConstantEventHandler, SimpleButtonView, SimpleButtonViewInit,
        StaticTextViewInit,
    },
    utils::Size,
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

        let mask = ctx.construct_view_direct(|_| Box::new(OverlayPopupBasicMaskView::new()));
        let frame = ctx.construct_view_direct(|_| {
            Box::new(OverlayPopupBasicFrameView::new(Size::new_logical(
                text_width + Self::AROUND_PADDING * 2.0,
                tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
            )))
        });
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
                event_handler: Some(Box::new(SimpleButtonConstantEventHandler(
                    Event::PopupClose { id: popup_id },
                ))),
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

    fn close(
        &mut self,
        context: &mut PopupCloseContext,
        composite_tree: &mut CompositeTree<SyncEvent>,
        _ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        // disable button interaction while animating
        context
            .view_instance_mut(self.confirm_button)
            .expect("query failed")
            .set_interactive(false);

        context
            .view_instance(self.mask)
            .expect("query failed")
            .play_close_animation(composite_tree, current_sec);
        context
            .view_instance(self.frame)
            .expect("query failed")
            .play_close_animation(
                composite_tree,
                current_sec,
                SyncEvent::PopupUnmount { id: self.id },
            );
    }

    #[allow(unused_variables)]
    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}
