use crate::{
    Event, SyncEvent, WindowHandle,
    input::hittest::HitTestTreeManager,
    rendering::{
        composite::{CompositeRectTextHorizontalAlignment, CompositeTree},
        text::{FontID, TextLayout},
    },
    uikit::{
        OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup, PopupID,
        SimpleButtonConstantEventHandler, SimpleButtonView, StaticTextView, TeardownContext,
        ViewElementSize, ViewIdentifier, ViewInitContext, ViewLocation, ViewPlacement,
        ViewRegisterable, ViewRelationControllable, popup::PopupCloseContext,
    },
    utils::{Point, Size},
};

pub struct AlertDialogPresenter {
    id: PopupID,
    root_view_id: ViewIdentifier,
    frame: ViewIdentifier,
    confirm_button: ViewIdentifier,
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

        let mask = ctx.construct_view(|_| Box::new(OverlayPopupBasicMaskView::new()));
        let frame = ctx.construct_view(|_| {
            Box::new(OverlayPopupBasicFrameView::new(Size::new_logical(
                text_width + Self::AROUND_PADDING * 2.0,
                tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
            )))
        });

        let msg = ctx.construct_view(|_| {
            let mut v = Box::new(StaticTextView::new(
                message,
                ViewPlacement {
                    location: ViewLocation {
                        offset: Point::new_logical(0.0, Self::AROUND_PADDING),
                        anchor: [0.5, 0.0],
                        parent_anchor: [0.5, 0.0],
                    },
                    size: ViewElementSize::Fixed(Size::new_logical(text_width, 16.0)),
                    size_anchor: [0.0, 0.0],
                },
            ));
            v.allow_wrapping();
            v.set_horizontal_alignment(CompositeRectTextHorizontalAlignment::Middle);
            v
        });

        let confirm_button = ctx.construct_view(|_| {
            Box::new(SimpleButtonView::new(
                "OK".into(),
                ViewPlacement {
                    location: ViewLocation {
                        parent_anchor: [0.5, 1.0],
                        anchor: [0.5, 1.0],
                        offset: Point::new_logical(0.0, -Self::AROUND_PADDING),
                    },
                    size: ViewElementSize::Fixed(Size::new_logical(64.0, 24.0)),
                    size_anchor: [0.0, 0.0],
                },
                Some(Box::new(SimpleButtonConstantEventHandler(
                    Event::PopupClose { id: popup_id },
                ))),
            ))
        });

        ctx.view_set_parent(msg, frame);
        ctx.view_set_parent(confirm_button, frame);
        ctx.view_set_parent(frame, mask);

        Self {
            id: popup_id,
            root_view_id: mask,
            frame,
            confirm_button,
        }
    }
}
impl Popup for AlertDialogPresenter {
    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id
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
            .view_instance_mut::<SimpleButtonView>(self.confirm_button)
            .expect("query failed")
            .set_interactive(false);

        context
            .view_instance::<OverlayPopupBasicMaskView>(self.root_view_id)
            .expect("query failed")
            .play_close_animation(composite_tree, current_sec);
        context
            .view_instance::<OverlayPopupBasicFrameView>(self.frame)
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
