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
        ViewRegistry,
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

        let mask = OverlayPopupBasicMaskView::new();
        let frame = OverlayPopupBasicFrameView::new(Size::new_logical(
            text_width + Self::AROUND_PADDING * 2.0,
            tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
        ));

        let confirm_button = SimpleButtonView::new(
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

        let mask = ctx.view_registry.alloc(Box::new(mask));
        let frame = ctx.view_registry.alloc(Box::new(frame));
        let msg = ctx.view_registry.alloc(Box::new(msg));
        let confirm_button = ctx.view_registry.alloc(Box::new(confirm_button));
        ctx.view_registry.set_parent(frame, mask);
        ctx.view_registry.set_parent(msg, frame);
        ctx.view_registry.set_parent(confirm_button, frame);

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
        view_registry: &mut ViewRegistry,
        composite_tree: &mut CompositeTree<SyncEvent>,
        _ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        // disable button interaction while animating
        view_registry
            .instance_mut::<SimpleButtonView>(self.confirm_button)
            .expect("query failed")
            .set_interactive(false);

        view_registry
            .instance::<OverlayPopupBasicMaskView>(self.root_view_id)
            .expect("query failed")
            .play_close_animation(composite_tree, current_sec);
        view_registry
            .instance::<OverlayPopupBasicFrameView>(self.frame)
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
