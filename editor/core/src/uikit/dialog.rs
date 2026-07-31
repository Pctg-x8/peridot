use crate::{
    Event, SyncEvent, WindowHandle,
    input::{KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, hittest::HitTestTreeManager},
    rendering::{
        composite::{CompositeRectTextHorizontalAlignment, CompositeTree},
        text::{FontID, TextLayout},
    },
    uikit::{
        MountContext, MountTarget, OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup,
        PopupID, Positioning, RawMountTarget, RenderContext, SimpleButtonConstantEventHandler,
        SimpleButtonView, StaticTextView, TeardownContext, ViewElementSize, ViewInitContext,
        ViewLocation, ViewPlacement,
    },
    utils::{Point, Size},
};

pub struct AlertDialogPresenter {
    id: PopupID,
    mask: OverlayPopupBasicMaskView,
    frame: OverlayPopupBasicFrameView,
    msg: StaticTextView,
    confirm_button: SimpleButtonView,
    first_rendered: bool,
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

        let mut msg = StaticTextView::new(
            message,
            ViewPlacement {
                location: ViewLocation {
                    offset: Point::new_logical(-text_width * 0.5, Self::AROUND_PADDING),
                    parent_anchor_x: 0.5,
                    parent_anchor_y: 0.0,
                },
                size: ViewElementSize::Fixed(Size::new_logical(text_width, 16.0)),
            },
        );
        msg.allow_wrapping();
        msg.set_horizontal_alignment(CompositeRectTextHorizontalAlignment::Middle);

        confirm_button.locate(
            &Positioning {
                parent_anchor: [0.5, 1.0],
                anchor: [0.5, 1.0],
                offset: [0.0, -Self::AROUND_PADDING],
            },
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        Self {
            id: popup_id,
            mask,
            frame,
            msg,
            confirm_button,
            first_rendered: false,
        }
    }
}
impl Popup for AlertDialogPresenter {
    fn render(&mut self, ctx: &mut RenderContext, parent: &RawMountTarget) {
        self.msg.render(ctx, &self.frame);

        if !self.first_rendered {
            let mut mount_context = ctx.make_mount_context();
            self.confirm_button.mount(&mut mount_context, &self.frame);
            self.frame.mount(&mut mount_context, &self.mask);
            self.mask.mount(&mut mount_context, parent);

            self.mask
                .play_open_animation(ctx.composite_tree, ctx.current_sec);
            self.frame
                .play_open_animation(ctx.composite_tree, ctx.current_sec);
        }

        self.first_rendered = true;
    }

    fn set_keyboard_focus_group(
        &self,
        group: KeyboardFocusGroupRef,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        self.confirm_button
            .set_keyboard_focus_group(group, keyboard_focus_registry);
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

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        self.mask.unmount(&mut ctx.mount_context);

        self.confirm_button.unmount(&mut ctx.mount_context);
        self.confirm_button.terminate(&mut ctx.mount_context);
        self.msg.teardown(ctx);

        ctx.mount_context
            .composite_tree
            .free_all(self.mask.ct_root());
        ctx.mount_context.ht_manager.free_all(self.mask.ht_root());
    }
}
