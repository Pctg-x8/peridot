use crate::{
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, CompositeRect, CompositeRectScaleFactor,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{MountTarget, RenderContext, TeardownContext, ViewElementSize, ViewPlacement},
};

pub struct StaticTextView {
    content: String,
    font: FontID,
    placement: ViewPlacement,
    ct: Option<CompositeTreeRef>,
}
impl Drop for StaticTextView {
    fn drop(&mut self) {
        if self.ct.is_some() {
            tracing::warn!("view element dropped without calling teardown");
        }
    }
}
impl StaticTextView {
    pub fn new(content: String, init_placement: ViewPlacement) -> Self {
        Self {
            content,
            font: FontID::UIDefault,
            placement: init_placement,
            ct: None,
        }
    }

    pub fn set_font(&mut self, font: FontID) {
        self.font = font;
    }

    pub fn render(&mut self, ctx: &mut RenderContext, target: &(impl MountTarget + ?Sized)) {
        match self.ct {
            // TODO: needs reflect modified properties
            Some(_) => (),
            None => {
                let size = match self.placement.size {
                    ViewElementSize::Fixed(s) => s,
                    ViewElementSize::Automatic => TextLayout::new_single(
                        &self.content,
                        self.font,
                        ctx.system_link.font_set(),
                        CompositeRectTextHorizontalAlignment::Start,
                        None,
                    )
                    .size(),
                };

                let ct = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(self.placement.location.x),
                        AnimatableFloat::Value(self.placement.location.y),
                    ],
                    size: [
                        AnimatableFloat::Value(size.width),
                        AnimatableFloat::Value(size.height),
                    ],
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: self.content.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            font_id: self.font,
                            ..Default::default()
                        }],
                        ..Default::default()
                    }),
                    ..Default::default()
                });

                ctx.composite_tree.add_child(target.ct_root(), ct);
                self.ct = Some(ct);
            }
        };
    }

    pub fn teardown(&mut self, ctx: &mut TeardownContext) {
        if let Some(ct) = self.ct.take() {
            ctx.mount_context.composite_tree.free(ct);
        }
    }
}
