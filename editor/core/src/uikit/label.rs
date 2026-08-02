use crate::{
    SystemLink,
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, CompositeRect, CompositeRectScaleFactor,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{
        RawMountTarget, RenderChildScheduler, RenderContext, TeardownContext, View,
        ViewElementSize, ViewNewRenderElements, ViewPlacement,
    },
    utils::{LogicalUnit, Size},
};

pub struct StaticTextView {
    content: String,
    font: FontID,
    placement: ViewPlacement,
    allow_wrapping: bool,
    horizontal_alignment: CompositeRectTextHorizontalAlignment,
    vertical_alignment: CompositeRectTextVerticalAlignment,
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
            allow_wrapping: false,
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
            vertical_alignment: CompositeRectTextVerticalAlignment::Start,
            ct: None,
        }
    }

    pub fn set_font(&mut self, font: FontID) {
        self.font = font;
    }

    pub fn allow_wrapping(&mut self) {
        self.allow_wrapping = true;
    }

    pub fn set_horizontal_alignment(&mut self, alignment: CompositeRectTextHorizontalAlignment) {
        self.horizontal_alignment = alignment;
    }

    pub fn set_vertical_alignment(&mut self, alignment: CompositeRectTextVerticalAlignment) {
        self.vertical_alignment = alignment;
    }

    pub fn compute_size_without_render(&self, system_link: &SystemLink) -> Size<LogicalUnit> {
        match self.placement.size {
            ViewElementSize::Fixed(s) => s,
            ViewElementSize::Automatic => TextLayout::new_single(
                &self.content,
                self.font,
                system_link.font_set(),
                CompositeRectTextHorizontalAlignment::Start,
                None,
            )
            .size(),
        }
    }
}
impl View for StaticTextView {
    fn render(
        &mut self,
        ctx: &mut RenderContext,
        _sched: &mut RenderChildScheduler,
    ) -> ViewNewRenderElements {
        match self.ct {
            // TODO: needs reflect modified properties
            Some(_) => ViewNewRenderElements::EMPTY,
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
                        AnimatableFloat::Value(
                            self.placement.location.offset.x
                                - size.width * self.placement.location.anchor[0],
                        ),
                        AnimatableFloat::Value(
                            self.placement.location.offset.y
                                - size.height * self.placement.location.anchor[1],
                        ),
                    ],
                    relative_offset_adjustment: [
                        self.placement.location.parent_anchor[0],
                        self.placement.location.parent_anchor[1],
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
                        allow_wrapping: self.allow_wrapping,
                        horizontal_alignment: self.horizontal_alignment,
                        vertical_alignment: self.vertical_alignment,
                        ..Default::default()
                    }),
                    ..Default::default()
                });

                self.ct = Some(ct);
                ViewNewRenderElements {
                    composite_tree: Some(ct),
                    ..ViewNewRenderElements::EMPTY
                }
            }
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        if let Some(ct) = self.ct.take() {
            ctx.mount_context.composite_tree.free(ct);
        }
    }
}
