use shared::{LogicalUnit, Rect, Size};

use crate::{
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, CompositeRect, CompositeRectScaleFactor,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uicore::{
        MeasureContext, RenderContext, TeardownContext, TypedViewIdentifier, View, ViewConstructor,
        ViewLayoutStateStore, ViewRenderElements,
    },
};

pub struct StaticTextViewInit {
    pub content: String,
    pub font: FontID,
    pub allow_wrapping: bool,
    pub horizontal_alignment: CompositeRectTextHorizontalAlignment,
    pub vertical_alignment: CompositeRectTextVerticalAlignment,
}
impl Default for StaticTextViewInit {
    #[inline(always)]
    fn default() -> Self {
        Self {
            content: String::new(),
            font: FontID::UIDefault,
            allow_wrapping: false,
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
            vertical_alignment: CompositeRectTextVerticalAlignment::Start,
        }
    }
}
impl ViewConstructor for StaticTextViewInit {
    type ConcreteView = StaticTextView;

    #[inline(always)]
    fn construct(self, _id: TypedViewIdentifier<Self::ConcreteView>) -> Self::ConcreteView {
        StaticTextView::new(self)
    }
}

pub struct StaticTextView {
    content: String,
    font: FontID,
    allow_wrapping: bool,
    horizontal_alignment: CompositeRectTextHorizontalAlignment,
    vertical_alignment: CompositeRectTextVerticalAlignment,
    ct: Option<CompositeTreeRef>,
    content_changed: bool,
}
impl Drop for StaticTextView {
    fn drop(&mut self) {
        if self.ct.is_some() {
            tracing::warn!("view element dropped without calling teardown");
        }
    }
}
impl StaticTextView {
    pub fn new(init: StaticTextViewInit) -> Self {
        Self {
            content: init.content,
            font: init.font,
            allow_wrapping: init.allow_wrapping,
            horizontal_alignment: init.horizontal_alignment,
            vertical_alignment: init.vertical_alignment,
            ct: None,
            content_changed: false,
        }
    }

    pub fn set_text(&mut self, content: String) {
        self.content = content;
        self.content_changed = true;
    }
}
impl View for StaticTextView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let content_changed = core::mem::replace(&mut self.content_changed, false);

        let e = match self.ct {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(*e)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();

                if content_changed {
                    ctx.composite_tree
                        .begin_mod_chain(*e)
                        .text_run(CompositeRectTextRun {
                            content: self.content.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            font_id: self.font,
                            ..Default::default()
                        })
                        .apply();
                }

                e
            }
            None => {
                let ct = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(layout_rect.left),
                        AnimatableFloat::Value(layout_rect.top),
                    ],
                    size: [
                        AnimatableFloat::Value(layout_rect.width),
                        AnimatableFloat::Value(layout_rect.height),
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
                    // has_bitmap: true,
                    // border: Some(crate::rendering::composite::Border {
                    //     thickness: 1.0,
                    //     color: AnimatableColor::Value([1.0; 4]),
                    //     ..Default::default()
                    // }),
                    ..Default::default()
                });

                &*self.ct.insert(ct)
            }
        };

        ViewRenderElements {
            composite_tree: Some(*e),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        if let Some(ct) = self.ct.take() {
            ctx.composite_tree.free(ct);
        }
    }

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        TextLayout::new_single(
            &self.content,
            self.font,
            ctx.system_link.font_set(),
            CompositeRectTextHorizontalAlignment::Start,
            None,
            None,
        )
        .size()
    }
}
