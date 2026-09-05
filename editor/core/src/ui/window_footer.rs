use crate::{
    rendering::composite::{
        CompositeRect, CompositeRectText, CompositeRectTextRun, CompositeTreeRef,
    },
    uicore::{
        MeasureContext, RenderContext, TeardownContext, ViewLayoutStateStore, ViewRenderElements,
    },
    utils::{LogicalUnit, Rect, Size},
};

pub struct View {
    entity: Option<ViewEntity>,
}
impl View {
    pub const THICKNESS: f32 = 16.0;

    pub fn new() -> Self {
        Self { entity: None }
    }
}
impl crate::uicore::View for View {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .anchor_parent_bottom()
                    .offset_imm(0.0, -Self::THICKNESS)
                    .expand_width()
                    .size_imm(0.0, Self::THICKNESS)
                    .text(
                        CompositeRectText::build()
                            .run(
                                CompositeRectTextRun::build("Footer View".into())
                                    .color_imm([1.0, 1.0, 1.0, 1.0]),
                            )
                            .vertical_middle(),
                    )
                    .create(ctx.composite_tree);

                &*self.entity.insert(ViewEntity { ct_root })
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, Self::THICKNESS)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct ViewEntity {
    ct_root: CompositeTreeRef,
}
