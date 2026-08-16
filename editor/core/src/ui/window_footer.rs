use crate::{
    rendering::composite::{
        AnimatableColor, AnimatableFloat, CompositeRect, CompositeRectScaleFactor,
        CompositeRectText, CompositeRectTextRun, CompositeRectTextVerticalAlignment,
        CompositeTreeRef,
    },
    utils::{LogicalUnit, Size},
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
impl crate::uikit::View for View {
    fn render(
        &mut self,
        _layout_rect: crate::utils::Rect<crate::utils::LogicalUnit>,
        ctx: &mut crate::uikit::RenderContext,
        _layout_state: &crate::uikit::ViewLayoutStateStore,
    ) -> crate::uikit::ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_offset_adjustment: [0.0, 1.0],
                    offset: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(-Self::THICKNESS),
                    ],
                    relative_size_adjustment: [1.0, 0.0],
                    size: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(Self::THICKNESS),
                    ],
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: "Footer View".into(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        ..Default::default()
                    }),
                    ..Default::default()
                });

                &*self.entity.insert(ViewEntity { ct_root })
            }
        };

        crate::uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            ..crate::uikit::ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut crate::uikit::TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.mount_context.composite_tree.free_all(entity.ct_root);
    }

    fn measure_preferred_content_size(
        &self,
        ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, Self::THICKNESS)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct ViewEntity {
    ct_root: CompositeTreeRef,
}
