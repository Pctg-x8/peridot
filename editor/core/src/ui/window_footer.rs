use crate::{
    rendering::composite::{
        AnimatableColor, AnimatableFloat, CompositeRect, CompositeRectScaleFactor,
        CompositeRectText, CompositeRectTextRun, CompositeRectTextVerticalAlignment,
        CompositeTreeRef,
    },
    uikit::{MountContext, MountTarget, ViewInitContext},
};

pub struct View {
    ct_root: CompositeTreeRef,
}
impl View {
    pub const THICKNESS: f32 = 16.0;

    pub fn new(ctx: &mut ViewInitContext) -> Self {
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

        Self { ct_root }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
    }
}
