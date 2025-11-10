use crate::{CGFloat, CGSize, CGVector};

#[repr(C)]
pub struct CGAffineTransform {
    pub a: CGFloat,
    pub b: CGFloat,
    pub c: CGFloat,
    pub d: CGFloat,
    pub tx: CGFloat,
    pub ty: CGFloat,
}
impl CGAffineTransform {
    pub const IDENTITY: Self = Self {
        a: 1.0,
        b: 0.0,
        c: 0.0,
        d: 1.0,
        tx: 0.0,
        ty: 0.0,
    };
}

#[repr(C)]
pub struct CGAffineTransformComponents {
    pub scale: CGSize,
    pub horizontal_shear: CGFloat,
    pub rotation: CGFloat,
    pub translation: CGVector,
}
