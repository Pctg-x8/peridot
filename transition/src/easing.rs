//! Easing Functions

pub const fn linear(x: f32) -> f32 {
    x
}
pub const fn quadratic(x: f32) -> f32 {
    x.powi(2)
}
pub const fn cubic(x: f32) -> f32 {
    x.powi(3)
}
pub const fn quartic(x: f32) -> f32 {
    x.powi(4)
}
pub const fn quintic(x: f32) -> f32 {
    x.powi(5)
}
