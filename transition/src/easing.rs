//! Easing Functions

pub const fn linear(x: f32) -> f32 {
    x
}
pub fn quadratic(x: f32) -> f32 {
    x.powi(2)
}
pub fn cubic(x: f32) -> f32 {
    x.powi(3)
}
pub fn quartic(x: f32) -> f32 {
    x.powi(4)
}
pub fn quintic(x: f32) -> f32 {
    x.powi(5)
}
