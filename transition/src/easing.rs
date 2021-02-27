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

pub fn sin(x: f32) -> f32 {
    1.0 + ((x - 1.0) * std::f32::consts::FRAC_PI_2).sin()
}
pub fn exp(x: f32) -> f32 {
    2.0f32.powf(10.0 * (x - 1.0))
}
