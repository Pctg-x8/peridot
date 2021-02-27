
/// Linear Interpolation: x = a * (1.0 - self) + b * self
pub trait Lerp<T = Self>: Sized {
    type Output;
    fn lerp(self, a: T, b: T) -> Self::Output;
}
impl Lerp for f32 {
    type Output = f32;
    fn lerp(self, a: f32, b: f32) -> f32 { a * (1.0 - self) + b * self }
}
impl Lerp for f64 {
    type Output = f64;
    fn lerp(self, a: f64, b: f64) -> f64 { a * (1.0 - self) + b * self }
}

#[cfg(test)]
mod test {
    use super::Lerp;

    fn test_lerp() {
        assert_eq!(0.5f32.lerp(0.0, 1.0), 0.5);
        assert_eq!(0.5f64.lerp(0.0, 1.0), 0.5);
        assert_eq!(1.0f32.lerp(0.0, 50.0), 50.0);
        assert_eq!(0.0f64.lerp(-9.375, -60.0), -60.0);
        
        // out-of-range
        assert_eq!(2.0f32.lerp(-5.0, 5.0), 15.0);
        assert_eq!(-1.0f64.lerp(0.0, 1.25), -1.25);
    }
}
