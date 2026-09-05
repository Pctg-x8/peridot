use std::ops::{Add, Mul};

use crate::{Real, Vector3};

/// Represents a ray in 3D space.
#[derive(Debug, Clone)]
pub struct Ray3<T> {
    pub origin: Vector3<T>,
    pub direction: Vector3<T>,
}
impl<T> Ray3<T> {
    #[inline(always)]
    pub fn from_origin_to(origin: Vector3<T>, to: Vector3<T>) -> Self
    where
        T: Copy + Real,
    {
        Self {
            origin,
            direction: (to - origin).normalize(),
        }
    }

    #[inline(always)]
    pub fn point(&self, t: T) -> Vector3<T>
    where
        T: Copy + Mul<T, Output = T> + Add<T, Output = T>,
    {
        self.origin + self.direction * t
    }
}
