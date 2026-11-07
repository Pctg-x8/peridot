use std::ops::{Add, Mul};

use crate::{Max, Min, Ray3, Real, Vector3, Zero};

/// Axis-aligned bounding box in 3D space.
#[derive(Debug, Clone)]
pub struct AABB3<T> {
    pub min: Vector3<T>,
    pub max: Vector3<T>,
}
impl<T> AABB3<T> {
    #[inline(always)]
    pub fn scale(&self, scale: &Vector3<T>) -> Self
    where
        T: Copy + Mul<T, Output = T>,
    {
        Self {
            min: Vector3(
                self.min.0 * scale.0,
                self.min.1 * scale.1,
                self.min.2 * scale.2,
            ),
            max: Vector3(
                self.max.0 * scale.0,
                self.max.1 * scale.1,
                self.max.2 * scale.2,
            ),
        }
    }

    #[inline(always)]
    pub fn translate(&self, offset: &Vector3<T>) -> Self
    where
        T: Copy + Add<T, Output = T>,
    {
        Self {
            min: self.min + offset.clone(),
            max: self.max + offset.clone(),
        }
    }

    pub fn intersect(&self, ray: &Ray3<T>) -> Option<core::range::Range<T>>
    where
        T: Copy + Real + Min<Output = T> + Max<Output = T>,
    {
        let tx0 = (self.min.0 - ray.origin.0) / ray.direction.0;
        let tx1 = (self.max.0 - ray.origin.0) / ray.direction.0;
        let ty0 = (self.min.1 - ray.origin.1) / ray.direction.1;
        let ty1 = (self.max.1 - ray.origin.1) / ray.direction.1;
        let tz0 = (self.min.2 - ray.origin.2) / ray.direction.2;
        let tz1 = (self.max.2 - ray.origin.2) / ray.direction.2;

        let (tx0, tx1) = (tx0.min(tx1), tx0.max(tx1));
        let (ty0, ty1) = (ty0.min(ty1), ty0.max(ty1));
        let (tz0, tz1) = (tz0.min(tz1), tz0.max(tz1));
        let t0 = tx0.max(ty0).max(tz0);
        let t1 = tx1.min(ty1).min(tz1);

        if t0 < t1 {
            Some((t0..t1).into())
        } else {
            None
        }
    }
}

/// A sphere in 3D space.
#[derive(Debug, Clone)]
pub struct Sphere3<T> {
    pub center: Vector3<T>,
    pub radius: T,
}
impl<T> Sphere3<T> {
    #[inline(always)]
    pub fn scale(&self, scale: T) -> Self
    where
        T: Copy + Mul<T, Output = T>,
    {
        Self {
            center: self.center.clone(),
            radius: self.radius * scale,
        }
    }

    #[inline(always)]
    pub fn translate(&self, offset: &Vector3<T>) -> Self
    where
        T: Copy + Add<T, Output = T>,
    {
        Self {
            center: self.center + offset.clone(),
            radius: self.radius,
        }
    }

    pub fn intersect(&self, ray: &Ray3<T>) -> Option<core::range::Range<T>>
    where
        T: Copy + Real + Zero,
    {
        let l = self.center - ray.origin;
        let t_center = l.dot(ray.direction);
        let d2 = (self.center - ray.point(t_center)).len2();
        let t_bounds2 = self.radius * self.radius - d2;
        if t_bounds2 < T::ZERO {
            // not hit
            return None;
        }

        let t_bounds = t_bounds2.sqrt();
        Some(((t_center - t_bounds)..(t_center + t_bounds)).into())
    }
}
