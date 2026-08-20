use crate::{Max, Min, Ray3, Real, Vector3};

/// Axis-aligned bounding box in 3D space.
#[derive(Debug, Clone)]
pub struct AABB3<T> {
    pub min: Vector3<T>,
    pub max: Vector3<T>,
}
impl<T> AABB3<T> {
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
