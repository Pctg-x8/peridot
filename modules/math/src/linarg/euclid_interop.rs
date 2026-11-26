//! Euclid interop

use super::*;

impl<T> From<Vector2<T>> for euclid::Vector2D<T> {
    #[inline(always)]
    fn from(value: Vector2<T>) -> Self {
        Self::new(value.0, value.1)
    }
}
impl<T: Copy> From<&'_ Vector2<T>> for euclid::Vector2D<T> {
    #[inline(always)]
    fn from(value: &Vector2<T>) -> Self {
        Self::new(value.0, value.1)
    }
}
impl<T> From<euclid::Vector2D<T>> for Vector2<T> {
    #[inline(always)]
    fn from(value: euclid::Vector2D<T>) -> Self {
        Self(value.x, value.y)
    }
}
impl<T: Copy> From<&'_ euclid::Vector2D<T>> for Vector2<T> {
    #[inline(always)]
    fn from(value: &euclid::Vector2D<T>) -> Self {
        Self(value.x, value.y)
    }
}

impl<T> From<Vector2<T>> for euclid::Point2D<T> {
    #[inline(always)]
    fn from(value: Vector2<T>) -> Self {
        Self::new(value.0, value.1)
    }
}
impl<T: Copy> From<&'_ Vector2<T>> for euclid::Point2D<T> {
    #[inline(always)]
    fn from(value: &Vector2<T>) -> Self {
        Self::new(value.0, value.1)
    }
}
impl<T> From<euclid::Point2D<T>> for Vector2<T> {
    #[inline(always)]
    fn from(value: euclid::Point2D<T>) -> Self {
        Self(value.x, value.y)
    }
}
impl<T: Copy> From<&'_ euclid::Point2D<T>> for Vector2<T> {
    #[inline(always)]
    fn from(value: &euclid::Point2D<T>) -> Self {
        Self(value.x, value.y)
    }
}

impl<T> From<Vector3<T>> for euclid::Vector3D<T> {
    #[inline(always)]
    fn from(value: Vector3<T>) -> Self {
        Self::new(value.0, value.1, value.2)
    }
}
impl<T: Copy> From<&'_ Vector3<T>> for euclid::Vector3D<T> {
    #[inline(always)]
    fn from(value: &Vector3<T>) -> Self {
        Self::new(value.0, value.1, value.2)
    }
}
impl<T> From<euclid::Vector3D<T>> for Vector3<T> {
    #[inline(always)]
    fn from(value: euclid::Vector3D<T>) -> Self {
        Self(value.x, value.y, value.z)
    }
}
impl<T: Copy> From<&'_ euclid::Vector3D<T>> for Vector3<T> {
    #[inline(always)]
    fn from(value: &euclid::Vector3D<T>) -> Self {
        Self(value.x, value.y, value.z)
    }
}

impl<T: Copy> From<Vector3<T>> for euclid::Point3D<T> {
    #[inline(always)]
    fn from(value: Vector3<T>) -> Self {
        Self::new(value.0, value.1, value.2)
    }
}
impl<T: Copy> From<&'_ Vector3<T>> for euclid::Point3D<T> {
    #[inline(always)]
    fn from(value: &Vector3<T>) -> Self {
        Self::new(value.0, value.1, value.2)
    }
}
impl<T> From<euclid::Point3D<T>> for Vector3<T> {
    #[inline(always)]
    fn from(value: euclid::Point3D<T>) -> Self {
        Self(value.x, value.y, value.z)
    }
}
impl<T: Copy> From<&'_ euclid::Point3D<T>> for Vector3<T> {
    #[inline(always)]
    fn from(value: &euclid::Point3D<T>) -> Self {
        Self(value.x, value.y, value.z)
    }
}
