//! Bedrock interop

use super::*;
use bedrock as br;

impl<T: Into<u32> + Copy> br::ImageSize for Vector2<T> {
    const DIMENSION: br::vk::VkImageType = br::vk::VK_IMAGE_TYPE_2D;

    #[inline(always)]
    fn conv(self) -> br::vk::VkExtent3D {
        br::vk::VkExtent2D::from(self).with_depth(1)
    }
}
impl<T: Into<u32> + Copy> br::ImageSize for Vector3<T> {
    const DIMENSION: br::vk::VkImageType = br::vk::VK_IMAGE_TYPE_3D;

    #[inline(always)]
    fn conv(self) -> br::vk::VkExtent3D {
        br::vk::VkExtent3D::from(self)
    }
}

impl<T: Into<u32>> From<Vector2<T>> for br::vk::VkExtent2D {
    #[inline(always)]
    fn from(v: Vector2<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
        }
    }
}
impl<T: Into<u32> + Copy> From<&'_ Vector2<T>> for br::vk::VkExtent2D {
    #[inline(always)]
    fn from(v: &Vector2<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
        }
    }
}
impl<T> From<br::vk::VkExtent2D> for Vector2<T>
where
    u32: Into<T>,
{
    #[inline(always)]
    fn from(value: br::vk::VkExtent2D) -> Self {
        Self(value.width.into(), value.height.into())
    }
}
impl<T: Into<u32>> From<Vector3<T>> for br::vk::VkExtent3D {
    #[inline(always)]
    fn from(v: Vector3<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
            depth: v.2.into(),
        }
    }
}
impl<T: Into<u32> + Copy> From<&'_ Vector3<T>> for br::vk::VkExtent3D {
    #[inline(always)]
    fn from(v: &Vector3<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
            depth: v.2.into(),
        }
    }
}
impl<T> From<br::vk::VkExtent3D> for Vector3<T>
where
    u32: Into<T>,
{
    #[inline(always)]
    fn from(value: br::vk::VkExtent3D) -> Self {
        Self(value.width.into(), value.height.into(), value.depth.into())
    }
}

impl<T: Into<i32>> From<Vector2<T>> for br::vk::VkOffset2D {
    #[inline(always)]
    fn from(v: Vector2<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
        }
    }
}
impl<T: Into<i32> + Copy> From<&'_ Vector2<T>> for br::vk::VkOffset2D {
    #[inline(always)]
    fn from(v: &Vector2<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
        }
    }
}
impl<T> From<br::vk::VkOffset2D> for Vector2<T>
where
    i32: Into<T>,
{
    #[inline(always)]
    fn from(value: br::vk::VkOffset2D) -> Self {
        Self(value.x.into(), value.y.into())
    }
}
impl<T: Into<i32>> From<Vector3<T>> for br::vk::VkOffset3D {
    #[inline(always)]
    fn from(v: Vector3<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
            z: v.2.into(),
        }
    }
}
impl<T: Into<i32> + Copy> From<&'_ Vector3<T>> for br::vk::VkOffset3D {
    #[inline(always)]
    fn from(v: &Vector3<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
            z: v.2.into(),
        }
    }
}
impl<T> From<br::vk::VkOffset3D> for Vector3<T>
where
    i32: Into<T>,
{
    #[inline(always)]
    fn from(value: br::vk::VkOffset3D) -> Self {
        Self(value.x.into(), value.y.into(), value.z.into())
    }
}
