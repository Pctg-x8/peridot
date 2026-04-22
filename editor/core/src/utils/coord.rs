use std::marker::PhantomData;

pub trait Unit {
    const DBG_NAME: &'static str;

    type UnsignedValueType: Copy;
    type SignedValueType: Copy;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicalUnit {}
impl Unit for LogicalUnit {
    const DBG_NAME: &'static str = "LogicalUnit";

    type UnsignedValueType = f32;
    type SignedValueType = f32;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PixelsUnit {}
impl Unit for PixelsUnit {
    const DBG_NAME: &'static str = "PixelsUnit";

    type UnsignedValueType = u32;
    type SignedValueType = i32;
}

pub trait Zero {
    const ZERO: Self;
}
impl Zero for f32 {
    const ZERO: f32 = 0.0;
}
impl Zero for u32 {
    const ZERO: u32 = 0;
}
impl Zero for i32 {
    const ZERO: i32 = 0;
}

#[derive(Clone, Copy, PartialEq)]
pub struct Point<U: Unit> {
    pub x: U::SignedValueType,
    pub y: U::SignedValueType,
    _marker: PhantomData<U>,
}
impl<U: Unit<SignedValueType: core::fmt::Debug>> core::fmt::Debug for Point<U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Point<{}>", U::DBG_NAME))
            .field("x", &self.x)
            .field("y", &self.y)
            .finish()
    }
}
impl Point<LogicalUnit> {
    pub const fn new_logical(x: f32, y: f32) -> Self {
        Self {
            x,
            y,
            _marker: PhantomData,
        }
    }

    pub const fn x_pixels(&self, scale: f32) -> f32 {
        self.x * scale
    }

    pub const fn y_pixels(&self, scale: f32) -> f32 {
        self.y * scale
    }

    pub const fn to_pixels_round(&self, scale: f32) -> Point<PixelsUnit> {
        Point {
            x: (self.x * scale).round() as _,
            y: (self.y * scale).round() as _,
            _marker: PhantomData,
        }
    }

    pub const fn distance_sq(&self, other: &Self) -> f32 {
        (self.x - other.x) * (self.x - other.x) + (self.y - other.y) * (self.y - other.y)
    }
}
impl Point<PixelsUnit> {
    pub const fn new_pixels(x: i32, y: i32) -> Self {
        Self {
            x,
            y,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub const fn to_logical(&self, pixels_scale: f32) -> Point<LogicalUnit> {
        Point {
            x: self.x as f32 / pixels_scale,
            y: self.y as f32 / pixels_scale,
            _marker: PhantomData,
        }
    }

    #[cfg(windows)]
    #[inline(always)]
    pub const fn to_win32(&self) -> windows::Win32::Foundation::POINT {
        windows::Win32::Foundation::POINT {
            x: self.x,
            y: self.y,
        }
    }

    #[cfg(windows)]
    #[inline(always)]
    pub const fn from_win32(point: windows::Win32::Foundation::POINT) -> Self {
        Self {
            x: point.x,
            y: point.y,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Size<U: Unit> {
    pub width: U::UnsignedValueType,
    pub height: U::UnsignedValueType,
    _marker: PhantomData<U>,
}
impl<U: Unit<UnsignedValueType: core::fmt::Debug>> core::fmt::Debug for Size<U> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Size<{}>", U::DBG_NAME))
            .field("width", &self.width)
            .field("height", &self.height)
            .finish()
    }
}
impl Size<LogicalUnit> {
    #[inline(always)]
    pub const fn new_logical(width: f32, height: f32) -> Self {
        Self {
            width,
            height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub const fn width_pixels(&self, scale: f32) -> f32 {
        self.width * scale
    }

    #[inline(always)]
    pub const fn height_pixels(&self, scale: f32) -> f32 {
        self.height * scale
    }

    #[inline(always)]
    pub const fn to_pixels_ceil(&self, scale: f32) -> Size<PixelsUnit> {
        Size {
            width: (self.width * scale).ceil() as _,
            height: (self.height * scale).ceil() as _,
            _marker: PhantomData,
        }
    }
}
impl Size<PixelsUnit> {
    #[inline(always)]
    pub const fn new_pixels(width: u32, height: u32) -> Self {
        Self {
            width,
            height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub const fn to_logical(&self, scale: f32) -> Size<LogicalUnit> {
        Size {
            width: self.width as f32 / scale,
            height: self.height as f32 / scale,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub const fn to_vk(&self) -> bedrock::Extent2D {
        bedrock::Extent2D {
            width: self.width,
            height: self.height,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Rect<U: Unit> {
    pub left: U::SignedValueType,
    pub top: U::SignedValueType,
    pub width: U::UnsignedValueType,
    pub height: U::UnsignedValueType,
    _marker: PhantomData<U>,
}
impl<U: Unit> Rect<U> {
    pub const fn from_lt_size(lt: Point<U>, size: Size<U>) -> Self {
        Self {
            left: lt.x,
            top: lt.y,
            width: size.width,
            height: size.height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn right(&self) -> <U::SignedValueType as core::ops::Add<U::UnsignedValueType>>::Output
    where
        U::SignedValueType: core::ops::Add<U::UnsignedValueType>,
    {
        self.left + self.width
    }

    #[inline(always)]
    pub fn bottom(&self) -> <U::SignedValueType as core::ops::Add<U::UnsignedValueType>>::Output
    where
        U::SignedValueType: core::ops::Add<U::UnsignedValueType>,
    {
        self.top + self.height
    }

    #[inline(always)]
    pub fn point_in_inclusive(&self, p: &Point<U>) -> bool
    where
        U::SignedValueType: core::ops::Add<U::UnsignedValueType, Output = U::SignedValueType>
            + core::cmp::PartialOrd,
    {
        self.left <= p.x && p.x <= self.right() && self.top <= p.y && p.y <= self.bottom()
    }
}
impl<U: Unit<SignedValueType: Zero>> From<Size<U>> for Rect<U> {
    #[inline(always)]
    fn from(size: Size<U>) -> Self {
        Self::from(&size)
    }
}
impl<U: Unit<SignedValueType: Zero>> From<&'_ Size<U>> for Rect<U> {
    #[inline(always)]
    fn from(size: &Size<U>) -> Self {
        Self {
            left: <U::SignedValueType as Zero>::ZERO,
            top: <U::SignedValueType as Zero>::ZERO,
            width: size.width,
            height: size.height,
            _marker: PhantomData,
        }
    }
}
