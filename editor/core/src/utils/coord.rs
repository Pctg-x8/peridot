use std::marker::PhantomData;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicalUnit {}
impl Unit for LogicalUnit {
    type ValueType = f32;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PixelsUnit {}
impl Unit for PixelsUnit {
    type ValueType = u32;
}

pub trait Unit {
    type ValueType: Copy;
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Point<U: Unit> {
    pub x: U::ValueType,
    pub y: U::ValueType,
    _marker: PhantomData<U>,
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

    pub const fn distance_sq(&self, other: &Self) -> f32 {
        (self.x - other.x) * (self.x - other.x) + (self.y - other.y) * (self.y - other.y)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Size<U: Unit> {
    pub width: U::ValueType,
    pub height: U::ValueType,
    _marker: PhantomData<U>,
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
}

#[derive(Debug, Clone)]
pub struct Rect<U: Unit> {
    pub left: U::ValueType,
    pub top: U::ValueType,
    pub width: U::ValueType,
    pub height: U::ValueType,
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
    pub fn right(&self) -> <U::ValueType as core::ops::Add>::Output
    where
        U::ValueType: core::ops::Add,
    {
        self.left + self.width
    }

    #[inline(always)]
    pub fn bottom(&self) -> <U::ValueType as core::ops::Add>::Output
    where
        U::ValueType: core::ops::Add,
    {
        self.top + self.height
    }

    #[inline(always)]
    pub fn point_in_inclusive(&self, p: &Point<U>) -> bool
    where
        U::ValueType: core::ops::Add<Output = U::ValueType> + core::cmp::PartialOrd,
    {
        self.left <= p.x && p.x <= self.right() && self.top <= p.y && p.y <= self.bottom()
    }
}
impl<U: Unit<ValueType: Zero>> From<Size<U>> for Rect<U> {
    #[inline(always)]
    fn from(size: Size<U>) -> Self {
        Self::from(&size)
    }
}
impl<U: Unit<ValueType: Zero>> From<&'_ Size<U>> for Rect<U> {
    #[inline(always)]
    fn from(size: &Size<U>) -> Self {
        Self {
            left: <U::ValueType as Zero>::ZERO,
            top: <U::ValueType as Zero>::ZERO,
            width: size.width,
            height: size.height,
            _marker: PhantomData,
        }
    }
}
