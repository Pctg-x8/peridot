use core::{cell::Cell, marker::PhantomData};

use peridot_math::Zero;

pub trait Unit {
    const DBG_NAME: &'static str;

    type UnsignedValueType;
    type SignedValueType;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogicalUnit {}
impl Unit for LogicalUnit {
    const DBG_NAME: &'static str = "LogicalUnit";

    type UnsignedValueType = f32;
    type SignedValueType = f32;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InteriorMutableLogicalUnit {}
impl Unit for InteriorMutableLogicalUnit {
    const DBG_NAME: &'static str = "InteriorMutableLogicalUnit";

    type UnsignedValueType = Cell<f32>;
    type SignedValueType = Cell<f32>;
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PixelsUnit {}
impl Unit for PixelsUnit {
    const DBG_NAME: &'static str = "PixelsUnit";

    type UnsignedValueType = u32;
    type SignedValueType = i32;
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
impl<U: Unit> Point<U> {
    pub const fn new_custom(x: U::SignedValueType, y: U::SignedValueType) -> Self {
        Self {
            x,
            y,
            _marker: core::marker::PhantomData,
        }
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
}
impl Point<InteriorMutableLogicalUnit> {
    #[inline(always)]
    pub const fn new_logical_interior_mutable(x: f32, y: f32) -> Self {
        Self {
            x: Cell::new(x),
            y: Cell::new(y),
            _marker: PhantomData,
        }
    }
}
impl<U: Unit> Point<U>
where
    U::SignedValueType: core::ops::Add<U::SignedValueType, Output = U::SignedValueType>,
{
    #[inline(always)]
    pub fn with_offset(self, offset: Point<U>) -> Self {
        Self {
            x: offset.x + self.x,
            y: offset.y + self.y,
            _marker: PhantomData,
        }
    }
}

#[derive(Clone, Copy)]
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
impl<U: Unit<UnsignedValueType: core::cmp::PartialEq>> core::cmp::PartialEq for Size<U> {
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.width.eq(&other.width) && self.height.eq(&other.height)
    }
}
impl<U: Unit<UnsignedValueType: core::cmp::Eq>> core::cmp::Eq for Size<U> {}
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
}
impl Size<InteriorMutableLogicalUnit> {
    #[inline(always)]
    pub const fn new_logical_interior_mutable(width: f32, height: f32) -> Self {
        Self {
            width: Cell::new(width),
            height: Cell::new(height),
            _marker: PhantomData,
        }
    }
}

#[derive(Clone)]
pub struct Rect<U: Unit> {
    pub left: U::SignedValueType,
    pub top: U::SignedValueType,
    pub width: U::UnsignedValueType,
    pub height: U::UnsignedValueType,
    _marker: PhantomData<U>,
}
impl<U: Unit<UnsignedValueType: core::fmt::Debug, SignedValueType: core::fmt::Debug>>
    core::fmt::Debug for Rect<U>
{
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Rect<{}>", U::DBG_NAME))
            .field("left", &self.left)
            .field("top", &self.top)
            .field("width", &self.width)
            .field("height", &self.height)
            .finish()
    }
}
impl<U: Unit<UnsignedValueType: core::cmp::PartialEq, SignedValueType: core::cmp::PartialEq>>
    core::cmp::PartialEq for Rect<U>
{
    #[inline(always)]
    fn eq(&self, other: &Self) -> bool {
        self.left.eq(&other.left)
            && self.top.eq(&other.top)
            && self.width.eq(&other.width)
            && self.height.eq(&other.height)
    }
}
impl<U: Unit<UnsignedValueType: core::cmp::Eq, SignedValueType: core::cmp::Eq>> core::cmp::Eq
    for Rect<U>
{
}
impl<U: Unit> Rect<U> {
    #[inline(always)]
    pub fn from_lt_size(lt: Point<U>, size: Size<U>) -> Self {
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
        U::SignedValueType: Copy + core::ops::Add<U::UnsignedValueType>,
        U::UnsignedValueType: Copy,
    {
        self.left + self.width
    }

    #[inline(always)]
    pub fn bottom(&self) -> <U::SignedValueType as core::ops::Add<U::UnsignedValueType>>::Output
    where
        U::SignedValueType: Copy + core::ops::Add<U::UnsignedValueType>,
        U::UnsignedValueType: Copy,
    {
        self.top + self.height
    }

    #[inline(always)]
    pub fn point_in_inclusive(&self, p: &Point<U>) -> bool
    where
        U::SignedValueType: core::ops::Add<U::UnsignedValueType, Output = U::SignedValueType>
            + core::cmp::PartialOrd
            + Copy,
        U::UnsignedValueType: Copy,
    {
        self.left <= p.x && p.x <= self.right() && self.top <= p.y && p.y <= self.bottom()
    }

    #[inline(always)]
    pub fn left_top(&self) -> Point<U>
    where
        U::SignedValueType: Copy,
    {
        Point {
            x: self.left,
            y: self.top,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn size(&self) -> Size<U>
    where
        U::UnsignedValueType: Copy,
    {
        Size {
            width: self.width,
            height: self.height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn slice_left(&self, width: U::UnsignedValueType) -> Self
    where
        U::SignedValueType: Copy,
        U::UnsignedValueType: Copy,
    {
        Self {
            left: self.left,
            top: self.top,
            width,
            height: self.height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn slice_right(&self, width: U::UnsignedValueType) -> Self
    where
        U::SignedValueType: Copy
            + core::ops::Add<U::UnsignedValueType, Output = U::SignedValueType>
            + core::ops::Sub<U::UnsignedValueType, Output = U::SignedValueType>,
        U::UnsignedValueType: Copy,
    {
        Self {
            left: self.right() - width,
            top: self.top,
            width,
            height: self.height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn slice_top(&self, height: U::UnsignedValueType) -> Self
    where
        U::SignedValueType: Copy,
        U::UnsignedValueType: Copy,
    {
        Self {
            left: self.left,
            top: self.top,
            width: self.width,
            height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn slice_bottom(&self, height: U::UnsignedValueType) -> Self
    where
        U::SignedValueType: Copy
            + core::ops::Add<U::UnsignedValueType, Output = U::SignedValueType>
            + core::ops::Sub<U::UnsignedValueType, Output = U::SignedValueType>,
        U::UnsignedValueType: Copy,
    {
        Self {
            left: self.left,
            top: self.bottom() - height,
            width: self.width,
            height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn relocate(self, new_lt: Point<U>) -> Self {
        Self {
            left: new_lt.x,
            top: new_lt.y,
            width: self.width,
            height: self.height,
            _marker: PhantomData,
        }
    }

    #[inline(always)]
    pub fn ref_relocate(&self, new_lt: &Point<U>) -> Self
    where
        U::SignedValueType: Copy,
        U::UnsignedValueType: Copy,
    {
        Self {
            left: new_lt.x,
            top: new_lt.y,
            width: self.width,
            height: self.height,
            _marker: PhantomData,
        }
    }
}
impl<U: Unit<SignedValueType: Zero>> From<Size<U>> for Rect<U> {
    #[inline(always)]
    fn from(size: Size<U>) -> Self {
        Self {
            left: <U::SignedValueType as Zero>::ZERO,
            top: <U::SignedValueType as Zero>::ZERO,
            width: size.width,
            height: size.height,
            _marker: PhantomData,
        }
    }
}
impl<U: Unit<SignedValueType: Zero, UnsignedValueType: Copy>> From<&'_ Size<U>> for Rect<U> {
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
