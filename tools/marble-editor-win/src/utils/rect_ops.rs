use windows::Foundation::{Numerics::Vector2, Rect};

#[inline(always)]
pub fn rect_slice_left(r: Rect, width: f32) -> (Rect, Rect) {
    let sliced = Rect {
        Width: width,
        ..r.clone()
    };
    let rest = Rect {
        Width: r.Width - width,
        X: r.X + width,
        ..r
    };

    (sliced, rest)
}

#[inline(always)]
pub fn rect_slice_right(r: Rect, width: f32) -> (Rect, Rect) {
    let sliced = Rect {
        Width: width,
        X: r.X + r.Width - width,
        ..r.clone()
    };
    let rest = Rect {
        Width: r.Width - width,
        ..r
    };

    (sliced, rest)
}

#[inline(always)]
pub fn rect_slice_top(r: Rect, height: f32) -> (Rect, Rect) {
    let sliced = Rect {
        Height: height,
        ..r.clone()
    };
    let rest = Rect {
        Height: r.Height - height,
        Y: r.Y + height,
        ..r
    };

    (sliced, rest)
}

#[inline(always)]
pub fn rect_slice_bottom(r: Rect, height: f32) -> (Rect, Rect) {
    let sliced = Rect {
        Height: height,
        Y: r.Y + r.Height - height,
        ..r.clone()
    };
    let rest = Rect {
        Height: r.Height - height,
        ..r
    };

    (sliced, rest)
}

pub trait RectExtensions {
    fn contains_point(&self, x: f32, y: f32) -> bool;
    fn from_size(width: f32, height: f32) -> Self
    where
        Self: Sized;
    fn size(&self) -> Vector2;
}
impl RectExtensions for Rect {
    #[inline(always)]
    fn contains_point(&self, x: f32, y: f32) -> bool {
        self.X <= x && x <= self.X + self.Width && self.Y <= y && y <= self.Y + self.Height
    }

    #[inline(always)]
    fn from_size(width: f32, height: f32) -> Self {
        Rect {
            X: 0.0,
            Y: 0.0,
            Width: width,
            Height: height,
        }
    }

    #[inline(always)]
    fn size(&self) -> Vector2 {
        Vector2 {
            X: self.Width,
            Y: self.Height,
        }
    }
}
