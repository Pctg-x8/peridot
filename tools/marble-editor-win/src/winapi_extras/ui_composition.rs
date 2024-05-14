use windows::{
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan,
    },
    UI::Composition::{
        AnimationIterationBehavior, CompositionBrush, CompositionEasingFunction, ContainerVisual,
        ICompositionAnimation2, IKeyFrameAnimation, IVisual, IVisual2, KeyFrameAnimation,
        ScalarKeyFrameAnimation, ShapeVisual, SpriteVisual, Vector3KeyFrameAnimation,
    },
};
use windows_core::{Interface, HSTRING};

#[repr(transparent)]
pub struct KeyFrameAnimationPropertySetter<'r, T: 'r + Interface>(&'r T);
impl<T: Interface> KeyFrameAnimationPropertySetter<'_, T> {
    #[inline]
    pub fn duration(&self, duration: TimeSpan) -> windows::core::Result<&Self> {
        self.0
            .cast::<KeyFrameAnimation>()?
            .SetDuration(duration)
            .map(|_| self)
    }

    #[inline]
    pub fn target(&self, target: &HSTRING) -> windows::core::Result<&Self> {
        let this = self.0.cast::<ICompositionAnimation2>()?;
        unsafe {
            (this.vtable().SetTarget)(this.as_raw(), core::mem::transmute_copy(target))
                .ok()
                .map(|_| self)
        }
    }
}

pub trait KeyFrameAnimationExtension: Interface {
    type Element;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self>;
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self>;

    #[inline]
    fn iterate_forever(&self) -> windows::core::Result<&Self> {
        let x = self.cast::<IKeyFrameAnimation>()?;
        unsafe {
            (x.vtable().SetIterationBehavior)(x.as_raw(), AnimationIterationBehavior::Forever)
                .ok()?;
        }
        Ok(self)
    }
}
impl KeyFrameAnimationExtension for ScalarKeyFrameAnimation {
    type Element = f32;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self> {
        self.InsertKeyFrame(at, value).map(|_| self)
    }
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self> {
        self.InsertKeyFrameWithEasingFunction(to, to_value, f)
            .map(|_| self)
    }
}
impl KeyFrameAnimationExtension for Vector3KeyFrameAnimation {
    type Element = Vector3;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self> {
        self.InsertKeyFrame(at, value).map(|_| self)
    }
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self> {
        self.InsertKeyFrameWithEasingFunction(to, to_value, f)
            .map(|_| self)
    }
}

pub trait KeyFrameAnimationPropertySetterExtension: Interface {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self>;
}
impl KeyFrameAnimationPropertySetterExtension for KeyFrameAnimation {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self> {
        KeyFrameAnimationPropertySetter(self)
    }
}
impl KeyFrameAnimationPropertySetterExtension for ScalarKeyFrameAnimation {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self> {
        KeyFrameAnimationPropertySetter(self)
    }
}
impl KeyFrameAnimationPropertySetterExtension for Vector3KeyFrameAnimation {
    fn set_properties(&self) -> KeyFrameAnimationPropertySetter<Self> {
        KeyFrameAnimationPropertySetter(self)
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct VisualPropertySetter<'a, T: Interface + ?Sized + 'a>(&'a T);
impl<'a, T: Interface + ?Sized + 'a> VisualPropertySetter<'a, T> {
    #[inline]
    pub fn center_point(self, p: Vector3) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual>()?;
        unsafe {
            (x.vtable().SetCenterPoint)(x.as_raw(), p).ok()?;
        }
        Ok(self)
    }

    #[inline]
    pub fn anchor_point(self, p: Vector2) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual>()?;
        unsafe {
            (x.vtable().SetAnchorPoint)(x.as_raw(), p).ok()?;
        }
        Ok(self)
    }

    #[inline]
    pub fn relative_offset_adjustment(self, p: Vector3) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual2>()?;
        unsafe {
            (x.vtable().SetRelativeOffsetAdjustment)(x.as_raw(), p).ok()?;
        }
        Ok(self)
    }

    #[inline]
    pub fn relative_size_adjustment(self, p: Vector2) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual2>()?;
        unsafe {
            (x.vtable().SetRelativeSizeAdjustment)(x.as_raw(), p).ok()?;
        }
        Ok(self)
    }

    #[inline]
    pub fn offset(self, p: Vector3) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual>()?;
        unsafe {
            (x.vtable().SetOffset)(x.as_raw(), p).ok()?;
        }
        Ok(self)
    }

    #[inline]
    pub fn size(self, p: Vector2) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual>()?;
        unsafe {
            (x.vtable().SetSize)(x.as_raw(), p).ok()?;
        }
        Ok(self)
    }

    #[inline]
    pub fn rect(self, rect: Rect) -> windows::core::Result<Self> {
        let x = self.0.cast::<IVisual>()?;
        let (vt, this) = (x.vtable(), x.as_raw());
        unsafe {
            (vt.SetOffset)(
                this,
                Vector3 {
                    X: rect.X,
                    Y: rect.Y,
                    Z: 0.0,
                },
            )
            .ok()?;
            (vt.SetSize)(
                this,
                Vector2 {
                    X: rect.Width,
                    Y: rect.Height,
                },
            )
            .ok()?;
        }

        Ok(self)
    }
}
impl VisualPropertySetter<'_, SpriteVisual> {
    #[inline]
    pub fn brush(
        self,
        brush: impl windows::core::Param<CompositionBrush>,
    ) -> windows::core::Result<Self> {
        self.0.SetBrush(brush)?;
        Ok(self)
    }
}

pub trait VisualExtensions: Interface {
    fn set_properties(&self) -> VisualPropertySetter<Self> {
        VisualPropertySetter(self)
    }
}
impl VisualExtensions for ContainerVisual {}
impl VisualExtensions for SpriteVisual {}
impl VisualExtensions for ShapeVisual {}

pub trait VectorScalarConstructor {
    fn scalar(v: f32) -> Self;
}
impl VectorScalarConstructor for Vector2 {
    #[inline]
    fn scalar(v: f32) -> Self {
        Vector2 { X: v, Y: v }
    }
}
impl VectorScalarConstructor for Vector3 {
    #[inline]
    fn scalar(v: f32) -> Self {
        Vector3 { X: v, Y: v, Z: v }
    }
}

pub trait Vector2Extension {
    fn with_z(self, z: f32) -> Vector3;
}
impl Vector2Extension for Vector2 {
    #[inline(always)]
    fn with_z(self, z: f32) -> Vector3 {
        Vector3 {
            X: self.X,
            Y: self.Y,
            Z: z,
        }
    }
}

pub trait Vector3Extension {
    fn down(y: f32) -> Vector3;
}
impl Vector3Extension for Vector3 {
    #[inline(always)]
    fn down(y: f32) -> Vector3 {
        Vector3 {
            X: 0.0,
            Y: y,
            Z: 0.0,
        }
    }
}
