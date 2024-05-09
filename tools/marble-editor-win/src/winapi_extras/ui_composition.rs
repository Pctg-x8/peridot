use windows::{
    Foundation::{
        Numerics::{Vector2, Vector3},
        Rect, TimeSpan,
    },
    UI::Composition::{
        CompositionEasingFunction, ContainerVisual, ICompositionAnimation2, IVisual,
        KeyFrameAnimation, ScalarKeyFrameAnimation, ShapeVisual, SpriteVisual,
        Vector3KeyFrameAnimation,
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

pub trait KeyFrameAnimationExtension {
    type Element;

    fn keyframe(&self, at: f32, value: Self::Element) -> windows::core::Result<&Self>;
    fn interpolate(
        &self,
        to: f32,
        to_value: Self::Element,
        f: impl windows_core::Param<CompositionEasingFunction>,
    ) -> windows::core::Result<&Self>;
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

pub trait VisualExtensions: Interface {
    fn set_rect(&self, rect: Rect) -> windows::core::Result<()> {
        let x = self.cast::<IVisual>()?;
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

        Ok(())
    }
}
impl VisualExtensions for ContainerVisual {}
impl VisualExtensions for SpriteVisual {}
impl VisualExtensions for ShapeVisual {}

pub trait Vector2Extension {
    fn scalar(v: f32) -> Self;
    fn with_z(self, z: f32) -> Vector3;
}
impl Vector2Extension for Vector2 {
    #[inline(always)]
    fn scalar(v: f32) -> Self {
        Vector2 { X: v, Y: v }
    }

    #[inline(always)]
    fn with_z(self, z: f32) -> Vector3 {
        Vector3 {
            X: self.X,
            Y: self.Y,
            Z: z,
        }
    }
}
