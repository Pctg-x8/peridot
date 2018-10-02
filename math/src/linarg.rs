//! PeridotExtendedMathematics: Vector/Matrix

use numtraits::{One, Zero};

/// 2-dimensional vector
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Vector2<T>(pub T, pub T);
/// 3-dimensional vector
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Vector3<T>(pub T, pub T, pub T);
/// 4-dimensional vector
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Vector4<T>(pub T, pub T, pub T, pub T);

/// Type alias for vec2 of f32
pub type Vector2F32 = Vector2<f32>;
/// Type alias for vec3 of f32
pub type Vector3F32 = Vector3<f32>;
/// Type alias for vec4 of f32
pub type Vector4F32 = Vector4<f32>;

/// 2x2 matrix
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Matrix2<T>(pub [T; 2], pub [T; 2]);
/// 3x3 matrix
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Matrix3<T>(pub [T; 3], pub [T; 3]);
/// 4x4 matrix
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Matrix4<T>(pub [T; 4], pub [T; 4]);
/// 2x3 matrix
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Matrix2x3<T>(pub [T; 3], pub [T; 3]);
/// 3x4 matrix
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Matrix3x4<T>(pub [T; 4], pub [T; 4], pub [T; 4]);

/// Type alias for mat2 of f32
pub type Matrix2F32 = Matrix2<f32>;
/// Type alias for mat3 of f32
pub type Matrix3F32 = Matrix3<f32>;
/// Type alias for mat4 of f32
pub type Matrix4F32 = Matrix4<f32>;
/// Type alias for mat23 of f32
pub type Matrix2x3F32 = Matrix2x3<f32>;
/// Type alias for mat34 of f32
pub type Matrix3x4F32 = Matrix3x4<f32>;

impl<T> Vector2<T> {
    pub fn x(&self) -> &T { &self.0 }
    pub fn y(&self) -> &T { &self.1 }
}
impl<T> Vector3<T> {
    pub fn x(&self) -> &T { &self.0 }
    pub fn y(&self) -> &T { &self.1 }
    pub fn z(&self) -> &T { &self.2 }
}
impl<T> Vector4<T> {
    pub fn x(&self) -> &T { &self.0 }
    pub fn y(&self) -> &T { &self.1 }
    pub fn z(&self) -> &T { &self.2 }
    pub fn w(&self) -> &T { &self.3 }
}

// Extending/Shrinking Vector Types //
impl<T: One> From<Vector2<T>> for Vector3<T> {
    fn from(Vector2(x, y): Vector2) -> Self { Vector3(x, y, T::ONE) }
}
impl<T: One> From<Vector3<T>> for Vector4<T> {
    fn from(Vector3(x, y, z): Vector3) -> Self { Vector4(x, y, z, T::ONE) }
}
impl<T: Div<T>> From<Vector4<T>> for Vector3<<T as Div>::Output> {
    fn from(Vector4(x, y, z, w): Vector4) -> Self { Vector3(x / w, y / w, z / w) }
}

// Vector as Fixed Arrays //
impl<T> From<Vector2<T>> for [T; 2] { fn from(Vector2(x, y): Vector2) -> Self { [x, y] } }
impl<T> From<Vector3<T>> for [T; 3] { fn from(Vector3(x, y, z): Vector3) -> Self { [x, y, z] } }
impl<T> From<Vector4<T>> for [T; 4] { fn from(Vector4(x, y, z, w): Vector4) -> Self { [x, y, z, w] } }

// Identities(for Multiplication) //
impl<T: Zero + One> One for Matrix2<T> {
    const ONE: Self = Matrix2([T::ONE, T::ZERO], [T::ZERO, T::ONE]);
}
impl<T: Zero + One> One for Matrix3<T> {
    const ONE: Self = Matrix3([T::ONE, T::ZERO: T::ZERO], [T::ZERO, T::ONE, T::ZERO], [T::ZERO, T::ZERO, T::ONE]);
}
impl<T: Zero + One> One for Matrix4<T> {
    const ONE: Self = Matrix4(
        [T::ONE, T::ZERO: T::ZERO, T::ZERO], [T::ZERO, T::ONE, T::ZERO, T::ZERO],
        [T::ZERO, T::ZERO, T::ONE, T::ZERO], [T::ZERO, T::ZERO, T::ZERO, T::ONE]);
}
impl<T: Zero + One> One for Matrix2x3<T> {
    const ONE: Self = Matrix2x3([T::ONE, T::ZERO, T::ZERO], [T::ZERO, T::ONE, T::ZERO]);
}
impl<T: Zero + One> One for Matrix3x4<T> {
    const ONE: Self = Matrix3x4([T::ONE, T::ZERO: T::ZERO, T::ZERO],
        [T::ZERO, T::ONE, T::ZERO, T::ZERO], [T::ZERO, T::ZERO, T::ONE, T::ZERO]);
}

fn dotproduct2<T: Mul>(a: &[T; 2], b: &[T; 2]) -> <T as Mul>::Output
        where <T as Mul>::Output: Add<<T as Mul>::Output, Output = <T as Mul>::Output> {
    a[0] * b[0] + a[1] * b[1]
}
fn dotproduct3<T: Mul>(a: &[T; 3], b: &[T; 3]) -> <T as Mul>::Output
        where <T as Mul>::Output: Add<<T as Mul>::Output, Output = <T as Mul>::Output> {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}
fn dotproduct4<T: Mul>(a: &[T; 4], b: &[T; 4]) -> <T as Mul>::Output
        where <T as Mul>::Output: Add<<T as Mul>::Output, Output = <T as Mul>::Output> {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3]
}

// Multiplication //
impl<T: Mul> Mul<Vector2<T>> for Matrix2<T> {
    type Output = Vector2<<T as Mul>::Output>;
    fn mul(self, v: Vector2) -> Self::Output {
        let va = v.into();
        Vector2(dotproduct2(&va, &self.0), dotproduct2(&va, &self.1))
    }
}
impl<T: Mul> Mul<Vector3<T>> for Matrix3<T> {
    type Output = Vector3<<T as Mul>::Output>;
    fn mul(self, v: Vector3) -> Self::Output {
        let va = v.into();
        Vector3(dotproduct3(&va, &self.0), dotproduct3(&va, &self.1), dotproduct3(&va, &self.2))
    }
}
impl<T: Mul> Mul<Vector4<T>> for Matrix4<T> {
    type Output = Vector4<<T as Mul>::Output>;
    fn mul(self, v: Vector4) -> Self::Output {
        let va = v.into();
        Vector4(dotproduct4(&va, &self.0), dotproduct4(&va, &self.1),
            dotproduct4(&va, &self.2), dotproduct4(&va, &self.3))
    }
}
// shortcuts //
impl<T: Mul> Mul<Vector2<T>> for Matrix3<T> {
    type Output = <Matrix3 as Mul<Vector3<T>>>::Output;
    fn mul(self, v: Vector2) -> Self::Output { self * Vector3::from(v) }
}
impl<T: Mul> Mul<Vector2<T>> for Matrix4<T> {
    type Output = <Matrix4 as Mul<Vector4<T>>>::Output;
    fn mul(self, v: Vector2) -> Self::Output { self * Vector3::from(v) }
}
impl<T: Mul> Mul<Vector3<T>> for Matrix4<T> {
    type Output = <Matrix4 as Mul<Vector4<T>>>::Output;
    fn mul(self, v: Vector3) -> Self::Output { self * Vector4::from(v) }
}
