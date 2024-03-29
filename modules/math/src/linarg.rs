//! PeridotExtendedMathematics: Vector/Matrix

use crate::numtraits::{One, Zero};
use std::mem::transmute;
use std::ops::*;

/// 2-dimensional vector
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Vector2<T>(pub T, pub T);
/// 3-dimensional vector
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Vector3<T>(pub T, pub T, pub T);
/// 4-dimensional vector
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Vector4<T>(pub T, pub T, pub T, pub T);
/// Arbitrary rotating
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Quaternion<T>(pub T, pub T, pub T, pub T);

/// Type alias for vec2 of f32
pub type Vector2F32 = Vector2<f32>;
/// Type alias for vec3 of f32
pub type Vector3F32 = Vector3<f32>;
/// Type alias for vec4 of f32
pub type Vector4F32 = Vector4<f32>;
/// Type alias for qvec of f32
pub type QuaternionF32 = Quaternion<f32>;

/// 2x2 matrix
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Matrix2<T>(pub [T; 2], pub [T; 2]);
/// 3x3 matrix
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Matrix3<T>(pub [T; 3], pub [T; 3], pub [T; 3]);
/// 4x4 matrix
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Matrix4<T>(pub [T; 4], pub [T; 4], pub [T; 4], pub [T; 4]);
/// 2x3 matrix
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Matrix2x3<T>(pub [T; 3], pub [T; 3]);
/// 3x4 matrix
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Matrix3x4<T>(pub [T; 4], pub [T; 4], pub [T; 4]);

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
    pub const fn x(&self) -> &T {
        &self.0
    }

    pub const fn y(&self) -> &T {
        &self.1
    }
}
impl<T> Vector3<T> {
    pub const fn x(&self) -> &T {
        &self.0
    }

    pub const fn y(&self) -> &T {
        &self.1
    }

    pub const fn z(&self) -> &T {
        &self.2
    }
}
impl<T> Vector4<T> {
    pub const fn x(&self) -> &T {
        &self.0
    }

    pub const fn y(&self) -> &T {
        &self.1
    }

    pub const fn z(&self) -> &T {
        &self.2
    }

    pub const fn w(&self) -> &T {
        &self.3
    }
}

// Identities of Vectors //
impl<T: Zero> Zero for Vector2<T> {
    const ZERO: Self = Vector2(T::ZERO, T::ZERO);
}
impl<T: One> One for Vector2<T> {
    const ONE: Self = Vector2(T::ONE, T::ONE);
}
impl<T: Zero> Zero for Vector3<T> {
    const ZERO: Self = Vector3(T::ZERO, T::ZERO, T::ZERO);
}
impl<T: One> One for Vector3<T> {
    const ONE: Self = Vector3(T::ONE, T::ONE, T::ONE);
}
impl<T: Zero> Zero for Vector4<T> {
    const ZERO: Self = Vector4(T::ZERO, T::ZERO, T::ZERO, T::ZERO);
}
impl<T: One> One for Vector4<T> {
    const ONE: Self = Vector4(T::ONE, T::ONE, T::ONE, T::ONE);
}

// Per-Element Identities of Vectors //
impl<T: Zero + One> Vector2<T> {
    pub fn left() -> Self
    where
        Self: Neg<Output = Self>,
    {
        -Self::right()
    }

    pub fn down() -> Self
    where
        Self: Neg<Output = Self>,
    {
        -Self::up()
    }

    pub const fn right() -> Self {
        Self(T::ONE, T::ZERO)
    }

    pub const fn up() -> Self {
        Self(T::ZERO, T::ONE)
    }
}

impl<T: Zero + One> Vector3<T> {
    pub fn left() -> Self
    where
        Self: Neg<Output = Self>,
    {
        -Self::right()
    }

    pub const fn right() -> Self {
        Self(T::ONE, T::ZERO, T::ZERO)
    }

    pub fn down() -> Self
    where
        Self: Neg<Output = Self>,
    {
        -Self::up()
    }

    pub const fn up() -> Self {
        Self(T::ZERO, T::ONE, T::ZERO)
    }

    pub fn back() -> Self
    where
        Self: Neg<Output = Self>,
    {
        -Self::forward()
    }

    pub const fn forward() -> Self {
        Self(T::ZERO, T::ZERO, T::ONE)
    }
}

// Extending/Shrinking Vector Types //
/// Vector2(x, y) -> Vector3(x, y, 1)
impl<T: One> From<Vector2<T>> for Vector3<T> {
    fn from(Vector2(x, y): Vector2<T>) -> Self {
        Vector3(x, y, T::ONE)
    }
}
/// Vector3(x, y, z) -> Vector4(x, y, z, 1)
impl<T: One> From<Vector3<T>> for Vector4<T> {
    fn from(Vector3(x, y, z): Vector3<T>) -> Self {
        Vector4(x, y, z, T::ONE)
    }
}
/// Vector4(x, y, z, w) -> Vector3(x / w, y / w, z / w)
/// panic occured when w == 0
impl<T: Div<T> + Copy> From<Vector4<T>> for Vector3<<T as Div>::Output> {
    fn from(Vector4(x, y, z, w): Vector4<T>) -> Self {
        Vector3(x / w, y / w, z / w)
    }
}

impl<T> Vector2<T> {
    pub fn with_z(self, z: T) -> Vector3<T> {
        Vector3(self.0, self.1, z)
    }
}
impl<T> Vector3<T> {
    pub fn with_w(self, w: T) -> Vector4<T> {
        Vector4(self.0, self.1, self.2, w)
    }
}

// Safe Transmuting as Slice //
impl<T> AsRef<[T; 2]> for Vector2<T> {
    fn as_ref(&self) -> &[T; 2] {
        unsafe { transmute(self) }
    }
}
impl<T> AsRef<[T; 3]> for Vector3<T> {
    fn as_ref(&self) -> &[T; 3] {
        unsafe { transmute(self) }
    }
}
impl<T> AsRef<[T; 4]> for Vector4<T> {
    fn as_ref(&self) -> &[T; 4] {
        unsafe { transmute(self) }
    }
}
impl<T> AsRef<[T; 4]> for Quaternion<T> {
    fn as_ref(&self) -> &[T; 4] {
        unsafe { transmute(self) }
    }
}

// Identities(for Multiplication) //
impl<T: Zero + One> One for Matrix2<T> {
    const ONE: Self = Matrix2([T::ONE, T::ZERO], [T::ZERO, T::ONE]);
}
impl<T: Zero + One> One for Matrix3<T> {
    const ONE: Self = Matrix3(
        [T::ONE, T::ZERO, T::ZERO],
        [T::ZERO, T::ONE, T::ZERO],
        [T::ZERO, T::ZERO, T::ONE],
    );
}
impl<T: Zero + One> One for Matrix4<T> {
    const ONE: Self = Matrix4(
        [T::ONE, T::ZERO, T::ZERO, T::ZERO],
        [T::ZERO, T::ONE, T::ZERO, T::ZERO],
        [T::ZERO, T::ZERO, T::ONE, T::ZERO],
        [T::ZERO, T::ZERO, T::ZERO, T::ONE],
    );
}
impl<T: Zero + One> One for Matrix2x3<T> {
    const ONE: Self = Matrix2x3([T::ONE, T::ZERO, T::ZERO], [T::ZERO, T::ONE, T::ZERO]);
}
impl<T: Zero + One> One for Matrix3x4<T> {
    const ONE: Self = Matrix3x4(
        [T::ONE, T::ZERO, T::ZERO, T::ZERO],
        [T::ZERO, T::ONE, T::ZERO, T::ZERO],
        [T::ZERO, T::ZERO, T::ONE, T::ZERO],
    );
}

// Extending Matrix Dimensions //
impl<T: Zero + One> From<Matrix2<T>> for Matrix3<T> {
    fn from(Matrix2([a, b], [c, d]): Matrix2<T>) -> Self {
        Matrix3([a, b, T::ZERO], [c, d, T::ZERO], [T::ZERO, T::ZERO, T::ONE])
    }
}
impl<T: Zero + One> From<Matrix3<T>> for Matrix4<T> {
    fn from(Matrix3([a, b, c], [d, e, f], [g, h, i]): Matrix3<T>) -> Self {
        Matrix4(
            [a, b, c, T::ZERO],
            [d, e, f, T::ZERO],
            [g, h, i, T::ZERO],
            [T::ZERO, T::ZERO, T::ZERO, T::ONE],
        )
    }
}

// Matrix and Matrix Multiplication //
impl<T: Copy> Mul for Matrix2<T>
where
    T: Mul<T>,
    <T as Mul>::Output: Add,
{
    type Output = Matrix2<<<T as Mul>::Output as Add>::Output>;
    fn mul(self, other: Matrix2<T>) -> Self::Output {
        let dp =
            |src: &[T; 2], colindex: usize| src[0] * other.0[colindex] + src[1] * other.1[colindex];

        Matrix2(
            [dp(&self.0, 0), dp(&self.0, 1)],
            [dp(&self.1, 0), dp(&self.1, 1)],
        )
    }
}
impl<T: Copy> Mul for Matrix3<T>
where
    T: Mul<T>,
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Matrix3<<T as Mul>::Output>;
    fn mul(self, other: Matrix3<T>) -> Self::Output {
        let dp = |src: &[T; 3], colindex: usize| {
            src[0] * other.0[colindex] + src[1] * other.1[colindex] + src[2] * other.2[colindex]
        };

        Matrix3(
            [dp(&self.0, 0), dp(&self.0, 1), dp(&self.0, 2)],
            [dp(&self.1, 0), dp(&self.1, 1), dp(&self.1, 2)],
            [dp(&self.2, 0), dp(&self.2, 1), dp(&self.2, 2)],
        )
    }
}
impl<T: Copy> Mul for Matrix4<T>
where
    T: Mul<T>,
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Matrix4<<T as Mul>::Output>;
    fn mul(self, other: Matrix4<T>) -> Self::Output {
        let dp = |src: &[T; 4], colindex: usize| {
            src[0] * other.0[colindex]
                + src[1] * other.1[colindex]
                + src[2] * other.2[colindex]
                + src[3] * other.3[colindex]
        };

        Matrix4(
            [
                dp(&self.0, 0),
                dp(&self.0, 1),
                dp(&self.0, 2),
                dp(&self.0, 3),
            ],
            [
                dp(&self.1, 0),
                dp(&self.1, 1),
                dp(&self.1, 2),
                dp(&self.1, 3),
            ],
            [
                dp(&self.2, 0),
                dp(&self.2, 1),
                dp(&self.2, 2),
                dp(&self.2, 3),
            ],
            [
                dp(&self.3, 0),
                dp(&self.3, 1),
                dp(&self.3, 2),
                dp(&self.3, 3),
            ],
        )
    }
}

// Scaling, Rotating //
impl<T: Zero> Matrix2<T> {
    pub fn scale(Vector2(x, y): Vector2<T>) -> Self {
        Matrix2([x, T::ZERO], [T::ZERO, y])
    }
}
impl Matrix2<f32> {
    pub fn rotate(rad: f32) -> Self {
        let (s, c) = rad.sin_cos();
        Matrix2([c, -s], [s, c])
    }
}
// Scaling/Translating //
impl<T: Zero> Matrix3<T> {
    pub fn scale(Vector3(x, y, z): Vector3<T>) -> Self {
        Matrix3(
            [x, T::ZERO, T::ZERO],
            [T::ZERO, y, T::ZERO],
            [T::ZERO, T::ZERO, z],
        )
    }
}
impl<T: One + Zero> Matrix3<T> {
    pub fn translation(Vector2(x, y): Vector2<T>) -> Self {
        Matrix3(
            [T::ONE, T::ZERO, x],
            [T::ZERO, T::ONE, y],
            [T::ZERO, T::ZERO, T::ONE],
        )
    }
}
impl<T: Zero> Matrix4<T> {
    pub fn scale(Vector4(x, y, z, w): Vector4<T>) -> Self {
        Matrix4(
            [x, T::ZERO, T::ZERO, T::ZERO],
            [T::ZERO, y, T::ZERO, T::ZERO],
            [T::ZERO, T::ZERO, z, T::ZERO],
            [T::ZERO, T::ZERO, T::ZERO, w],
        )
    }
}
impl<T: One + Zero> Matrix4<T> {
    pub fn translation(Vector3(x, y, z): Vector3<T>) -> Self {
        Matrix4(
            [T::ONE, T::ZERO, T::ZERO, x],
            [T::ZERO, T::ONE, T::ZERO, y],
            [T::ZERO, T::ZERO, T::ONE, z],
            [T::ZERO, T::ZERO, T::ZERO, T::ONE],
        )
    }
}

fn dotproduct2<T: Mul + Copy>(a: &[T; 2], b: &[T; 2]) -> <T as Mul>::Output
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    a[0] * b[0] + a[1] * b[1]
}
fn dotproduct3<T: Mul + Copy>(a: &[T; 3], b: &[T; 3]) -> <T as Mul>::Output
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}
fn dotproduct4<T: Mul + Copy>(a: &[T; 4], b: &[T; 4]) -> <T as Mul>::Output
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3]
}

// Point Translation by Multiplication //
impl<T: Mul + Copy> Mul<Vector2<T>> for Matrix2<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Vector2<<T as Mul>::Output>;
    fn mul(self, v: Vector2<T>) -> Self::Output {
        Vector2(
            dotproduct2(v.as_ref(), &self.0),
            dotproduct2(v.as_ref(), &self.1),
        )
    }
}
impl<T: Mul + Copy> Mul<Vector3<T>> for Matrix3<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Vector3<<T as Mul>::Output>;
    fn mul(self, v: Vector3<T>) -> Self::Output {
        Vector3(
            dotproduct3(v.as_ref(), &self.0),
            dotproduct3(v.as_ref(), &self.1),
            dotproduct3(v.as_ref(), &self.2),
        )
    }
}
impl<T: Mul + Copy> Mul<Vector4<T>> for Matrix4<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Vector4<<T as Mul>::Output>;
    fn mul(self, v: Vector4<T>) -> Self::Output {
        Vector4(
            dotproduct4(v.as_ref(), &self.0),
            dotproduct4(v.as_ref(), &self.1),
            dotproduct4(v.as_ref(), &self.2),
            dotproduct4(v.as_ref(), &self.3),
        )
    }
}
impl<T: Mul + One + Copy> Mul<Vector2<T>> for Matrix2x3<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Vector2<<T as Mul>::Output>;
    fn mul(self, v: Vector2<T>) -> Self::Output {
        let v3 = Vector3::from(v);
        Vector2(
            dotproduct3(v3.as_ref(), &self.0),
            dotproduct3(v3.as_ref(), &self.1),
        )
    }
}
impl<T: Mul + One + Copy> Mul<Vector3<T>> for Matrix3x4<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = Vector3<<T as Mul>::Output>;
    fn mul(self, v: Vector3<T>) -> Self::Output {
        let v4 = Vector4::from(v);
        Vector3(
            dotproduct4(v4.as_ref(), &self.0),
            dotproduct4(v4.as_ref(), &self.1),
            dotproduct4(v4.as_ref(), &self.2),
        )
    }
}
// shortcuts //
impl<T: Mul + One + Copy> Mul<Vector2<T>> for Matrix3<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = <Matrix3<T> as Mul<Vector3<T>>>::Output;
    fn mul(self, v: Vector2<T>) -> Self::Output {
        self * Vector3::from(v)
    }
}
impl<T: Mul + One + Copy> Mul<Vector2<T>> for Matrix4<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = <Matrix4<T> as Mul<Vector4<T>>>::Output;
    fn mul(self, v: Vector2<T>) -> Self::Output {
        self * Vector3::from(v)
    }
}
impl<T: Mul + One + Copy> Mul<Vector3<T>> for Matrix4<T>
where
    <T as Mul>::Output: Add<Output = <T as Mul>::Output>,
{
    type Output = <Matrix4<T> as Mul<Vector4<T>>>::Output;
    fn mul(self, v: Vector3<T>) -> Self::Output {
        self * Vector4::from(v)
    }
}

// Length Function and Normalization //
impl Vector2<f32> {
    pub fn len(&self) -> f32 {
        self.len2().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let l0 = self.len();
        Vector2(self.0 / l0, self.1 / l0)
    }
}
impl Vector2<f64> {
    pub fn len(&self) -> f64 {
        self.len2().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let l0 = self.len();
        Vector2(self.0 / l0, self.1 / l0)
    }
}
impl Vector3<f32> {
    pub fn len(&self) -> f32 {
        self.len2().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let l0 = self.len();
        Vector3(self.0 / l0, self.1 / l0, self.2 / l0)
    }
}
impl Vector3<f64> {
    pub fn len(&self) -> f64 {
        self.len2().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let l0 = self.len();
        Vector3(self.0 / l0, self.1 / l0, self.2 / l0)
    }
}
impl Vector4<f32> {
    pub fn len(&self) -> f32 {
        self.len2().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let l0 = self.len();
        Vector4(self.0 / l0, self.1 / l0, self.2 / l0, self.3 / l0)
    }
}
impl Vector4<f64> {
    pub fn len(&self) -> f64 {
        self.len2().sqrt()
    }
    pub fn normalize(&self) -> Self {
        let l0 = self.len();
        Vector4(self.0 / l0, self.1 / l0, self.2 / l0, self.3 / l0)
    }
}

/// identity
impl<T: One + Zero> One for Quaternion<T> {
    const ONE: Self = Quaternion(T::ZERO, T::ZERO, T::ZERO, T::ONE);
}

// quaternion shortcuts //
impl Quaternion<f32> {
    /// Creates new quaternion from rotation axis and angle in radian.
    pub fn new(rad: f32, axis: Vector3<f32>) -> Self {
        let (s, c) = (rad / 2.0).sin_cos();
        let axis = axis.normalize();

        Quaternion(axis.0 * s, axis.1 * s, axis.2 * s, c)
    }
    /// Calculates a difference of angle between 2 quaternions.
    pub fn angle(&self, other: &Self) -> f32 {
        dotproduct4(self.as_ref(), other.as_ref()).acos()
    }
    /// Calculates the lerp-ed quaternion between 2 quaternions by `t`.
    pub fn lerp(&self, other: &Self, t: f32) -> Self {
        let omg = self.angle(other);
        let (fa, fb) = (
            (omg * (1.0 - t)).sin() / omg.sin(),
            (omg * t).sin() / omg.sin(),
        );
        return Quaternion(
            fa * self.0 + fb * other.0,
            fa * self.1 + fb * other.1,
            fa * self.2 + fb * other.2,
            fa * self.3 + fb * other.3,
        );
    }
    /// Calculates a normalized quaternion
    pub fn normalize(self) -> Self {
        let d = (self.0.powf(2.0) + self.1.powf(2.0) + self.2.powf(2.0) + self.3.powf(2.0)).sqrt();
        Quaternion(self.0 / d, self.1 / d, self.2 / d, self.3 / d)
    }
}
impl<T: Mul<Output = T> + Add<Output = T> + Sub<Output = T> + Copy> Mul for Quaternion<T> {
    type Output = Quaternion<T>;
    fn mul(self, Quaternion(x, y, z, w): Quaternion<T>) -> Self::Output {
        let x0 = self.3 * x + self.0 * w + self.1 * z - self.2 * y;
        let y0 = self.3 * y - self.0 * z + self.1 * w + self.2 * x;
        let z0 = self.3 * z + self.0 * y - self.1 * x + self.2 * w;
        let w0 = self.3 * w - self.0 * x - self.1 * y - self.2 * z;

        Quaternion(x0, y0, z0, w0)
    }
}

// q * neg q = id
impl<T: Neg<Output = T>> Neg for Quaternion<T> {
    type Output = Quaternion<T>;
    fn neg(self) -> Self {
        Quaternion(-self.0, -self.1, -self.2, self.3)
    }
}

/// quaternion to matrix conversion
impl<T: One + Mul<Output = T> + Sub<Output = T> + Add<Output = T> + Copy> From<Quaternion<T>>
    for Matrix3<T>
{
    fn from(Quaternion(x, y, z, w): Quaternion<T>) -> Self {
        let two = T::ONE + T::ONE;
        let m11 = T::ONE - two * y * y - two * z * z;
        let m22 = T::ONE - two * x * x - two * z * z;
        let m33 = T::ONE - two * x * x - two * y * y;
        let m12 = two * (x * y + w * z);
        let m13 = two * (x * z - w * y);
        let m21 = two * (x * y - w * z);
        let m23 = two * (y * z + w * x);
        let m31 = two * (x * z + w * y);
        let m32 = two * (y * z - w * x);

        Matrix3([m11, m12, m13], [m21, m22, m23], [m31, m32, m33])
    }
}
/// quaternion to matrix conversion
impl<T: One + Zero + Mul<Output = T> + Sub<Output = T> + Add<Output = T> + Copy> From<Quaternion<T>>
    for Matrix4<T>
{
    fn from(Quaternion(x, y, z, w): Quaternion<T>) -> Self {
        let two = T::ONE + T::ONE;
        let m11 = T::ONE - two * y * y - two * z * z;
        let m22 = T::ONE - two * x * x - two * z * z;
        let m33 = T::ONE - two * x * x - two * y * y;
        let m12 = two * (x * y + w * z);
        let m13 = two * (x * z - w * y);
        let m21 = two * (x * y - w * z);
        let m23 = two * (y * z + w * x);
        let m31 = two * (x * z + w * y);
        let m32 = two * (y * z - w * x);

        Matrix4(
            [m11, m12, m13, T::ZERO],
            [m21, m22, m23, T::ZERO],
            [m31, m32, m33, T::ZERO],
            [T::ZERO, T::ZERO, T::ZERO, T::ONE],
        )
    }
}

// Per-element Operations //
macro_rules! VariadicElementOps
{
    (for $e: ident ($($n: tt),*)) =>
    {
        impl<T: Add> Add for $e<T>
        {
            type Output = $e<<T as Add>::Output>;
            fn add(self, other: Self) -> Self::Output { $e($(self.$n + other.$n),*) }
        }
        impl<T: AddAssign> AddAssign for $e<T>
        {
            fn add_assign(&mut self, other: Self) { $(self.$n += other.$n);* }
        }

        impl<T: Sub> Sub for $e<T>
        {
            type Output = $e<<T as Sub>::Output>;
            fn sub(self, other: Self) -> Self::Output { $e($(self.$n - other.$n),*) }
        }
        impl<T: SubAssign> SubAssign for $e<T>
        {
            fn sub_assign(&mut self, other: Self) { $(self.$n -= other.$n);* }
        }

        impl<T: Mul + Copy> Mul<T> for $e<T>
        {
            type Output = $e<<T as Mul>::Output>;
            fn mul(self, other: T) -> Self::Output { $e($(self.$n * other),*) }
        }
        impl<T: MulAssign + Copy> MulAssign<T> for $e<T>
        {
            fn mul_assign(&mut self, other: T) { $(self.$n *= other);* }
        }

        impl<T: Neg> Neg for $e<T>
        {
            type Output = $e<<T as Neg>::Output>;
            fn neg(self) -> Self::Output { $e($(-self.$n),*) }
        }

        impl<T: Mul<Output = T> + Add<Output = T> + Copy> $e<T>
        {
            /// Calculates an inner product between 2 vectors
            pub fn dot(self, other: Self) -> T
            {
                CTSummation!($(self.$n * other.$n),*)
            }
        }
        impl<T: Mul<Output = T> + Add<Output = T> + Copy> $e<T>
        {
            /// Calculates a squared length of a vector
            pub fn len2(self) -> T
            {
                CTSummation!($(self.$n * self.$n),*)
            }
        }

        impl<T> $e<T> {
            /// Calculates minimum value of each element
            pub fn min(self, other: Self) -> Self where T: crate::numtraits::Min<Output = T> {
                $e($(self.$n.min(other.$n)),*)
            }
            /// Calculates maximum value of each element
            pub fn max(self, other: Self) -> Self where T: crate::numtraits::Max<Output = T> {
                $e($(self.$n.max(other.$n)),*)
            }
        }
    }
}
macro_rules! CTSummation
{
    ($e: expr, $($e2: expr),+) => { $e $(.add($e2))* }
}
VariadicElementOps!(for Vector2 (0, 1));
VariadicElementOps!(for Vector3 (0, 1, 2));
VariadicElementOps!(for Vector4 (0, 1, 2, 3));

impl<T> Vector2<T>
where
    T: Mul<Output = T> + Sub<Output = T> + Copy,
{
    /// Cross product between 2d vectors
    pub fn cross(&self, other: &Self) -> T {
        self.0 * other.1 - self.1 * other.0
    }
}
impl<T> Vector3<T>
where
    T: Mul<T, Output = T> + Sub<T, Output = T> + Copy,
{
    /// Cross product between 3d vectors
    pub fn cross(&self, other: &Self) -> Self {
        let x = self.1 * other.2 - self.2 * other.1;
        let y = self.2 * other.0 - self.0 * other.2;
        let z = self.0 * other.1 - self.1 * other.0;

        Vector3(x, y, z)
    }
}

// Bedrock Interop //
use bedrock as br;
impl<T: Into<u32> + Copy> br::ImageSize for Vector2<T> {
    const DIMENSION: br::vk::VkImageType = br::vk::VK_IMAGE_TYPE_2D;
    fn conv(self) -> br::vk::VkExtent3D {
        br::vk::VkExtent2D::from(self).with_depth(1)
    }
}
impl<T: Into<u32> + Copy> br::ImageSize for Vector3<T> {
    const DIMENSION: br::vk::VkImageType = br::vk::VK_IMAGE_TYPE_3D;
    fn conv(self) -> br::vk::VkExtent3D {
        br::vk::VkExtent3D::from(self)
    }
}
impl<T: Into<u32>> From<Vector2<T>> for br::vk::VkExtent2D {
    fn from(v: Vector2<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
        }
    }
}
impl<T: Into<u32> + Copy> From<&'_ Vector2<T>> for br::vk::VkExtent2D {
    fn from(v: &Vector2<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
        }
    }
}
impl<T: Into<u32>> From<Vector3<T>> for br::vk::VkExtent3D {
    fn from(v: Vector3<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
            depth: v.2.into(),
        }
    }
}
impl<T: Into<u32> + Copy> From<&'_ Vector3<T>> for br::vk::VkExtent3D {
    fn from(v: &Vector3<T>) -> Self {
        Self {
            width: v.0.into(),
            height: v.1.into(),
            depth: v.2.into(),
        }
    }
}
impl<T: Into<i32>> From<Vector2<T>> for br::vk::VkOffset2D {
    fn from(v: Vector2<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
        }
    }
}
impl<T: Into<i32> + Copy> From<&'_ Vector2<T>> for br::vk::VkOffset2D {
    fn from(v: &Vector2<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
        }
    }
}
impl<T: Into<i32>> From<Vector3<T>> for br::vk::VkOffset3D {
    fn from(v: Vector3<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
            z: v.2.into(),
        }
    }
}
impl<T: Into<i32> + Copy> From<&'_ Vector3<T>> for br::vk::VkOffset3D {
    fn from(v: &Vector3<T>) -> Self {
        Self {
            x: v.0.into(),
            y: v.1.into(),
            z: v.2.into(),
        }
    }
}

impl<T: From<u32>> From<br::vk::VkExtent2D> for Vector2<T> {
    fn from(value: br::vk::VkExtent2D) -> Self {
        Self(value.width.into(), value.height.into())
    }
}
impl<T: From<u32>> From<br::vk::VkExtent3D> for Vector3<T> {
    fn from(value: br::vk::VkExtent3D) -> Self {
        Self(value.width.into(), value.height.into(), value.depth.into())
    }
}
impl<T: From<i32>> From<br::vk::VkOffset2D> for Vector2<T> {
    fn from(value: br::vk::VkOffset2D) -> Self {
        Self(value.x.into(), value.y.into())
    }
}
impl<T: From<i32>> From<br::vk::VkOffset3D> for Vector3<T> {
    fn from(value: br::vk::VkOffset3D) -> Self {
        Self(value.x.into(), value.y.into(), value.z.into())
    }
}

// euclid interops (for vg) //
impl<T> Into<euclid::Point2D<T>> for Vector2<T> {
    fn into(self) -> euclid::Point2D<T> {
        euclid::Point2D::new(self.0, self.1)
    }
}
impl<T> Into<euclid::Vector2D<T>> for Vector2<T> {
    fn into(self) -> euclid::Vector2D<T> {
        euclid::Vector2D::new(self.0, self.1)
    }
}
impl<T> From<euclid::Point2D<T>> for Vector2<T> {
    fn from(value: euclid::Point2D<T>) -> Self {
        Self(value.x, value.y)
    }
}
impl<T: Copy> From<&'_ euclid::Point2D<T>> for Vector2<T> {
    fn from(value: &'_ euclid::Point2D<T>) -> Self {
        Self(value.x, value.y)
    }
}
impl<T> From<euclid::Vector2D<T>> for Vector2<T> {
    fn from(value: euclid::Vector2D<T>) -> Self {
        Self(value.x, value.y)
    }
}
impl<T: Copy> From<&'_ euclid::Vector2D<T>> for Vector2<T> {
    fn from(value: &'_ euclid::Vector2D<T>) -> Self {
        Self(value.x, value.y)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std;

    #[test]
    fn vector_dimension_transform() {
        assert_eq!(Vector3::from(Vector2(2, 3)), Vector3(2, 3, 1));
        assert_eq!(
            Vector4::from(Vector3(2.5, 3.0, 4.1)),
            Vector4(2.5, 3.0, 4.1, 1.0)
        );
        assert_eq!(Vector3::from(Vector4(4, 6, 8, 2)), Vector3(2, 3, 4));
    }
    #[test]
    fn matrix_multiplication_identity() {
        assert_eq!(Matrix3::ONE * Vector3(1, 2, 3), Vector3(1, 2, 3));
        assert_eq!(
            Matrix2::ONE * Matrix2::scale(Vector2(2, 3)),
            Matrix2::scale(Vector2(2, 3))
        );
        assert_eq!(
            Matrix3::ONE * Matrix3::scale(Vector3(2, 3, 4)),
            Matrix3::scale(Vector3(2, 3, 4))
        );
        assert_eq!(
            Matrix4::ONE * Matrix4::scale(Vector4(2, 3, 4, 5)),
            Matrix4::scale(Vector4(2, 3, 4, 5))
        );
    }
    #[test]
    fn matrix_transforming() {
        assert_eq!(
            Matrix3([1, 0, 2], [0, 1, 3], [0, 0, 1]) * Vector2(0, 0),
            Vector3(2, 3, 1)
        );
        assert_eq!(
            Matrix4([1, 0, 0, 2], [0, 1, 0, 3], [0, 0, 1, 4], [0, 0, 0, 1]) * Vector3(1, 2, 3),
            Vector4(3, 5, 7, 1)
        );
        assert_eq!(Matrix2::scale(Vector2(2, 3)) * Vector2(1, 1), Vector2(2, 3));
    }
    #[test]
    fn matrix_extension() {
        assert_eq!(
            Matrix3::from(Matrix2([0, 1], [1, 0])),
            Matrix3([0, 1, 0], [1, 0, 0], [0, 0, 1])
        );
        assert_eq!(
            Matrix4::from(Matrix3::from(Matrix2([0, 1], [2, 3]))),
            Matrix4([0, 1, 0, 0], [2, 3, 0, 0], [0, 0, 1, 0], [0, 0, 0, 1])
        );
    }
    #[test]
    fn vector_ops() {
        assert_eq!(Vector3(0, 1, 2) + Vector3(3, 4, 5), Vector3(3, 5, 7));
        assert_eq!(Vector3(6, 7, 8) - Vector3(3, 4, 5), Vector3(3, 3, 3));
        assert_eq!(Vector3(0, 2, 4) * 3, Vector3(0, 6, 12));
        assert_eq!(-Vector3(1, 2, -1), Vector3(-1, -2, 1));
        assert_eq!(
            Vector3(2, 3, 4).dot(Vector3(5, 6, 7)),
            2 * 5 + 3 * 6 + 4 * 7
        );
        assert_eq!(Vector2(0, 1).dot(Vector2(1, 0)), 0);
        assert_eq!(Vector3(1, 2, 3).len2(), 1 * 1 + 2 * 2 + 3 * 3);
        assert_eq!(Vector2(10, 3).cross(&Vector2(4, 30)), 10 * 30 - 3 * 4);
        assert_eq!(
            Vector3(2, 3, 4).cross(&Vector3(6, 7, 8)),
            Vector3(3 * 8 - 4 * 7, 4 * 6 - 2 * 8, 2 * 7 - 6 * 3)
        );
    }
    #[test]
    fn inv_quaternion() {
        let q = Quaternion(0.0, 1.0, 0.0, 3.0).normalize();
        let Quaternion(a, b, c, d) = q.clone() * -q;
        // approximate
        assert!(a.abs() <= std::f32::EPSILON);
        assert!(b.abs() <= std::f32::EPSILON);
        assert!(c.abs() <= std::f32::EPSILON);
        assert!((1.0 - d).abs() <= std::f32::EPSILON);
    }
    #[test]
    fn vector_ops_assign() {
        let mut v1 = Vector3(0, 1, 2);
        v1 += Vector3(1, -3, 2);
        assert_eq!(v1, Vector3(0, 1, 2) + Vector3(1, -3, 2));
        v1 -= Vector3(1, -3, 2);
        assert_eq!(v1, Vector3(0, 1, 2) + Vector3(1, -3, 2) - Vector3(1, -3, 2));
        v1 *= 4;
        assert_eq!(
            v1,
            (Vector3(0, 1, 2) + Vector3(1, -3, 2) - Vector3(1, -3, 2)) * 4
        );
    }
}
