//! PeridotExtendedMathematics: Vector/Matrix

use numtraits::{One, Zero};

/// 2-dimensional vector
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Vector2<T>(pub T, pub T);
/// 3-dimensional vector
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Vector3<T>(pub T, pub T, pub T);
/// 4-dimensional vector
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Vector4<T>(pub T, pub T, pub T, pub T);
/// Arbitrary rotating
#[derive(Debug, Clone, PartialEq, Eq)] #[repr(C)] pub struct Quaternion<T>(pub T, pub T, pub T, pub T);

/// Type alias for vec2 of f32
pub type Vector2F32 = Vector2<f32>;
/// Type alias for vec3 of f32
pub type Vector3F32 = Vector3<f32>;
/// Type alias for vec4 of f32
pub type Vector4F32 = Vector4<f32>;
/// Type alias for qvec of f32
pub type QuaternionF32 = Quaternion<f32>;

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

// Matrix and Matrix Multiplication //
impl<T> Mul for Matrix2<T> where T: Mul<T>, <T as Mul>::Output: Add {
    type Output = Matirx2<<<T as Mul>::Output as Add>::Output>;
    fn mul(self, other: Matrix2<T>) -> Self::Output {
        let dp = |src: &[T; 2], colindex: usize| {
            src[0] * other.0[colindex] + src[1] * other.1[colindex]
        };

        Matrix2([dp(&self.0, 0), dp(&self.0, 1)], [dp(&self.1, 0), dp(&self.1, 1)])
    }
}
impl<T> Mul for Matrix3<T> where T: Mul<T>, <T as Mul>::Output: Add {
    type Output = Matirx3<<<T as Mul>::Output as Add>::Output>;
    fn mul(self, other: Matrix3<T>) -> Self::Output {
        let dp = |src: &[T; 3], colindex: usize| {
            src[0] * other.0[colindex] + src[1] * other.1[colindex] + src[2] * other.2[colindex]
        };
        
        Matrix3([dp(&self.0, 0), dp(&self.0, 1), dp(&self.0, 2)],
            [dp(&self.1, 0), dp(&self.1, 1), dp(&self.1, 2)], [dp(&self.2, 0), dp(&self.2, 1), dp(&self.2, 2)])
    }
}
impl<T> Mul for Matrix4<T> where T: Mul<T>, <T as Mul>::Output: Add {
    type Output = Matirx4<<<T as Mul>::Output as Add>::Output>;
    fn mul(self, other: Matrix4<T>) -> Self::Output {
        let dp = |src: &[T; 4], colindex: usize| {
            src[0] * other.0[colindex] + src[1] * other.1[colindex] +
            src[2] * other.2[colindex] + src[3] * other.3[colindex]
        };
        
        Matrix4([dp(&self.0, 0), dp(&self.0, 1), dp(&self.0, 2), dp(&self.0, 3)],
            [dp(&self.1, 0), dp(&self.1, 1), dp(&self.1, 2), dp(&self.2, 3)],
            [dp(&self.2, 0), dp(&self.2, 1), dp(&self.2, 2), dp(&self.2, 3)],
            [dp(&self.3, 0), dp(&self.3, 1), dp(&self.3, 2), dp(&self.3, 3)])
    }
}

// Scaling, Rotating //
impl<T> Matrix2<T> {
    pub fn scale(self, v: Vector2) -> Self {
        Matrix2([self.0[0] * v.x, self.0[1] * v.x], [self.1[0] * v.y, self.1[1] * v.y])
    }
}
// Rotating //
impl<T> Matrix2<T> where T: Mul<f32, Output = T> {
    pub fn rotate(self, rad: f32) -> Self {
        let (s, c) = rad.sin_cos();
        /*
| a b |   | c -s |   | (a, b).(c, s) (a, b).(-s, c) |
| n d | x | s  c | = | (n, d).(c, s) (n, d).(-s, c) |
        */
        Matrix2(
            [dotproduct2(&self.0, &[c, s]), dotproduct2(&self.0, &[-s, c])],
            [dotproduct2(&self.1, &[c, s]), dotproduct2(&self.1, &[-s, c])]
        )
    }
}
impl<T> Matrix3<T> {
    pub fn scale(self, v: Vector3) -> Self {
        Matrix3([self.0[0] * v.x, self.0[1] * v.x, self.0[2] * v.x],
            [self.1[0] * v.y, self.1[1] * v.y, self.2[2] * v.y],
            [self.2[0] * v.z, self.2[1] * v.z, self.2[2] * v.z])
    }
}
impl<T> Matrix4<T> {
    pub fn scale(self, v: Vector3) -> Self {
        Matrix4([self.0[0] * v.0, self.0[1] * v.0, self.0[2] * v.0, self.0[3] * v.0],
            [self.1[0] * v.1, self.1[1] * v.1, self.2[2] * v.1, self.1[3] * v.1],
            [self.2[0] * v.2, self.2[1] * v.2, self.2[2] * v.2, self.2[3] * v.2],
            [self.3[0] * v.3, self.3[1] * v.3, self.3[2] * v.3, self.3[3] * v.3])
    }
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

// Point Translation by Multiplication //
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
impl<T: Mul> Mul<Vector2<T>> for Matrix2x3<T> {
    type Output = Vector2<<T as Mul>::Output>;
    fn mul(self, v: Vector2) -> Self::Output {
        let va = Vector3::from(v).into();
        Vector2(dotproduct3(&va, &self.0), dotproduct3(&va, &self.1))
    }
}
impl<T: Mul> Mul<Vector3<T>> for Matrix3x4<T> {
    type Output = Vector3<<T as Mul>::Output>;
    fn mul(self, v: Vector3) -> Self::Output {
        let va = Vector4::from(v).into();
        Vector2(dotproduct4(&va, &self.0), dotproduct4(&va, &self.1))
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

// Length Function and Normalization //
impl Vector2<f32> {
    pub fn len2(&self) -> f32 { self.0.powf(2.0) + self.1.powf(2.0) }
    pub fn len(&self) -> f32 { self.len2().sqrt() }
    pub fn normalize(self) -> Self {
        let l0 = self.len();
        Vector2(self.0 / l0, self.1 / l0)
    }
}
impl Vector2<f64> {
    pub fn len2(&self) -> f64 { self.0.powf(2.0) + self.1.powf(2.0) }
    pub fn len(&self) -> f64 { self.len2().sqrt() }
    pub fn normalize(self) -> Self {
        let l0 = self.len();
        Vector2(self.0 / l0, self.1 / l0)
    }
}
impl Vector3<f32> {
    pub fn len2(&self) -> f32 { self.0.powf(2.0) + self.1.powf(2.0) + self.2.powf(2.0) }
    pub fn len(&self) -> f32 { self.len2().sqrt() }
    pub fn normalize(self) -> Self {
        let l0 = self.len();
        Vector3(self.0 / l0, self.1 / l0, self.2 / l0)
    }
}
impl Vector3<f64> {
    pub fn len2(&self) -> f64 { self.0.powf(2.0) + self.1.powf(2.0) + self.2.powf(2.0) }
    pub fn len(&self) -> f64 { self.len2().sqrt() }
    pub fn normalize(self) -> Self {
        let l0 = self.len();
        Vector3(self.0 / l0, self.1 / l0, self.2 / l0)
    }
}
impl Vector4<f32> {
    pub fn len2(&self) -> f32 { self.0.powf(2.0) + self.1.powf(2.0) + self.2.powf(2.0) + self.3.powf(2.0) }
    pub fn len(&self) -> f32 { self.len2().sqrt() }
    pub fn normalize(self) -> Self {
        let l0 = self.len();
        Vector4(self.0 / l0, self.1 / l0, self.2 / l0, self.3 / l0)
    }
}
impl Vector4<f64> {
    pub fn len2(&self) -> f64 { self.0.powf(2.0) + self.1.powf(2.0) + self.2.powf(2.0) + self.3.powf(2.0) }
    pub fn len(&self) -> f64 { self.len2().sqrt() }
    pub fn normalize(self) -> Self {
        let l0 = self.len();
        Vector4(self.0 / l0, self.1 / l0, self.2 / l0, self.3 / l0)
    }
}

// quaternion shortcuts //
impl<T> Quaternion<T> where T: Mul<Output = T> {
    pub fn new(rad: f32, axis: Vector3<T>) -> Self {
        let (s, c) = (rad / 2.0).sin_cos();
        let axis = axis.normalize();

        Quaternion(axis.0 * s, axis.1 * s, axis.2 * s, c)
    }
}
