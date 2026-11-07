//! Easing Functions

use once_cell::sync::Lazy;

pub const fn linear(x: f32) -> f32 {
    x
}
pub fn quadratic(x: f32) -> f32 {
    x.powi(2)
}
pub fn cubic(x: f32) -> f32 {
    x.powi(3)
}
pub fn quartic(x: f32) -> f32 {
    x.powi(4)
}
pub fn quintic(x: f32) -> f32 {
    x.powi(5)
}

pub fn sin(x: f32) -> f32 {
    1.0 + ((x - 1.0) * std::f32::consts::FRAC_PI_2).sin()
}
pub fn exp(x: f32) -> f32 {
    2.0f32.powf(10.0 * (x - 1.0))
}

pub fn back(x: f32) -> f32 {
    // intro: h /= 0, 0 < a < 1
    // f(0) = 0
    // f(1) = 1
    // f(a) = h
    // f'(1) = 0
    // f'(a) = 0

    // f'(x) = n(x - 1)(x - a) = 0
    // f'(x) = n(x^2 - (1 + a)x + a) = nx^2 - (1 + a)nx + an = 0

    // f(x) = (n / 3)x^3 - ((1 + a) / 2) * nx^2 + anx + d
    // f(0) = 0 なので d = 0
    // f(x) = (n / 3)x^3 - ((1 + a) / 2) * nx^2 + anx

    // let x = 1
    // (n / 3) - ((1 + a) / 2) * n + an = 1
    // 2n - 3 * (1 + a) * n + 6an = 6
    // 2n - 3n - 3an + 6an = 6
    // -n + 3an = 6
    // n * (3a - 1) = 6

    // let x = a
    // (n / 3) * a^3 - 0.5 * (1 + a) * n * a^2 + n * a^2 = h
    // 2n * a^3 - 3 * (1 + a) * n * a^2 + 6n * a^2 = 6h
    // 2n * a^3 - 3n * a^2 - 3n * a^3 + 6n * a^2 = 6h
    // -n * a^3 + 3n * a^2 = 6h
    // n * (3a^2 - a^3) = 6h

    // n / 3 /= 0 なので n /= 0
    // 3a - 1 = 6 / n
    // 3a^2 - a^3 = 6h / n
    // (3a^2 - a^3) / h = 6 / n
    // 3a - 1 = (3a^2 - a^3) / h
    // 3ah - h = 3a^2 - a^3
    // a^3 - 3a^2 + 3ah - h = 0
    // let a = t + 3 / 3 = t + 1
    // (t + 1)^3 - 3 * (t + 1)^2 + 3h * (t + 1) - h = 0
    // t^3 + 3t^2 + 3t + 1 - 3t^2 - 6t - 3 + 3ht + 3h - h = 0
    // t^3 + 3t^2 - 3t^2 + 3t - 6t + 3ht + 3h - h + 1 - 3 = 0
    // t^3 + (3h - 3)t + 2h - 2 = 0
    // D -(4 * (3h - 3)^3 + 27 * (2h - 2)^2)
    // = -(4 * (27h^3 - 81h^2 + 81h - 27) + 27 * (4h^2 - 8h + 4))
    // = -(108h^3 - 324h^2 + 324h - 108 + 108h^2 - 216h + 108)
    // = -(108h^3 - 216h^2 + 108h)
    // = -108h * (h^2 - 2h + 1) = -108h * (h - 1)^2
    // discriminant roots are h = 0 or h = 1
    // Dd -324h^2 + 432h - 108
    // DdD 432 * 432 - 432 * 324 = 432 * (432 - 324), always DdD > 0 so has 2 real roots
    // flatpoints h = (-432 +- sqrt(432 * (432 - 324))) / -648 = (432 +- sqrt(432 * 108)) / 648
    // sqrt(432 * 108) = sqrt(4 * 108 * 108) = sqrt(2^2 * 2^2 * 3^3 * 2^2 * 3^3) = sqrt(2^6 * 3^6) = 2^3 * 3^3 = 8 * 27 = 216
    // flatpoints h = (432 + 216) / 648 = 1, (432 - 216) / 648 = 216 / 648 = 1 / 3, 1 and 1/3
    // let h = 1 in -108h * (h - 1)^2 = 0
    // let h = 1/3 in -108h * (h - 1)^2 = -36 * (-2/3)^2 = -36 * 4/9 = -16
    // D(0) = 0, D(1/3) = -16(minpoint), D(1) = 0(maxpoint) so always D(h) <= 0, the cubic equation has always one real root, when h >= 0
    // t = -2 * sqrt(p / 3) * sinh(arcsinh(sqrt(3 / p) * 3q / 2p) / 3)
    // let p = 3h - 3, q = 2h - 2, t = -2 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1)) * (6h - 6) / (6h - 6)) / 3)
    // t = -2 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3)
    // a = t + 1 = 1 - 2 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3)

    // let previous a in n * (3a - 1) = 6
    // n * (3 - 6 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3) - 1) = 6
    // n * (2 - 6 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3)) = 6
    // n = 6 / (2 - 6 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3))
    // n = 3 / (1 - 3 * sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3))

    // let b = sqrt(h - 1) * sinh(arcsinh(sqrt(1 / (h - 1))) / 3)
    // a = 1 - 2b
    // n = 3 / (1 - 3b)
    // f(x) = (n / 3)x^3 - 0.5 * (1 + p) * nx^2 + pnx
    // f(x) = x^3 / (1 - 3b) - (1 - 1b) * (3 / (1 - 3b)) * n^2 + (1 - 2b) * (3 / (1 - 3b)) * x
    // f(x) = x^3 / (1 - 3b) - x^2 * (3 - 3b) / (1 - 3b) + x * (3 - 6b) / (1 - 3b)
    // f(x) = (x^3 - 3 * (1 - b) * x^2 + 3 * (1 - 2b) * x) / (1 - 3b)

    // peak is 10%
    const H: f32 = 1.1;
    static B: Lazy<f32> = Lazy::new(|| (H - 1.0).sqrt() * ((H - 1.0).recip().asinh() / 3.0).sinh());
    let b = *B;

    (x.powi(3) - 3.0 * (1.0 - b) * x.powi(2) + 3.0 * (1.0 - 2.0 * b) * x) / (1.0 - 3.0 * b)
}
