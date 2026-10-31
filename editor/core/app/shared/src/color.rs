/// 32-bit color(r8g8b8a8)
pub struct Color32 {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}
impl Color32 {
    pub const fn premultiplied(&self) -> Self {
        Self {
            r: (self.r as f32 * self.a as f32 / 255.0).round() as u8,
            g: (self.g as f32 * self.a as f32 / 255.0).round() as u8,
            b: (self.b as f32 * self.a as f32 / 255.0).round() as u8,
            a: self.a,
        }
    }

    pub const fn argb8888(&self) -> u32 {
        ((self.a as u32) << 24) | ((self.r as u32) << 16) | ((self.g as u32) << 8) | (self.b as u32)
    }

    pub const fn r_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.r as f32 / 255.0).min(1.0)) as u32
    }

    pub const fn g_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.g as f32 / 255.0).min(1.0)) as u32
    }

    pub const fn b_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.b as f32 / 255.0).min(1.0)) as u32
    }

    pub const fn a_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.a as f32 / 255.0).min(1.0)) as u32
    }
}
