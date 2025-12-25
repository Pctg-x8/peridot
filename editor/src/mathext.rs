#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct Matrix4(pub [f32; 4 * 4]);
impl Matrix4 {
    pub const IDENTITY: Self = Self([
        1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
    ]);

    pub const fn scale(x: f32, y: f32) -> Self {
        Self([
            x, 0.0, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ])
    }

    pub const fn translate(x: f32, y: f32) -> Self {
        Self([
            1.0, 0.0, 0.0, x, 0.0, 1.0, 0.0, y, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0,
        ])
    }

    pub const fn transpose(self) -> Self {
        Self([
            self.0[0], self.0[4], self.0[8], self.0[12], self.0[1], self.0[5], self.0[9],
            self.0[13], self.0[2], self.0[6], self.0[10], self.0[14], self.0[3], self.0[7],
            self.0[11], self.0[15],
        ])
    }

    const fn row(&self, r: usize) -> [f32; 4] {
        [
            self.0[r * 4 + 0],
            self.0[r * 4 + 1],
            self.0[r * 4 + 2],
            self.0[r * 4 + 3],
        ]
    }

    pub const fn mul_mat4(self, other: Self) -> Self {
        let other = other.transpose();

        Self([
            dot4(self.row(0), other.row(0)),
            dot4(self.row(0), other.row(1)),
            dot4(self.row(0), other.row(2)),
            dot4(self.row(0), other.row(3)),
            dot4(self.row(1), other.row(0)),
            dot4(self.row(1), other.row(1)),
            dot4(self.row(1), other.row(2)),
            dot4(self.row(1), other.row(3)),
            dot4(self.row(2), other.row(0)),
            dot4(self.row(2), other.row(1)),
            dot4(self.row(2), other.row(2)),
            dot4(self.row(2), other.row(3)),
            dot4(self.row(3), other.row(0)),
            dot4(self.row(3), other.row(1)),
            dot4(self.row(3), other.row(2)),
            dot4(self.row(3), other.row(3)),
        ])
    }
}

const fn dot4(a: [f32; 4], b: [f32; 4]) -> f32 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * b[3]
}
