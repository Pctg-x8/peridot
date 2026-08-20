//! Manipulation Handles

use peridot_math::{AABB3, Sphere3, Vector3};

pub struct HandleVertex {
    pub pos: [f32; 4],
    pub col_index: u32,
}

const TRANSLATE_HANDLE_BAR_LENGTH: f32 = 0.2;
const TRANSLATE_HANDLE_ARROW_SIZE: f32 = 0.05;
const TRANSLATE_HANDLE_BAR_RADIUS: f32 = 0.005;
const TRANSLATE_HANDLE_ARROW_RADIUS: f32 = 0.02;
const TRANSLATE_HANDLE_BAR_DIVISION: u32 = 6;
const TRANSLATE_HANDLE_ARROW_DIVISION: u32 = 12;

pub const TRANSLATE_HANDLE_HITBOX_X: AABB3<f32> = AABB3 {
    min: Vector3(
        0.0,
        -TRANSLATE_HANDLE_ARROW_RADIUS,
        -TRANSLATE_HANDLE_ARROW_RADIUS,
    ),
    max: Vector3(
        TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
        TRANSLATE_HANDLE_ARROW_RADIUS,
        TRANSLATE_HANDLE_ARROW_RADIUS,
    ),
};
pub const TRANSLATE_HANDLE_HITBOX_Y: AABB3<f32> = AABB3 {
    min: Vector3(
        -TRANSLATE_HANDLE_ARROW_RADIUS,
        0.0,
        -TRANSLATE_HANDLE_ARROW_RADIUS,
    ),
    max: Vector3(
        TRANSLATE_HANDLE_ARROW_RADIUS,
        TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
        TRANSLATE_HANDLE_ARROW_RADIUS,
    ),
};
pub const TRANSLATE_HANDLE_HITBOX_Z: AABB3<f32> = AABB3 {
    min: Vector3(
        -TRANSLATE_HANDLE_ARROW_RADIUS,
        -TRANSLATE_HANDLE_ARROW_RADIUS,
        0.0,
    ),
    max: Vector3(
        TRANSLATE_HANDLE_ARROW_RADIUS,
        TRANSLATE_HANDLE_ARROW_RADIUS,
        TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
    ),
};

pub const TRANSLATE_HANDLE_VCOUNT: usize =
    (TRANSLATE_HANDLE_BAR_DIVISION as usize * 2 + TRANSLATE_HANDLE_ARROW_DIVISION as usize + 1) * 3;
pub const TRANSLATE_HANDLE_ICOUNT: usize = (TRANSLATE_HANDLE_BAR_DIVISION as usize * 6
    + TRANSLATE_HANDLE_ARROW_DIVISION as usize * 3
    + (TRANSLATE_HANDLE_ARROW_DIVISION as usize - 2) * 3)
    * 3;
pub unsafe fn gen_translate_handle_mesh(vs: *mut HandleVertex, is: *mut u16) {
    let base_vindex_x = 0;
    let base_vindex_y = base_vindex_x
        + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2
        + 1
        + TRANSLATE_HANDLE_ARROW_DIVISION as usize;
    let base_vindex_z = base_vindex_y
        + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2
        + 1
        + TRANSLATE_HANDLE_ARROW_DIVISION as usize;
    let mut iindex_x = 0;
    let mut iindex_y = iindex_x
        + TRANSLATE_HANDLE_BAR_DIVISION as usize * 6
        + TRANSLATE_HANDLE_ARROW_DIVISION as usize * 3
        + (TRANSLATE_HANDLE_ARROW_DIVISION as usize - 2) * 3;
    let mut iindex_z = iindex_y
        + TRANSLATE_HANDLE_BAR_DIVISION as usize * 6
        + TRANSLATE_HANDLE_ARROW_DIVISION as usize * 3
        + (TRANSLATE_HANDLE_ARROW_DIVISION as usize - 2) * 3;
    for r in 0..TRANSLATE_HANDLE_BAR_DIVISION {
        let (s, c) =
            (core::f32::consts::TAU * r as f32 / TRANSLATE_HANDLE_BAR_DIVISION as f32).sin_cos();

        unsafe {
            vs.add(base_vindex_x + r as usize).write(HandleVertex {
                pos: [
                    0.0,
                    TRANSLATE_HANDLE_BAR_RADIUS * s,
                    TRANSLATE_HANDLE_BAR_RADIUS * c,
                    1.0,
                ],
                col_index: 0,
            });
            vs.add(base_vindex_x + r as usize + TRANSLATE_HANDLE_BAR_DIVISION as usize)
                .write(HandleVertex {
                    pos: [
                        TRANSLATE_HANDLE_BAR_LENGTH,
                        TRANSLATE_HANDLE_BAR_RADIUS * s,
                        TRANSLATE_HANDLE_BAR_RADIUS * c,
                        1.0,
                    ],
                    col_index: 0,
                });
            vs.add(base_vindex_y + r as usize).write(HandleVertex {
                pos: [
                    TRANSLATE_HANDLE_BAR_RADIUS * s,
                    0.0,
                    TRANSLATE_HANDLE_BAR_RADIUS * c,
                    1.0,
                ],
                col_index: 1,
            });
            vs.add(base_vindex_y + r as usize + TRANSLATE_HANDLE_BAR_DIVISION as usize)
                .write(HandleVertex {
                    pos: [
                        TRANSLATE_HANDLE_BAR_RADIUS * s,
                        TRANSLATE_HANDLE_BAR_LENGTH,
                        TRANSLATE_HANDLE_BAR_RADIUS * c,
                        1.0,
                    ],
                    col_index: 1,
                });
            vs.add(base_vindex_z + r as usize).write(HandleVertex {
                pos: [
                    TRANSLATE_HANDLE_BAR_RADIUS * s,
                    TRANSLATE_HANDLE_BAR_RADIUS * c,
                    0.0,
                    1.0,
                ],
                col_index: 2,
            });
            vs.add(base_vindex_z + r as usize + TRANSLATE_HANDLE_BAR_DIVISION as usize)
                .write(HandleVertex {
                    pos: [
                        TRANSLATE_HANDLE_BAR_RADIUS * s,
                        TRANSLATE_HANDLE_BAR_RADIUS * c,
                        TRANSLATE_HANDLE_BAR_LENGTH,
                        1.0,
                    ],
                    col_index: 2,
                });
        }

        let prev_r = if r > 0 {
            r as u16
        } else {
            TRANSLATE_HANDLE_BAR_DIVISION as u16
        } - 1;

        let a0 = base_vindex_x as u16 + prev_r;
        let b0 = base_vindex_x as u16 + prev_r + TRANSLATE_HANDLE_BAR_DIVISION as u16;
        let a1 = base_vindex_x as u16 + r as u16;
        let b1 = base_vindex_x as u16 + r as u16 + TRANSLATE_HANDLE_BAR_DIVISION as u16;
        unsafe {
            is.add(iindex_x + 0).write(a0);
            is.add(iindex_x + 1).write(b0);
            is.add(iindex_x + 2).write(a1);
            is.add(iindex_x + 3).write(a1);
            is.add(iindex_x + 4).write(b1);
            is.add(iindex_x + 5).write(b0);
        }
        iindex_x += 6;

        let a0 = base_vindex_y as u16 + prev_r;
        let b0 = base_vindex_y as u16 + prev_r + TRANSLATE_HANDLE_BAR_DIVISION as u16;
        let a1 = base_vindex_y as u16 + r as u16;
        let b1 = base_vindex_y as u16 + r as u16 + TRANSLATE_HANDLE_BAR_DIVISION as u16;
        unsafe {
            is.add(iindex_y + 0).write(a0);
            is.add(iindex_y + 1).write(b0);
            is.add(iindex_y + 2).write(a1);
            is.add(iindex_y + 3).write(a1);
            is.add(iindex_y + 4).write(b1);
            is.add(iindex_y + 5).write(b0);
        }
        iindex_y += 6;

        let a0 = base_vindex_z as u16 + prev_r;
        let b0 = base_vindex_z as u16 + prev_r + TRANSLATE_HANDLE_BAR_DIVISION as u16;
        let a1 = base_vindex_z as u16 + r as u16;
        let b1 = base_vindex_z as u16 + r as u16 + TRANSLATE_HANDLE_BAR_DIVISION as u16;
        unsafe {
            is.add(iindex_z + 0).write(a0);
            is.add(iindex_z + 1).write(b0);
            is.add(iindex_z + 2).write(a1);
            is.add(iindex_z + 3).write(a1);
            is.add(iindex_z + 4).write(b1);
            is.add(iindex_z + 5).write(b0);
        }
        iindex_z += 6;
    }
    let arrow_top_vindex_x = base_vindex_x + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2;
    let arrow_top_vindex_y = base_vindex_y + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2;
    let arrow_top_vindex_z = base_vindex_z + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2;
    unsafe {
        vs.add(arrow_top_vindex_x).write(HandleVertex {
            pos: [
                TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
                0.0,
                0.0,
                1.0,
            ],
            col_index: 0,
        });
        vs.add(arrow_top_vindex_y).write(HandleVertex {
            pos: [
                0.0,
                TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
                0.0,
                1.0,
            ],
            col_index: 1,
        });
        vs.add(arrow_top_vindex_z).write(HandleVertex {
            pos: [
                0.0,
                0.0,
                TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
                1.0,
            ],
            col_index: 2,
        });
    }
    let base_vindex_x = arrow_top_vindex_x + 1;
    let base_vindex_y = arrow_top_vindex_y + 1;
    let base_vindex_z = arrow_top_vindex_z + 1;
    for r in 0..TRANSLATE_HANDLE_ARROW_DIVISION {
        let (s, c) =
            (core::f32::consts::TAU * r as f32 / TRANSLATE_HANDLE_ARROW_DIVISION as f32).sin_cos();

        unsafe {
            vs.add(base_vindex_x + r as usize).write(HandleVertex {
                pos: [
                    TRANSLATE_HANDLE_BAR_LENGTH,
                    TRANSLATE_HANDLE_ARROW_RADIUS * s,
                    TRANSLATE_HANDLE_ARROW_RADIUS * c,
                    1.0,
                ],
                col_index: 0,
            });
            vs.add(base_vindex_y + r as usize).write(HandleVertex {
                pos: [
                    TRANSLATE_HANDLE_ARROW_RADIUS * s,
                    TRANSLATE_HANDLE_BAR_LENGTH,
                    TRANSLATE_HANDLE_ARROW_RADIUS * c,
                    1.0,
                ],
                col_index: 1,
            });
            vs.add(base_vindex_z + r as usize).write(HandleVertex {
                pos: [
                    TRANSLATE_HANDLE_ARROW_RADIUS * s,
                    TRANSLATE_HANDLE_ARROW_RADIUS * c,
                    TRANSLATE_HANDLE_BAR_LENGTH,
                    1.0,
                ],
                col_index: 2,
            });
        }

        let prev_r = if r > 0 {
            r as u16
        } else {
            TRANSLATE_HANDLE_ARROW_DIVISION as u16
        } - 1;
        unsafe {
            is.add(iindex_x + 0).write(arrow_top_vindex_x as u16);
            is.add(iindex_x + 1).write(base_vindex_x as u16 + prev_r);
            is.add(iindex_x + 2).write(base_vindex_x as u16 + r as u16);
        }
        iindex_x += 3;
        unsafe {
            is.add(iindex_y + 0).write(arrow_top_vindex_y as u16);
            is.add(iindex_y + 1).write(base_vindex_y as u16 + prev_r);
            is.add(iindex_y + 2).write(base_vindex_y as u16 + r as u16);
        }
        iindex_y += 3;
        unsafe {
            is.add(iindex_z + 0).write(arrow_top_vindex_z as u16);
            is.add(iindex_z + 1).write(base_vindex_z as u16 + prev_r);
            is.add(iindex_z + 2).write(base_vindex_z as u16 + r as u16);
        }
        iindex_z += 3;

        if r > 1 {
            unsafe {
                is.add(iindex_x + 0).write(base_vindex_x as u16 + 0);
                is.add(iindex_x + 1)
                    .write(base_vindex_x as u16 + r as u16 - 1);
                is.add(iindex_x + 2).write(base_vindex_x as u16 + r as u16);
            }
            iindex_x += 3;
            unsafe {
                is.add(iindex_y + 0).write(base_vindex_y as u16 + 0);
                is.add(iindex_y + 1)
                    .write(base_vindex_y as u16 + r as u16 - 1);
                is.add(iindex_y + 2).write(base_vindex_y as u16 + r as u16);
            }
            iindex_y += 3;
            unsafe {
                is.add(iindex_z + 0).write(base_vindex_z as u16 + 0);
                is.add(iindex_z + 1)
                    .write(base_vindex_z as u16 + r as u16 - 1);
                is.add(iindex_z + 2).write(base_vindex_z as u16 + r as u16);
            }
            iindex_z += 3;
        }
    }
}

const ROTATION_HANDLE_DIVS: u32 = 60;
const ROTATION_HANDLE_RADIUS: f32 = 0.25;

pub const ROTATION_HANDLE_HITSPHERE: Sphere3<f32> = Sphere3 {
    center: Vector3(0.0, 0.0, 0.0),
    radius: ROTATION_HANDLE_RADIUS,
};

pub const ROTATION_HANDLE_VCOUNT: usize = (ROTATION_HANDLE_DIVS * 4) as usize;
pub const ROTATION_HANDLE_ICOUNT: usize = (((ROTATION_HANDLE_DIVS) * 2) * 4) as usize;
pub const ROTATION_HANDLE_AXES_DRAW_ICOUNT: u32 = (ROTATION_HANDLE_DIVS * 2) * 3;
pub unsafe fn gen_rotation_handle_mesh(vs: *mut HandleVertex, is: *mut u16) {
    for n in 0..ROTATION_HANDLE_DIVS {
        let th = core::f32::consts::TAU * n as f32 / ROTATION_HANDLE_DIVS as f32;
        let (s, c) = th.sin_cos();

        unsafe {
            vs.add(n as usize).write(HandleVertex {
                pos: [
                    s * ROTATION_HANDLE_RADIUS,
                    c * ROTATION_HANDLE_RADIUS,
                    0.0,
                    1.0,
                ],
                col_index: 2,
            });
            vs.add((n + ROTATION_HANDLE_DIVS) as usize)
                .write(HandleVertex {
                    pos: [
                        s * ROTATION_HANDLE_RADIUS,
                        0.0,
                        c * ROTATION_HANDLE_RADIUS,
                        1.0,
                    ],
                    col_index: 1,
                });
            vs.add((n + ROTATION_HANDLE_DIVS * 2) as usize)
                .write(HandleVertex {
                    pos: [
                        0.0,
                        s * ROTATION_HANDLE_RADIUS,
                        c * ROTATION_HANDLE_RADIUS,
                        1.0,
                    ],
                    col_index: 0,
                });

            is.add((n * 2) as usize).write(n as u16);
            is.add((n * 2 + 1) as usize)
                .write(((n + 1) % ROTATION_HANDLE_DIVS) as u16);
            is.add(((n + ROTATION_HANDLE_DIVS) * 2) as usize)
                .write((n + ROTATION_HANDLE_DIVS) as u16);
            is.add(((n + ROTATION_HANDLE_DIVS) * 2 + 1) as usize)
                .write((((n + 1) % ROTATION_HANDLE_DIVS) + ROTATION_HANDLE_DIVS) as u16);
            is.add(((n + ROTATION_HANDLE_DIVS * 2) * 2) as usize)
                .write((n + ROTATION_HANDLE_DIVS * 2) as u16);
            is.add(((n + ROTATION_HANDLE_DIVS * 2) * 2 + 1) as usize)
                .write((((n + 1) % ROTATION_HANDLE_DIVS) + ROTATION_HANDLE_DIVS * 2) as u16);

            // facing to camera
            vs.add((n + ROTATION_HANDLE_DIVS * 3) as usize)
                .write(HandleVertex {
                    pos: [c, s, 0.0, c],
                    col_index: 3,
                });
            is.add(((n + ROTATION_HANDLE_DIVS * 3) * 2) as usize)
                .write((n + ROTATION_HANDLE_DIVS * 3) as u16);
            is.add(((n + ROTATION_HANDLE_DIVS * 3) * 2 + 1) as usize)
                .write((((n + 1) % ROTATION_HANDLE_DIVS) + ROTATION_HANDLE_DIVS * 3) as u16);
        }
    }
}

const SCALE_HANDLE_BAR_LENGTH: f32 = 0.2;
const SCALE_HANDLE_CUBE_SIZE: f32 = 0.02;
const SCALE_HANDLE_BAR_THICKNESS: f32 = 0.005;

pub const SCALE_HANDLE_HITBOX_X: AABB3<f32> = AABB3 {
    min: Vector3(
        SCALE_HANDLE_CUBE_SIZE,
        -SCALE_HANDLE_CUBE_SIZE,
        -SCALE_HANDLE_CUBE_SIZE,
    ),
    max: Vector3(
        SCALE_HANDLE_BAR_LENGTH + SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
    ),
};
pub const SCALE_HANDLE_HITBOX_Y: AABB3<f32> = AABB3 {
    min: Vector3(
        -SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
        -SCALE_HANDLE_CUBE_SIZE,
    ),
    max: Vector3(
        SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_BAR_LENGTH + SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
    ),
};
pub const SCALE_HANDLE_HITBOX_Z: AABB3<f32> = AABB3 {
    min: Vector3(
        -SCALE_HANDLE_CUBE_SIZE,
        -SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
    ),
    max: Vector3(
        SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_BAR_LENGTH + SCALE_HANDLE_CUBE_SIZE,
    ),
};
pub const SCALE_HANDLE_HITBOX_CENTER: AABB3<f32> = AABB3 {
    min: Vector3(
        -SCALE_HANDLE_CUBE_SIZE,
        -SCALE_HANDLE_CUBE_SIZE,
        -SCALE_HANDLE_CUBE_SIZE,
    ),
    max: Vector3(
        SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
        SCALE_HANDLE_CUBE_SIZE,
    ),
};

pub const SCALE_HANDLE_VCOUNT: usize = (8 * 4) + (8 * 3); // tip cube + bar cube
pub const SCALE_HANDLE_ICOUNT: usize = (6 * 6 * 4) + (6 * 4 * 3); // tip cube(6 faces) + bar cube(4 faces)
pub unsafe fn gen_scale_handle_mesh(vs: *mut HandleVertex, is: *mut u16) {
    // Note: 編集しづらいので一部rustfmtを意図的にスキップする
    const fn hv(x: f32, y: f32, z: f32, cx: u32) -> HandleVertex {
        HandleVertex {
            pos: [x, y, z, 1.0],
            col_index: cx,
        }
    }

    // bar cubes
    unsafe {
        let t = SCALE_HANDLE_BAR_THICKNESS;
        let am = 0.0;
        let ar = SCALE_HANDLE_BAR_LENGTH;
        let col = 4;
        vs.copy_from_nonoverlapping(
            [
                // x
                hv(am, -t, -t, col),
                hv(ar, -t, -t, col),
                hv(am, -t, t, col),
                hv(ar, -t, t, col),
                hv(am, t, -t, col),
                hv(ar, t, -t, col),
                hv(am, t, t, col),
                hv(ar, t, t, col),
                // y
                hv(-t, am, -t, col),
                hv(-t, ar, -t, col),
                hv(-t, am, t, col),
                hv(-t, ar, t, col),
                hv(t, am, -t, col),
                hv(t, ar, -t, col),
                hv(t, am, t, col),
                hv(t, ar, t, col),
                // z
                hv(-t, -t, am, col),
                hv(-t, -t, ar, col),
                hv(-t, t, am, col),
                hv(-t, t, ar, col),
                hv(t, -t, am, col),
                hv(t, -t, ar, col),
                hv(t, t, am, col),
                hv(t, t, ar, col),
            ]
            .as_ptr(),
            8 * 3,
        );
        #[rustfmt::skip]
        is.copy_from_nonoverlapping(
            [
                // x
                0, 1, 2, 2, 3, 1,
                4, 5, 6, 6, 7, 5,
                0, 1, 4, 4, 5, 1,
                2, 3, 6, 6, 7, 3,
                // y
                8 + 0, 8 + 1, 8 + 2, 8 + 2, 8 + 3, 8 + 1,
                8 + 4, 8 + 5, 8 + 6, 8 + 6, 8 + 7, 8 + 5,
                8 + 0, 8 + 1, 8 + 4, 8 + 4, 8 + 5, 8 + 1,
                8 + 2, 8 + 3, 8 + 6, 8 + 6, 8 + 7, 8 + 3,
                // z
                8 * 2 + 0, 8 * 2 + 1, 8 * 2 + 2, 8 * 2 + 2, 8 * 2 + 3, 8 * 2 + 1,
                8 * 2 + 4, 8 * 2 + 5, 8 * 2 + 6, 8 * 2 + 6, 8 * 2 + 7, 8 * 2 + 5,
                8 * 2 + 0, 8 * 2 + 1, 8 * 2 + 4, 8 * 2 + 4, 8 * 2 + 5, 8 * 2 + 1,
                8 * 2 + 2, 8 * 2 + 3, 8 * 2 + 6, 8 * 2 + 6, 8 * 2 + 7, 8 * 2 + 3,
            ]
            .as_ptr(),
            6 * 4 * 3,
        );
    }

    // tip cubes
    unsafe {
        let s = SCALE_HANDLE_CUBE_SIZE;
        let c = SCALE_HANDLE_BAR_LENGTH;

        #[rustfmt::skip]
        vs.add(8 * 3).copy_from_nonoverlapping(
            [
                // x
                hv(c - s, -s, -s, 0),
                hv(c + s, -s, -s, 0),
                hv(c - s,  s, -s, 0),
                hv(c + s,  s, -s, 0),
                hv(c - s, -s,  s, 0),
                hv(c + s, -s,  s, 0),
                hv(c - s,  s,  s, 0),
                hv(c + s,  s,  s, 0),
                // y
                hv(-s, c - s, -s, 1),
                hv( s, c - s, -s, 1),
                hv(-s, c + s, -s, 1),
                hv( s, c + s, -s, 1),
                hv(-s, c - s,  s, 1),
                hv( s, c - s,  s, 1),
                hv(-s, c + s,  s, 1),
                hv( s, c + s,  s, 1),
                // z
                hv(-s, -s, c - s, 2),
                hv( s, -s, c - s, 2),
                hv(-s,  s, c - s, 2),
                hv( s,  s, c - s, 2),
                hv(-s, -s, c + s, 2),
                hv( s, -s, c + s, 2),
                hv(-s,  s, c + s, 2),
                hv( s,  s, c + s, 2),
                // center
                hv(-s, -s, -s, 3),
                hv( s, -s, -s, 3),
                hv(-s,  s, -s, 3),
                hv( s,  s, -s, 3),
                hv(-s, -s,  s, 3),
                hv( s, -s,  s, 3),
                hv(-s,  s,  s, 3),
                hv( s,  s,  s, 3),
            ]
            .as_ptr(),
            8 * 4,
        );
        #[rustfmt::skip]
        is.add(6 * 4 * 3).copy_from_nonoverlapping(
            [
                // x
                8 * 3 + 0, 8 * 3 + 1, 8 * 3 + 2, 8 * 3 + 2, 8 * 3 + 3, 8 * 3 + 1,
                8 * 3 + 4, 8 * 3 + 5, 8 * 3 + 6, 8 * 3 + 6, 8 * 3 + 7, 8 * 3 + 5,
                8 * 3 + 0, 8 * 3 + 1, 8 * 3 + 4, 8 * 3 + 4, 8 * 3 + 5, 8 * 3 + 1,
                8 * 3 + 2, 8 * 3 + 3, 8 * 3 + 6, 8 * 3 + 6, 8 * 3 + 7, 8 * 3 + 3,
                8 * 3 + 0, 8 * 3 + 2, 8 * 3 + 4, 8 * 3 + 4, 8 * 3 + 6, 8 * 3 + 2,
                8 * 3 + 1, 8 * 3 + 3, 8 * 3 + 5, 8 * 3 + 5, 8 * 3 + 7, 8 * 3 + 3,
                // y
                8 * 3 + 8 + 0, 8 * 3 + 8 + 1, 8 * 3 + 8 + 2, 8 * 3 + 8 + 2, 8 * 3 + 8 + 3, 8 * 3 + 8 + 1,
                8 * 3 + 8 + 4, 8 * 3 + 8 + 5, 8 * 3 + 8 + 6, 8 * 3 + 8 + 6, 8 * 3 + 8 + 7, 8 * 3 + 8 + 5,
                8 * 3 + 8 + 0, 8 * 3 + 8 + 1, 8 * 3 + 8 + 4, 8 * 3 + 8 + 4, 8 * 3 + 8 + 5, 8 * 3 + 8 + 1,
                8 * 3 + 8 + 2, 8 * 3 + 8 + 3, 8 * 3 + 8 + 6, 8 * 3 + 8 + 6, 8 * 3 + 8 + 7, 8 * 3 + 8 + 3,
                8 * 3 + 8 + 0, 8 * 3 + 8 + 2, 8 * 3 + 8 + 4, 8 * 3 + 8 + 4, 8 * 3 + 8 + 6, 8 * 3 + 8 + 2,
                8 * 3 + 8 + 1, 8 * 3 + 8 + 3, 8 * 3 + 8 + 5, 8 * 3 + 8 + 5, 8 * 3 + 8 + 7, 8 * 3 + 8 + 3,
                // z
                8 * 3 + 8 * 2 + 0, 8 * 3 + 8 * 2 + 1, 8 * 3 + 8 * 2 + 2, 8 * 3 + 8 * 2 + 2, 8 * 3 + 8 * 2 + 3, 8 * 3 + 8 * 2 + 1,
                8 * 3 + 8 * 2 + 4, 8 * 3 + 8 * 2 + 5, 8 * 3 + 8 * 2 + 6, 8 * 3 + 8 * 2 + 6, 8 * 3 + 8 * 2 + 7, 8 * 3 + 8 * 2 + 5,
                8 * 3 + 8 * 2 + 0, 8 * 3 + 8 * 2 + 1, 8 * 3 + 8 * 2 + 4, 8 * 3 + 8 * 2 + 4, 8 * 3 + 8 * 2 + 5, 8 * 3 + 8 * 2 + 1,
                8 * 3 + 8 * 2 + 2, 8 * 3 + 8 * 2 + 3, 8 * 3 + 8 * 2 + 6, 8 * 3 + 8 * 2 + 6, 8 * 3 + 8 * 2 + 7, 8 * 3 + 8 * 2 + 3,
                8 * 3 + 8 * 2 + 0, 8 * 3 + 8 * 2 + 2, 8 * 3 + 8 * 2 + 4, 8 * 3 + 8 * 2 + 4, 8 * 3 + 8 * 2 + 6, 8 * 3 + 8 * 2 + 2,
                8 * 3 + 8 * 2 + 1, 8 * 3 + 8 * 2 + 3, 8 * 3 + 8 * 2 + 5, 8 * 3 + 8 * 2 + 5, 8 * 3 + 8 * 2 + 7, 8 * 3 + 8 * 2 + 3,
                // center
                8 * 3 + 8 * 3 + 0, 8 * 3 + 8 * 3 + 1, 8 * 3 + 8 * 3 + 2, 8 * 3 + 8 * 3 + 2, 8 * 3 + 8 * 3 + 3, 8 * 3 + 8 * 3 + 1,
                8 * 3 + 8 * 3 + 4, 8 * 3 + 8 * 3 + 5, 8 * 3 + 8 * 3 + 6, 8 * 3 + 8 * 3 + 6, 8 * 3 + 8 * 3 + 7, 8 * 3 + 8 * 3 + 5,
                8 * 3 + 8 * 3 + 0, 8 * 3 + 8 * 3 + 1, 8 * 3 + 8 * 3 + 4, 8 * 3 + 8 * 3 + 4, 8 * 3 + 8 * 3 + 5, 8 * 3 + 8 * 3 + 1,
                8 * 3 + 8 * 3 + 2, 8 * 3 + 8 * 3 + 3, 8 * 3 + 8 * 3 + 6, 8 * 3 + 8 * 3 + 6, 8 * 3 + 8 * 3 + 7, 8 * 3 + 8 * 3 + 3,
                8 * 3 + 8 * 3 + 0, 8 * 3 + 8 * 3 + 2, 8 * 3 + 8 * 3 + 4, 8 * 3 + 8 * 3 + 4, 8 * 3 + 8 * 3 + 6, 8 * 3 + 8 * 3 + 2,
                8 * 3 + 8 * 3 + 1, 8 * 3 + 8 * 3 + 3, 8 * 3 + 8 * 3 + 5, 8 * 3 + 8 * 3 + 5, 8 * 3 + 8 * 3 + 7, 8 * 3 + 8 * 3 + 3,
            ].as_ptr(),
            6 * 6 * 4,
        );
    }
}
