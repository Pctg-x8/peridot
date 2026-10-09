/// Construct a range from a start value and a length.
#[inline(always)]
pub const fn range_from_len(from: f32, len: f32) -> core::range::Range<f32> {
    core::range::Range {
        start: from,
        end: from + len,
    }
}

/// Construct a range from a start value and a length.
#[inline(always)]
pub const fn range_from_len_u64(from: u64, len: u64) -> core::range::Range<u64> {
    core::range::Range {
        start: from,
        end: from + len,
    }
}

/// value rate between the range
#[inline(always)]
pub const fn rate_of_range(r: &core::range::Range<f32>, v: f32) -> f32 {
    (v - r.start) / (r.end - r.start)
}

#[inline(always)]
pub const fn is_beyond_of_range(r: &core::range::Range<f32>, v: f32) -> bool {
    r.end <= v
}
