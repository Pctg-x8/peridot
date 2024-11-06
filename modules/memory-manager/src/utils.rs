/// `base*2^n`(n=0..)の無限数列
pub fn power_of_2_series_from_u64(mut base: u64) -> impl Iterator<Item = u64> {
    core::iter::from_fn(move || {
        base <<= 1;
        Some(base >> 1)
    })
}

/// `base*2^n`(n=0..)の無限数列
pub fn power_of_2_series_from(mut base: usize) -> impl Iterator<Item = usize> {
    core::iter::from_fn(move || {
        base <<= 1;
        Some(base >> 1)
    })
}

/// 切り上げ a(アラインメント)は2の累乗数
pub const fn align2(x: usize, a: usize) -> usize {
    (x + (a - 1)) & !(a - 1)
}

/// 切り上げ a(アラインメント)は2の累乗数
pub const fn align2_u64(x: u64, a: u64) -> u64 {
    (x + (a - 1)) & !(a - 1)
}

/// 最上位ビットのみが立っている状態にする
/// # Example
/// ```
/// assert_eq!(only_top_bit(0b0100_0000), 0b0100_0000);
/// assert_eq!(only_top_bit(0b0110_1101), 0b0100_0000);
/// ```
pub const fn only_top_bit(x: u64) -> u64 {
    1u64 << (64 - x.leading_zeros() - 1)
}

/// 2の累乗数に切り上げ
pub const fn round_up_to_next_power_of_two(value: u64) -> u64 {
    // Note: 最上位ビット以下のみ立っている状態（= 最上位ビットのみ立っている状態 - 1）を足すと
    // 純粋な2の累乗数以外は繰り上がるので、その状態で再び最上位ビットのみの状態にすると切り上げになる
    only_top_bit(value + only_top_bit(value) - 1) as _
}
