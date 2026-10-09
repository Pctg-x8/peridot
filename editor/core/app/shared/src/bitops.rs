/// # Examples
///
/// ```
/// assert_eq!(most_top_bit_pos_u64(0), 0);
/// assert_eq!(most_top_bit_pos_u64(64), 6);
/// assert_eq!(most_top_bit_pos_u64(128), 7);
/// ```
pub const fn most_top_bit_pos_u64(v: u64) -> u32 {
    64 - v.leading_zeros() - 1
}

/// # Examples
///
/// ```
/// assert_eq!(lowest_bit_pos_u64(1), 0);
/// assert_eq!(lowest_bit_pos_u64(5), 0);
/// assert_eq!(lowest_bit_pos_u64(10), 1);
/// assert_eq!(lowest_bit_pos_u64(0x24), 2);
/// ```
pub const fn lowest_bit_pos_u16(v: u16) -> u8 {
    v.trailing_zeros() as _
}

pub const fn find_lowest_bit_pos_from_u16(v: u16, bitpos: u16) -> Option<u8> {
    match v & (!0 << bitpos) {
        0 => None,
        x => Some(lowest_bit_pos_u16(x)),
    }
}
