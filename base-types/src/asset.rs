use std::fmt::Write;

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AssetID([u8; 16]);
impl core::fmt::Debug for AssetID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for &b in &self.0[0..4] {
            fmt_hex2(f, b)?;
        }
        f.write_char('-')?;
        for &b in &self.0[4..8] {
            fmt_hex2(f, b)?;
        }
        f.write_char('-')?;
        for &b in &self.0[8..12] {
            fmt_hex2(f, b)?;
        }
        f.write_char('-')?;
        for &b in &self.0[12..16] {
            fmt_hex2(f, b)?;
        }

        Ok(())
    }
}
impl AssetID {
    pub const GLOBAL_ASSET_GROUP_ID: Self = Self([0; _]);

    pub const fn from_bytes(b: [u8; 16]) -> Self {
        Self(b)
    }

    pub const fn as_bytes(&self) -> &[u8; 16] {
        &self.0
    }

    pub const fn as_bytes_mut(&mut self) -> &mut [u8; 16] {
        &mut self.0
    }

    pub fn build_runtime_asset_path_relative(&self) -> String {
        let mut x = String::with_capacity(16 * 2 + 1);
        for &b in &self.0[..2] {
            let _ = fmt_hex2(&mut x, b);
        }
        x.push('/');
        for &b in &self.0[2..] {
            let _ = fmt_hex2(&mut x, b);
        }

        x
    }

    pub const SERIALIZE_TEXT_LEN: usize = 32;
    pub fn serialize_chars<'a>(&'a self) -> impl Iterator<Item = char> + 'a {
        self.0.iter().flat_map(|&b| {
            #[inline(always)]
            const fn h(v: u8) -> char {
                match v {
                    0..=9 => (v + b'0') as _,
                    _ => (v - 10 + b'a') as _,
                }
            }

            [h(b >> 4), h(b & 0x0f)]
        })
    }

    pub fn deserialize_chars(chars: impl Iterator<Item = char>) -> Option<Self> {
        let mut bytes = [0u8; 16];
        let mut wptr = 0;
        let mut ub = None;
        for c in chars {
            let v = match c {
                '0'..='9' => c as u8 - b'0',
                'a'..='f' => c as u8 - b'a' + 10,
                _ => return None,
            };

            ub = match ub {
                None => Some(v),
                Some(ub1) => {
                    bytes[wptr] = (ub1 << 4) | v;

                    wptr += 1;
                    None
                }
            };
        }

        Some(Self(bytes))
    }
}

#[inline(always)]
fn fmt_hex2(f: &mut (impl core::fmt::Write + ?Sized), v: u8) -> core::fmt::Result {
    #[inline(always)]
    fn h(v: u8) -> char {
        match v {
            0..=9 => (v + b'0') as char,
            10..=15 => (v - 10 + b'a') as char,
            _ => unreachable!(),
        }
    }

    f.write_char(h(v >> 4))?;
    f.write_char(h(v & 0x0f))?;

    Ok(())
}
