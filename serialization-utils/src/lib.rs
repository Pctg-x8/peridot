use std::io::prelude::{BufRead, Write};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::str::from_utf8;

/// octet variadic unsigned integer
pub struct VariableUInt(pub u32);
impl VariableUInt {
    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<usize> {
        let mut iteration_count = 0;
        Self::iter_fragment(self.0, |v| { iteration_count += 1; writer.write_all(&[v]) }).map(|_| iteration_count)
    }
    /// u32 to break apart into bytes, and calls closure with fragment
    fn iter_fragment<CB: FnMut(u8) -> IOResult<()>>(v: u32, mut callback: CB) -> IOResult<()> {
        let (n7, nr) = ((v & 0x7f) as u8, v >> 7);
        callback(n7 | if nr != 0 { 0x80 } else { 0 })?;
        if nr != 0 { Self::iter_fragment(nr, callback) } else { Ok(()) }
    }
    pub fn read<R: BufRead>(reader: &mut R) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u32, 0usize);
        loop {
            let (consumed, done) = {
                let mut available = match reader.fill_buf() {
                    Ok(v) => v,
                    Err(e) => if e.kind() == ErrorKind::Interrupted { continue; } else { return Err(e); }
                };
                let (mut consumed, mut done) = (0, false);
                while !available.is_empty() {
                    v |= ((available[0] & 0x7f) as u32) << shifts;
                    shifts += 7;
                    consumed += 1;
                    if (available[0] & 0x80) == 0 { done = true; break; }
                    available = &available[1..];
                }
                (consumed, done)
            };
            reader.consume(consumed);
            if done { return Ok(VariableUInt(v)); }
        }
    }
}

/// a utf-8 string representation leading its byte length as `VariableUInt`.
pub struct PascalString(pub String);
pub struct PascalStr<'s>(pub &'s str);
impl PascalString {
    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<usize> {
        PascalStr(&self.0).write(writer)
    }
    pub fn read<R: BufRead>(reader: &mut R) -> IOResult<Self> {
        let VariableUInt(bytelength) = VariableUInt::read(reader)?;
        let mut bytes = Vec::with_capacity(bytelength as _); unsafe { bytes.set_len(bytelength as _); }
        reader.read_exact(&mut bytes).map(drop)?;
        return from_utf8(&bytes[..]).map(|s| PascalString(s.to_owned())).map_err(|e| IOError::new(ErrorKind::Other, e));
    }
}
impl<'s> PascalStr<'s> {
    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<usize> {
        VariableUInt(self.0.as_bytes().len() as _).write(writer)
            .and_then(|wl| writer.write_all(self.0.as_bytes()).map(move |_| wl + self.0.as_bytes().len()))
    }
}
