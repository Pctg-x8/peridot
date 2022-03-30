use async_std::io::prelude::{BufRead, ReadExt, Write, WriteExt};
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::str::from_utf8;

/// u32 to break apart into bytes
pub struct UIntFragmentIterator(Option<u32>);
impl From<u32> for UIntFragmentIterator {
    fn from(v: u32) -> Self {
        UIntFragmentIterator(Some(v))
    }
}
impl Iterator for UIntFragmentIterator {
    type Item = u8;
    fn next(&mut self) -> Option<u8> {
        self.0.map(|v| {
            let (n7, nr) = ((v & 0x7f) as u8, v >> 7);
            let rv = n7 | if nr != 0 { 0x80 } else { 0 };
            self.0 = if nr != 0 { Some(nr) } else { None };
            rv
        })
    }
}

/// octet variadic unsigned integer
pub struct VariableUInt(pub u32);
impl VariableUInt {
    pub async fn write<W: Write + Unpin>(&self, writer: &mut W) -> IOResult<usize> {
        let mut iteration_count = 0;
        for v in UIntFragmentIterator::from(self.0) {
            iteration_count += 1;
            writer.write_all(&[v]).await?;
        }

        Ok(iteration_count)
    }
    pub async fn read<R: BufRead + Unpin>(reader: &mut R) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u32, 0usize);
        loop {
            let mut b1 = vec![0u8];
            match reader.read(&mut b1).await {
                Ok(_) => (),
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => {
                    return Err(e);
                }
            };

            v |= ((b1[0] & 0x7f) as u32) << shifts;
            shifts += 7;
            if (b1[0] & 0x80) == 0 {
                break;
            }
        }

        Ok(VariableUInt(v))
    }
}

/// a utf-8 string representation leading its byte length as `VariableUInt`.
pub struct PascalString(pub String);
pub struct PascalStr<'s>(pub &'s str);
impl PascalString {
    pub async fn write<W: Write + Unpin>(&self, writer: &mut W) -> IOResult<usize> {
        PascalStr(&self.0).write(writer).await
    }

    pub async fn read<R: BufRead + Unpin>(reader: &mut R) -> IOResult<Self> {
        let VariableUInt(bytelength) = VariableUInt::read(reader).await?;
        let mut bytes = Vec::with_capacity(bytelength as _);
        unsafe {
            bytes.set_len(bytelength as _);
        }
        reader.read_exact(&mut bytes).await.map(drop)?;

        from_utf8(&bytes[..])
            .map(|s| PascalString(s.to_owned()))
            .map_err(|e| IOError::new(ErrorKind::Other, e))
    }
}
impl<'s> PascalStr<'s> {
    pub async fn write<W: Write + Unpin>(&self, writer: &mut W) -> IOResult<usize> {
        let wlen = VariableUInt(self.0.as_bytes().len() as _)
            .write(writer)
            .await?;
        writer.write_all(self.0.as_bytes()).await?;

        Ok(wlen + self.0.as_bytes().len())
    }
}
