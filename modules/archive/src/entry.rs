use std::{
    hash::{Hash, Hasher},
    io::{BufRead, IoSlice, Result as IOResult, Write},
};

use peridot_serialization_utils::VariableULong;

pub fn asset_name_hash(name: &str, ext: &str) -> u64 {
    let mut hasher = xxhash_rust::xxh3::Xxh3::new();
    name.hash(&mut hasher);
    ext.hash(&mut hasher);

    hasher.finish()
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssetName {
    pub name: String,
    pub ext: String,
}
impl AssetName {
    #[inline(always)]
    pub fn hash(&self) -> u64 {
        asset_name_hash(&self.name, &self.ext)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssetNameRef<'s> {
    pub name: &'s str,
    pub ext: &'s str,
}
impl AssetNameRef<'_> {
    #[inline(always)]
    pub fn hash(&self) -> u64 {
        asset_name_hash(self.name, self.ext)
    }
}

#[derive(Debug)]
pub struct AssetEntryHeadingPair {
    pub byte_length: u64,
    pub relative_offset: u64,
}
impl AssetEntryHeadingPair {
    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        let b1 = VariableULong(self.byte_length).to_bytes();
        let b2 = VariableULong(self.relative_offset).to_bytes();

        crate::utils::write_all_vectored(writer, &mut [IoSlice::new(&b1), IoSlice::new(&b2)])?;

        Ok(b1.len() + b2.len())
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + Unpin + ?Sized),
    ) -> IOResult<usize> {
        let b1 = VariableULong(self.byte_length).to_bytes();
        let b2 = VariableULong(self.relative_offset).to_bytes();

        crate::utils::write_all_vectored_async(writer, &mut [IoSlice::new(&b1), IoSlice::new(&b2)])
            .await?;

        Ok(b1.len() + b2.len())
    }

    pub fn read(reader: &mut (impl BufRead + ?Sized)) -> IOResult<Self> {
        let VariableULong(byte_length) = VariableULong::read(reader)?;
        let VariableULong(relative_offset) = VariableULong::read(reader)?;

        Ok(Self {
            byte_length,
            relative_offset,
        })
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn read_async(
        reader: &mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> IOResult<Self> {
        let VariableULong(byte_length) = VariableULong::read_async(reader).await?;
        let VariableULong(relative_offset) = VariableULong::read_async(reader).await?;

        Ok(Self {
            byte_length,
            relative_offset,
        })
    }

    pub fn from_bytes_head(bytes: &[u8]) -> (Self, usize) {
        let (VariableULong(byte_length), bl_bytes) = VariableULong::from_bytes_head(bytes);
        let (VariableULong(relative_offset), ro_bytes) =
            VariableULong::from_bytes_head(&bytes[bl_bytes..]);

        (
            Self {
                byte_length,
                relative_offset,
            },
            bl_bytes + ro_bytes,
        )
    }
}
