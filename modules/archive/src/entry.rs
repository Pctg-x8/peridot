use std::io::{IoSlice, IoSliceMut, Read, Result as IOResult, Write};

#[derive(Debug)]
pub struct AssetEntryHeadingPair {
    pub byte_length: u64,
    pub relative_offset: u64,
}
impl AssetEntryHeadingPair {
    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        crate::utils::write_all_vectored(
            writer,
            &mut [
                IoSlice::new(&self.byte_length.to_le_bytes()),
                IoSlice::new(&self.relative_offset.to_le_bytes()),
            ],
        )
        .map(|_| 16)
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + Unpin + ?Sized),
    ) -> IOResult<usize> {
        crate::utils::write_all_vectored_async(
            writer,
            &mut [
                IoSlice::new(&self.byte_length.to_le_bytes()),
                IoSlice::new(&self.relative_offset.to_le_bytes()),
            ],
        )
        .await
        .map(|_| 16)
    }

    pub fn read(reader: &mut (impl Read + ?Sized)) -> IOResult<Self> {
        let (mut byte_length_sink, mut relative_offset_sink) = ([0u8; 8], [0u8; 8]);
        crate::utils::read_all_vectored(
            reader,
            &mut [
                IoSliceMut::new(&mut byte_length_sink),
                IoSliceMut::new(&mut relative_offset_sink),
            ],
        )?;

        Ok(Self {
            byte_length: u64::from_le_bytes(byte_length_sink),
            relative_offset: u64::from_le_bytes(relative_offset_sink),
        })
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn read_async(
        reader: &mut (impl async_std::io::Read + Unpin + ?Sized),
    ) -> IOResult<Self> {
        let (mut byte_length_sink, mut relative_offset_sink) = ([0u8; 8], [0u8; 8]);
        crate::utils::read_all_vectored_async(
            reader,
            &mut [
                IoSliceMut::new(&mut byte_length_sink),
                IoSliceMut::new(&mut relative_offset_sink),
            ],
        )
        .await?;

        Ok(Self {
            byte_length: u64::from_le_bytes(byte_length_sink),
            relative_offset: u64::from_le_bytes(relative_offset_sink),
        })
    }
}
