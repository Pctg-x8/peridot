use crc::crc32;
use libflate::deflate as zlib;
use peridot_serialization_utils::{PascalStr, VariableUInt};
use std::{
    collections::HashMap,
    io::{IoSlice, Result as IOResult, Write},
};

use crate::{entry::AssetEntryHeadingPair, CompressionMethod};

pub struct ArchiveWrite {
    compression_method: CompressionMethod,
    entries: HashMap<String, AssetEntryHeadingPair>,
    data_bytes: Vec<u8>,
}
impl ArchiveWrite {
    pub fn new(comp: CompressionMethod) -> Self {
        Self {
            compression_method: comp,
            entries: HashMap::new(),
            data_bytes: Vec::new(),
        }
    }

    /// エントリを追加する 成功したらtrue
    pub fn add(&mut self, name: String, content: Vec<u8>) -> bool {
        if self.entries.contains_key(&name) {
            // すでにある
            return false;
        }

        let relative_offset = self.data_bytes.len() as u64;
        self.data_bytes.extend(content);
        self.entries.insert(
            name,
            AssetEntryHeadingPair {
                relative_offset,
                byte_length: self.data_bytes.len() as u64 - relative_offset,
            },
        );

        true
    }

    /// return -> written bytes(raw)
    fn write_asset_entries(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        let mut written_bytes = VariableUInt(self.entries.len() as _).write(writer)?;
        for (n, h) in &self.entries {
            written_bytes += h
                .write(writer)
                .and_then(|w1| PascalStr(n).write(writer).map(move |w2| w1 + w2))?;
        }

        Ok(written_bytes)
    }

    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<()> {
        match self.compression_method {
            CompressionMethod::None => {
                let mut body = Vec::new();
                self.write_asset_entries(&mut body)?;
                body.write_all(&self.data_bytes)?;

                Self::write_common(writer, b"par ", None, &body)
            }
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Vec::new());
                let uncompressed_bytes = self.write_asset_entries(&mut body).and_then(|wa| {
                    body.write_all(&self.data_bytes)
                        .map(move |_| wa + self.data_bytes.len())
                })? as u64;

                Self::write_common(
                    writer,
                    b"pard",
                    Some(uncompressed_bytes),
                    &body.finish().into_result()?,
                )
            }
            CompressionMethod::Lz4(_) => {
                let mut body = Vec::new();
                let uncompressed_bytes = self.write_asset_entries(&mut body).and_then(|wa| {
                    body.write_all(&self.data_bytes)
                        .map(move |_| wa + self.data_bytes.len())
                })? as u64;
                let body = lz4_compression::prelude::compress(&body);

                Self::write_common(writer, b"parz", Some(uncompressed_bytes), &body[..])
            }
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Vec::new(), 11)?;
                let uncompressed_bytes = self.write_asset_entries(&mut body).and_then(|wa| {
                    body.write_all(&self.data_bytes)
                        .map(move |_| wa + self.data_bytes.len())
                })? as u64;

                Self::write_common(writer, b"par1", Some(uncompressed_bytes), &body.finish()?)
            }
        }
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + Unpin + ?Sized),
    ) -> IOResult<()> {
        match self.compression_method {
            CompressionMethod::None => {
                let mut body = Vec::new();
                self.write_asset_entries(&mut body)?;
                body.write_all(&self.data_bytes)?;

                Self::write_common_async(writer, b"par ", None, &body).await
            }
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Vec::new());
                let uncompressed_bytes = self.write_asset_entries(&mut body).and_then(|wa| {
                    body.write_all(&self.data_bytes)
                        .map(move |_| wa + self.data_bytes.len())
                })? as u64;

                Self::write_common_async(
                    writer,
                    b"pard",
                    Some(uncompressed_bytes),
                    &body.finish().into_result()?,
                )
                .await
            }
            CompressionMethod::Lz4(_) => {
                let mut body = Vec::new();
                let uncompressed_bytes = self.write_asset_entries(&mut body).and_then(|wa| {
                    body.write_all(&self.data_bytes)
                        .map(move |_| wa + self.data_bytes.len())
                })? as u64;
                let body = lz4_compression::prelude::compress(&body);

                Self::write_common_async(writer, b"parz", Some(uncompressed_bytes), &body[..]).await
            }
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Vec::new(), 11)?;
                let uncompressed_bytes = self.write_asset_entries(&mut body).and_then(|wa| {
                    body.write_all(&self.data_bytes)
                        .map(move |_| wa + self.data_bytes.len())
                })? as u64;

                Self::write_common_async(writer, b"par1", Some(uncompressed_bytes), &body.finish()?)
                    .await
            }
        }
    }

    fn write_common(
        writer: &mut (impl Write + ?Sized),
        signature: &[u8],
        uncompressed_bytes: Option<u64>,
        body: &[u8],
    ) -> IOResult<()> {
        let checksum = crc32::checksum_ieee(body);
        writer.write_all(signature)?;
        if let Some(ub) = uncompressed_bytes {
            writer.write_all(&ub.to_le_bytes())?;
        }
        writer.write_all(&checksum.to_le_bytes())?;
        writer.write_all(body)
    }

    #[cfg(feature = "async-rt-async-std")]
    async fn write_common_async(
        writer: &mut (impl async_std::io::Write + Unpin + ?Sized),
        signature: &[u8],
        uncompressed_byte_length: Option<u64>,
        body: &[u8],
    ) -> IOResult<()> {
        let checksum = crc32::checksum_ieee(body);
        let checksum_bytes = checksum.to_le_bytes();
        let uncompressed_byte_length_bytes = uncompressed_byte_length.map(u64::to_le_bytes);

        let mut write_buffers = Vec::with_capacity(4);
        write_buffers.push(IoSlice::new(signature));
        write_buffers.extend(
            uncompressed_byte_length_bytes
                .as_ref()
                .map(|bs| IoSlice::new(&bs[..])),
        );
        write_buffers.extend([IoSlice::new(&checksum_bytes), IoSlice::new(body)]);

        crate::utils::write_all_vectored_async(writer, &mut write_buffers).await?;
        Ok(())
    }
}
