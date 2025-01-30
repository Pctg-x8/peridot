use crc::crc32;
use libflate::deflate as zlib;
use peridot_serialization_utils::{PascalStr, VariableUInt};
use std::{
    collections::HashMap,
    io::{IoSlice, Result as IOResult, Write},
};

use crate::{entry::AssetEntryHeadingPair, CompressionMethod, ContentFlags};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryTreePointer(u64);
impl EntryTreePointer {
    const EXACT_TREE_BIT: u64 = 0x8000_0000_0000_0000;

    pub const fn from_u64(x: u64) -> Self {
        Self(x)
    }

    pub const fn to_le_bytes(self) -> [u8; 8] {
        self.0.to_le_bytes()
    }

    pub const fn exact_tree(self) -> Self {
        Self(self.0 | Self::EXACT_TREE_BIT)
    }
}

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

    fn gen_asset_entry_blocks(
        &self,
        header_size: usize,
    ) -> IOResult<(Vec<u8>, Vec<u8>, ContentFlags)> {
        // ここもしかしたらもうちょい最適化できるかも？（毎回binary_searchするとでかいテーブルになったときにメモリのキャッシュ効率が悪そう）
        // 一旦これはオフラインで動くコードなので（Readよりは頻度低い）あとで考える
        let mut sorted_hash_table: Vec<(u64, Vec<(&str, &AssetEntryHeadingPair)>)> =
            Vec::with_capacity(self.entries.len());
        for (name, heading) in self.entries.iter() {
            let name_hash = xxhash_rust::xxh3::xxh3_64(name.as_bytes());
            match sorted_hash_table.binary_search_by_key(&name_hash, |&(nh, _)| nh) {
                Ok(x) => sorted_hash_table[x].1.push((name, heading)),
                Err(x) => sorted_hash_table.insert(x, (name_hash, vec![(name, heading)])),
            }
        }

        const TARGET_PAGE_BLOCK_SIZE: usize = 8192;

        let mut content_flags = ContentFlags::EMPTY;
        let mut exact_match_block = Vec::new();
        let mut hash_tree_block = Vec::with_capacity(TARGET_PAGE_BLOCK_SIZE - header_size);
        let exact_entry_count = TARGET_PAGE_BLOCK_SIZE / (8 * 2);
        if sorted_hash_table.len() < exact_entry_count {
            // このページで十分入ってしまう
            content_flags |= ContentFlags::ROOT_HASH_TREE_EXACT;

            for (k, es) in sorted_hash_table {
                let exact_match_pointer = exact_match_block.len() as u64;
                for (n, h) in es {
                    PascalStr(n).write(&mut exact_match_block)?;
                    h.write(&mut exact_match_block)?;
                }

                hash_tree_block.extend(k.to_le_bytes());
                hash_tree_block.extend(exact_match_pointer.to_le_bytes());
            }
        } else {
            // サブツリー構成が必要
            fn gen_subtree(
                hash_table: &[(u64, Vec<(&str, &AssetEntryHeadingPair)>)],
                hash_block: &mut Vec<u8>,
                exact_match_block: &mut Vec<u8>,
            ) -> IOResult<EntryTreePointer> {
                let exact_entry_count = TARGET_PAGE_BLOCK_SIZE / (8 * 2);
                let this_tree_ptr;
                if hash_table.len() < exact_entry_count {
                    // このページで十分に入る
                    this_tree_ptr = EntryTreePointer::from_u64(hash_block.len() as _).exact_tree();
                    let mut hash_block_index = hash_block.len();
                    hash_block.resize(hash_table.len() * 8 * 2, 0);
                    for (k, es) in hash_table {
                        let exact_match_pointer = exact_match_block.len() as u64;
                        for (n, h) in es {
                            PascalStr(n).write(exact_match_block)?;
                            h.write(exact_match_block)?;
                        }

                        hash_block[hash_block_index..hash_block_index + 8]
                            .copy_from_slice(&k.to_le_bytes());
                        hash_block[hash_block_index + 8..hash_block_index + 16]
                            .copy_from_slice(&exact_match_pointer.to_le_bytes());
                        hash_block_index += 16;
                    }
                } else {
                    // まだサブツリーが必要
                    this_tree_ptr = EntryTreePointer::from_u64(hash_block.len() as _);

                    let entry_count = (TARGET_PAGE_BLOCK_SIZE - 8) / (8 * 3);
                    let mut hash_block_index = hash_block.len();
                    hash_block.resize(entry_count * (8 * 3) + 8, 0);
                    let mut subtree_base = 0;
                    for n in 0..entry_count {
                        let nx = hash_table.len() * (n + 1) / (entry_count + 2);
                        let less_ptr = gen_subtree(
                            &hash_table[subtree_base..nx],
                            hash_block,
                            exact_match_block,
                        )?;

                        let exact_match_pointer = exact_match_block.len() as u64;
                        for (n, h) in hash_table[nx].1.iter() {
                            PascalStr(n).write(exact_match_block)?;
                            h.write(exact_match_block)?;
                        }

                        hash_block[hash_block_index..hash_block_index + 8]
                            .copy_from_slice(&hash_table[nx].0.to_le_bytes());
                        hash_block[hash_block_index + 8..hash_block_index + 16]
                            .copy_from_slice(&exact_match_pointer.to_le_bytes());
                        hash_block[hash_block_index + 16..hash_block_index + 24]
                            .copy_from_slice(&less_ptr.to_le_bytes());
                        hash_block_index += 24;

                        subtree_base = nx + 1;
                    }

                    let greater_ptr =
                        gen_subtree(&hash_table[subtree_base..], hash_block, exact_match_block)?;
                    hash_block[hash_block_index..hash_block_index + 8]
                        .copy_from_slice(&greater_ptr.to_le_bytes());
                }

                Ok(this_tree_ptr)
            }

            let entry_count = (TARGET_PAGE_BLOCK_SIZE - header_size - 8) / (8 * 3);
            let mut hash_block_index = 0;
            hash_tree_block.resize(entry_count * (8 * 3) + 8, 0);
            let mut subtree_base = 0;
            for n in 0..entry_count {
                let nx = sorted_hash_table.len() * (n + 1) / (entry_count + 2);
                let less_ptr = gen_subtree(
                    &sorted_hash_table[subtree_base..nx],
                    &mut hash_tree_block,
                    &mut exact_match_block,
                )?;

                let exact_match_pointer = exact_match_block.len() as u64;
                for (n, h) in sorted_hash_table[nx].1.iter() {
                    PascalStr(n).write(&mut exact_match_block)?;
                    h.write(&mut exact_match_block)?;
                }

                hash_tree_block[hash_block_index..hash_block_index + 8]
                    .copy_from_slice(&sorted_hash_table[nx].0.to_le_bytes());
                hash_tree_block[hash_block_index + 8..hash_block_index + 16]
                    .copy_from_slice(&exact_match_pointer.to_le_bytes());
                hash_tree_block[hash_block_index + 16..hash_block_index + 24]
                    .copy_from_slice(&less_ptr.to_le_bytes());
                hash_block_index += 24;

                subtree_base = nx + 1;
            }

            let greater_ptr = gen_subtree(
                &sorted_hash_table[subtree_base..],
                &mut hash_tree_block,
                &mut exact_match_block,
            )?;
            hash_tree_block[hash_block_index..hash_block_index + 8]
                .copy_from_slice(&greater_ptr.to_le_bytes());
        }

        Ok((hash_tree_block, exact_match_block, content_flags))
    }

    fn write_compression_target_contents(
        &self,
        writer: &mut (impl Write + ?Sized),
        file_header_size: usize,
    ) -> IOResult<()> {
        let (hash_tree_block, exact_match_block, content_flags) =
            self.gen_asset_entry_blocks(file_header_size)?;

        writer.write_all(&[content_flags.bits()])?;
        VariableUInt(hash_tree_block.len() as _).write(writer)?;
        VariableUInt(exact_match_block.len() as _).write(writer)?;
        writer.write_all(&hash_tree_block)?;
        writer.write_all(&exact_match_block)?;
        writer.write_all(&self.data_bytes)?;

        Ok(())
    }

    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<()> {
        match self.compression_method {
            CompressionMethod::None => {
                let mut body = Vec::new();
                self.write_compression_target_contents(&mut body, 4 + 4)?;

                Self::write_common(writer, b"par ", None, &body)
            }
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Vec::new());
                self.write_compression_target_contents(&mut body, 4 + 8 + 4)?;
                let uncompressed_bytes = body.as_inner_ref().len() as u64;

                Self::write_common(
                    writer,
                    b"pard",
                    Some(uncompressed_bytes),
                    &body.finish().into_result()?,
                )
            }
            CompressionMethod::Lz4(_) => {
                let mut body = Vec::new();
                self.write_compression_target_contents(&mut body, 4 + 8 + 4)?;
                let uncompressed_bytes = body.len() as u64;
                let body = lz4_compression::prelude::compress(&body);

                Self::write_common(writer, b"parz", Some(uncompressed_bytes), &body[..])
            }
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Vec::new(), 11)?;
                self.write_compression_target_contents(&mut body, 4 + 8 + 4)?;
                let uncompressed_bytes = body.get_ref().len() as u64;

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
                self.write_compression_target_contents(&mut body, 4 + 4)?;

                Self::write_common_async(writer, b"par ", None, &body).await
            }
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Vec::new());
                self.write_compression_target_contents(&mut body, 4 + 8 + 4)?;
                let uncompressed_bytes = body.as_inner_ref().len() as u64;

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
                self.write_compression_target_contents(&mut body, 4 + 8 + 4)?;
                let uncompressed_bytes = body.len() as u64;
                let body = lz4_compression::prelude::compress(&body);

                Self::write_common_async(writer, b"parz", Some(uncompressed_bytes), &body[..]).await
            }
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Vec::new(), 11)?;
                self.write_compression_target_contents(&mut body, 4 + 8 + 4)?;
                let uncompressed_bytes = body.get_ref().len() as u64;

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
        let checksum_buf = checksum.to_le_bytes();
        let uncompressed_bytes_buf = uncompressed_bytes.map(u64::to_le_bytes);

        let mut vectors = Vec::with_capacity(4);
        vectors.push(IoSlice::new(signature));
        vectors.extend(
            uncompressed_bytes_buf
                .as_ref()
                .map(|x| IoSlice::new(&x[..])),
        );
        vectors.extend([IoSlice::new(&checksum_buf), IoSlice::new(body)]);

        crate::utils::write_all_vectored(writer, &mut vectors)?;
        Ok(())
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
