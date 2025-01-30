use crc::crc32;
use libflate::deflate as zlib;
use peridot_serialization_utils::{PascalStr, VariableULong};
use std::{
    collections::HashMap,
    io::{IoSlice, Result as IOResult, Write},
};

use crate::{
    entry::AssetEntryHeadingPair, entry_tree::EntryTreePointer, CompressionMethod, ContentFlags,
};

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

    fn emit_exact_match_block(
        block_buffer: &mut Vec<u8>,
        entries: &[(&str, &AssetEntryHeadingPair)],
    ) -> IOResult<u64> {
        let ptr = block_buffer.len() as u64;
        VariableULong(entries.len() as _).write(block_buffer)?;
        for (n, h) in entries {
            PascalStr(n).write(block_buffer)?;
            h.write(block_buffer)?;
        }

        Ok(ptr)
    }

    fn write_exact_hash_tree(
        hash_tree_block: &mut Vec<u8>,
        exact_match_block: &mut Vec<u8>,
        sorted_hash_list: &[(u64, Vec<(&str, &AssetEntryHeadingPair)>)],
    ) -> IOResult<()> {
        let write_base_ptr = hash_tree_block.len();
        hash_tree_block.resize(
            hash_tree_block.len()
                + sorted_hash_list.len() * crate::entry_tree::EXACT_TREE_ENTRY_STRIDE,
            0,
        );

        for (n, &(k, ref xs)) in sorted_hash_list.iter().enumerate() {
            let exact_match_pointer = Self::emit_exact_match_block(exact_match_block, xs)?;

            let mut e = crate::entry_tree::ExactBlockMutableView::at(
                &mut hash_tree_block[write_base_ptr..],
                n,
            );
            e.set_name_hash(k);
            e.set_exact_block_offset(exact_match_pointer);
        }

        Ok(())
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

        let mut content_flags = ContentFlags::EMPTY;
        let mut exact_match_block = Vec::new();
        let mut hash_tree_block = Vec::new();

        let first_block_size = crate::entry_tree::first_hash_tree_block_size(header_size);
        if crate::entry_tree::exact_root_tree_block_size(sorted_hash_table.len())
            <= first_block_size
        {
            // このページで十分入ってしまう
            content_flags |= ContentFlags::ROOT_HASH_TREE_EXACT;

            Self::write_exact_hash_tree(
                &mut hash_tree_block,
                &mut exact_match_block,
                &sorted_hash_table,
            )?;
        } else {
            // サブツリー構成が必要
            fn gen_subtree(
                hash_table: &[(u64, Vec<(&str, &AssetEntryHeadingPair)>)],
                hash_block: &mut Vec<u8>,
                exact_match_block: &mut Vec<u8>,
            ) -> IOResult<EntryTreePointer> {
                if hash_table.len() < crate::entry_tree::NON_ROOT_EXACT_TREE_MAX_ELEMENT_COUNT {
                    // このページで十分に入る
                    let this_tree_ptr =
                        EntryTreePointer::from_u64(hash_block.len() as _).exact_tree();
                    hash_block.extend(u16::to_le_bytes(hash_table.len() as _));
                    ArchiveWrite::write_exact_hash_tree(hash_block, exact_match_block, hash_table)?;

                    return Ok(this_tree_ptr);
                }

                // まだサブツリーが必要
                let this_tree_ptr = EntryTreePointer::from_u64(hash_block.len() as _);

                let entry_count = crate::entry_tree::MAX_ENTRY_COUNT;
                let hash_block_base = hash_block.len();
                hash_block.resize(
                    hash_block.len() + crate::entry_tree::normal_tree_block_size(entry_count),
                    0,
                );
                let mut subtree_base = 0;
                for n in 0..entry_count {
                    let nx = hash_table.len() * (n + 1) / (entry_count + 2);
                    let less_ptr =
                        gen_subtree(&hash_table[subtree_base..nx], hash_block, exact_match_block)?;
                    subtree_base = nx + 1;

                    let mut e =
                        crate::entry_tree::EntryMutableView::at(hash_block, hash_block_base, n);
                    e.set_name_hash(hash_table[nx].0);
                    e.set_exact_block_offset(ArchiveWrite::emit_exact_match_block(
                        exact_match_block,
                        &hash_table[nx].1,
                    )?);
                    e.set_smaller_tree_pointer(less_ptr);
                }

                let greater_ptr =
                    gen_subtree(&hash_table[subtree_base..], hash_block, exact_match_block)?;
                crate::entry_tree::BlockMutableView::from_offset_and_element_count(
                    hash_block,
                    hash_block_base,
                    entry_count,
                )
                .set_larger_tree_pointer(greater_ptr);

                Ok(this_tree_ptr)
            }

            let entry_count = crate::entry_tree::normal_tree_entry_count(first_block_size);
            let hash_block_base = 0;
            hash_tree_block.resize(
                hash_tree_block.len() + crate::entry_tree::normal_tree_block_size(entry_count),
                0,
            );
            let mut subtree_base = 0;
            for n in 0..entry_count {
                let nx = sorted_hash_table.len() * (n + 1) / (entry_count + 2);
                let less_ptr = gen_subtree(
                    &sorted_hash_table[subtree_base..nx],
                    &mut hash_tree_block,
                    &mut exact_match_block,
                )?;
                subtree_base = nx + 1;

                let mut e = crate::entry_tree::EntryMutableView::at(
                    &mut hash_tree_block,
                    hash_block_base,
                    n,
                );
                e.set_name_hash(sorted_hash_table[nx].0);
                e.set_exact_block_offset(Self::emit_exact_match_block(
                    &mut exact_match_block,
                    &sorted_hash_table[nx].1,
                )?);
                e.set_smaller_tree_pointer(less_ptr);
            }

            let greater_ptr = gen_subtree(
                &sorted_hash_table[subtree_base..],
                &mut hash_tree_block,
                &mut exact_match_block,
            )?;
            crate::entry_tree::BlockMutableView::from_offset_and_element_count(
                &mut hash_tree_block,
                hash_block_base,
                entry_count,
            )
            .set_larger_tree_pointer(greater_ptr);
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

        crate::utils::write_all_vectored(
            writer,
            &mut [
                IoSlice::new(&[content_flags.bits()]),
                IoSlice::new(&u32::to_le_bytes((hash_tree_block.len() >> 3) as _)),
                IoSlice::new(&u64::to_le_bytes(exact_match_block.len() as _)),
                IoSlice::new(&hash_tree_block),
                IoSlice::new(&exact_match_block),
                IoSlice::new(&self.data_bytes),
            ],
        )?;

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
