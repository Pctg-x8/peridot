//! RIFF Loader

use std::collections::BTreeMap;

use crate::{audio::Int24, InputStream};

use super::WaveSamplesInFile;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Fourcc(u32);
impl Fourcc {
    #[inline(always)]
    const fn from_bytes(cc: &[u8; 4]) -> Self {
        Self(u32::from_le_bytes(*cc))
    }

    const RIFF: Self = Self::from_bytes(b"RIFF");
    const FMT_: Self = Self::from_bytes(b"fmt ");
    const DATA: Self = Self::from_bytes(b"data");
}

#[repr(C)]
pub struct RIFFChunkHeader {
    fourcc: Fourcc,
    length: u32,
}
impl RIFFChunkHeader {
    const BYTE_LENGTH: usize = 4 * 2;

    fn read(
        reader: &(impl peridot_native_io::RandomReadBlob + ?Sized),
        pos: u64,
    ) -> std::io::Result<Self> {
        let mut hdr = std::mem::MaybeUninit::<Self>::uninit();
        reader.read_exact(pos, unsafe {
            &mut *hdr
                .as_mut_ptr()
                .cast::<[core::mem::MaybeUninit<u8>; Self::BYTE_LENGTH]>()
        })?;

        Ok(unsafe { hdr.assume_init() })
    }

    async fn read_async(
        reader: &(impl peridot_native_io::RandomReadBlobAsync + ?Sized),
        pos: u64,
    ) -> std::io::Result<Self> {
        let mut hdr = core::mem::MaybeUninit::<Self>::uninit();
        reader
            .read_exact_async(pos, unsafe {
                &mut *hdr
                    .as_mut_ptr()
                    .cast::<[core::mem::MaybeUninit<u8>; Self::BYTE_LENGTH]>()
            })
            .await?;

        Ok(unsafe { hdr.assume_init() })
    }

    #[inline(always)]
    const fn padded_length(&self) -> u32 {
        (self.length + 1) & !1
    }

    /// Chunk Header + Data( + Padding)
    #[inline(always)]
    const fn total_byte_length(&self) -> usize {
        Self::BYTE_LENGTH + self.padded_length() as usize
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct RIFFWaveFormatData {
    encoding: u16,
    num_channels: u8,
    sample_rate: u32,
    avg_bytes_per_sec: u32,
    block_size: u16,
    bits_per_sample: u16,
}

pub struct RIFFLoader<F: peridot_native_io::RandomReadBlob> {
    reader: F,
    riff_subchunk_offsets: BTreeMap<Fourcc, (u64, u32)>,
    subchunk_nondiscover_head: u64,
}
impl<F: peridot_native_io::RandomReadBlob> RIFFLoader<F> {
    pub fn new(reader: F) -> std::io::Result<Self> {
        let riff_chunk_start =
            seek_next_fourcc(&reader, 0, Fourcc::RIFF)? + RIFFChunkHeader::BYTE_LENGTH as u64;
        let mut file_type = core::mem::MaybeUninit::<u32>::uninit();
        reader.read_exact(riff_chunk_start, b32m_uninit(&mut file_type))?;
        assert_eq!(
            unsafe { file_type.assume_init().to_le_bytes() },
            *b"WAVE",
            "not a WAVE file"
        );

        Ok(RIFFLoader {
            reader,
            riff_subchunk_offsets: BTreeMap::new(),
            subchunk_nondiscover_head: riff_chunk_start + 4,
        })
    }

    fn seek_chunk(&mut self, fcc: Fourcc) -> std::io::Result<(u64, u32)> {
        if let Some(&(c, l)) = self.riff_subchunk_offsets.get(&fcc) {
            return Ok((c + RIFFChunkHeader::BYTE_LENGTH as u64, l));
        }

        loop {
            let next_hdr = RIFFChunkHeader::read(&self.reader, self.subchunk_nondiscover_head)?;
            let chunk_pos = self.subchunk_nondiscover_head;
            let old_chunk = self
                .riff_subchunk_offsets
                .insert(next_hdr.fourcc, (chunk_pos, next_hdr.length));
            if let Some(c) = old_chunk {
                tracing::warn!(
                    fourcc = ?next_hdr.fourcc,
                    old_position = c.0,
                    new_position = chunk_pos,
                    "multiple chunk with same fourcc found, some information may lack"
                );
            }
            self.subchunk_nondiscover_head += next_hdr.total_byte_length() as u64;

            if next_hdr.fourcc == fcc {
                return Ok((
                    chunk_pos + RIFFChunkHeader::BYTE_LENGTH as u64,
                    next_hdr.length,
                ));
            }
        }
    }

    pub fn read_fmt(&mut self) -> std::io::Result<RIFFWaveFormatData> {
        let (data_start_ptr, len) = self.seek_chunk(Fourcc::FMT_)?;
        assert!(
            len >= std::mem::size_of::<RIFFWaveFormatData>() as u32,
            "smaller wav fmt chunk"
        );
        let mut fmt = std::mem::MaybeUninit::<RIFFWaveFormatData>::uninit();
        self.reader.read_exact(data_start_ptr, unsafe {
            &mut *fmt.as_mut_ptr().cast::<[core::mem::MaybeUninit<u8>; 16]>()
        })?;

        Ok(unsafe { fmt.assume_init() })
    }

    fn read_data(&mut self) -> std::io::Result<Vec<u8>> {
        let (data_start_ptr, len) = self.seek_chunk(Fourcc::DATA)?;
        let mut bytes = Vec::<u8>::with_capacity(len as _);
        self.reader
            .read_exact(data_start_ptr, bytes.spare_capacity_mut())?;
        unsafe {
            bytes.set_len(bytes.capacity());
        }

        Ok(bytes)
    }

    fn read_data_typed<T>(&mut self) -> std::io::Result<Vec<T>> {
        let (data_start_ptr, len) = self.seek_chunk(Fourcc::DATA)?;
        let mut bytes = Vec::<T>::with_capacity(len as usize / core::mem::size_of::<T>());
        self.reader.read_exact(data_start_ptr, unsafe {
            core::slice::from_raw_parts_mut(
                bytes
                    .spare_capacity_mut()
                    .as_mut_ptr()
                    .cast::<core::mem::MaybeUninit<u8>>(),
                len as _,
            )
        })?;
        unsafe {
            bytes.set_len(bytes.capacity());
        }

        Ok(bytes)
    }

    pub fn read_data_uncompressed(
        &mut self,
        fmt: &RIFFWaveFormatData,
    ) -> std::io::Result<WaveSamplesInFile> {
        match (fmt.num_channels, fmt.bits_per_sample, fmt.encoding) {
            (1, 8, 0x01) => Ok(WaveSamplesInFile::Mono8(self.read_data()?)),
            (1, 16, 0x01) => Ok(WaveSamplesInFile::Mono16(self.read_data_typed()?)),
            (1, 32, 0x01) => Ok(WaveSamplesInFile::Mono32(self.read_data_typed()?)),
            (1, 64, 0x01) => Ok(WaveSamplesInFile::Mono64(self.read_data_typed()?)),
            (1, 32, 0x03) => Ok(WaveSamplesInFile::MonoF32(self.read_data_typed()?)),
            (1, 64, 0x03) => Ok(WaveSamplesInFile::MonoF64(self.read_data_typed()?)),
            (2, 8, 0x01) => Ok(WaveSamplesInFile::Stereo8(self.read_data_typed()?)),
            (2, 16, 0x01) => Ok(WaveSamplesInFile::Stereo16(self.read_data_typed()?)),
            (2, 32, 0x01) => Ok(WaveSamplesInFile::Stereo32(self.read_data_typed()?)),
            (2, 64, 0x01) => Ok(WaveSamplesInFile::Stereo64(self.read_data_typed()?)),
            (2, 32, 0x03) => Ok(WaveSamplesInFile::StereoF32(self.read_data_typed()?)),
            (2, 64, 0x03) => Ok(WaveSamplesInFile::StereoF64(self.read_data_typed()?)),
            (1, 24, 0x01) => Ok(WaveSamplesInFile::Mono24(
                self.read_data()?.chunks(3).map(slice_i24_value).collect(),
            )),
            (2, 24, 0x01) => Ok(WaveSamplesInFile::Stereo24(
                self.read_data()?
                    .chunks(3 * 2)
                    .map(|bs| [slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])])
                    .collect(),
            )),
            (ch, bits, 0x01) => Ok(WaveSamplesInFile::Unknown {
                bytes: self.read_data()?,
                channels: ch as _,
                bits: bits as _,
            }),
            (ch, bits, 0x03) => Ok(WaveSamplesInFile::UnknownF {
                bytes: self.read_data()?,
                channels: ch as _,
                bits: bits as _,
            }),
            (ch, b, f) => unimplemented!("unhandled triple: ch={ch} bits={b} fmt={f}"),
        }
    }
}

// TODO: Streamingといいつつチャンク探すのに前後しないといけないのでちょっと考え直したほうが良さそう（wavそのまま使わないとか）
pub struct RIFFStreamingLoader<F: InputStream> {
    pub file: F,
}
impl<F: InputStream> RIFFStreamingLoader<F> {
    fn read_next_chunk_header(&mut self) -> std::io::Result<RIFFChunkHeader> {
        let mut hdr = std::mem::MaybeUninit::uninit();

        self.file
            .read_exact(unsafe {
                std::mem::transmute::<&mut RIFFChunkHeader, &mut [u8; 4 * 2]>(
                    &mut *hdr.as_mut_ptr(),
                )
            })
            .map(move |_| unsafe { hdr.assume_init() })
    }

    fn seek_chunk(&mut self, fcc: Fourcc) -> std::io::Result<u32> {
        loop {
            let next_hdr = self.read_next_chunk_header()?;
            if next_hdr.fourcc == fcc {
                return Ok(next_hdr.padded_length());
            }
            self.file.skip(next_hdr.padded_length() as _)?;
        }
    }

    pub fn read_fmt(&mut self) -> std::io::Result<RIFFWaveFormatData> {
        let len = self.seek_chunk(Fourcc::FMT_)?;
        assert!(
            len >= std::mem::size_of::<RIFFWaveFormatData>() as u32,
            "Invalid WAVE fmt chunk"
        );
        let mut fmt = std::mem::MaybeUninit::uninit();
        self.file.read_exact(unsafe {
            std::mem::transmute::<&mut RIFFWaveFormatData, &mut [u8; 16]>(&mut *fmt.as_mut_ptr())
        })?;
        self.file.skip((len - 16) as _)?;

        Ok(unsafe { fmt.assume_init() })
    }

    pub fn seek_data(&mut self) -> std::io::Result<u32> {
        self.seek_chunk(Fourcc::DATA)
    }

    fn read_data(&mut self, max_bytes: usize) -> std::io::Result<Vec<u8>> {
        let mut bytes = Vec::with_capacity(max_bytes);
        let v = self.file.read(unsafe {
            core::mem::transmute::<&mut [core::mem::MaybeUninit<_>], &mut [_]>(
                bytes.spare_capacity_mut(),
            )
        })?;
        unsafe {
            bytes.set_len(v);
        }
        Ok(bytes)
    }

    fn read_data_typed<T>(&mut self, max_samples: usize) -> std::io::Result<Vec<T>> {
        let mut samples = Vec::with_capacity(max_samples);
        let v = self.file.read(unsafe {
            std::slice::from_raw_parts_mut(
                samples.spare_capacity_mut().as_mut_ptr().cast::<u8>(),
                max_samples * core::mem::size_of::<T>(),
            )
        })?;
        assert_eq!(
            v % core::mem::size_of::<T>(),
            0,
            "read buf length not aligned"
        );

        unsafe {
            samples.set_len(v / core::mem::size_of::<T>());
        }
        Ok(samples)
    }

    pub fn read_data_uncompressed(
        &mut self,
        fmt: &RIFFWaveFormatData,
        max_samples: usize,
    ) -> std::io::Result<WaveSamplesInFile> {
        match (fmt.num_channels, fmt.bits_per_sample, fmt.encoding) {
            (1, 8, 0x01) => Ok(WaveSamplesInFile::Mono8(self.read_data(max_samples)?)),
            (1, 16, 0x01) => Ok(WaveSamplesInFile::Mono16(
                self.read_data_typed(max_samples)?,
            )),
            (1, 32, 0x01) => Ok(WaveSamplesInFile::Mono32(
                self.read_data_typed(max_samples)?,
            )),
            (1, 64, 0x01) => Ok(WaveSamplesInFile::Mono64(
                self.read_data_typed(max_samples)?,
            )),
            (1, 32, 0x03) => Ok(WaveSamplesInFile::MonoF32(
                self.read_data_typed(max_samples)?,
            )),
            (1, 64, 0x03) => Ok(WaveSamplesInFile::MonoF64(
                self.read_data_typed(max_samples)?,
            )),
            (2, 8, 0x01) => Ok(WaveSamplesInFile::Stereo8(
                self.read_data_typed(max_samples)?,
            )),
            (2, 16, 0x01) => Ok(WaveSamplesInFile::Stereo16(
                self.read_data_typed(max_samples)?,
            )),
            (2, 32, 0x01) => Ok(WaveSamplesInFile::Stereo32(
                self.read_data_typed(max_samples)?,
            )),
            (2, 64, 0x01) => Ok(WaveSamplesInFile::Stereo64(
                self.read_data_typed(max_samples)?,
            )),
            (2, 32, 0x03) => Ok(WaveSamplesInFile::StereoF32(
                self.read_data_typed(max_samples)?,
            )),
            (2, 64, 0x03) => Ok(WaveSamplesInFile::StereoF64(
                self.read_data_typed(max_samples)?,
            )),
            (1, 24, 0x01) => Ok(WaveSamplesInFile::Mono24(
                self.read_data(max_samples / 3)?
                    .chunks(3)
                    .map(slice_i24_value)
                    .collect(),
            )),
            (2, 24, 0x01) => Ok(WaveSamplesInFile::Stereo24(
                self.read_data(max_samples / (3 * 2))?
                    .chunks(3 * 2)
                    .map(|bs| [slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])])
                    .collect(),
            )),
            (ch, b, f) => unimplemented!("unhandled triple: ch={ch} bits={b} fmt={f}"),
        }
    }
}

impl<F: InputStream> From<F> for RIFFStreamingLoader<F> {
    fn from(file: F) -> Self {
        RIFFStreamingLoader { file }
    }
}

pub struct RIFFLoaderAsync<F: peridot_native_io::RandomReadBlobAsync> {
    reader: F,
    subchunk_offsets: BTreeMap<Fourcc, (u64, u32)>,
    subchunk_nondiscover_head: u64,
}
impl<F: peridot_native_io::RandomReadBlobAsync> RIFFLoaderAsync<F> {
    pub async fn new(reader: F) -> std::io::Result<Self> {
        let riff_chunk_start = seek_next_fourcc_async(&reader, 0, Fourcc::RIFF).await?
            + RIFFChunkHeader::BYTE_LENGTH as u64;
        let mut file_type = core::mem::MaybeUninit::<u32>::uninit();
        reader
            .read_exact_async(riff_chunk_start, b32m_uninit(&mut file_type))
            .await?;
        assert_eq!(
            unsafe { file_type.assume_init().to_le_bytes() },
            *b"WAVE",
            "not a WAVE file"
        );

        Ok(Self {
            reader,
            subchunk_offsets: BTreeMap::new(),
            subchunk_nondiscover_head: riff_chunk_start + 4,
        })
    }

    async fn seek_chunk(&mut self, target: Fourcc) -> std::io::Result<(u64, u32)> {
        if let Some(&(c, l)) = self.subchunk_offsets.get(&target) {
            // already found chunk
            return Ok((c + RIFFChunkHeader::BYTE_LENGTH as u64, l));
        }

        loop {
            let next_hdr =
                RIFFChunkHeader::read_async(&self.reader, self.subchunk_nondiscover_head).await?;
            let chunk_pos = self.subchunk_nondiscover_head;
            let old_chunk = self
                .subchunk_offsets
                .insert(next_hdr.fourcc, (chunk_pos, next_hdr.length));
            if let Some(c) = old_chunk {
                tracing::warn!(
                    fourcc = ?next_hdr.fourcc,
                    old_position = c.0,
                    new_position = chunk_pos,
                    "multiple chunk with same fourcc found, some information may lack"
                );
            }
            self.subchunk_nondiscover_head += next_hdr.total_byte_length() as u64;

            if next_hdr.fourcc == target {
                return Ok((
                    chunk_pos + RIFFChunkHeader::BYTE_LENGTH as u64,
                    next_hdr.length,
                ));
            }
        }
    }

    pub async fn read_fmt(&mut self) -> std::io::Result<RIFFWaveFormatData> {
        let (data_start_ptr, len) = self.seek_chunk(Fourcc::FMT_).await?;
        assert!(
            len >= core::mem::size_of::<RIFFWaveFormatData>() as u32,
            "smaller fmt chunk"
        );
        let mut fmt = core::mem::MaybeUninit::<RIFFWaveFormatData>::uninit();
        self.reader.read_exact_async(data_start_ptr, unsafe { &mut *fmt.as_mut_ptr().cast::<[core::mem::MaybeUninit<u8>; core::mem::size_of::<RIFFWaveFormatData>()]>() }).await?;

        Ok(unsafe { fmt.assume_init() })
    }

    async fn read_data(&mut self) -> std::io::Result<Vec<u8>> {
        let (data_start_ptr, len) = self.seek_chunk(Fourcc::DATA).await?;
        let mut bytes = Vec::with_capacity(len as _);
        self.reader
            .read_exact_async(data_start_ptr, bytes.spare_capacity_mut())
            .await?;
        unsafe {
            bytes.set_len(len as _);
        }

        Ok(bytes)
    }

    async fn read_data_typed<T>(&mut self) -> std::io::Result<Vec<T>> {
        let (data_start_ptr, len) = self.seek_chunk(Fourcc::DATA).await?;
        let mut bytes = Vec::<T>::with_capacity(len as usize / core::mem::size_of::<T>());
        self.reader
            .read_exact_async(data_start_ptr, unsafe {
                core::slice::from_raw_parts_mut(
                    bytes.spare_capacity_mut().as_mut_ptr().cast(),
                    len as _,
                )
            })
            .await?;
        unsafe {
            bytes.set_len(bytes.capacity());
        }

        Ok(bytes)
    }

    pub async fn extract_data_uncompressed(
        &mut self,
        fmt: &RIFFWaveFormatData,
    ) -> std::io::Result<WaveSamplesInFile> {
        match (fmt.num_channels, fmt.bits_per_sample, fmt.encoding) {
            (1, 8, 0x01) => Ok(WaveSamplesInFile::Mono8(self.read_data().await?)),
            (1, 16, 0x01) => Ok(WaveSamplesInFile::Mono16(self.read_data_typed().await?)),
            (1, 32, 0x01) => Ok(WaveSamplesInFile::Mono32(self.read_data_typed().await?)),
            (1, 64, 0x01) => Ok(WaveSamplesInFile::Mono64(self.read_data_typed().await?)),
            (1, 32, 0x03) => Ok(WaveSamplesInFile::MonoF32(self.read_data_typed().await?)),
            (1, 64, 0x03) => Ok(WaveSamplesInFile::MonoF64(self.read_data_typed().await?)),
            (2, 8, 0x01) => Ok(WaveSamplesInFile::Stereo8(self.read_data_typed().await?)),
            (2, 16, 0x01) => Ok(WaveSamplesInFile::Stereo16(self.read_data_typed().await?)),
            (2, 32, 0x01) => Ok(WaveSamplesInFile::Stereo32(self.read_data_typed().await?)),
            (2, 64, 0x01) => Ok(WaveSamplesInFile::Stereo64(self.read_data_typed().await?)),
            (2, 32, 0x03) => Ok(WaveSamplesInFile::StereoF32(self.read_data_typed().await?)),
            (2, 64, 0x03) => Ok(WaveSamplesInFile::StereoF64(self.read_data_typed().await?)),
            (1, 24, 0x01) => Ok(WaveSamplesInFile::Mono24(
                self.read_data()
                    .await?
                    .chunks(3)
                    .map(slice_i24_value)
                    .collect(),
            )),
            (2, 24, 0x01) => Ok(WaveSamplesInFile::Stereo24(
                self.read_data()
                    .await?
                    .chunks(3 * 2)
                    .map(|bs| [slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])])
                    .collect(),
            )),
            (ch, bits, 0x01) => Ok(WaveSamplesInFile::Unknown {
                bytes: self.read_data().await?,
                channels: ch as _,
                bits: bits as _,
            }),
            (ch, bits, 0x03) => Ok(WaveSamplesInFile::UnknownF {
                bytes: self.read_data().await?,
                channels: ch as _,
                bits: bits as _,
            }),
            (ch, b, f) => unimplemented!("unhandled triple: ch={ch} bits={b} fmt={f}"),
        }
    }
}

fn seek_next_fourcc(
    f: &(impl peridot_native_io::RandomReadBlob + ?Sized),
    mut init_pos: u64,
    fcc: Fourcc,
) -> std::io::Result<u64> {
    loop {
        let next_hdr = RIFFChunkHeader::read(f, init_pos)?;
        if next_hdr.fourcc == fcc {
            return Ok(init_pos);
        }

        init_pos += next_hdr.total_byte_length() as u64;
    }
}

async fn seek_next_fourcc_async(
    reader: &(impl peridot_native_io::RandomReadBlobAsync + ?Sized),
    mut init_pos: u64,
    target: Fourcc,
) -> std::io::Result<u64> {
    loop {
        let next_hdr = RIFFChunkHeader::read_async(reader, init_pos).await?;
        if next_hdr.fourcc == target {
            return Ok(init_pos);
        }

        init_pos += next_hdr.total_byte_length() as u64;
    }
}

const fn b32m_uninit<'a>(
    x: &'a mut core::mem::MaybeUninit<u32>,
) -> &'a mut [core::mem::MaybeUninit<u8>; 4] {
    unsafe { core::mem::transmute(x) }
}

fn slice_i24_value(s: &[u8]) -> Int24 {
    assert!(s.len() == 3, "Unable to cast &[u8] as i24");

    let vu = s[0] as u32 | ((s[1] as u32) << 8) | ((s[2] as u32) << 16);
    // 符号拡張する
    let fill_one = s[2] & 0x80 != 0;
    Int24(if fill_one {
        (vu | 0xff_000000) as _
    } else {
        vu as _
    })
}
