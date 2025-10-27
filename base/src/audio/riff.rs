//! RIFF Loader

use std::collections::BTreeMap;

use crate::{audio::Int24, InputStream};

use super::WaveSamplesInFile;

macro_rules! ReadWaveData {
    ($this: expr, [$e: expr; $c: expr]) => {{
        let len = $this.jump_chunk(Fourcc::from_bytes(b"data"))?;
        let mut bytes = vec![[$e; $c]; len as usize / std::mem::size_of_val(&$e) / $c];
        let buf = unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as *mut u8, len as _) };
        $this.file.read_exact(buf).map(move |_| bytes)
    }};
    ($this: expr, $e: expr) => {{
        let len = $this.jump_chunk(Fourcc::from_bytes(b"data"))?;
        let mut bytes = vec![$e; len as usize / std::mem::size_of_val(&$e)];
        let buf = unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as *mut u8, len as _) };
        $this.file.read_exact(buf).map(move |_| bytes)
    }};
    ($this: expr, [$e: expr; $c: expr]; $smp: expr) => {{
        let mut bytes = vec![[$e; $c]; $smp as usize];
        let buf = unsafe
        {
            let len = $smp as usize * $c * std::mem::size_of_val(&$e);
            std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as *mut u8, len)
        };
        $this.file.read(buf).map(move |v| { bytes.truncate(v / ($c * std::mem::size_of_val(&$e))); bytes })
    }};
    ($this: expr, $e: expr; $smp: expr) => {{
        let mut bytes = vec![$e; $smp as usize];
        let buf = unsafe
        {
            let len = $smp as usize * std::mem::size_of_val(&$e);
            std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as *mut u8, len)
        };
        $this.file.read(buf).map(move |v| { bytes.truncate(v / std::mem::size_of_val(&$e)); bytes })
    }}
}

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
    riff_chunk_start: u64,
    riff_subchunk_offsets: BTreeMap<Fourcc, (u64, u32)>,
}
impl<F: peridot_native_io::RandomReadBlob> RIFFLoader<F> {
    pub fn new(reader: F) -> std::io::Result<Self> {
        let riff_chunk_start =
            seek_next_fourcc(&reader, 0, Fourcc::RIFF)? + RIFFChunkHeader::BYTE_LENGTH as u64;

        Ok(RIFFLoader {
            reader,
            riff_chunk_start,
            riff_subchunk_offsets: BTreeMap::new(),
        })
    }

    fn seek_chunk(&mut self, fcc: Fourcc) -> std::io::Result<(u64, u32)> {
        if let Some(&(c, l)) = self.riff_subchunk_offsets.get(&fcc) {
            return Ok((c + RIFFChunkHeader::BYTE_LENGTH as u64, l));
        }

        let mut pos = self.riff_chunk_start;
        loop {
            let next_hdr = RIFFChunkHeader::read(&self.reader, pos)?;
            let old_chunk = self
                .riff_subchunk_offsets
                .insert(next_hdr.fourcc, (pos, next_hdr.length));
            if let Some(c) = old_chunk {
                tracing::warn!(
                    fourcc = ?next_hdr.fourcc,
                    old_position = c.0,
                    new_position = pos,
                    "multiple chunk with same fourcc found, some information may lack"
                );
            }
            if next_hdr.fourcc == fcc {
                return Ok((pos + RIFFChunkHeader::BYTE_LENGTH as u64, next_hdr.length));
            }

            pos += next_hdr.total_byte_length() as u64;
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
        let mut bytes = vec![0u8; max_bytes];
        self.file.read(&mut bytes).map(move |v| {
            bytes.truncate(v);
            bytes
        })
    }

    pub fn read_data_uncompressed(
        &mut self,
        fmt: &RIFFWaveFormatData,
        max_samples: usize,
    ) -> std::io::Result<WaveSamplesInFile> {
        fn slice_i24_value(s: &[u8]) -> Int24 {
            assert!(s.len() == 3, "Unable to cast &[u8] as i24");

            let vu = s[0] as u32 | ((s[1] as u32) << 8) | ((s[2] as u32) << 16);
            // 符号拡張する
            let fillone = s[2] & 0x80 != 0;
            Int24(if fillone {
                (vu | 0xff_000000) as _
            } else {
                vu as _
            })
        }

        match (fmt.num_channels, fmt.bits_per_sample, fmt.encoding) {
            (1, 8, 0x01) => self.read_data(max_samples).map(WaveSamplesInFile::Mono8),
            (1, 16, 0x01) => ReadWaveData!(self, 0i16; max_samples).map(WaveSamplesInFile::Mono16),
            (1, 32, 0x01) => ReadWaveData!(self, 0i32; max_samples).map(WaveSamplesInFile::Mono32),
            (1, 64, 0x01) => ReadWaveData!(self, 0i64; max_samples).map(WaveSamplesInFile::Mono64),
            (1, 32, 0x03) => ReadWaveData!(self, 0f32; max_samples).map(WaveSamplesInFile::MonoF32),
            (1, 64, 0x03) => ReadWaveData!(self, 0f64; max_samples).map(WaveSamplesInFile::MonoF64),
            (2, 8, 0x01) => {
                ReadWaveData!(self, [0u8; 2]; max_samples).map(WaveSamplesInFile::Stereo8)
            }
            (2, 16, 0x01) => {
                ReadWaveData!(self, [0i16; 2]; max_samples).map(WaveSamplesInFile::Stereo16)
            }
            (2, 32, 0x01) => {
                ReadWaveData!(self, [0i32; 2]; max_samples).map(WaveSamplesInFile::Stereo32)
            }
            (2, 64, 0x01) => {
                ReadWaveData!(self, [0i64; 2]; max_samples).map(WaveSamplesInFile::Stereo64)
            }
            (2, 32, 0x03) => {
                ReadWaveData!(self, [0f32; 2]; max_samples).map(WaveSamplesInFile::StereoF32)
            }
            (2, 64, 0x03) => {
                ReadWaveData!(self, [0f64; 2]; max_samples).map(WaveSamplesInFile::StereoF64)
            }
            (1, 24, 0x01) => {
                let b = self.read_data(max_samples / 3)?;
                Ok(WaveSamplesInFile::Mono24(
                    b.chunks(3).map(slice_i24_value).collect(),
                ))
            }
            (2, 24, 0x01) => {
                let b = self.read_data(max_samples / (3 * 2))?;
                Ok(WaveSamplesInFile::Stereo24(
                    b.chunks(3 * 2)
                        .map(|bs| [slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])])
                        .collect(),
                ))
            }
            (ch, b, f) => unimplemented!("unhandleable triple: ch={ch} bits={b} fmt={f}"),
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
    riff_chunk_start: u64,
    subchunk_offsets: BTreeMap<Fourcc, (u64, u32)>,
}
impl<F: peridot_native_io::RandomReadBlobAsync> RIFFLoaderAsync<F> {
    pub async fn new(reader: F) -> std::io::Result<Self> {
        let riff_chunk_start = seek_next_fourcc_async(&reader, 0, Fourcc::RIFF).await?
            + RIFFChunkHeader::BYTE_LENGTH as u64;

        Ok(Self {
            reader,
            riff_chunk_start,
            subchunk_offsets: BTreeMap::new(),
        })
    }

    async fn seek_chunk(&mut self, target: Fourcc) -> std::io::Result<(u64, u32)> {
        if let Some(&(c, l)) = self.subchunk_offsets.get(&target) {
            // already found chunk
            return Ok((c + RIFFChunkHeader::BYTE_LENGTH as u64, l));
        }

        let mut seek_ptr = self.riff_chunk_start;
        loop {
            let next_hdr = RIFFChunkHeader::read_async(&self.reader, seek_ptr).await?;
            let old_chunk = self
                .subchunk_offsets
                .insert(next_hdr.fourcc, (seek_ptr, next_hdr.length));
            if let Some(c) = old_chunk {
                tracing::warn!(
                    fourcc = ?next_hdr.fourcc,
                    old_position = c.0,
                    new_position = seek_ptr,
                    "multiple chunk with same fourcc found, some information may lack"
                );
            }

            if next_hdr.fourcc == target {
                return Ok((
                    seek_ptr + RIFFChunkHeader::BYTE_LENGTH as u64,
                    next_hdr.length,
                ));
            }

            seek_ptr += next_hdr.total_byte_length() as u64;
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
