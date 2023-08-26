//! RIFF Loader

use std::{
    collections::BTreeMap,
    io::{Read, Seek, SeekFrom},
};

use crate::{audio::Int24, InputStream};

use super::WaveSamplesInFile;

macro_rules! ReadWaveData {
    ($this: expr, [$e: expr; $c: expr]) => {{
        let len = $this.jump_chunk(fourcc(b"data"))?;
        let mut bytes = vec![[$e; $c]; len as usize / std::mem::size_of_val(&$e) / $c];
        let buf = unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as *mut u8, len as _) };
        $this.file.read_exact(buf).map(move |_| bytes)
    }};
    ($this: expr, $e: expr) => {{
        let len = $this.jump_chunk(fourcc(b"data"))?;
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

const fn fourcc(cc: &[u8; 4]) -> u32 {
    u32::from_le_bytes(*cc)
}

pub struct RIFFLoader<F: Read + Seek> {
    file: F,
    riff_chunk_start: u64,
    riff_subchunk_offsets: BTreeMap<u32, (u64, u32)>,
}
pub struct RIFFStreamingLoader<F: InputStream> {
    pub file: F,
}

#[repr(C)]
pub struct RIFFChunkHeader {
    fourcc: u32,
    length: u32,
}
impl RIFFChunkHeader {
    fn padded_length(&self) -> u32 {
        (self.length + 1) & !1
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

impl<F: Read + Seek> RIFFLoader<F> {
    fn read_next_chunk_header(f: &mut F) -> std::io::Result<RIFFChunkHeader> {
        let mut hdr = std::mem::MaybeUninit::uninit();

        f.read_exact(unsafe { std::mem::transmute::<_, &mut [u8; 4 * 2]>(&mut *hdr.as_mut_ptr()) })
            .map(move |_| unsafe { hdr.assume_init() })
    }
    fn seek_fourcc_in_file(f: &mut F, fcc: u32) -> std::io::Result<u64> {
        loop {
            let next_hdr = Self::read_next_chunk_header(f)?;
            if next_hdr.fourcc == fcc {
                return f.seek(SeekFrom::Current(-4));
            }
            f.seek(SeekFrom::Current(next_hdr.padded_length() as _))?;
        }
    }

    pub fn new(mut file: F) -> std::io::Result<Self> {
        Self::seek_fourcc_in_file(&mut file, fourcc(b"RIFF"))?;
        let riff_chunk_start = file.seek(SeekFrom::Current(8))?;

        Ok(RIFFLoader {
            file,
            riff_chunk_start,
            riff_subchunk_offsets: BTreeMap::new(),
        })
    }
    fn jump_chunk(&mut self, fcc: u32) -> std::io::Result<u32> {
        if let Some((c, l)) = self.riff_subchunk_offsets.get(&fcc).cloned() {
            return self.file.seek(SeekFrom::Start(c)).map(|_| l);
        }

        self.file.seek(SeekFrom::Start(self.riff_chunk_start))?;
        loop {
            let next_hdr = Self::read_next_chunk_header(&mut self.file)?;
            let chunk_start = self.file.seek(SeekFrom::Current(0))?;
            if next_hdr.fourcc == fcc {
                return Ok(next_hdr.length);
            }
            self.riff_subchunk_offsets
                .insert(next_hdr.fourcc, (chunk_start, next_hdr.length));
            self.file
                .seek(SeekFrom::Current(next_hdr.padded_length() as _))?;
        }
    }
    pub fn read_fmt(&mut self) -> std::io::Result<RIFFWaveFormatData> {
        let len = self.jump_chunk(fourcc(b"fmt "))?;
        assert!(
            len >= std::mem::size_of::<RIFFWaveFormatData>() as u32,
            "Invalid WAVE fmt chunk"
        );
        let mut fmt = std::mem::MaybeUninit::uninit();
        self.file.read_exact(unsafe {
            std::mem::transmute::<_, &mut [u8; 16]>(&mut *fmt.as_mut_ptr())
        })?;

        Ok(unsafe { fmt.assume_init() })
    }
    fn read_data(&mut self) -> std::io::Result<Vec<u8>> {
        let len = self.jump_chunk(fourcc(b"data"))?;
        let mut bytes = vec![0u8; len as usize];
        self.file.read_exact(&mut bytes).map(move |_| bytes)
    }
    pub fn read_data_uncompressed(
        &mut self,
        fmt: &RIFFWaveFormatData,
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
            (1, 8, 0x01) => self.read_data().map(WaveSamplesInFile::Mono8),
            (1, 16, 0x01) => ReadWaveData!(self, 0i16).map(WaveSamplesInFile::Mono16),
            (1, 32, 0x01) => ReadWaveData!(self, 0i32).map(WaveSamplesInFile::Mono32),
            (1, 64, 0x01) => ReadWaveData!(self, 0i64).map(WaveSamplesInFile::Mono64),
            (1, 32, 0x03) => ReadWaveData!(self, 0f32).map(WaveSamplesInFile::MonoF32),
            (1, 64, 0x03) => ReadWaveData!(self, 0f64).map(WaveSamplesInFile::MonoF64),
            (2, 8, 0x01) => ReadWaveData!(self, [0u8; 2]).map(WaveSamplesInFile::Stereo8),
            (2, 16, 0x01) => ReadWaveData!(self, [0i16; 2]).map(WaveSamplesInFile::Stereo16),
            (2, 32, 0x01) => ReadWaveData!(self, [0i32; 2]).map(WaveSamplesInFile::Stereo32),
            (2, 64, 0x01) => ReadWaveData!(self, [0i64; 2]).map(WaveSamplesInFile::Stereo64),
            (2, 32, 0x03) => ReadWaveData!(self, [0f32; 2]).map(WaveSamplesInFile::StereoF32),
            (2, 64, 0x03) => ReadWaveData!(self, [0f64; 2]).map(WaveSamplesInFile::StereoF64),
            (1, 24, 0x01) => {
                let b = self.read_data()?;
                Ok(WaveSamplesInFile::Mono24(
                    b.chunks(3).map(slice_i24_value).collect(),
                ))
            }
            (2, 24, 0x01) => {
                let b = self.read_data()?;
                Ok(WaveSamplesInFile::Stereo24(
                    b.chunks(3 * 2)
                        .map(|bs| [slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])])
                        .collect(),
                ))
            }
            (ch, bits, 0x01) => self.read_data().map(|bytes| WaveSamplesInFile::Unknown {
                bytes,
                channels: ch as _,
                bits: bits as _,
            }),
            (ch, bits, 0x03) => self.read_data().map(|bytes| WaveSamplesInFile::UnknownF {
                bytes,
                channels: ch as _,
                bits: bits as _,
            }),
            (ch, b, f) => unimplemented!("unhandleable triple: ch={ch} bits={b} fmt={f}"),
        }
    }
}

impl<F: InputStream> RIFFStreamingLoader<F> {
    fn read_next_chunk_header(&mut self) -> std::io::Result<RIFFChunkHeader> {
        let mut hdr = std::mem::MaybeUninit::uninit();

        self.file
            .read_exact(unsafe {
                std::mem::transmute::<_, &mut [u8; 4 * 2]>(&mut *hdr.as_mut_ptr())
            })
            .map(move |_| unsafe { hdr.assume_init() })
    }
    fn seek_chunk(&mut self, fcc: u32) -> std::io::Result<u32> {
        loop {
            let next_hdr = self.read_next_chunk_header()?;
            if next_hdr.fourcc == fcc {
                return Ok(next_hdr.padded_length());
            }
            self.file.skip(next_hdr.padded_length() as _)?;
        }
    }

    pub fn read_fmt(&mut self) -> std::io::Result<RIFFWaveFormatData> {
        let len = self.seek_chunk(fourcc(b"fmt "))?;
        assert!(
            len >= std::mem::size_of::<RIFFWaveFormatData>() as u32,
            "Invalid WAVE fmt chunk"
        );
        let mut fmt = std::mem::MaybeUninit::uninit();
        self.file.read_exact(unsafe {
            std::mem::transmute::<_, &mut [u8; 16]>(&mut *fmt.as_mut_ptr())
        })?;
        self.file.skip((len - 16) as _)?;

        Ok(unsafe { fmt.assume_init() })
    }
    pub fn seek_data(&mut self) -> std::io::Result<u32> {
        self.seek_chunk(fourcc(b"data"))
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
