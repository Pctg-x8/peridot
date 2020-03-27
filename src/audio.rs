//! Peridot EngineCore: AudioSection

use rayon::{ThreadPool, ThreadPoolBuilder, prelude::*};
use std::sync::{Arc, RwLock};
use num_cpus;
use super::InputStream;

/// Processes an Audio, handles sample rate changing
pub trait Processor
{
    fn process(&mut self, flattened_buffer: &mut [f32]);
    fn set_sample_rate(&mut self, _samples_per_sec: f32) {}
}

/// Ensures that same memory regions are not shared as mutable
struct UniqueRawSliceMut<T> { ptr: *mut T, length: usize }
impl<T> UniqueRawSliceMut<T>
{
    unsafe fn as_slice_mut(&self) -> &mut [T]
    {
        std::slice::from_raw_parts_mut(self.ptr, self.length)
    }
    unsafe fn as_slice(&self) -> &[T]
    {
        std::slice::from_raw_parts(self.ptr, self.length)
    }
}
unsafe impl<T> Sync for UniqueRawSliceMut<T> {}
unsafe impl<T> Send for UniqueRawSliceMut<T> {}

struct BoxedInputStream(Box<dyn InputStream>);
impl BoxedInputStream
{
    fn new<S: InputStream + 'static>(stream: S) -> Self { BoxedInputStream(Box::new(stream)) }
}
impl InputStream for BoxedInputStream
{
    fn skip(&mut self, fwd: u64) -> IOResult<u64> { self.0.skip(fwd) }
}
impl Read for BoxedInputStream
{
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> { self.0.read(buf) }
}
unsafe impl Sync for BoxedInputStream {}
unsafe impl Send for BoxedInputStream {}

pub struct Mixer
{
    processes: Vec<Arc<RwLock<dyn Processor + Sync + Send>>>,
    subprocess_pool: ThreadPool, parallelize: u32,
    subprocess_buffers: Vec<f32>, subprocess_buffers_refs: Vec<UniqueRawSliceMut<f32>>,
    master_amp: f32,
    running: bool, last_sample: [f32; 2]
}
impl Mixer
{
    pub fn new() -> Self
    {
        let parallelize = (num_cpus::get() >> 1) as u32;
        info!("Processing Audio with {} threads.", parallelize);
        let subprocess_pool = ThreadPoolBuilder::new().num_threads(parallelize as _).build()
            .expect("Building ThreadPool for SubAudioProcesses");

        Mixer
        {
            parallelize, subprocess_pool, subprocess_buffers: Vec::new(), subprocess_buffers_refs: Vec::new(),
            processes: Vec::new(),
            master_amp: 1.0,
            running: false, last_sample: [0.0; 2]
        }
    }
    pub fn set_sample_rate(&mut self, samples_per_sec: f32)
    {
        for p in &self.processes
        {
            p.write().expect("Setting SampleRate").set_sample_rate(samples_per_sec);
        }
    }
    pub fn set_master_volume(&mut self, vol: f32)
    {
        self.master_amp = vol;
    }
    fn setup_process_frames(&mut self, frames: u32)
    {
        let frames_dup = frames << 1;
        self.subprocess_buffers = Vec::<f32>::with_capacity(frames_dup as usize * self.parallelize as usize);
        unsafe { self.subprocess_buffers.set_len(frames_dup as usize * self.parallelize as usize); }
        self.subprocess_buffers_refs = (0..self.parallelize).map(|i| unsafe
        {
            let ofs = i * frames_dup;
            UniqueRawSliceMut
            {
                ptr: self.subprocess_buffers.as_mut_ptr().offset(ofs as isize),
                length: frames_dup as usize
            }
        }).collect::<Vec<_>>();
    }
    fn combine_all_subprocess_buffers(&self, buf: &mut [f32])
    {
        let buflen = buf.len();
        for sp in &self.subprocess_buffers_refs
        {
            for (b, sp) in buf.iter_mut().zip(unsafe { sp.as_slice()[..buflen].iter() })
            {
                *b += sp * self.master_amp;
            }
        }
    }
    pub fn start(&mut self) { self.running = true; }
    pub fn stop(&mut self) { self.running = false; }
    /// return true if silence
    pub fn process(&mut self, buf: &mut [f32]) -> bool
    {
        if !self.running
        {
            if self.last_sample.iter().any(|&s| s != 0.0)
            {
                // 停止時のノイズ除去(gentle mute)
                buf[..2].copy_from_slice(&self.last_sample);
                self.last_sample = [0.0; 2];
                return false;
            }
            return true;
        }

        if buf.len() > self.subprocess_buffers_refs.first().map(|s| s.length).unwrap_or(0)
        {
            self.setup_process_frames(buf.len() as _);
        }
        for b in &mut self.subprocess_buffers { *b = 0.0; }

        let buflen = buf.len();
        self.subprocess_pool.install(|| for ps in self.processes.chunks(self.parallelize as _)
        {
            ps.par_iter().enumerate().for_each(|(i, p)| unsafe
            {
                let slice = self.subprocess_buffers_refs[i].as_slice_mut();
                p.write().expect("WriteLocking").process(&mut slice[..buflen]);
            });
        });
        self.combine_all_subprocess_buffers(buf);
        self.last_sample[0] = buf[buf.len() - 2];
        self.last_sample[1] = buf[buf.len() - 1];

        return self.processes.is_empty();
    }

    pub fn add_process(&mut self, p: Arc<RwLock<dyn Processor + Send + Sync>>)
    {
        self.processes.push(p);
    }
}

/// Applies AudioProcessors in serial. And applies amplification at output.
pub struct AudioEffectStack
{
    pub effect_stack: Vec<Arc<RwLock<dyn Processor>>>, pub output_amp: f32
}
impl AudioEffectStack
{
    pub fn new() -> Self { AudioEffectStack { effect_stack: Vec::new(), output_amp: 1.0 } }
}
impl Processor for AudioEffectStack
{
    fn set_sample_rate(&mut self, samples_per_sec: f32)
    {
        for e in &self.effect_stack
        {
            e.write().expect("Setting SampleRate").set_sample_rate(samples_per_sec);
        }
    }
    fn process(&mut self, flattened_buffer: &mut [f32])
    {
        for e in &self.effect_stack
        {
            e.write().expect("Processing in AudioEffectStack").process(flattened_buffer);
        }
        if self.output_amp != 1.0 { for b in flattened_buffer { *b *= self.output_amp; } }
    }
}

pub enum WaveSamples { Mono(Vec<f32>), Stereo(Vec<[f32; 2]>) }
impl WaveSamples
{
    pub fn frames(&self) -> usize
    {
        match self
        {
            WaveSamples::Mono(v) => v.len(),
            WaveSamples::Stereo(v) => v.len()
        }
    }

    pub fn copy_into_stereo(&self, dest: &mut [[f32; 2]])
    {
        match self
        {
            WaveSamples::Stereo(v) =>
            {
                let copying_count = dest.len().min(v.len());
                dest[..copying_count].copy_from_slice(&v[..copying_count]);
            },
            WaveSamples::Mono(v) =>
            {
                let fillcount = dest.len().min(v.len());
                for (d, &s) in dest[..fillcount].iter_mut().zip(v[..fillcount].iter())
                {
                    *d = [s, s];
                }
            }
        }
    }
}
impl Into<Vec<f32>> for WaveSamples
{
    fn into(self) -> Vec<f32>
    {
        match self {
            WaveSamples::Mono(v) => v,
            WaveSamples::Stereo(v) => v.into_iter().map(|x| (x[0] + x[1]) * 0.5).collect()
        }
    }
}
impl Into<Vec<[f32; 2]>> for WaveSamples
{
    fn into(self) -> Vec<[f32; 2]>
    {
        match self
        {
            WaveSamples::Mono(v) => v.into_iter().map(|x| [x; 2]).collect(),
            WaveSamples::Stereo(v) => v
        }
    }
}
pub enum WaveSamplesRefMut<'d>
{
    Mono(&'d mut [f32]), Stereo(&'d mut [[f32; 2]]), Surround5(&'d mut [[f32; 6]])
}
impl<'d> WaveSamplesRefMut<'d>
{
    // fn reinterpret_mono(s: &'d mut [f32]) -> Self { WaveSamplesRefMut::Mono(s) }
    // unsafe fn reinterpret_stereo(s: &'d mut [f32]) -> Self
    // {
    //     WaveSamplesRefMut::Stereo(std::slice::from_raw_parts_mut(s.as_mut_ptr() as _, s.len() >> 1))
    // }
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum PlayableAudioState { Ready, Playing, EndOfBuffer }

use std::collections::BTreeMap;
fn fourcc(cc: &[u8; 4]) -> u32 { u32::from_le_bytes(*cc) }
pub struct RIFFLoader<F: Read + Seek>
{
    file: F, riff_chunk_start: u64, riff_subchunk_offsets: BTreeMap<u32, (u64, u32)>
}
pub struct RIFFStreamingLoader<F: InputStream> { file: F }
#[repr(C)] pub struct RIFFChunkHeader { fourcc: u32, length: u32 }
impl RIFFChunkHeader
{
    fn padded_length(&self) -> u32 { (self.length + 1) & !1 }
}
#[repr(C)] #[derive(Debug)]
pub struct RIFFWaveFormatData
{
    encoding: u16, num_channels: u8, sample_rate: u32,
    avg_bytes_per_sec: u32, block_size: u16,
    bits_per_sample: u16
}
#[repr(C)]
pub enum WaveSamplesInFile
{
    Mono8(Vec<u8>), Mono16(Vec<i16>), Mono24(Vec<Int24>),
    Mono32(Vec<i32>), Mono64(Vec<i64>), MonoF32(Vec<f32>), MonoF64(Vec<f64>),

    Stereo8(Vec<[u8; 2]>), Stereo16(Vec<[i16; 2]>), Stereo24(Vec<[Int24; 2]>),
    Stereo32(Vec<[i32; 2]>), Stereo64(Vec<[i64; 2]>), StereoF32(Vec<[f32; 2]>), StereoF64(Vec<[f64; 2]>),
    
    Unknown { bytes: Vec<u8>, channels: usize, bits: usize },
    UnknownF { bytes: Vec<u8>, channels: usize, bits: usize }
}
trait WaveSample: Copy { fn to_f32(self) -> f32; }
impl WaveSample for u8 { fn to_f32(self) -> f32 { (self as i32 - 128) as f32 / 128.0 } }
impl WaveSample for i16 { fn to_f32(self) -> f32 { self as f32 / 0x8000 as f32 } }
impl WaveSample for i32 { fn to_f32(self) -> f32 { self as f32 / 0x8000_0000u32 as f32 } }
impl WaveSample for i64 { fn to_f32(self) -> f32 { (self as f64 / 0x8000_0000_0000_0000u64 as f64) as _ } }
impl WaveSample for f32 { fn to_f32(self) -> f32 { self } }
impl WaveSample for f64 { fn to_f32(self) -> f32 { self as _ } }
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Int24(i32);
impl WaveSample for Int24 { fn to_f32(self) -> f32 { self.0 as f32 / 0x80_0000 as f32 } }
impl From<WaveSamplesInFile> for WaveSamples
{
    fn from(s: WaveSamplesInFile) -> Self
    {
        match s
        {
            WaveSamplesInFile::Mono8(v) => WaveSamples::Mono(v.into_iter().map(WaveSample::to_f32).collect()),
            WaveSamplesInFile::Mono16(v) => WaveSamples::Mono(v.into_iter().map(WaveSample::to_f32).collect()),
            WaveSamplesInFile::Mono24(v) => WaveSamples::Mono(v.into_iter().map(WaveSample::to_f32).collect()),
            WaveSamplesInFile::Mono32(v) => WaveSamples::Mono(v.into_iter().map(WaveSample::to_f32).collect()),
            WaveSamplesInFile::Mono64(v) => WaveSamples::Mono(v.into_iter().map(WaveSample::to_f32).collect()),
            WaveSamplesInFile::MonoF64(v) => WaveSamples::Mono(v.into_iter().map(|v| v as f32).collect()),

            WaveSamplesInFile::Stereo8(v) =>
                WaveSamples::Stereo(v.into_iter().map(|v| [v[0].to_f32(), v[1].to_f32()]).collect()),
            WaveSamplesInFile::Stereo16(v) =>
                WaveSamples::Stereo(v.into_iter().map(|v| [v[0].to_f32(), v[1].to_f32()]).collect()),
            WaveSamplesInFile::Stereo24(v) =>
                WaveSamples::Stereo(v.into_iter().map(|v| [v[0].to_f32(),v[1].to_f32()]).collect()),
            WaveSamplesInFile::Stereo32(v) =>
                WaveSamples::Stereo(v.into_iter().map(|v| [v[0].to_f32(), v[1].to_f32()]).collect()),
            WaveSamplesInFile::Stereo64(v) =>
                WaveSamples::Stereo(v.into_iter().map(|v| [v[0].to_f32(), v[1].to_f32()]).collect()),
            WaveSamplesInFile::StereoF64(v) =>
                WaveSamples::Stereo(v.into_iter().map(|v| [v[0] as f32, v[1] as f32]).collect()),
            
            WaveSamplesInFile::MonoF32(v) => WaveSamples::Mono(v),
            WaveSamplesInFile::StereoF32(v) => WaveSamples::Stereo(v),

            _ => unimplemented!("Unable to convert automatically unknown format")
        }
    }
}

use std::io::Result as IOResult;
macro_rules! ReadWaveData
{
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
impl<F: Read + Seek> RIFFLoader<F>
{
    fn read_next_chunk_header(f: &mut F) -> IOResult<RIFFChunkHeader>
    {
        let mut hdr = RIFFChunkHeader { fourcc: 0, length: 0 };
        f.read_exact(unsafe { std::mem::transmute::<_, &mut [u8; 4 * 2]>(&mut hdr) }).map(move |_| hdr)
    }
    fn search_fourcc_in_file(f: &mut F, fcc: u32) -> IOResult<u64>
    {
        loop
        {
            let next_hdr = Self::read_next_chunk_header(f)?;
            if next_hdr.fourcc == fcc { return f.seek(SeekFrom::Current(-4)); }
            f.seek(SeekFrom::Current(next_hdr.padded_length() as _))?;
        }
    }

    fn new(mut file: F) -> IOResult<Self>
    {
        Self::search_fourcc_in_file(&mut file, fourcc(b"RIFF"))?;
        let riff_chunk_start = file.seek(SeekFrom::Current(8))?;
        return Ok(RIFFLoader { file, riff_chunk_start, riff_subchunk_offsets: BTreeMap::new() });
    }
    fn jump_chunk(&mut self, fcc: u32) -> IOResult<u32>
    {
        if let Some((c, l)) = self.riff_subchunk_offsets.get(&fcc).cloned()
        {
            return self.file.seek(SeekFrom::Start(c)).map(|_| l);
        }

        self.file.seek(SeekFrom::Start(self.riff_chunk_start))?;
        loop
        {
            let next_hdr = Self::read_next_chunk_header(&mut self.file)?;
            let chunk_start = self.file.seek(SeekFrom::Current(0))?;
            if next_hdr.fourcc == fcc { return Ok(next_hdr.length); }
            self.riff_subchunk_offsets.insert(next_hdr.fourcc, (chunk_start, next_hdr.length));
            self.file.seek(SeekFrom::Current(next_hdr.padded_length() as _))?;
        }
    }
    fn read_fmt(&mut self) -> IOResult<RIFFWaveFormatData>
    {
        let len = self.jump_chunk(fourcc(b"fmt "))?;
        assert!(len >= std::mem::size_of::<RIFFWaveFormatData>() as u32, "Invalid WAVE fmt chunk");
        let mut fmt = unsafe { std::mem::zeroed() };
        self.file.read_exact(unsafe { std::mem::transmute::<_, &mut [u8; 16]>(&mut fmt) })?;
        return Ok(fmt);
    }
    fn read_data(&mut self) -> IOResult<Vec<u8>>
    {
        let len = self.jump_chunk(fourcc(b"data"))?;
        let mut bytes = vec![0u8; len as usize];
        self.file.read_exact(&mut bytes).map(move |_| bytes)
    }
    fn read_data_uncompressed(&mut self, fmt: &RIFFWaveFormatData) -> IOResult<WaveSamplesInFile>
    {
        fn slice_i24_value(s: &[u8]) -> Int24
        {
            assert!(s.len() == 3, "Unable to cast &[u8] as i24");

            let vu = s[0] as u32 | ((s[1] as u32) << 8) | ((s[2] as u32) << 16);
            // 符号拡張する
            let fillone = s[2] & 0x80 != 0;
            Int24(if fillone { (vu | 0x11_000000) as _ } else { vu as _ })
        }

        match (fmt.num_channels, fmt.bits_per_sample, fmt.encoding)
        {
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
            (1, 24, 0x01) =>
            {
                let b = self.read_data()?;
                Ok(WaveSamplesInFile::Mono24(b.chunks(3).map(slice_i24_value).collect()))
            },
            (2, 24, 0x01) =>
            {
                let b = self.read_data()?;
                Ok(WaveSamplesInFile::Stereo24(b.chunks(3 * 2)
                    .map(|bs| [slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])])
                    .collect()))
            },
            (ch, bits, 0x01) => self.read_data()
                .map(|bytes| WaveSamplesInFile::Unknown { bytes, channels: ch as _, bits: bits as _ }),
            (ch, bits, 0x03) => self.read_data()
                .map(|bytes| WaveSamplesInFile::UnknownF { bytes, channels: ch as _, bits: bits as _ }),
            (ch, b, f) => unimplemented!("unhandleable triple: ch={} bits={} fmt={}", ch, b, f)
        }
    }
}
impl<F: InputStream> RIFFStreamingLoader<F>
{
    fn read_next_chunk_header(&mut self) -> IOResult<RIFFChunkHeader>
    {
        let mut hdr = RIFFChunkHeader { fourcc: 0, length: 0 };
        self.file.read_exact(unsafe { std::mem::transmute::<_, &mut [u8; 4 * 2]>(&mut hdr) }).map(move |_| hdr)
    }
    fn search_chunk(&mut self, fcc: u32) -> IOResult<u32>
    {
        loop
        {
            let next_hdr = self.read_next_chunk_header()?;
            if next_hdr.fourcc == fcc { return Ok(next_hdr.padded_length()); }
            self.file.skip(next_hdr.padded_length() as _)?;
        }
    }
    fn read_fmt(&mut self) -> IOResult<RIFFWaveFormatData>
    {
        let len = self.search_chunk(fourcc(b"fmt "))?;
        assert!(len >= std::mem::size_of::<RIFFWaveFormatData>() as u32, "Invalid WAVE fmt chunk");
        let mut fmt = unsafe { std::mem::zeroed() };
        self.file.read_exact(unsafe { std::mem::transmute::<_, &mut [u8; 16]>(&mut fmt) })?;
        self.file.skip((len - 16) as _)?;
        return Ok(fmt);
    }
    fn seek_data(&mut self) -> IOResult<u32> { self.search_chunk(fourcc(b"data")) }
    fn read_data(&mut self, max_bytes: usize) -> IOResult<Vec<u8>>
    {
        let mut bytes = vec![0u8; max_bytes];
        self.file.read(&mut bytes).map(move |v| { bytes.truncate(v); bytes })
    }
    fn read_data_uncompressed(&mut self, fmt: &RIFFWaveFormatData, max_samples: usize) -> IOResult<WaveSamplesInFile>
    {
        fn slice_i24_value(s: &[u8]) -> Int24
        {
            assert!(s.len() == 3, "Unable to cast &[u8] as i24");

            let vu = s[0] as u32 | ((s[1] as u32) << 8) | ((s[2] as u32) << 16);
            // 符号拡張する
            let fillone = s[2] & 0x80 != 0;
            Int24(if fillone { (vu | 0x11_000000) as _ } else { vu as _ })
        }

        match (fmt.num_channels, fmt.bits_per_sample, fmt.encoding)
        {
            (1, 8, 0x01) => self.read_data(max_samples).map(WaveSamplesInFile::Mono8),
            (1, 16, 0x01) => ReadWaveData!(self, 0i16; max_samples).map(WaveSamplesInFile::Mono16),
            (1, 32, 0x01) => ReadWaveData!(self, 0i32; max_samples).map(WaveSamplesInFile::Mono32),
            (1, 64, 0x01) => ReadWaveData!(self, 0i64; max_samples).map(WaveSamplesInFile::Mono64),
            (1, 32, 0x03) => ReadWaveData!(self, 0f32; max_samples).map(WaveSamplesInFile::MonoF32),
            (1, 64, 0x03) => ReadWaveData!(self, 0f64; max_samples).map(WaveSamplesInFile::MonoF64),
            (2, 8, 0x01) => ReadWaveData!(self, [0u8; 2]; max_samples).map(WaveSamplesInFile::Stereo8),
            (2, 16, 0x01) => ReadWaveData!(self, [0i16; 2]; max_samples).map(WaveSamplesInFile::Stereo16),
            (2, 32, 0x01) => ReadWaveData!(self, [0i32; 2]; max_samples).map(WaveSamplesInFile::Stereo32),
            (2, 64, 0x01) => ReadWaveData!(self, [0i64; 2]; max_samples).map(WaveSamplesInFile::Stereo64),
            (2, 32, 0x03) => ReadWaveData!(self, [0f32; 2]; max_samples).map(WaveSamplesInFile::StereoF32),
            (2, 64, 0x03) => ReadWaveData!(self, [0f64; 2]; max_samples).map(WaveSamplesInFile::StereoF64),
            (1, 24, 0x01) =>
            {
                let b = self.read_data(max_samples / 3)?;
                Ok(WaveSamplesInFile::Mono24(b.chunks(3).map(slice_i24_value).collect()))
            },
            (2, 24, 0x01) =>
            {
                let b = self.read_data(max_samples / (3 * 2))?;
                Ok(WaveSamplesInFile::Stereo24(b.chunks(3 * 2).map(|bs| [
                    slice_i24_value(&bs[..3]), slice_i24_value(&bs[3..])
                ]).collect()))
            },
            (ch, b, f) => unimplemented!("unhandleable triple: ch={} bits={} fmt={}", ch, b, f)
        }
    }
}
impl<F: InputStream> From<F> for RIFFStreamingLoader<F>
{
    fn from(file: F) -> Self { RIFFStreamingLoader { file } }
}

// Reading Asset //

use std::io::{Read, Seek, SeekFrom, ErrorKind};
/// An WaveFile as AudioSource which all of audio data is in the memory.
pub struct PreloadedPlayableWav
{
    samples: Vec<[f32; 2]>, current_smp: usize, state: PlayableAudioState
}
impl super::LogicalAssetData for PreloadedPlayableWav { const EXT: &'static str = "wav"; }
impl super::FromAsset for PreloadedPlayableWav
{
    type Error = std::io::Error;
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self>
    {
        let mut loader = RIFFLoader::new(asset)?;
        let fmt = loader.read_fmt()?;
        let data = WaveSamples::from(loader.read_data_uncompressed(&fmt)?).into();
        return Ok(PreloadedPlayableWav { samples: data, current_smp: 0, state: PlayableAudioState::Ready });
    }
}
impl Processor for PreloadedPlayableWav
{
    fn process(&mut self, flattened_buffer: &mut [f32])
    {
        if self.state != PlayableAudioState::Playing { return; }

        let fill_count = (flattened_buffer.len() >> 1).min(self.samples.len() - self.current_smp);
        for (n, s) in self.samples[self.current_smp .. self.current_smp + fill_count].iter().enumerate()
        {
            flattened_buffer[(n << 1) + 0] = s[0];
            flattened_buffer[(n << 1) + 1] = s[1];
        }
        self.current_smp += fill_count;
    }
}

const WAV_STREAMING_DEFAULT_BUFFER_SAMPLES: usize = 8192;

/// An WaveFile as AudioSource which streaming audio data from disk. Default buffering rate is 8192smp/buf
pub struct StreamingPlayableWav
{
    loader: RIFFStreamingLoader<BoxedInputStream>, fmt: RIFFWaveFormatData,
    buffered_samples: Box<[[f32; 2]]>, current_smp: usize, buffered_smp: usize, buffer_length: usize,
    state: PlayableAudioState
}
impl super::LogicalAssetData for StreamingPlayableWav { const EXT: &'static str = "wav"; }
impl super::FromStreamingAsset for StreamingPlayableWav
{
    type Error = std::io::Error;
    fn from_asset<Asset: super::InputStream + 'static>(asset: Asset) -> IOResult<Self>
    {
        let mut loader = RIFFStreamingLoader::from(BoxedInputStream::new(asset));
        loader.file.skip(4 * 3)?;
        let fmt = loader.read_fmt()?;
        debug!("fmt: {:?}", fmt);
        loader.seek_data()?;
        // initial buffering
        let mut buffered_samples = Vec::with_capacity(WAV_STREAMING_DEFAULT_BUFFER_SAMPLES * 2);
        unsafe { buffered_samples.set_len(WAV_STREAMING_DEFAULT_BUFFER_SAMPLES * 2); }
        let mut buffered_samples = buffered_samples.into_boxed_slice();
        let buffer_length = match loader.read_data_uncompressed(&fmt, buffered_samples.len())
        {
            Ok(v) =>
            {
                let loaded = WaveSamples::from(v);
                loaded.copy_into_stereo(&mut buffered_samples); loaded.frames()
            },
            Err(e) if e.kind() == ErrorKind::UnexpectedEof => 0,
            Err(e) => { return Err(e); }
        };

        Ok(StreamingPlayableWav
        {
            loader, fmt, buffered_samples, current_smp: 0, buffered_smp: 0, buffer_length,
            state: PlayableAudioState::Ready
        })
    }
}
impl Processor for StreamingPlayableWav
{
    fn process(&mut self, flattened_buffer: &mut [f32])
    {
        if self.state != PlayableAudioState::Playing { return; }

        let frames = if self.buffered_smp < self.buffer_length
        {
            let frames = (flattened_buffer.len() >> 1).min(self.buffer_length - self.buffered_smp);
            for (n, s) in self.buffered_samples[self.buffered_smp .. self.buffered_smp + frames].iter().enumerate()
            {
                flattened_buffer[(n << 1) + 0] = s[0];
                flattened_buffer[(n << 1) + 1] = s[1];
            }
            self.buffered_smp += frames; frames
        }
        else { 0 };

        if self.buffered_smp >= self.buffer_length { self.buffer(); }
        if self.state != PlayableAudioState::Playing { return; }

        let left_frames = ((flattened_buffer.len() >> 1) - frames).min(self.buffer_length);
        for (n, s) in self.buffered_samples[self.buffered_smp .. self.buffered_smp + left_frames].iter().enumerate()
        {
            flattened_buffer[((n + frames) << 1) + 0] = s[0];
            flattened_buffer[((n + frames) << 1) + 1] = s[1];
        }

        let fill_frames = frames + left_frames;
        self.current_smp += fill_frames;
        self.buffered_smp += left_frames;
    }
}
impl StreamingPlayableWav
{
    fn buffer(&mut self)
    {
        let loaded = match self.loader.read_data_uncompressed(&self.fmt, self.buffered_samples.len())
        {
            Ok(v) => WaveSamples::from(v),
            Err(e) if e.kind() == ErrorKind::UnexpectedEof =>
            {
                self.state = PlayableAudioState::EndOfBuffer;
                return;
            },
            Err(e) => panic!("Buffering WaveSample: {:?}", e)
        };
        loaded.copy_into_stereo(&mut self.buffered_samples);
        self.buffered_smp = 0;
        self.buffer_length = loaded.frames();
    }
}
impl PreloadedPlayableWav
{
    pub fn play(&mut self) { self.state = PlayableAudioState::Playing; }
}
impl StreamingPlayableWav
{
    pub fn play(&mut self) { self.state = PlayableAudioState::Playing; }
}
