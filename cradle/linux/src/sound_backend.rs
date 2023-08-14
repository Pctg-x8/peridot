pub mod pa;
pub mod pipewire;

pub trait SoundBackend {}

trait AudioBitstreamConverter {
    fn sample_count(&self, bytes: usize) -> usize;
    fn convert(&self, floats: &[f32], into: &mut [u8]);
}
pub struct SignedInt24LEConverter;
pub struct SignedInt32LEConverter;
pub struct Float32Converter;
impl AudioBitstreamConverter for SignedInt24LEConverter {
    fn sample_count(&self, bytes: usize) -> usize {
        bytes / 3
    }
    fn convert(&self, floats: &[f32], into: &mut [u8]) {
        for (n, &e) in floats.into_iter().enumerate() {
            let v = unsafe { std::mem::transmute::<_, u32>((e * 8388607.0) as i32) };
            into[n * 3 + 0] = (v & 0xff) as _;
            into[n * 3 + 1] = ((v >> 8) & 0xff) as _;
            into[n * 3 + 2] = ((v >> 16) & 0xff) as _;
        }
    }
}
impl AudioBitstreamConverter for SignedInt32LEConverter {
    fn sample_count(&self, bytes: usize) -> usize {
        bytes / 4
    }

    fn convert(&self, floats: &[f32], into: &mut [u8]) {
        let blocks =
            unsafe { std::slice::from_raw_parts_mut(into.as_ptr() as *mut i32, into.len() / 4) };
        for (n, e) in floats
            .into_iter()
            .map(|x| i32::to_le((x * 8388607.0) as _))
            .enumerate()
        {
            blocks[n] = e;
        }
    }
}
impl AudioBitstreamConverter for Float32Converter {
    fn sample_count(&self, bytes: usize) -> usize {
        bytes >> 2
    }
    fn convert(&self, floats: &[f32], into: &mut [u8]) {
        into.copy_from_slice(unsafe {
            std::slice::from_raw_parts(floats.as_ptr() as _, floats.len() << 2)
        });
    }
}
