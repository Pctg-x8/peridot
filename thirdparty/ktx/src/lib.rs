use core::{cell::UnsafeCell, mem::MaybeUninit, ptr::NonNull};
use std::ops::{Deref, DerefMut};

use bitflags::bitflags;

pub mod ffi;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Error(ffi::ktx_error_code_e);
impl Error {
    pub const FILE_DATA_ERROR: Self = Self(ffi::KTX_FILE_DATA_ERROR);
    pub const FILE_ISPIPE: Self = Self(ffi::KTX_FILE_ISPIPE);
    pub const FILE_OPEN_FAILED: Self = Self(ffi::KTX_FILE_OPEN_FAILED);
    pub const FILE_OVERFLOW: Self = Self(ffi::KTX_FILE_OVERFLOW);
    pub const FILE_READ_ERROR: Self = Self(ffi::KTX_FILE_READ_ERROR);
    pub const FILE_SEEK_ERROR: Self = Self(ffi::KTX_FILE_SEEK_ERROR);
    pub const FILE_UNEXPECTED_EOF: Self = Self(ffi::KTX_FILE_UNEXPECTED_EOF);
    pub const FILE_WRITE_ERROR: Self = Self(ffi::KTX_FILE_WRITE_ERROR);
    pub const GL_ERROR: Self = Self(ffi::KTX_GL_ERROR);
    pub const INVALID_OPERATION: Self = Self(ffi::KTX_INVALID_OPERATION);
    pub const INVALID_VALUE: Self = Self(ffi::KTX_INVALID_VALUE);
    pub const NOT_FOUND: Self = Self(ffi::KTX_NOT_FOUND);
    pub const OUT_OF_MEMORY: Self = Self(ffi::KTX_OUT_OF_MEMORY);
    pub const TRANSCODE_FAILED: Self = Self(ffi::KTX_TRANSCODE_FAILED);
    pub const UNKNOWN_FILE_FORMAT: Self = Self(ffi::KTX_UNKNOWN_FILE_FORMAT);
    pub const UNSUPPORTED_TEXTURE_TYPE: Self = Self(ffi::KTX_UNSUPPORTED_TEXTURE_TYPE);
    pub const UNSUPPORTED_FEATURE: Self = Self(ffi::KTX_UNSUPPORTED_FEATURE);
    pub const LIBRARY_NOT_LINKED: Self = Self(ffi::KTX_LIBRARY_NOT_LINKED);
    pub const DECOMPRESS_LENGTH_ERROR: Self = Self(ffi::KTX_DECOMPRESS_LENGTH_ERROR);
    pub const DECOMPRESS_CHECKSUM_ERROR: Self = Self(ffi::KTX_DECOMPRESS_CHECKSUM_ERROR);
}
impl core::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cstr = unsafe { core::ffi::CStr::from_ptr(ffi::ktxErrorString(self.0)) };
        write!(f, "{} {:?}", self.0, cstr.to_str())
    }
}
impl core::fmt::Display for Error {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Debug::fmt(self, f)
    }
}
impl core::error::Error for Error {}

pub type Result<T> = core::result::Result<T, Error>;
const fn r(e: ffi::ktx_error_code_e) -> Result<()> {
    if e == ffi::KTX_SUCCESS {
        Ok(())
    } else {
        Err(Error(e))
    }
}

pub trait Texture {
    fn vtbl(&self) -> &ffi::ktxTexture_vtbl;
    fn as_thisptr(&self) -> *mut ffi::ktxTexture;
    fn as_thisptr_mut(&mut self) -> *mut ffi::ktxTexture;

    #[inline(always)]
    unsafe fn destroy(&mut self) {
        unsafe { (self.vtbl().Destroy)(self.as_thisptr()) }
    }

    #[inline(always)]
    fn image_offset(&self, level: u32, layer: u32, face_slice: u32) -> Result<usize> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            (self.vtbl().GetImageOffset)(
                self.as_thisptr(),
                level,
                layer,
                face_slice,
                sink.as_mut_ptr(),
            )
        })?;
        Ok(unsafe { sink.assume_init() })
    }

    #[inline(always)]
    fn data_size_uncompressed(&self) -> usize {
        unsafe { (self.vtbl().GetDataSizeUncompressed)(self.as_thisptr()) }
    }

    #[inline(always)]
    fn image_size(&self, level: u32) -> usize {
        unsafe { (self.vtbl().GetImageSize)(self.as_thisptr(), level) }
    }

    #[inline(always)]
    fn level_size(&self, level: u32) -> usize {
        unsafe { (self.vtbl().GetLevelSize)(self.as_thisptr(), level) }
    }

    #[inline(always)]
    unsafe fn iterate_levels_raw(
        &self,
        iter_cb: ffi::PFNKTXITERCB,
        user_data: *mut core::ffi::c_void,
    ) -> Result<()> {
        r(unsafe { (self.vtbl().IterateLevels)(self.as_thisptr(), iter_cb, user_data) })
    }

    #[inline(always)]
    unsafe fn iterate_load_level_faces_raw(
        &self,
        iter_cb: ffi::PFNKTXITERCB,
        user_data: *mut core::ffi::c_void,
    ) -> Result<()> {
        r(unsafe { (self.vtbl().IterateLoadLevelFaces)(self.as_thisptr(), iter_cb, user_data) })
    }

    #[inline(always)]
    fn load_image_data(&self, buffer: &mut [MaybeUninit<u8>]) -> Result<()> {
        r(unsafe {
            (self.vtbl().LoadImageData)(self.as_thisptr(), buffer.as_mut_ptr() as _, buffer.len())
        })
    }

    #[inline(always)]
    fn needs_transcoding(&self) -> bool {
        unsafe { (self.vtbl().NeedsTranscoding)(self.as_thisptr()) }
    }

    #[inline(always)]
    fn set_image_from_memory(
        &mut self,
        level: u32,
        layer: u32,
        face_slice: u32,
        data: &[u8],
    ) -> Result<()> {
        r(unsafe {
            (self.vtbl().SetImageFromMemory)(
                self.as_thisptr_mut(),
                level,
                layer,
                face_slice,
                data.as_ptr(),
                data.len(),
            )
        })
    }

    #[inline(always)]
    fn write_to_named_file(&self, name: &core::ffi::CStr) -> Result<()> {
        r(unsafe { (self.vtbl().WriteToNamedFile)(self.as_thisptr(), name.as_ptr()) })
    }

    #[inline(always)]
    fn write_to_memory(&self) -> Result<(*mut u8, usize)> {
        let mut sink_bytes = MaybeUninit::uninit();
        let mut sink_size = MaybeUninit::uninit();
        r(unsafe {
            (self.vtbl().WriteToMemory)(
                self.as_thisptr(),
                sink_bytes.as_mut_ptr(),
                sink_size.as_mut_ptr(),
            )
        })?;
        Ok(unsafe { (sink_bytes.assume_init(), sink_size.assume_init()) })
    }

    #[inline(always)]
    unsafe fn write_to_stream_raw(&self, stream: &mut ffi::ktxStream) -> Result<()> {
        r(unsafe { (self.vtbl().WriteToStream)(self.as_thisptr(), stream) })
    }

    // extra functions that not in the vtable //

    #[inline(always)]
    fn data(&self) -> *mut u8 {
        unsafe { ffi::ktxTexture_GetData(self.as_thisptr()) }
    }

    #[inline(always)]
    fn row_pitch(&self, level: u32) -> u32 {
        unsafe { ffi::ktxTexture_GetRowPitch(self.as_thisptr(), level) }
    }

    #[inline(always)]
    fn element_size(&self) -> u32 {
        unsafe { ffi::ktxTexture_GetElementSize(self.as_thisptr()) }
    }

    #[inline(always)]
    fn data_size(&self) -> usize {
        unsafe { ffi::ktxTexture_GetDataSize(self.as_thisptr()) }
    }

    #[inline(always)]
    unsafe fn iterate_level_faces_raw(
        &self,
        iter_cb: ffi::PFNKTXITERCB,
        user_data: *mut core::ffi::c_void,
    ) -> Result<()> {
        r(unsafe { ffi::ktxTexture_IterateLevelFaces(self.as_thisptr(), iter_cb, user_data) })
    }

    // direct accessor of ktxTexture fields //

    #[inline(always)]
    fn is_array(&self) -> bool {
        unsafe { (*self.as_thisptr()).isArray }
    }

    #[inline(always)]
    fn is_cubemap(&self) -> bool {
        unsafe { (*self.as_thisptr()).isCubemap }
    }

    #[inline(always)]
    fn is_compressed(&self) -> bool {
        unsafe { (*self.as_thisptr()).isCompressed }
    }

    #[inline(always)]
    fn needs_generate_mipmaps(&mut self) -> bool {
        unsafe { (*self.as_thisptr()).generateMipmaps }
    }

    #[inline(always)]
    fn base_width(&self) -> u32 {
        unsafe { (*self.as_thisptr()).baseWidth }
    }

    #[inline(always)]
    fn base_height(&self) -> u32 {
        unsafe { (*self.as_thisptr()).baseHeight }
    }

    #[inline(always)]
    fn base_depth(&self) -> u32 {
        unsafe { (*self.as_thisptr()).baseDepth }
    }

    #[inline(always)]
    fn num_dimensions(&self) -> u32 {
        unsafe { (*self.as_thisptr()).numDimensions }
    }

    #[inline(always)]
    fn num_levels(&self) -> u32 {
        unsafe { (*self.as_thisptr()).numLevels }
    }

    #[inline(always)]
    fn num_layers(&self) -> u32 {
        unsafe { (*self.as_thisptr()).numLayers }
    }

    #[inline(always)]
    fn num_faces(&self) -> u32 {
        unsafe { (*self.as_thisptr()).numFaces }
    }
}

pub trait TextureEx: Texture {
    #[inline(always)]
    fn iterate_levels<F>(&self, mut cb: F) -> Result<()>
    where
        F: FnMut(
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            u64,
            *mut core::ffi::c_void,
        ) -> Result<()>,
    {
        extern "C" fn wrapper<F>(
            mip_level: core::ffi::c_int,
            face: core::ffi::c_int,
            width: core::ffi::c_int,
            height: core::ffi::c_int,
            depth: core::ffi::c_int,
            face_lod_size: u64,
            pixels: *mut core::ffi::c_void,
            user_data: *mut core::ffi::c_void,
        ) -> ffi::ktx_error_code_e
        where
            F: FnMut(
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                u64,
                *mut core::ffi::c_void,
            ) -> Result<()>,
        {
            match unsafe {
                (*user_data.cast::<F>())(
                    mip_level,
                    face,
                    width,
                    height,
                    depth,
                    face_lod_size,
                    pixels,
                )
            } {
                Ok(()) => ffi::KTX_SUCCESS,
                Err(e) => e.0,
            }
        }

        unsafe { self.iterate_levels_raw(wrapper::<F>, &mut cb as *mut F as _) }
    }

    #[inline(always)]
    fn iterate_load_level_faces<F>(&self, mut cb: F) -> Result<()>
    where
        F: FnMut(
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            u64,
            *mut core::ffi::c_void,
        ) -> Result<()>,
    {
        extern "C" fn wrapper<F>(
            mip_level: core::ffi::c_int,
            face: core::ffi::c_int,
            width: core::ffi::c_int,
            height: core::ffi::c_int,
            depth: core::ffi::c_int,
            face_lod_size: u64,
            pixels: *mut core::ffi::c_void,
            user_data: *mut core::ffi::c_void,
        ) -> ffi::ktx_error_code_e
        where
            F: FnMut(
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                u64,
                *mut core::ffi::c_void,
            ) -> Result<()>,
        {
            match unsafe {
                (*user_data.cast::<F>())(
                    mip_level,
                    face,
                    width,
                    height,
                    depth,
                    face_lod_size,
                    pixels,
                )
            } {
                Ok(()) => ffi::KTX_SUCCESS,
                Err(e) => e.0,
            }
        }

        unsafe { self.iterate_load_level_faces_raw(wrapper::<F>, &mut cb as *mut F as _) }
    }

    #[inline(always)]
    fn iterate_level_faces<F>(&self, mut cb: F) -> Result<()>
    where
        F: FnMut(
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            core::ffi::c_int,
            u64,
            *mut core::ffi::c_void,
        ) -> Result<()>,
    {
        extern "C" fn wrapper<F>(
            mip_level: core::ffi::c_int,
            face: core::ffi::c_int,
            width: core::ffi::c_int,
            height: core::ffi::c_int,
            depth: core::ffi::c_int,
            face_lod_size: u64,
            pixels: *mut core::ffi::c_void,
            user_data: *mut core::ffi::c_void,
        ) -> ffi::ktx_error_code_e
        where
            F: FnMut(
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                core::ffi::c_int,
                u64,
                *mut core::ffi::c_void,
            ) -> Result<()>,
        {
            match unsafe {
                (*user_data.cast::<F>())(
                    mip_level,
                    face,
                    width,
                    height,
                    depth,
                    face_lod_size,
                    pixels,
                )
            } {
                Ok(()) => ffi::KTX_SUCCESS,
                Err(e) => e.0,
            }
        }

        unsafe { self.iterate_level_faces_raw(wrapper::<F>, &mut cb as *mut F as _) }
    }
}
impl<T: Texture> TextureEx for T {}

#[repr(transparent)]
pub struct Owned<T: Ownable>(NonNull<T>);
impl<T: Ownable> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            T::destruct(self.0.as_mut());
        }
    }
}
impl<T: Ownable> Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: Ownable> DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: Ownable + Texture> Texture for Owned<T> {
    #[inline(always)]
    fn vtbl(&self) -> &ffi::ktxTexture_vtbl {
        unsafe { self.0.as_ref().vtbl() }
    }

    #[inline(always)]
    fn as_thisptr(&self) -> *mut ffi::ktxTexture {
        unsafe { self.0.as_ref().as_thisptr() }
    }

    #[inline(always)]
    fn as_thisptr_mut(&mut self) -> *mut ffi::ktxTexture {
        unsafe { self.0.as_mut().as_thisptr_mut() }
    }
}

pub trait Ownable {
    unsafe fn destruct(&mut self);
}

#[repr(transparent)]
pub struct Texture1(UnsafeCell<ffi::ktxTexture1>);
impl Ownable for Texture1 {
    #[inline(always)]
    unsafe fn destruct(&mut self) {
        unsafe { Texture::destroy(self) }
    }
}
impl Texture for Texture1 {
    #[inline(always)]
    fn vtbl(&self) -> &ffi::ktxTexture_vtbl {
        unsafe { &*(*self.0.get()).base.vtbl }
    }

    #[inline(always)]
    fn as_thisptr(&self) -> *mut ffi::ktxTexture {
        self.0.get().cast()
    }

    #[inline(always)]
    fn as_thisptr_mut(&mut self) -> *mut ffi::ktxTexture {
        self.0.get_mut() as *mut _ as _
    }
}
impl Texture1 {
    #[inline]
    pub fn new(
        create_info: &ffi::ktxTextureCreateInfo,
        alloc_storage: bool,
    ) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture1_Create(
                create_info,
                if alloc_storage {
                    ffi::KTX_TEXTURE_CREATE_ALLOC_STORAGE
                } else {
                    ffi::KTX_TEXTURE_CREATE_NO_STORAGE
                },
                sink.as_mut_ptr(),
            )
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline]
    pub fn from_named_file(
        filename: &core::ffi::CStr,
        create_flags: TextureCreateFlags,
    ) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture_CreateFromNamedFile(
                filename.as_ptr(),
                create_flags.bits(),
                sink.as_mut_ptr(),
            )
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline]
    pub fn from_memory(bytes: &[u8], create_flags: TextureCreateFlags) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture_CreateFromMemory(
                bytes.as_ptr(),
                bytes.len(),
                create_flags.bits(),
                sink.as_mut_ptr(),
            )
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline]
    pub unsafe fn from_stream_raw(
        stream: &mut ffi::ktxStream,
        create_flags: TextureCreateFlags,
    ) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture_CreateFromStream(stream, create_flags.bits(), sink.as_mut_ptr())
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline(always)]
    pub fn write_ktx2_to_named_file(&self, name: &core::ffi::CStr) -> Result<()> {
        r(unsafe { ffi::ktxTexture1_WriteKTX2ToNamedFile(self.0.get(), name.as_ptr()) })
    }

    #[inline(always)]
    pub fn write_ktx2_to_memory(&self) -> Result<(*mut u8, usize)> {
        let mut sink_ptr = MaybeUninit::uninit();
        let mut sink_size = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture1_WriteKTX2ToMemory(
                self.0.get(),
                sink_ptr.as_mut_ptr(),
                sink_size.as_mut_ptr(),
            )
        })?;
        Ok(unsafe { (sink_ptr.assume_init(), sink_size.assume_init()) })
    }

    #[inline(always)]
    pub unsafe fn write_ktx2_to_stream(&self, stream: &mut ffi::ktxStream) -> Result<()> {
        r(unsafe { ffi::ktxTexture1_WriteKTX2ToStream(self.0.get(), stream) })
    }
}

#[repr(transparent)]
pub struct Texture2(UnsafeCell<ffi::ktxTexture2>);
impl Ownable for Texture2 {
    #[inline(always)]
    unsafe fn destruct(&mut self) {
        unsafe { Texture::destroy(self) }
    }
}
impl Texture for Texture2 {
    #[inline(always)]
    fn vtbl(&self) -> &ffi::ktxTexture_vtbl {
        unsafe { &*(*self.0.get()).base.vtbl }
    }

    #[inline(always)]
    fn as_thisptr(&self) -> *mut ffi::ktxTexture {
        self.0.get().cast()
    }

    #[inline(always)]
    fn as_thisptr_mut(&mut self) -> *mut ffi::ktxTexture {
        self.0.get_mut() as *mut _ as _
    }
}
impl Texture2 {
    #[inline]
    pub fn new(
        create_info: &ffi::ktxTextureCreateInfo,
        storage_alloc: bool,
    ) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture2_Create(
                create_info,
                if storage_alloc {
                    ffi::KTX_TEXTURE_CREATE_ALLOC_STORAGE
                } else {
                    ffi::KTX_TEXTURE_CREATE_NO_STORAGE
                },
                sink.as_mut_ptr(),
            )
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline(always)]
    pub fn create_copy(&self) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe { ffi::ktxTexture2_CreateCopy(self.0.get(), sink.as_mut_ptr()) })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline]
    pub fn from_named_file(
        name: &core::ffi::CStr,
        create_flags: TextureCreateFlags,
    ) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture2_CreateFromNamedFile(
                name.as_ptr(),
                create_flags.bits(),
                sink.as_mut_ptr(),
            )
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline]
    pub fn from_memory(bytes: &[u8], create_flags: TextureCreateFlags) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture2_CreateFromMemory(
                bytes.as_ptr(),
                bytes.len(),
                create_flags.bits(),
                sink.as_mut_ptr(),
            )
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline]
    pub unsafe fn from_stream_raw(
        stream: &mut ffi::ktxStream,
        create_flags: TextureCreateFlags,
    ) -> Result<Owned<Self>> {
        let mut sink = MaybeUninit::uninit();
        r(unsafe {
            ffi::ktxTexture2_CreateFromStream(stream, create_flags.bits(), sink.as_mut_ptr())
        })?;
        Ok(Owned(unsafe {
            NonNull::new_unchecked(sink.assume_init().cast())
        }))
    }

    #[inline(always)]
    pub fn compress_basis(&mut self, quality: u32) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_CompressBasis(self.0.get(), quality) })
    }

    #[inline(always)]
    pub fn deflate_zstd(&mut self, level: u32) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_DeflateZstd(self.0.get(), level) })
    }

    #[inline(always)]
    pub fn deflate_zlib(&mut self, level: u32) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_DeflateZLIB(self.0.get(), level) })
    }

    #[inline(always)]
    pub fn component_info(
        &self,
        num_components: &mut MaybeUninit<u32>,
        component_byte_length: &mut MaybeUninit<u32>,
    ) {
        unsafe {
            ffi::ktxTexture2_GetComponentInfo(
                self.0.get(),
                num_components.as_mut_ptr(),
                component_byte_length.as_mut_ptr(),
            )
        }
    }

    #[inline(always)]
    pub fn num_components(&self) -> u32 {
        unsafe { ffi::ktxTexture2_GetNumComponents(self.0.get()) }
    }

    #[inline(always)]
    pub fn compress_astc_ex(&mut self, params: &mut AstcParams) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_CompressAstcEx(self.0.get(), &mut params.0) })
    }

    #[inline(always)]
    pub fn compress_astc(&mut self, quality: u32) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_CompressAstc(self.0.get(), quality) })
    }

    #[inline(always)]
    pub fn decode_astc(&mut self) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_DecodeAstc(self.0.get()) })
    }

    #[inline(always)]
    pub fn compress_basis_ex(&mut self, params: &mut BasisParams) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_CompressBasisEx(self.0.get(), &mut params.0) })
    }

    #[inline(always)]
    pub fn transcode_basis(
        &mut self,
        fmt: ffi::ktx_transcode_fmt_e,
        transcode_flags: TranscodeFlags,
    ) -> Result<()> {
        r(unsafe { ffi::ktxTexture2_TranscodeBasis(self.0.get(), fmt, transcode_flags.bits()) })
    }

    #[inline(always)]
    pub fn vk_format(&self) -> core::ffi::c_int {
        unsafe { ffi::ktxTexture2_GetVkFormat(self.0.get()) }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SupercompressionScheme(ffi::ktxSupercmpScheme);
impl SupercompressionScheme {
    pub const NONE: Self = Self(ffi::KTX_SS_NONE);
    pub const BASIS_LZ: Self = Self(ffi::KTX_SS_BASIS_LZ);
    pub const ZSTD: Self = Self(ffi::KTX_SS_ZSTD);
    pub const ZLIB: Self = Self(ffi::KTX_SS_ZLIB);

    #[inline(always)]
    pub const fn is_vendor(&self) -> bool {
        0x10000 <= self.0 && self.0 <= 0x1ffff
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct TextureCreateFlags : ffi::ktxTextureCreateFlagBits {
        const NO_FLAGS = ffi::KTX_TEXTURE_CREATE_NO_FLAGS;
        const LOAD_IMAGE_DATA = ffi::KTX_TEXTURE_CREATE_LOAD_IMAGE_DATA_BIT;
        const RAW_KVDATA = ffi::KTX_TEXTURE_CREATE_RAW_KVDATA_BIT;
        const SKIP_KVDATA = ffi::KTX_TEXTURE_CREATE_SKIP_KVDATA_BIT;
        const CHECK_GLTF_BASISU = ffi::KTX_TEXTURE_CREATE_CHECK_GLTF_BASISU_BIT;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct TranscodeFlags : ffi::ktx_transcode_flag_bits_e {
        const PVRTC_DECODE_TO_NEXT_POW2 = ffi::KTX_TF_PVRTC_DECODE_TO_NEXT_POW2;
        const TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS = ffi::KTX_TF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS;
        const HIGH_QUALITY = ffi::KTX_TF_HIGH_QUALITY;
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct AstcParams(ffi::ktxAstcParams);
impl AstcParams {
    pub const fn new() -> Self {
        Self(ffi::ktxAstcParams {
            structSize: core::mem::size_of::<ffi::ktxAstcParams>() as _,
            ..unsafe { MaybeUninit::zeroed().assume_init() }
        })
    }

    pub const fn verbose(mut self) -> Self {
        self.0.verbose = true;
        self
    }

    pub const fn thread_count(mut self, count: u32) -> Self {
        self.0.threadCount = count;
        self
    }

    pub const fn block_dimension(mut self, v: u32) -> Self {
        self.0.blockDimension = v;
        self
    }

    pub const fn mode(mut self, v: u32) -> Self {
        self.0.mode = v;
        self
    }

    pub const fn quality_level(mut self, v: u32) -> Self {
        self.0.qualityLevel = v;
        self
    }

    pub const fn normal_map(mut self) -> Self {
        self.0.normalMap = true;
        self
    }

    pub const fn perceptual(mut self) -> Self {
        self.0.perceptual = true;
        self
    }

    pub const fn input_swizzle(mut self, v: [core::ffi::c_char; 4]) -> Self {
        self.0.inputSwizzle = v;
        self
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct BasisParams(ffi::ktxBasisParams);
impl BasisParams {
    pub const fn new() -> Self {
        Self(ffi::ktxBasisParams {
            structSize: core::mem::size_of::<ffi::ktxBasisParams>() as _,
            compressionLevel: unsafe { ffi::KTX_ETC1S_DEFAULT_COMPRESSION_LEVEL },
            ..unsafe { MaybeUninit::zeroed().assume_init() }
        })
    }

    pub const fn verbose(mut self) -> Self {
        self.0.verbose = true;
        self
    }

    pub const fn uastc(mut self) -> Self {
        self.0.uastc = true;
        self
    }

    pub const fn no_sse(mut self) -> Self {
        self.0.noSSE = true;
        self
    }

    pub const fn thread_count(mut self, v: u32) -> Self {
        self.0.threadCount = v;
        self
    }

    pub const fn compression_level(mut self, v: u32) -> Self {
        self.0.compressionLevel = v;
        self
    }

    pub const fn quality_level(mut self, v: u32) -> Self {
        self.0.qualityLevel = v;
        self
    }

    pub const fn max_cluster_counts(mut self, endpoints: u32, selectors: u32) -> Self {
        self.0.maxEndpoints = endpoints;
        self.0.maxSelectors = selectors;
        self
    }

    pub const fn endpoint_rdo_threshold(mut self, v: core::ffi::c_float) -> Self {
        self.0.endpointRDOThreshold = v;
        self
    }

    pub const fn selector_rdo_threshold(mut self, v: core::ffi::c_float) -> Self {
        self.0.selectorRDOThreshold = v;
        self
    }

    pub const fn input_swizzle(mut self, v: [core::ffi::c_char; 4]) -> Self {
        self.0.inputSwizzle = v;
        self
    }

    pub const fn normal_map(mut self) -> Self {
        self.0.normalMap = true;
        self
    }

    pub const fn pre_swizzle(mut self) -> Self {
        self.0.preSwizzle = true;
        self
    }

    pub const fn no_endpoint_rdo(mut self) -> Self {
        self.0.noEndpointRDO = true;
        self
    }

    pub const fn no_selector_rdo(mut self) -> Self {
        self.0.noSelectorRDO = true;
        self
    }

    pub const fn uastc_flags(mut self, v: ffi::ktx_pack_uastc_flags) -> Self {
        self.0.uastcFlags = v;
        self
    }

    pub const fn uastc_rdo(mut self) -> Self {
        self.0.uastcRDO = true;
        self
    }

    pub const fn uastc_rdo_quality_scalar(mut self, v: core::ffi::c_float) -> Self {
        self.0.uastcRDOQualityScalar = v;
        self
    }

    pub const fn uastc_rdo_dict_size(mut self, v: u32) -> Self {
        self.0.uastcRDODictSize = v;
        self
    }

    pub const fn uastc_rod_max_smooth_block_error_scale(mut self, v: core::ffi::c_float) -> Self {
        self.0.uastcRDOMaxSmoothBlockErrorScale = v;
        self
    }

    pub const fn uastc_rdo_max_smooth_block_stdev(mut self, v: core::ffi::c_float) -> Self {
        self.0.uastcRDOMaxSmoothBlockStdDev = v;
        self
    }

    pub const fn uastc_rdo_dont_favor_simpler_modes(mut self) -> Self {
        self.0.uastcRDODontFavorSimplerModes = true;
        self
    }

    pub const fn uastc_rdo_no_multithreading(mut self) -> Self {
        self.0.uastcRDONoMultithreading = true;
        self
    }
}
