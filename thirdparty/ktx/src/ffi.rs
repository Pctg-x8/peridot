#![allow(non_camel_case_types, non_snake_case)]

/// Error codes returned by library functions.
pub type ktx_error_code_e = core::ffi::c_int;
/// Operation was successful.
pub const KTX_SUCCESS: ktx_error_code_e = 0;
/// The data in the file is inconsistent with the spec.
pub const KTX_FILE_DATA_ERROR: ktx_error_code_e = KTX_SUCCESS + 1;
/// The file is a pipe or named pipe.
pub const KTX_FILE_ISPIPE: ktx_error_code_e = KTX_FILE_DATA_ERROR + 1;
/// The target file could not be opened.
pub const KTX_FILE_OPEN_FAILED: ktx_error_code_e = KTX_FILE_ISPIPE + 1;
/// The operation would exceed the max file size.
pub const KTX_FILE_OVERFLOW: ktx_error_code_e = KTX_FILE_OPEN_FAILED + 1;
/// An error occurred while reading from the file.
pub const KTX_FILE_READ_ERROR: ktx_error_code_e = KTX_FILE_OVERFLOW + 1;
/// An error occurred while seeking in the file.
pub const KTX_FILE_SEEK_ERROR: ktx_error_code_e = KTX_FILE_READ_ERROR + 1;
/// File does not have enough data to satisfy request.
pub const KTX_FILE_UNEXPECTED_EOF: ktx_error_code_e = KTX_FILE_SEEK_ERROR + 1;
/// An error occurred while writing to the file.
pub const KTX_FILE_WRITE_ERROR: ktx_error_code_e = KTX_FILE_UNEXPECTED_EOF + 1;
/// GL operations resulted in an error.
pub const KTX_GL_ERROR: ktx_error_code_e = KTX_FILE_WRITE_ERROR + 1;
/// The operation is not allowed in the current state.
pub const KTX_INVALID_OPERATION: ktx_error_code_e = KTX_GL_ERROR + 1;
/// A parameter value was not valid.
pub const KTX_INVALID_VALUE: ktx_error_code_e = KTX_INVALID_OPERATION + 1;
/// Requested metadata key or required dynamically loaded GPU function was not found.
pub const KTX_NOT_FOUND: ktx_error_code_e = KTX_INVALID_VALUE + 1;
/// Not enough memory to complete the operation.
pub const KTX_OUT_OF_MEMORY: ktx_error_code_e = KTX_NOT_FOUND + 1;
/// Transcoding of block compressed texture failed.
pub const KTX_TRANSCODE_FAILED: ktx_error_code_e = KTX_OUT_OF_MEMORY + 1;
/// The file not a KTX file
pub const KTX_UNKNOWN_FILE_FORMAT: ktx_error_code_e = KTX_TRANSCODE_FAILED + 1;
/// The KTX file specifies an unsupported texture type.
pub const KTX_UNSUPPORTED_TEXTURE_TYPE: ktx_error_code_e = KTX_UNKNOWN_FILE_FORMAT + 1;
/// Feature not included in in-use library or not yet implemented.
pub const KTX_UNSUPPORTED_FEATURE: ktx_error_code_e = KTX_UNSUPPORTED_TEXTURE_TYPE + 1;
/// Library dependency (OpenGL or Vulkan) not linked int application.
pub const KTX_LIBRARY_NOT_LINKED: ktx_error_code_e = KTX_UNSUPPORTED_FEATURE + 1;
/// Decompressed byte count does not match expected byte size
pub const KTX_DECOMPRESS_LENGTH_ERROR: ktx_error_code_e = KTX_LIBRARY_NOT_LINKED + 1;
/// Checksum mismatch when decompressing
pub const KTX_DECOMPRESS_CHECKSUM_ERROR: ktx_error_code_e = KTX_DECOMPRESS_LENGTH_ERROR + 1;

/// Result codes returned by library functions.
pub type ktxResult = ktx_error_code_e;

#[repr(C)]
pub struct ktxKVListEntry(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

/// Opaque handle to a ktxHashList.
pub type ktxHashList = *mut ktxKVListEntry;

/// Opaque handle to an entry in a [`ktxHashList`].
pub type ktxHashListEntry = ktxKVListEntry;

pub type ktxOrientationX = core::ffi::c_int;
pub const KTX_ORIENT_X_LEFT: ktxOrientationX = 'l' as _;
pub const KTX_ORIENT_X_RIGHT: ktxOrientationX = 'r' as _;

pub type ktxOrientationY = core::ffi::c_int;
pub const KTX_ORIENT_Y_UP: ktxOrientationY = 'u' as _;
pub const KTX_ORIENT_Y_DOWN: ktxOrientationY = 'd' as _;

pub type ktxOrientationZ = core::ffi::c_int;
pub const KTX_ORIENT_Z_IN: ktxOrientationZ = 'i' as _;
pub const KTX_ORIENT_Z_OUT: ktxOrientationZ = 'o' as _;

pub type class_id = core::ffi::c_int;
#[allow(non_upper_case_globals)]
pub const ktxTexture1_c: class_id = 1;
#[allow(non_upper_case_globals)]
pub const ktxTexture2_c: class_id = 2;

/// Struct describing the logical orientation of an image.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ktxOrientation {
    /// Orientation in X
    pub x: ktxOrientationX,
    /// Orientation in Y
    pub y: ktxOrientationY,
    /// Orientation in Z
    pub z: ktxOrientationZ,
}

/// Base class representing a texture.
///
/// ktxTextures should be created only by one of the provided functions and these fields should be considered read-only.
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxTexture {
    /// Identify the class type.
    ///
    /// Since there are no public ktxTexture constructors, this can only have values of ktxTexture1_c or ktxTexture2_c.
    pub classId: class_id,
    /// Pointer to the class's vtable.
    pub vtbl: *mut ktxTexture_vtbl,
    /// Pointer to the class's vtable for Vulkan functions.
    pub vvtbl: *mut ktxTexture_vvtbl,
    /// Opaque pointer to the class's protected variables.
    _protected: *mut core::ffi::c_void,
    /// `true` if the texture is an array texture, i.e, a GL_TEXTURE_*_ARRAY target is to be used.
    pub isArray: bool,
    /// `true` if the texture is a cubemap or cubemap array.
    pub isCubemap: bool,
    /// `true` if the texture's format is a block compressed format.
    pub isCompressed: bool,
    /// `true` if mipmaps should be generated for the texture by ktxTexture_GLUpload() or ktxTexture_VkUpload().
    pub generateMipmaps: bool,
    /// Width of the texture's base level.
    pub baseWidth: u32,
    /// Height of the texture's base level.
    pub baseHeight: u32,
    /// Depth of the texture's base level.
    pub baseDepth: u32,
    /// Number of dimensions in the texture: 1, 2, or 3.
    pub numDimensions: u32,
    /// Number of mip levels in the texture.
    ///
    /// Must be 1, if [`ktxTexture::generateMipmaps`] is `true`.
    /// Can be less than a full pyramid but always starts at the base level.
    pub numLevels: u32,
    /// Number of array layers in the texture.
    pub numLayers: u32,
    /// Number of faces: 6 for cube maps, 1 otherwise.
    pub numFaces: u32,
    /// Describes the logical orientation of the images in each dimension.
    ///
    /// [`ktxOrientationX`] for X, [`ktxOrientationY`] for Y and [`ktxOrientationZ`] for Z.
    pub orientation: ktxOrientation,
    /// Head of the hash list of metadata.
    pub kvDataHead: ktxHashList,
    /// Length of the metadata, if it has been extracted in its raw form, otherwise 0.
    pub kvDataLen: u32,
    /// Pointer to the metadata, if it has been extracted in its raw form, otherwise NULL.
    pub kvData: *mut u8,
    /// Byte length of the texture's uncompressed image data.
    pub dataSize: usize,
    /// Pointer to the start of the image data.
    pub pData: *mut u8,
}

/// Signature of function called by the `ktxTexture_Iterate*` functions to receive image data.
///
/// The function parameters are used to pass values which change for each image.
/// Obtain values which are uniform across all images from the [`ktxTexture`] object.
///
/// # Arguments
///
/// * `miplevel`: MIP level from 0 to the max level which is dependent on the texture size.
/// * `face`: usually 0; for cube maps, one of the 6 cube faces in the order +X, -X, +Y, -Y, +Z, -Z, 0 to 5.
/// * `width`: width of the image.
/// * `height`: height of the image or, for 1D textures, 1.
/// * `depth`: depth of the image or, for 1D & 2D textures, 1.
/// * `faceLodSize`: number of bytes of data pointed at by `pixels`.
/// * `pixels`: pointer to the image data.
/// * `userdata`: pointer for the application to pass data to and from the callback function.
pub type PFNKTXITERCB = extern "C" fn(
    miplevel: core::ffi::c_int,
    face: core::ffi::c_int,
    width: core::ffi::c_int,
    height: core::ffi::c_int,
    depth: core::ffi::c_int,
    faceLodSize: u64,
    pixels: *mut core::ffi::c_void,
    userdata: *mut core::ffi::c_void,
) -> ktx_error_code_e;
pub type PFNKTEXDESTROY = unsafe extern "system" fn(this: *mut ktxTexture);
pub type PFNKTEXGETIMAGEOFFSET = unsafe extern "system" fn(
    this: *mut ktxTexture,
    level: u32,
    layer: u32,
    faceSlice: u32,
    pOffset: *mut usize,
) -> ktx_error_code_e;
pub type PFNKTEXGETDATASIZEUNCOMPRESSED = unsafe extern "system" fn(this: *mut ktxTexture) -> usize;
pub type PFNKTEXGETIMAGESIZE =
    unsafe extern "system" fn(this: *mut ktxTexture, level: u32) -> usize;
pub type PFNKTEXGETLEVELSIZE =
    unsafe extern "system" fn(this: *mut ktxTexture, level: u32) -> usize;
pub type PFNKTEXITERATELEVELS = unsafe extern "system" fn(
    this: *mut ktxTexture,
    iterCb: PFNKTXITERCB,
    userdata: *mut core::ffi::c_void,
) -> ktx_error_code_e;
pub type PFNKTEXITERATELOADLEVELFACES = unsafe extern "system" fn(
    this: *mut ktxTexture,
    iterCb: PFNKTXITERCB,
    userdata: *mut core::ffi::c_void,
) -> ktx_error_code_e;
pub type PFNKTEXLOADIMAGEDATA = unsafe extern "system" fn(
    this: *mut ktxTexture,
    pBuffer: *mut u8,
    bufSize: usize,
) -> ktx_error_code_e;
pub type PFNKTEXNEEDSTRANSCODING = unsafe extern "system" fn(this: *mut ktxTexture) -> bool;
pub type PFNKTEXSETIMAGEFROMMEMORY = unsafe extern "system" fn(
    this: *mut ktxTexture,
    level: u32,
    layer: u32,
    faceSlice: u32,
    src: *const u8,
    srcSize: usize,
) -> ktx_error_code_e;
pub type PFNKTEXSETIMAGEFROMSTDIOSTREAM = unsafe extern "system" fn(
    this: *mut ktxTexture,
    level: u32,
    layer: u32,
    faceSlice: u32,
    src: *mut core::ffi::c_void, /* FILE */
    srcSize: usize,
) -> ktx_error_code_e;
pub type PFNKTEXWRITETOSTDIOSTREAM = unsafe extern "system" fn(
    this: *mut ktxTexture,
    dstsstr: *mut core::ffi::c_void, /* FILE */
) -> ktx_error_code_e;
pub type PFNKTEXWRITETONAMEDFILE = unsafe extern "system" fn(
    this: *mut ktxTexture,
    dstname: *const core::ffi::c_char,
) -> ktx_error_code_e;
pub type PFNKTEXWRITETOMEMORY = unsafe extern "system" fn(
    this: *mut ktxTexture,
    bytes: *mut *mut u8,
    size: *mut usize,
) -> ktx_error_code_e;
pub type PFNKTEXWRITETOSTREAM =
    unsafe extern "system" fn(this: *mut ktxTexture, dststr: *mut ktxStream) -> ktx_error_code_e;

/// Table of virtual ktxTexture methods.
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxTexture_vtbl {
    pub Destroy: PFNKTEXDESTROY,
    pub GetImageOffset: PFNKTEXGETIMAGEOFFSET,
    pub GetDataSizeUncompressed: PFNKTEXGETDATASIZEUNCOMPRESSED,
    pub GetImageSize: PFNKTEXGETIMAGESIZE,
    pub GetLevelSize: PFNKTEXGETLEVELSIZE,
    pub IterateLevels: PFNKTEXITERATELEVELS,
    pub IterateLoadLevelFaces: PFNKTEXITERATELOADLEVELFACES,
    pub NeedsTranscoding: PFNKTEXNEEDSTRANSCODING,
    pub LoadImageData: PFNKTEXLOADIMAGEDATA,
    pub SetImageFromMemory: PFNKTEXSETIMAGEFROMMEMORY,
    pub SetImageFromStdioStream: PFNKTEXSETIMAGEFROMSTDIOSTREAM,
    pub WriteToStdioStream: PFNKTEXWRITETOSTDIOSTREAM,
    pub WriteToNamedFile: PFNKTEXWRITETONAMEDFILE,
    pub WriteToMemory: PFNKTEXWRITETOMEMORY,
    pub WriteToStream: PFNKTEXWRITETOSTREAM,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct ktxTexture_vvtbl(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

/// Class representing a KTX version 1 format texture.
///
/// ktxTextures should be created only by one of the `ktxTexture_Create*`
/// functions and these fields should be considered read-only.
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxTexture1 {
    pub base: ktxTexture,
    /// Format of the texture data, e.g., GL_RGB.
    pub glFormat: u32,
    /// Internal format of the texture data, e.g., GL_RGB8.
    pub glInternalformat: u32,
    /// Base format of the texture data, e.g., GL_RGB.
    pub glBaseInternalfromat: u32,
    /// Type of the texture data, e.g, GL_UNSIGNED_BYTE.
    pub glType: u32,
    /// Private data.
    _private: *mut core::ffi::c_void,
}

/// Enumerators identifying the supercompression scheme.
pub type ktxSupercmpScheme = core::ffi::c_int;
/// No supercompression.
pub const KTX_SS_NONE: ktxSupercmpScheme = 0;
/// Basis LZ supercompression.
pub const KTX_SS_BASIS_LZ: ktxSupercmpScheme = 1;
/// ZStd supercompression.
pub const KTX_SS_ZSTD: ktxSupercmpScheme = 2;
/// ZLIB supercompression.
pub const KTX_SS_ZLIB: ktxSupercmpScheme = 3;

/// Class representing a KTX version 2 format texture.
///
/// ktxTextures should be created only by one of the `ktxTexture_Create*`
/// functions and these fields should be considered read-only.
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxTexture2 {
    pub base: ktxTexture,
    pub vkFormat: u32,
    pub pDfd: *mut u32,
    pub supercompressionScheme: ktxSupercmpScheme,
    pub isVideo: bool,
    pub duration: u32,
    pub timescale: u32,
    pub loopcount: u32,
    /// Private data.
    _private: *mut core::ffi::c_void,
}

/// Structure for passing texture information to [`ktxTexture1_Create`] and [`ktxTexture2_Create`].
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxTextureCreateInfo {
    /// Internal fromat for the texture, e.g., GL_RGB8. Ignores when creating a `ktxTexture2`.
    pub glInternalformat: u32,
    /// VkFormat for texture. Ignored when creating a `ktxTexture1`.
    pub vkFormat: u32,
    /// Pointer to DFD. Used only when creating a `ktxTexture2` and only if vkFormat is `VK_FORMAT_UNDEFINED`.
    pub pDfd: *mut u32,
    /// Width of the base level of the texture.
    pub baseWidth: u32,
    /// Height of the base level of the texture.
    pub baseHeight: u32,
    /// Depth of the base level of the texture.
    pub baseDepth: u32,
    /// Number of dimensions in the texture, 1, 2 or 3.
    pub numDimensions: u32,
    /// Number of mip levels in the texture. Should be 1 if [`ktxTextureCreateInfo::generateMipmaps`] is `true`.
    pub numLevels: u32,
    /// Number of array layers in the texture.
    pub numLayers: u32,
    /// Number of faces: 6 for cube maps, 1 otherwise.
    pub numFaces: u32,
    /// Set to `true` if the texture is to be an array texture. Means OpenGL will use a `GL_TEXTURE_*_ARRAY` targets.
    pub isArray: bool,
    /// Set to `true` if mipmaps should be generated for the texture when loading into a 3D API.
    pub generateMipmaps: bool,
}

/// Enum for requesting, or not, allocation of storage for images.
pub type ktxTextureCreateStorageEnum = core::ffi::c_int;
/// Don't allocate any image storage.
pub const KTX_TEXTURE_CREATE_NO_STORAGE: ktxTextureCreateStorageEnum = 0;
/// Allocate image storage.
pub const KTX_TEXTURE_CREATE_ALLOC_STORAGE: ktxTextureCreateStorageEnum = 1;

/// Flags for requesting services during creation.
pub type ktxTextureCreateFlagBits = u32;
pub const KTX_TEXTURE_CREATE_NO_FLAGS: ktxTextureCreateFlagBits = 0x00;
/// Load the images from the KTX source.
pub const KTX_TEXTURE_CREATE_LOAD_IMAGE_DATA_BIT: ktxTextureCreateFlagBits = 0x01;
/// Load the raw key-value data instead of creating a [`ktxHashList`] from it.
pub const KTX_TEXTURE_CREATE_RAW_KVDATA_BIT: ktxTextureCreateFlagBits = 0x02;
/// Skip any key-value data. This overrides the [`KTX_TEXTURE_CREATE_RAW_KVDATA_BIT`].
pub const KTX_TEXTURE_CREATE_SKIP_KVDATA_BIT: ktxTextureCreateFlagBits = 0x04;
/// Load texture compatible with the rules of `KHR_texture_basisu` glTF extension.
pub const KTX_TEXTURE_CREATE_CHECK_GLTF_BASISU_BIT: ktxTextureCreateFlagBits = 0x08;

/// Type for TextureCreateFlags parameters.
pub type ktxTextureCreateFlags = u32;

pub type ktx_off_t = i64; /* off_t in c */

#[repr(C)]
pub struct ktxMem(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

pub type streamType = core::ffi::c_int;
#[allow(non_upper_case_globals)]
pub const eStreamTypeFile: streamType = 1;
#[allow(non_upper_case_globals)]
pub const eStreamTypeMemory: streamType = 2;
#[allow(non_upper_case_globals)]
pub const eStreamTypeCustom: streamType = 3;

/// type for a pointer to a stream reading function
pub type ktxStream_read = extern "C" fn(
    str: *mut ktxStream,
    dst: *mut core::ffi::c_void,
    count: usize,
) -> ktx_error_code_e;
/// type for a pointer to a stream skipping function
pub type ktxStream_skip = extern "C" fn(str: *mut ktxStream, count: usize) -> ktx_error_code_e;
/// type for a pointer to a stream writing function
pub type ktxStream_write = extern "C" fn(
    str: *mut ktxStream,
    src: *const core::ffi::c_void,
    size: usize,
    count: usize,
) -> ktx_error_code_e;
/// type for a pointer to a stream position query function
pub type ktxStream_getpos =
    extern "C" fn(str: *mut ktxStream, offset: *mut ktx_off_t) -> ktx_error_code_e;
/// type for a pointer to a stream position query function
pub type ktxStream_setpos =
    extern "C" fn(str: *mut ktxStream, offset: ktx_off_t) -> ktx_error_code_e;
/// type for a pointer to a stream size query function
pub type ktxStream_getsize =
    extern "C" fn(str: *mut ktxStream, size: *mut usize) -> ktx_error_code_e;
/// Destruct a stream
pub type ktxStream_destruct = extern "C" fn(str: *mut ktxStream);

/// Interface of ktxStream.
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxStream {
    /// pointer to function for reading bytes.
    pub read: ktxStream_read,
    /// pointer to function for skipping bytes.
    pub skip: ktxStream_skip,
    /// pointer to function for writing bytes.
    pub write: ktxStream_write,
    /// pointer to function for getting current position in stream.
    pub getpos: ktxStream_getpos,
    /// pointer to function for setting current position in stream.
    pub setpos: ktxStream_setpos,
    /// pointer to function for querying size.
    pub getsize: ktxStream_getsize,
    /// destruct the stream.
    pub destruct: ktxStream_destruct,
    pub r#type: streamType,
    pub data: ktxStreamData,
    pub readpos: ktx_off_t,
    pub closeOnDestruct: bool,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub union ktxStreamData {
    /// a stdio FILE pointer for a `ktxFileStream`.
    pub file: *mut core::ffi::c_void, /* FILE */
    /// a pointer to a [`ktxMem`] struct for a `ktxMemStream`.
    pub mem: *mut ktxMem,
    /// pointer to a struct for custom streams.
    pub custom_ptr: ktxStreamDataCustomPtr,
}
impl core::fmt::Debug for ktxStreamData {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<union ktxStreamData...>")
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
#[allow(non_snake_case)]
pub struct ktxStreamDataCustomPtr {
    /// pointer to the data.
    pub address: *mut core::ffi::c_void,
    /// pointer to a memory allocator.
    pub allocatorAddress: *mut core::ffi::c_void,
    /// size of the data.
    pub size: usize,
}

/// typedef of function pointer returned by `GLGetProcAddress` functions.
pub type PFNVOIDFUNCTION = extern "system" fn();
/// typedef of pointer to function for retrieving OpenGL function pointers.
pub type PFNGLGETPROCADDRESS =
    extern "system" fn(proc: *const core::ffi::c_char) -> Option<PFNVOIDFUNCTION>;

unsafe extern "system" {
    /// Load pointers for the OpenGL functions needed by [`ktxTexture_GLUpload`].
    pub fn ktxLoadOpenGL(pfnGLGetProcAddress: PFNGLGETPROCADDRESS) -> ktx_error_code_e;

    pub fn ktxTexture_CreateFromStdioStream(
        stdioStream: *mut core::ffi::c_void, /* FILE */
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture,
    ) -> ktx_error_code_e;
    pub fn ktxTexture_CreateFromNamedFile(
        filename: *const core::ffi::c_char,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture,
    ) -> ktx_error_code_e;
    pub fn ktxTexture_CreateFromMemory(
        bytes: *const u8,
        size: usize,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture,
    ) -> ktx_error_code_e;
    pub fn ktxTexture_CreateFromStream(
        stream: *mut ktxStream,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture,
    ) -> ktx_error_code_e;

    /// Returns a pointer to the image data of a [`ktxTexture`] object.
    pub fn ktxTexture_GetData(this: *mut ktxTexture) -> *mut u8;
    /// Returns the pitch of a row of an image at the specified level.
    /// Similar to the rowPitch in a `VkSubResourceLayout`.
    pub fn ktxTexture_GetRowPitch(this: *mut ktxTexture, level: u32) -> u32;
    /// Return the element size of the texture's images.
    pub fn ktxTexture_GetElementSize(this: *mut ktxTexture) -> u32;
    /// Returns the size of all the image data of a [`ktxTexture`] object in bytes.
    pub fn ktxTexture_GetDataSize(this: *mut ktxTexture) -> usize;
    /// Uploads a texture to OpenGL {,ES}.
    pub fn ktxTexture_GLUpload(
        this: *mut ktxTexture,
        pTexture: *mut core::ffi::c_uint, /* GLuint */
        pTarget: *mut core::ffi::c_uint,  /* GLenum */
        pGlerror: *mut core::ffi::c_uint, /* GLenum */
    ) -> ktx_error_code_e;
    /// Iterate over the levels or faces in a [`ktxTexture`] object.
    pub fn ktxTexture_IterateLevelFaces(
        this: *mut ktxTexture,
        iterCb: PFNKTXITERCB,
        userdata: *mut core::ffi::c_void,
    ) -> ktx_error_code_e;

    /// CCreate a new [`ktxTexture1`].
    pub fn ktxTexture1_Create(
        createInfo: *const ktxTextureCreateInfo,
        storageAllocation: ktxTextureCreateStorageEnum,
        newTex: *mut *mut ktxTexture1,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_CreateFromStdioStream(
        stdioStream: *mut core::ffi::c_void, /* FILE */
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture1,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_CreateFromNamedFile(
        filename: *const core::ffi::c_char,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture1,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_CreateFromMemory(
        bytes: *const u8,
        size: usize,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture1,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_CreateFromStream(
        stream: *mut ktxStream,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture1,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_Destroy(this: *mut ktxTexture1);
    pub fn ktxTexture1_NeedsTranscoding(this: *mut ktxTexture1) -> bool;
    pub fn ktxTexture1_LoadImageData(
        this: *mut ktxTexture1,
        pBuffer: *mut u8,
        bufSize: usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteToStdioStream(
        this: *mut ktxTexture1,
        dstsstr: *mut core::ffi::c_void, /* FILE */
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteToNamedFile(
        this: *mut ktxTexture1,
        dstname: *const core::ffi::c_char,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteToMemory(
        this: *mut ktxTexture1,
        bytes: *mut *mut u8,
        size: *mut usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteToStream(
        this: *mut ktxTexture1,
        dststr: *mut ktxStream,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteKTX2ToStdioStream(
        this: *mut ktxTexture1,
        dstsstr: *mut core::ffi::c_void, /* FILE */
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteKTX2ToNamedFile(
        this: *mut ktxTexture1,
        dstname: *const core::ffi::c_char,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteKTX2ToMemory(
        this: *mut ktxTexture1,
        bytes: *mut *mut u8,
        size: *mut usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture1_WriteKTX2ToStream(
        this: *mut ktxTexture1,
        dststr: *mut ktxStream,
    ) -> ktx_error_code_e;

    /// Create a new [`ktxTexture2`].
    pub fn ktxTexture2_Create(
        createInfo: *const ktxTextureCreateInfo,
        storageAllocation: ktxTextureCreateStorageEnum,
        newTex: *mut *mut ktxTexture2,
    ) -> ktx_error_code_e;
    /// Create a new [`ktxTexture2`] as a copy of an existing texture.
    pub fn ktxTexture2_CreateCopy(
        orig: *mut ktxTexture2,
        newTex: *mut *mut ktxTexture2,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_CreateFromStdioStream(
        stdioStream: *mut core::ffi::c_void, /* FILE */
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture2,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_CreateFromNamedFile(
        filename: *const core::ffi::c_char,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture2,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_CreateFromMemory(
        bytes: *const u8,
        size: usize,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture2,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_CreateFromStream(
        stream: *mut ktxStream,
        createFlags: ktxTextureCreateFlags,
        newTex: *mut *mut ktxTexture2,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_Destroy(this: *mut ktxTexture2);
    pub fn ktxTexture2_CompressBasis(this: *mut ktxTexture2, quality: u32) -> ktx_error_code_e;
    pub fn ktxTexture2_DeflateZstd(this: *mut ktxTexture2, level: u32) -> ktx_error_code_e;
    pub fn ktxTexture2_DeflateZLIB(this: *mut ktxTexture2, level: u32) -> ktx_error_code_e;
    pub fn ktxTexture2_GetComponentInfo(
        this: *mut ktxTexture2,
        numComponents: *mut u32,
        componentByteLength: *mut u32,
    );
    pub fn ktxTexture2_GetImageOffset(
        this: *mut ktxTexture2,
        level: u32,
        layer: u32,
        faceSlice: u32,
        pOffset: *mut usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_GetNumComponents(this: *mut ktxTexture2) -> u32;
    pub fn ktxTexture2_NeedsTranscoding(this: *mut ktxTexture2) -> bool;
    pub fn ktxTexture2_LoadImageData(
        this: *mut ktxTexture2,
        pBuffer: *mut u8,
        bufSize: usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_LoadDeflatedImageData(
        this: *mut ktxTexture2,
        pBuffer: *mut u8,
        bufSize: usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_WriteToStdioStream(
        this: *mut ktxTexture2,
        dstsstr: *mut core::ffi::c_void, /* FILE */
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_WriteToNamedFile(
        this: *mut ktxTexture2,
        dstname: *const core::ffi::c_char,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_WriteToMemory(
        this: *mut ktxTexture2,
        bytes: *mut *mut u8,
        size: *mut usize,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_WriteToStream(
        this: *mut ktxTexture2,
        dststr: *mut ktxStream,
    ) -> ktx_error_code_e;
}

/// flags specifying UASTC encoding options.
pub type ktx_pack_uastc_flag_bits_e = u32;
/// Fastest compression. 43.45dB.
pub const KTX_PACK_UASTC_LEVEL_FASTEST: ktx_pack_uastc_flag_bits_e = 0;
/// Faster compression. 46.49dB.
pub const KTX_PACK_UASTC_LEVEL_FASTER: ktx_pack_uastc_flag_bits_e = 1;
/// Default compression. 47.47dB.
pub const KTX_PACK_UASTC_LEVEL_DEFAULT: ktx_pack_uastc_flag_bits_e = 2;
/// Slower compression. 48.01dB.
pub const KTX_PACK_UASTC_LEVEL_SLOWER: ktx_pack_uastc_flag_bits_e = 3;
/// Very slow compression. 48.24dB.
pub const KTX_PACK_UASTC_LEVEL_VERYSLOW: ktx_pack_uastc_flag_bits_e = 4;
/// Mas to extract the level from the other bits.
pub const KTX_PACK_UASTC_LEVEL_MAXK: ktx_pack_uastc_flag_bits_e = 0x0f;
/// Optimize for lowest UASTC error.
pub const KTX_PACK_UASTC_FAVOR_UASTC_ERROR: ktx_pack_uastc_flag_bits_e = 8;
/// Optimize for lowest BC7 error.
pub const KTX_PACK_UASTC_FAVOR_BC7_ERROR: ktx_pack_uastc_flag_bits_e = 16;
/// Optimize for faster transcoding to ETC1.
pub const KTX_PACK_UASTC_ETC1_FASTER_HINTS: ktx_pack_uastc_flag_bits_e = 64;
/// Optimize for fastest transcoding to ETC1.
pub const KTX_PACK_UASTC_ETC1_FASTEST_HINTS: ktx_pack_uastc_flag_bits_e = 128;
/// Not documented in BasisU code.
pub const KTX_PACK_UASTC_ETC1_DISABLE_FLIP_AND_INDIVIDUAL: ktx_pack_uastc_flag_bits_e = 256;

pub type ktx_pack_uastc_flags = u32;

/// Options specifying ASTC encoding quality levels.
pub type ktx_pack_astc_quality_levels_e = u32;
/// Fastest compression.
pub const KTX_PACK_ASTC_QUALITY_LEVEL_FASTEST: ktx_pack_astc_quality_levels_e = 0;
/// Fast compression.
pub const KTX_PACK_ASTC_QUALITY_LEVEL_FAST: ktx_pack_astc_quality_levels_e = 10;
/// Medium compression.
pub const KTX_PACK_ASTC_QUALITY_LEVEL_MEDIUM: ktx_pack_astc_quality_levels_e = 60;
/// Slower compression.
pub const KTX_PACK_ASTC_QUALITY_LEVEL_THOROUGH: ktx_pack_astc_quality_levels_e = 98;
/// Very slow compression.
pub const KTX_PACK_ASTC_QUALITY_LEVEL_EXHAUSTIVE: ktx_pack_astc_quality_levels_e = 100;

/// Options specifying ASTC encoding block dimensions
pub type ktx_pack_astc_block_dimension_e = u32;
/// 8.00 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_4x4: ktx_pack_astc_block_dimension_e = 0;
/// 6.40 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_5x4: ktx_pack_astc_block_dimension_e = 1;
/// 5.12 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_5x5: ktx_pack_astc_block_dimension_e = 2;
/// 4.27 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_6x5: ktx_pack_astc_block_dimension_e = 3;
/// 3.56 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_6x6: ktx_pack_astc_block_dimension_e = 4;
/// 3.20 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_8x5: ktx_pack_astc_block_dimension_e = 5;
/// 2.67 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_8x6: ktx_pack_astc_block_dimension_e = 6;
/// 2.56 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_10x5: ktx_pack_astc_block_dimension_e = 7;
/// 2.13 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_10x6: ktx_pack_astc_block_dimension_e = 8;
/// 2.00 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_8x8: ktx_pack_astc_block_dimension_e = 9;
/// 1.60 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_10x8: ktx_pack_astc_block_dimension_e = 10;
/// 1.28 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_10x10: ktx_pack_astc_block_dimension_e = 11;
/// 1.07 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_12x10: ktx_pack_astc_block_dimension_e = 12;
/// 0.89 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_12x12: ktx_pack_astc_block_dimension_e = 13;
/// 4.74 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_3x3x3: ktx_pack_astc_block_dimension_e = 14;
/// 3.56 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_4x3x3: ktx_pack_astc_block_dimension_e = 15;
/// 2.67 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_4x4x3: ktx_pack_astc_block_dimension_e = 16;
/// 2.00 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_4x4x4: ktx_pack_astc_block_dimension_e = 17;
/// 1.60 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_5x4x4: ktx_pack_astc_block_dimension_e = 18;
/// 1.28 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_5x5x4: ktx_pack_astc_block_dimension_e = 19;
/// 1.02 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_5x5x5: ktx_pack_astc_block_dimension_e = 20;
/// 0.85 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_6x5x5: ktx_pack_astc_block_dimension_e = 21;
/// 0.71 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_6x6x5: ktx_pack_astc_block_dimension_e = 22;
/// 0.59 bpp
#[allow(non_upper_case_globals)]
pub const KTX_PACK_ASTC_BLOCK_DIMENSION_6x6x6: ktx_pack_astc_block_dimension_e = 23;

/// Options specifying ASTC encoder mode.
pub type ktx_pack_astc_encoder_mode_e = u32;
/// Selects LDR mode if component size is <= 8-bit, HDR otherwise.
pub const KTX_PACK_ASTC_ENCODER_MODE_DEFAULT: ktx_pack_astc_encoder_mode_e = 0;
/// Always encode in low dynamic range mode.
pub const KTX_PACK_ASTC_ENCODER_MODE_LDR: ktx_pack_astc_encoder_mode_e = 1;
/// Always encode in high dynamic range mode.
pub const KTX_PACK_ASTC_ENCODER_MODE_HDR: ktx_pack_astc_encoder_mode_e = 2;

/// Structure for passing extended parameters to [`ktxTexture_CompressAstcEx`].
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxAstcParams {
    /// Size of this struct. Used so library can tell which version of struct is begin passed.
    pub structSize: u32,
    /// If `true`, prints Astc encoder operation details to `stdout`. No recommended for GUI apps.
    pub verbose: bool,
    /// Number of threads used for compression. Default is 1.
    pub threadCount: u32,
    /// Combinations of block dimensions that astcenc supports i.e. 6x6, 8x8, 6x5 etc
    pub blockDimension: u32,
    /// Can be {ldr/hdr} from astcenc
    pub mode: u32,
    /// astcenc supports -fastest, -fast, -medium, -thorough, -exhaustive
    pub qualityLevel: u32,
    /// Tunes codec parameters for better quality on normal maps
    ///
    /// In this mode normals are compressed to X,Y components discarding Z component,
    /// render will need to generate Z component in shaders.
    pub normalMap: bool,
    /// The codec should optimize for perceptual error, instead of direct RMS error.
    /// This aims to improves perceived image quality, but typically lowers the measured PSNR score.
    /// Perceptual methods are currently only available for normal maps and RGB color data.
    pub perceptual: bool,
    /// A swizzle to provide as input to astcenc. It must match the regular expression /^[rgba01]{4}$/.
    pub inputSwizzle: [core::ffi::c_char; 4],
}

// TODO: kind = dylibにしないとWindowsだとグローバル定数のリンクがうまくいかない
// Androidはsoコピーしないといけないのが面倒なのでstatic linkで対処 あと、libc++を明示的にリンクしてあげないといけないらしい
#[cfg_attr(windows, link(name = "ktx", kind = "dylib"))]
#[cfg_attr(target_os = "android", link(name = "ktx", kind = "static"))]
#[cfg_attr(not(any(windows, target_os = "android")), link(name = "ktx"))]
#[cfg_attr(target_os = "android", link(name = "c++_shared"))]
unsafe extern "C" {
    pub static KTX_ETC1S_DEFAULT_COMPRESSION_LEVEL: u32;
}

unsafe extern "system" {
    pub fn ktxTexture2_CompressAstcEx(
        this: *mut ktxTexture2,
        params: *mut ktxAstcParams,
    ) -> ktx_error_code_e;
    pub fn ktxTexture2_CompressAstc(this: *mut ktxTexture2, quality: u32) -> ktx_error_code_e;
    pub fn ktxTexture2_DecodeAstc(this: *mut ktxTexture2) -> ktx_error_code_e;
}

/// Structure for passing extended parameters to [`ktxTexture2_CompressBasisEx`].
#[repr(C)]
#[derive(Debug, Clone)]
#[allow(non_snake_case)]
pub struct ktxBasisParams {
    /// Size of this struct. Used so library can tell which version of struct is being passed.
    pub structSize: u32,
    /// `true` to use UASTC base, `false` to use ETC1S base.
    pub uastc: bool,
    /// If `true`, prints Basis Universal encoder operation details to `stdout`. Not recommended for GUI apps.
    pub verbose: bool,
    /// `true` to forbid use of the SSE instruction set. Ignored if CPU does not support SSE.
    pub noSSE: bool,
    /// Number of threads used for compression. Default is 1.
    pub threadCount: u32,
    /// Encoding speed vs. quality tradeoff.
    /// Range is `[0,6]`. Higher values are much slower, but give slightly higher quality.
    /// Higher levels are intended for video. There is not default. Callers must explicitly set this value.
    /// Callers can use [`KTX_ETC1S_DEFAULT_COMPRESSION_LEVEL`] as default value.
    /// Currently this is 2.
    pub compressionLevel: u32,
    /// Compression quality. Range is `[1,255]`. Lower gives better compression/lower quality/faster.
    /// Higher gives less compression/higher quality/slower.
    /// This automatically determines values for `maxEndpoints`, `maxSelectors`, `endpointRDOThreshold` and `selectorRDOThreashold` for the target quality level.
    /// Setting these parameters overrides the values determined by `qualityLevel` which defaults to 128 if neither it nor both `maxEndpoints` and `maxSelectors` have been set.
    pub qualityLevel: u32,
    /// Manually set the max number of color endpoint clusters.
    /// Range if `[1,16128]`. Default is 0, unset. If this is set, maxSelectors must also be set,
    /// otherwise the value will be ignored.
    pub maxEndpoints: u32,
    /// Set endpoint RDO quality threshold. The default is 1.25.
    /// Lower is higher quality but less quality per output bit (try `[1.0,3.0]`).
    /// This will override the value chosen by [`qualityLevel`].
    pub endpointRDOThreshold: core::ffi::c_float,
    /// Manually set the max number of color selector clusters. Range is `[1,16128]`.
    /// Default is 0, unset. If this is set, maxEndpoints must also be set, otherwise the value will be ignored.
    pub maxSelectors: u32,
    /// Set selector RDO quality threshold. The default is 1.5.
    /// Lower is higher quality but less quality per output bit (try `[1.0,3.0]`).
    /// This will override the value chosen by [`qualityLevel`].
    pub selectorRDOThreshold: core::ffi::c_float,
    /// A swizzle to apply before encoding. It must match the regular expression /^[rgba01]{4}$/.
    /// If both this and [`preSwizzle`] are specified [`ktxTexture_CompressBasisEx`]
    /// will raise [`KTX_INVALID_OPERATION`]. Usable with both ETC1S and UASTC.
    pub inputSwizzle: [core::ffi::c_char; 4],
    /// Tunes codec parameters for better quality on normal maps (no selector RDO, no endpoint RDO) and
    /// sets the texture's DFD appropriately. Only value for linear textures.
    pub normalMap: bool,
    /// 2-component input have always been automatically separated using an "rrrg" inputSwizzle.
    #[deprecated = "This was and is a no-op."]
    pub separateRGToRGB_A: bool,
    /// If the texture has `KTXSwizzle` metadata, apply it before compression.
    /// Swizzling, like `rabb` may yield drastically different error metrics if done after supercompression.
    /// Usable for both ETC1S and UASTC.
    pub preSwizzle: bool,
    /// Disable endpoint rate distortion optimizations. Slightly faster, less noisy output,
    /// but lower quality per output bit.
    /// Default is `false`.
    pub noEndpointRDO: bool,
    /// Disable selector rate distortion optimizations. Slightly faster, less noisy output,
    /// but lower quality per output bit.
    /// Default is `false`.
    pub noSelectorRDO: bool,
    /// A set of [`ktx_pack_uastc_flag_bits_e`] controlling UASTC encoding.
    /// The most important value is the level given in the least-significant 4 bits which selects
    /// a speed vs quality tradeoff.
    pub uastcFlags: ktx_pack_uastc_flags,
    /// Enable Rate Distortion Optimization (RDO) post-processing.
    pub uastcRDO: bool,
    /// UASTC RDO quality scalar (lambda). Lower values yield higher quality/larger LZ compressed files,
    /// higher values yield lower quality/smaller LZ compressed files.
    /// A good range to try is `[.2,4]`. Full range is `[.001,50.0]`. Default is 1.0.
    pub uastcRDOQualityScalar: core::ffi::c_float,
    /// UASTC RDO dictionary size in bytes. Default is 4096.
    /// Lower values=faster, but give less compression. Range is `[64, 65536]`.
    pub uastcRDODictSize: u32,
    /// UASTC RDO max smooth block error scale. Range is `[1,300]`.
    /// Default is 10.0, 1.0 is disabled. Larger values supress more artifacts (and allocate more bits)
    /// on smooth blocks.
    pub uastcRDOMaxSmoothBlockErrorScale: core::ffi::c_float,
    /// UASTC RDO max smooth block standard deviation. Range is `[.01,65536.0]`.
    /// Default is 18.0. Larger values expand the range of blocks considered smooth.
    pub uastcRDOMaxSmoothBlockStdDev: core::ffi::c_float,
    /// Do not favor simpler UASTC modes in RDO mode.
    pub uastcRDODontFavorSimplerModes: bool,
    /// Disable RDO multithreading (slightly higher compression, deterministic).
    pub uastcRDONoMultithreading: bool,
}

unsafe extern "system" {
    pub fn ktxTexture2_CompressBasisEx(
        this: *mut ktxTexture2,
        params: *mut ktxBasisParams,
    ) -> ktx_error_code_e;
}

pub type ktx_transcode_fmt_e = core::ffi::c_int;
/// Opaque only. Returns RGB or alpha data, if [`KTX_TF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS`] flag is specified.
pub const KTX_TTF_ETC1_RGB: ktx_transcode_fmt_e = 0;
/// Opaque+alpha. EAC_A8 block followed by an ETC1 block.
/// The alpha channel will be opaque for textures without an alpha channel.
pub const KTX_TTF_ETC2_RGBA: ktx_transcode_fmt_e = 1;
/// Opaque only, no punchthrough alpha support yet.
/// Returns RGB or alpha data, if [`KTX_KF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS`] flag is specified.
pub const KTX_TTF_BC1_RGB: ktx_transcode_fmt_e = 2;
/// Opaque+alpha. BC4 block with alpha followed by a BC1 block.
/// The alpha channel will be opaque for textures without an alpha channel.
pub const KTX_TTF_BC3_RGBA: ktx_transcode_fmt_e = 3;
/// One BC4 block. R = opaque.g or alpha.g, if [`KTX_TF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS`] flag is specified.
pub const KTX_TTF_BC4_R: ktx_transcode_fmt_e = 4;
/// Two BC4 blocks, R=opaque.g and G=alpha.g The texture should have an alpha channel
/// (if not G will be all 255's). For tangent space normal maps.
pub const KTX_TTF_BC5_RG: ktx_transcode_fmt_e = 5;
/// RGB or RGBA mode 5 for ETC1S, modes 1, 2, 3, 4, 5, 6, 7 for UASTC.
pub const KTX_TTF_BC7_RGBA: ktx_transcode_fmt_e = 6;
/// Opaque only. Returns RGB or alpha data, if [`KTX_TF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS`] flag is specified.
pub const KTX_TTF_PVRTC1_4_RGB: ktx_transcode_fmt_e = 8;
/// Opaque+alpha. Most useful for simple opacity maps.
/// If the texture doesn't have an alpha channel [`KTX_TTF_PVRTC1_4_RGB`] will be used instead.
/// Lowest quality of any supported texture format.
pub const KTX_TTF_PVRTC1_4_RGBA: ktx_transcode_fmt_e = 9;
/// Opaque+alpha, ASTC 4x4. The alpha channel will be opaque for textures without an alpha channel.
/// The transcoder uses RGB/RGBA/L/LA modes, void extent, and up to two (`[0,47]` and `[0,255]`) endpoint precisions.
#[allow(non_upper_case_globals)]
pub const KTX_TTF_ASTC_4x4_RGBA: ktx_transcode_fmt_e = 10;
/// Opaque-only. Almost BC1 quality, much faster to transcode and supports arbitrary texture dimensions
/// (unlike PVRTC1_RGB).
pub const KTX_TTF_PVRTC2_4_RGB: ktx_transcode_fmt_e = 18;
/// Opaque+alpha. Slower to transcode that [`KTX_TTF_PVRTC2_4_RGB`].
/// Premultiplied alpha is highly recommended, otherwise the color channel can leak into the alpha channel
/// on transparent blocks.
pub const KTX_TTF_PVRTC2_4_RGBA: ktx_transcode_fmt_e = 19;
/// R only (ETC2 EAC R11 unsigned). R = opaque.g or alpha.g,
/// if [`KTX_TF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS`] flag is specified.
pub const KTX_TTF_ETC2_EAC_R11: ktx_transcode_fmt_e = 20;
/// RG only (ETC2 EAC RG11 unsigned). R=opaque.g, G=alpha.g.
/// The texture should have an alpha channel (if not G will be all 255's). For tangent space normal maps.
pub const KTX_TTF_ETC2_EAC_RG11: ktx_transcode_fmt_e = 21;
/// 32bpp RGBA image stored in raster (not block) order in memory, R is first byte, A is last byte.
pub const KTX_TTF_RGBA32: ktx_transcode_fmt_e = 13;
/// 16bpp RGB image stored in raster (not block) order in memory, R at bit position 11.
pub const KTX_TTF_RGB565: ktx_transcode_fmt_e = 14;
/// 16bpp RGB image stored in raster (non block) order in memory, R at bit position 0.
pub const KTX_TTF_BGR565: ktx_transcode_fmt_e = 15;
/// 16bpp RGBA image stored in raster (non block) order in memory,
/// R at bit position 12, A at bit position 0.
pub const KTX_TTF_RGBA4444: ktx_transcode_fmt_e = 16;
/// Automatically selects [`KTX_TTF_ETC1_RGB`] or [`KTX_TTF_ETC2_RGBA`] according to presence of alpha.
pub const KTX_TTF_ETC: ktx_transcode_fmt_e = 22;
/// Automatically selected [`KTX_TTF_BC1_RGB`] or [`KTX_TTF_BC3_RGBA`] according to presence of alpha.
pub const KTX_TTF_BC1_OR_3: ktx_transcode_fmt_e = 23;
pub const KTX_TTF_NOSELECTION: ktx_transcode_fmt_e = 0x7fffffff;

/// Flags guiding transcoding of Basis Universal compressed textures.
pub type ktx_transcode_flag_bits_e = u32;
/// PVRTC1: decode non-pow2 ETC1S texture level to the next larger power of 2
/// (not implemented yes, but we're going to support it).
/// Ignored if the slice's dimensions are already a power of 2.
pub const KTX_TF_PVRTC_DECODE_TO_NEXT_POW2: ktx_transcode_flag_bits_e = 2;
/// When decoding to an opaque texture format, if the Basis data has alpha,
/// decode the alpha slice instead of the color slice to the output texture format.
/// Has no effect if there is not alpha data.
pub const KTX_TF_TRANSCODE_ALPHA_DATA_TO_OPAQUE_FORMATS: ktx_transcode_flag_bits_e = 4;
/// Request higher quality transcode of UASTC to BC1, BC3, ETC2_EAC_R11 and ETC_EAC_RG11.
/// The flag is unused by other UASTC transcoders.
pub const KTX_TF_HIGH_QUALITY: ktx_transcode_flag_bits_e = 32;

pub type ktx_transcode_flags = u32;

unsafe extern "system" {
    pub fn ktxTexture2_TranscodeBasis(
        this: *mut ktxTexture2,
        fmt: ktx_transcode_fmt_e,
        transcodeFlags: ktx_transcode_flags,
    ) -> ktx_error_code_e;

    /// Returns a string corresponding to a KTX error code.
    pub fn ktxErrorString(error: ktx_error_code_e) -> *const core::ffi::c_char;
    /// Returns a string corresponding to a supercompression scheme.
    pub fn ktxSupercompressionSchemeString(scheme: ktxSupercmpScheme) -> *const core::ffi::c_char;
    /// Returns a string corresponding to a transcode target format.
    pub fn ktxTranscodeFormatString(format: ktx_transcode_fmt_e) -> *const core::ffi::c_char;

    pub fn ktxHashList_Create(ppH1: *mut *mut ktxHashList) -> ktx_error_code_e;
    pub fn ktxHashList_CreateCopy(
        ppH1: *mut *mut ktxHashList,
        orig: ktxHashList,
    ) -> ktx_error_code_e;
    pub fn ktxHashList_Construct(pH1: *mut ktxHashList);
    pub fn ktxHashList_ConstructCopy(pH1: *mut ktxHashList, orig: ktxHashList);
    pub fn ktxHashList_Destroy(head: *mut ktxHashList);
    pub fn ktxHashList_Destruct(head: *mut ktxHashList);
    /// Adds a key-value pair to a hash list.
    pub fn ktxHashList_AddKVPair(
        pHead: *mut ktxHashList,
        key: *const core::ffi::c_char,
        valueLen: core::ffi::c_uint,
        value: *const core::ffi::c_void,
    ) -> ktx_error_code_e;
    /// Deletes a [`ktxHashListEntry`] from a [`ktxHashList`].
    pub fn ktxHashList_DeleteEntry(
        pHead: *mut ktxHashList,
        pEntry: *mut ktxHashListEntry,
    ) -> ktx_error_code_e;
    /// Finds the entry for a key in a [`ktxHashList`] and deletes it.
    pub fn ktxHashList_DeleteKVPair(
        pHead: *mut ktxHashList,
        key: *const core::ffi::c_char,
    ) -> ktx_error_code_e;
    /// Looks up a key and returns the [`ktxHashListEntry`].
    pub fn ktxHashList_FindEntry(
        pHead: *mut ktxHashList,
        key: *const core::ffi::c_char,
        ppEntry: *mut *mut ktxHashListEntry,
    ) -> ktx_error_code_e;
    /// Looks up a key and returns the value.
    pub fn ktxHashList_FindValue(
        pHead: *mut ktxHashList,
        key: *const core::ffi::c_char,
        pValueLen: *mut core::ffi::c_uint,
        pValue: *mut *mut core::ffi::c_void,
    ) -> ktx_error_code_e;
    /// Return the next entry in a [`ktxHashList`].
    pub fn ktxHashList_Next(entry: *mut ktxHashListEntry) -> *mut ktxHashListEntry;
    /// Sorts a [`ktxHashList`] into order of the key codepoints.
    pub fn ktxHashList_Sort(pHead: *mut ktxHashList) -> ktx_error_code_e;
    /// Serializes a [`ktxHashList`] to a block of memory suitable for writing to a KTX file.
    pub fn ktxHashList_Serialize(
        pHead: *mut ktxHashList,
        kvdLen: *mut core::ffi::c_uint,
        kvd: *mut *mut core::ffi::c_uchar,
    ) -> ktx_error_code_e;
    /// Creates a hash table from the serialized data read from a KTX file.
    pub fn ktxHashList_Deserialize(
        pHead: *mut ktxHashList,
        kvdLen: core::ffi::c_uint,
        kvd: *mut core::ffi::c_void,
    ) -> ktx_error_code_e;
    /// Get the key from a [`ktxHashListEntry`].
    pub fn ktxHashListEntry_GetKey(
        this: *mut ktxHashListEntry,
        pKeyLen: *mut core::ffi::c_uint,
        ppKey: *mut *mut core::ffi::c_char,
    ) -> ktx_error_code_e;
    /// Get the value from a [`ktxHashListEntry`].
    pub fn ktxHashListEntry_GetValue(
        this: *mut ktxHashListEntry,
        pValueLen: *mut core::ffi::c_uint,
        ppValue: *mut *mut core::ffi::c_void,
    ) -> ktx_error_code_e;

    pub fn ktxPrintInfoForStioSTream(
        stdioStream: *mut core::ffi::c_void, /* FILE */
    ) -> ktx_error_code_e;
    pub fn ktxPrintInfoForNamedFile(filename: *const core::ffi::c_char) -> ktx_error_code_e;
    pub fn ktxPrintInfoForMemory(bytes: *const u8, size: usize) -> ktx_error_code_e;

    pub fn ktxPrintKTX2InfoTextForMemory(bytes: *const u8, size: usize) -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoTextForNamedFile(filename: *const core::ffi::c_char)
    -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoTextForStdioStream(
        stdioStream: *mut core::ffi::c_void, /* FILE */
    ) -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoTextForStream(stream: *mut ktxStream) -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoJSONForMemory(
        bytes: *const u8,
        size: usize,
        base_indent: u32,
        indent_width: u32,
        minified: bool,
    ) -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoJSONForNamedFile(
        filename: *const core::ffi::c_char,
        base_indent: u32,
        indent_width: u32,
        minified: bool,
    ) -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoJSONForStdioStream(
        stdioStream: *mut core::ffi::c_void, /* FILE */
        base_indent: u32,
        indent_width: u32,
        minified: bool,
    ) -> ktx_error_code_e;
    pub fn ktxPrintKTX2InfoJSONForStream(
        stream: *mut ktxStream,
        base_indent: u32,
        indent_width: u32,
        minified: bool,
    ) -> ktx_error_code_e;

    pub fn ktxTexture2_GetVkFormat(this: *mut ktxTexture2) -> core::ffi::c_int;
}
