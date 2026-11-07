use core::{
    ffi::{CStr, c_char, c_int, c_long, c_void},
    mem::MaybeUninit,
    ptr::NonNull,
};

use crate::ffi::{
    BuiltinModuleName, CompileCoreModuleFlags, SLANG_FLOATING_POINT_MODE_DEFAULT,
    SLANG_LINE_DIRECTIVE_MODE_DEFAULT, SLANG_MATRIX_LAYOUT_ROW_MAJOR, SLANG_PROFILE_UNKNOWN,
    SLANG_TARGET_FLAG_GENERATE_SPIRV_DIRECTLY, SLANG_TARGET_UNKNOWN, SlangBool, SlangFuncPtr,
    SlangInt, SlangLayoutRules, SlangMatrixLayoutMode, SlangParameterCategory, SlangReflection,
    SlangReflectionDecl, SlangReflectionFunction, SlangReflectionType, SlangReflectionTypeLayout,
    SlangResult, SlangUInt, SlangWriterMode,
};

pub mod ffi;
pub use ffi::{
    SlangArchiveType as ArchiveType, SlangCapabilityID as CapabilityID,
    SlangCompileTarget as CompileTarget, SlangDeclKind as DeclKind,
    SlangFloatingPointMode as FloatingPointMode, SlangGlobalSessionDesc as GlobalSessionDesc,
    SlangImageFormat as ImageFormat, SlangLayoutRules as LayoutRules,
    SlangLineDirectiveMode as LineDirectiveMode, SlangMatrixLayoutMode as MatrixLayoutMode,
    SlangModifierID as ModifierID, SlangPassThrough as PassThrough, SlangProfileID as ProfileID,
    SlangResourceAccess as ResourceAccess, SlangResourceShape as ResourceShape,
    SlangSourceLanguage as SourceLanguage, SlangStage as Stage, SlangTargetFlags as TargetFlags,
};
pub mod reflection;

pub type Result<T> = core::result::Result<T, SlangResult>;
#[inline(always)]
pub(crate) const fn rw(r: SlangResult) -> Result<SlangResult> {
    if r >= 0 { Ok(r) } else { Err(r) }
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct SlangUUID {
    pub data1: u32,
    pub data2: u16,
    pub data3: u16,
    pub data4: [u8; 8],
}
impl SlangUUID {
    pub const fn new(d1: u32, d2: u16, d3: u16, d4: [u8; 8]) -> Self {
        Self {
            data1: d1,
            data2: d2,
            data3: d3,
            data4: d4,
        }
    }
}

pub unsafe trait SlangInterfacePtr: Sized {
    const GUID: SlangUUID;
    type VTable;

    unsafe fn from_ptr(ptr: NonNull<c_void>) -> Self;

    #[inline(always)]
    fn thisptr(&self) -> *mut c_void {
        unsafe { core::mem::transmute::<_, &NonNull<c_void>>(self).as_ptr() }
    }

    #[inline(always)]
    fn vtable(&self) -> &Self::VTable {
        unsafe { &*(*self.thisptr().cast::<*const Self::VTable>()) }
    }
}

macro_rules! slang_interface_ptr {
    ($name: ident, $uuid: expr, $vtbl: ty) => {
        #[derive(Debug)]
        #[repr(transparent)]
        pub struct $name(NonNull<c_void>);
        unsafe impl SlangInterfacePtr for $name {
            const GUID: SlangUUID = $uuid;
            type VTable = $vtbl;

            #[inline(always)]
            unsafe fn from_ptr(ptr: NonNull<c_void>) -> Self {
                Self(ptr)
            }
        }
        impl Drop for $name {
            #[inline(always)]
            fn drop(&mut self) {
                unsafe {
                    self.release();
                }
            }
        }
        impl Clone for $name {
            #[inline(always)]
            fn clone(&self) -> Self {
                self.add_ref();
                Self(self.0)
            }
        }
    };
}

pub const UNKNOWN_IID: SlangUUID = SlangUUID {
    data1: 0x00000000,
    data2: 0x0000,
    data3: 0x0000,
    data4: [0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x46],
};
#[repr(C)]
pub struct IUnknownVTable {
    pub query_interface: unsafe extern "system" fn(
        this: *mut c_void,
        guid: *const SlangUUID,
        out: *mut *mut c_void,
    ) -> SlangResult,
    pub add_ref: unsafe extern "system" fn(this: *mut c_void) -> u32,
    pub release: unsafe extern "system" fn(this: *mut c_void) -> u32,
}
pub trait IUnknown: SlangInterfacePtr {
    fn vt(&self) -> &IUnknownVTable;

    fn query_interface(&self, guid: &SlangUUID) -> Result<NonNull<c_void>> {
        let mut o = core::mem::MaybeUninit::uninit();
        rw(unsafe { (IUnknown::vt(self).query_interface)(self.thisptr(), guid, o.as_mut_ptr()) })?;

        Ok(unsafe { NonNull::new_unchecked(o.assume_init()) })
    }

    #[inline]
    fn clone_cast<T>(&self) -> Result<T>
    where
        T: SlangInterfacePtr,
    {
        Ok(unsafe { T::from_ptr(self.query_interface(&T::GUID)?) })
    }

    #[inline(always)]
    fn add_ref(&self) -> u32 {
        unsafe { (IUnknown::vt(self).add_ref)(self.thisptr()) }
    }

    #[inline(always)]
    unsafe fn release(&self) -> u32 {
        unsafe { (IUnknown::vt(self).release)(self.thisptr()) }
    }
}

slang_interface_ptr!(IUnknownPtr, UNKNOWN_IID, IUnknownVTable);
impl IUnknown for IUnknownPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        self.vtable()
    }
}

#[repr(C)]
pub struct ICastableVTable {
    pub base: IUnknownVTable,
    pub cast_as:
        unsafe extern "system" fn(this: *mut c_void, guid: *const SlangUUID) -> *mut c_void,
}
pub trait ICastable: IUnknown {
    fn vt(&self) -> &ICastableVTable;

    #[inline]
    fn cast_as(&self, guid: &SlangUUID) -> Option<NonNull<c_void>> {
        NonNull::new(unsafe { (ICastable::vt(self).cast_as)(self.thisptr(), guid) })
    }
}

#[repr(C)]
pub struct ICloneableVTable {
    pub base: ICastableVTable,
    pub clone: unsafe extern "system" fn(this: *mut c_void, out: *mut *mut c_void) -> *mut c_void,
}
pub trait ICloneable: ICastable {
    fn clone(&self) -> Option<NonNull<c_void>>;
}

pub const BLOB_IID: SlangUUID = SlangUUID {
    data1: 0x8ba5fb08,
    data2: 0x5195,
    data3: 0x40e2,
    data4: [0xac, 0x58, 0x0d, 0x98, 0x9c, 0x3a, 0x01, 0x02],
};
#[repr(C)]
pub struct IBlobVTable {
    pub base: IUnknownVTable,
    pub get_buffer_pointer: unsafe extern "system" fn(this: *mut c_void) -> *const c_void,
    pub get_buffer_size: unsafe extern "system" fn(this: *mut c_void) -> usize,
}
pub trait IBlob: IUnknown {
    fn vt(&self) -> &IBlobVTable;

    #[inline(always)]
    fn get_buffer_pointer(&self) -> *const c_void {
        unsafe { (IBlob::vt(self).get_buffer_pointer)(self.thisptr()) }
    }

    #[inline(always)]
    fn get_buffer_size(&self) -> usize {
        unsafe { (IBlob::vt(self).get_buffer_size)(self.thisptr()) }
    }
}

slang_interface_ptr!(IBlobPtr, BLOB_IID, IBlobVTable);
impl IUnknown for IBlobPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base
    }
}
impl IBlob for IBlobPtr {
    #[inline(always)]
    fn vt(&self) -> &IBlobVTable {
        self.vtable()
    }
}

pub const FILE_SYSTEM_IID: SlangUUID = SlangUUID::new(
    0x003a09fc,
    0x3a4d,
    0x4ba0,
    [0xad, 0x60, 0x1f, 0xd8, 0x63, 0xa9, 0x15, 0xab],
);
#[repr(C)]
pub struct IFileSystemVTable {
    pub base: ICastableVTable,
    pub load_file: unsafe extern "system" fn(
        this: *mut c_void,
        path: *const c_char,
        out_blob: *mut *mut c_void,
    ) -> SlangResult,
}
pub trait IFileSystem: ICastable {
    fn vt(&self) -> &IFileSystemVTable;

    fn load_file(&self, path: &CStr) -> Result<IBlobPtr> {
        let mut o = core::mem::MaybeUninit::uninit();
        rw(unsafe {
            (IFileSystem::vt(self).load_file)(self.thisptr(), path.as_ptr(), o.as_mut_ptr())
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }
}

slang_interface_ptr!(IFileSystemPtr, FILE_SYSTEM_IID, IFileSystemVTable);
impl IUnknown for IFileSystemPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl ICastable for IFileSystemPtr {
    #[inline(always)]
    fn vt(&self) -> &ICastableVTable {
        &self.vtable().base
    }
}
impl IFileSystem for IFileSystemPtr {
    #[inline(always)]
    fn vt(&self) -> &IFileSystemVTable {
        self.vtable()
    }
}

pub const SHARED_LIBRARY_IID: SlangUUID = SlangUUID::new(
    0x70dbc7c4,
    0xdc3b,
    0x4a07,
    [0xae, 0x7e, 0x75, 0x2a, 0xf6, 0xa8, 0x15, 0x55],
);
#[repr(C)]
pub struct ISharedLibraryVTable {
    pub base: ICastableVTable,
    pub find_symbol_address_by_name:
        unsafe extern "system" fn(this: *mut c_void, name: *const c_char) -> *mut c_void,
}
pub trait ISharedLibrary: ICastable {
    fn vt(&self) -> &ISharedLibraryVTable;

    #[inline(always)]
    fn find_func_by_name(&self, name: &CStr) -> Option<SlangFuncPtr> {
        unsafe { core::mem::transmute(self.find_symbol_address_by_name(name)) }
    }

    #[inline]
    fn find_symbol_address_by_name(&self, name: &CStr) -> Option<NonNull<c_void>> {
        NonNull::new(unsafe {
            (ISharedLibrary::vt(self).find_symbol_address_by_name)(self.thisptr(), name.as_ptr())
        })
    }
}

slang_interface_ptr!(ISharedLibraryPtr, SHARED_LIBRARY_IID, ISharedLibraryVTable);
impl IUnknown for ISharedLibraryPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl ICastable for ISharedLibraryPtr {
    #[inline(always)]
    fn vt(&self) -> &ICastableVTable {
        &self.vtable().base
    }
}
impl ISharedLibrary for ISharedLibraryPtr {
    #[inline(always)]
    fn vt(&self) -> &ISharedLibraryVTable {
        self.vtable()
    }
}

pub const SHARED_LIBRARY_LOADER_IID: SlangUUID = SlangUUID::new(
    0x6264ab2b,
    0xa3e8,
    0x4a06,
    [0x97, 0xf1, 0x49, 0xbc, 0x2d, 0x2a, 0xb1, 0x4d],
);
#[repr(C)]
pub struct ISharedLibraryLoaderVTable {
    pub base: IUnknownVTable,
    pub load_shared_library: unsafe extern "system" fn(
        this: *mut c_void,
        path: *const c_char,
        shared_library_out: *mut *mut c_void,
    ) -> SlangResult,
}
pub trait ISharedLibraryLoader: IUnknown {
    fn vt(&self) -> &ISharedLibraryLoaderVTable;

    fn load_shared_library(&self, path: &CStr) -> Result<ISharedLibraryPtr> {
        let mut o = core::mem::MaybeUninit::uninit();
        rw(unsafe {
            (ISharedLibraryLoader::vt(self).load_shared_library)(
                self.thisptr(),
                path.as_ptr(),
                o.as_mut_ptr(),
            )
        })?;

        Ok(ISharedLibraryPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }
}

slang_interface_ptr!(
    ISharedLibraryLoaderPtr,
    SHARED_LIBRARY_LOADER_IID,
    ISharedLibraryLoaderVTable
);
impl IUnknown for ISharedLibraryLoaderPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base
    }
}
impl ISharedLibraryLoader for ISharedLibraryLoaderPtr {
    #[inline(always)]
    fn vt(&self) -> &ISharedLibraryLoaderVTable {
        self.vtable()
    }
}

pub const WRITER_IID: SlangUUID = SlangUUID::new(
    0xec457f0e,
    0x9add,
    0x4e6b,
    [0x85, 0x1c, 0xd7, 0xfa, 0x71, 0x6d, 0x15, 0xfd],
);
#[repr(C)]
pub struct IWriterVTable {
    base: IUnknownVTable,
    begin_append_buffer:
        unsafe extern "system" fn(this: *mut c_void, max_num_chars: usize) -> *mut c_char,
    end_append_buffer: unsafe extern "system" fn(
        this: *mut c_void,
        buffer: *mut c_char,
        num_chars: usize,
    ) -> SlangResult,
    write: unsafe extern "system" fn(
        this: *mut c_void,
        chars: *const c_char,
        num_chars: usize,
    ) -> SlangResult,
    flush: unsafe extern "system" fn(this: *mut c_void),
    is_console: unsafe extern "system" fn(this: *mut c_void) -> SlangBool,
    set_mode: unsafe extern "system" fn(this: *mut c_void, mode: SlangWriterMode) -> SlangResult,
}
/// A stream typically of text, used for outputting diagnostic as well as other information.
pub trait IWriter: IUnknown {
    fn vt(&self) -> &IWriterVTable;

    #[inline]
    fn begin_append_buffer(&self, max_num_chars: usize) -> NonNull<c_char> {
        unsafe {
            NonNull::new_unchecked((IWriter::vt(self).begin_append_buffer)(
                self.thisptr(),
                max_num_chars,
            ))
        }
    }

    #[inline]
    fn end_append_buffer(&self, buffer: NonNull<c_char>, num_chars: usize) -> Result<()> {
        rw(unsafe {
            (IWriter::vt(self).end_append_buffer)(self.thisptr(), buffer.as_ptr(), num_chars)
        })?;

        Ok(())
    }

    #[inline]
    fn write(&self, chars: &CStr, num_chars: usize) -> Result<()> {
        rw(unsafe { (IWriter::vt(self).write)(self.thisptr(), chars.as_ptr(), num_chars) })?;

        Ok(())
    }

    #[inline]
    fn flush(&self) {
        unsafe { (IWriter::vt(self).flush)(self.thisptr()) }
    }

    #[inline]
    fn is_console(&self) -> bool {
        unsafe { (IWriter::vt(self).is_console)(self.thisptr()) }
    }

    #[inline]
    fn set_mode(&self, mode: SlangWriterMode) -> Result<()> {
        rw(unsafe { (IWriter::vt(self).set_mode)(self.thisptr(), mode) })?;

        Ok(())
    }
}

pub const PROFILER_IID: SlangUUID = SlangUUID::new(
    0x197772c7,
    0x0155,
    0x4b91,
    [0x84, 0xe8, 0x66, 0x68, 0xba, 0xff, 0x06, 0x19],
);
#[repr(C)]
pub struct IProfilerVTable {
    base: IUnknownVTable,
    get_entry_count: unsafe extern "system" fn(this: *mut c_void) -> usize,
    get_entry_name: unsafe extern "system" fn(this: *mut c_void, index: u32) -> *const c_char,
    get_entry_time_ms: unsafe extern "system" fn(this: *mut c_void, index: u32) -> c_long,
    get_entry_invocation_times: unsafe extern "system" fn(this: *mut c_void, index: u32) -> u32,
}
pub trait IProfiler: IUnknown {
    fn vt(&self) -> &IProfilerVTable;

    #[inline]
    fn get_entry_count(&self) -> usize {
        unsafe { (IProfiler::vt(self).get_entry_count)(self.thisptr()) }
    }

    #[inline]
    fn get_entry_name(&self, index: u32) -> &CStr {
        unsafe { CStr::from_ptr((IProfiler::vt(self).get_entry_name)(self.thisptr(), index)) }
    }

    #[inline]
    fn get_entry_time_ms(&self, index: u32) -> c_long {
        unsafe { (IProfiler::vt(self).get_entry_time_ms)(self.thisptr(), index) }
    }

    #[inline]
    fn get_entry_invocation_times(&self, index: u32) -> u32 {
        unsafe { (IProfiler::vt(self).get_entry_invocation_times)(self.thisptr(), index) }
    }
}

#[deprecated = "old interface"]
#[repr(transparent)]
pub struct ICompileRequestObject(NonNull<c_void>);

pub const GLOBAL_SESSION_IID: SlangUUID = SlangUUID::new(
    0xc140b5fd,
    0x0c78,
    0x452e,
    [0xba, 0x7c, 0x1a, 0x1e, 0x70, 0xc7, 0xf7, 0x1c],
);
#[repr(C)]
pub struct IGlobalSessionVTable {
    base: IUnknownVTable,
    create_session: unsafe extern "system" fn(
        this: *mut c_void,
        session_desc: *const SessionDesc,
        out_session: *mut *mut c_void,
    ) -> SlangResult,
    find_profile: unsafe extern "system" fn(this: *mut c_void, name: *const c_char) -> ProfileID,
    set_downstream_compiler_path: unsafe extern "system" fn(
        this: *mut c_void,
        pass_through: PassThrough,
        path: *const c_char,
    ),
    set_downstream_compiler_prelude: unsafe extern "system" fn(
        this: *mut c_void,
        pass_through: PassThrough,
        prelude_text: *const c_char,
    ),
    get_downstream_compiler_prelude: unsafe extern "system" fn(
        this: *mut c_void,
        pass_through: PassThrough,
        out_prelude: *mut *mut c_void,
    ),
    get_build_tag_string: unsafe extern "system" fn(this: *mut c_void) -> *const c_char,
    set_default_downstream_compiler: unsafe extern "system" fn(
        this: *mut c_void,
        source_language: SourceLanguage,
        default_compiler: PassThrough,
    ) -> SlangResult,
    get_default_downstream_compiler: unsafe extern "system" fn(
        this: *mut c_void,
        source_language: SourceLanguage,
    ) -> PassThrough,
    set_language_prelude: unsafe extern "system" fn(
        this: *mut c_void,
        source_language: SourceLanguage,
        prelude_text: *const c_char,
    ),
    get_language_prelude: unsafe extern "system" fn(
        this: *mut c_void,
        source_language: SourceLanguage,
        out_prelude: *mut *mut c_void,
    ),
    create_compile_request: unsafe extern "system" fn(
        this: *mut c_void,
        out_compile_request: *mut *mut c_void,
    ) -> SlangResult,
    add_builtins: unsafe extern "system" fn(
        this: *mut c_void,
        source_path: *const c_char,
        source_string: *const c_char,
    ),
    set_shared_library_loader: unsafe extern "system" fn(this: *mut c_void, loader: *mut c_void),
    get_shared_library_loader: unsafe extern "system" fn(this: *mut c_void) -> *mut c_void,
    check_compile_target_support:
        unsafe extern "system" fn(this: *mut c_void, target: CompileTarget) -> SlangResult,
    check_pass_through_support:
        unsafe extern "system" fn(this: *mut c_void, pass_through: PassThrough) -> SlangResult,
    compile_core_module:
        unsafe extern "system" fn(this: *mut c_void, flags: CompileCoreModuleFlags) -> SlangResult,
    load_core_module: unsafe extern "system" fn(
        this: *mut c_void,
        core_module: *const c_void,
        core_module_size_in_bytes: usize,
    ) -> SlangResult,
    save_core_module: unsafe extern "system" fn(
        this: *mut c_void,
        archive_type: ArchiveType,
        out_blob: *mut *mut c_void,
    ) -> SlangResult,
    find_capability:
        unsafe extern "system" fn(this: *mut c_void, name: *const c_char) -> CapabilityID,
    set_downstream_compiler_for_transition: unsafe extern "system" fn(
        this: *mut c_void,
        source: CompileTarget,
        target: CompileTarget,
        compiler: PassThrough,
    ),
    get_downstream_compiler_for_transition: unsafe extern "system" fn(
        this: *mut c_void,
        source: CompileTarget,
        target: CompileTarget,
    ) -> PassThrough,
    get_compiler_elapsed_time: unsafe extern "system" fn(
        this: *mut c_void,
        out_total_time: *mut core::ffi::c_double,
        out_downstream_time: *mut core::ffi::c_double,
    ),
    set_spirv_core_grammar:
        unsafe extern "system" fn(this: *mut c_void, json_path: *const c_char) -> SlangResult,
    parse_command_line_arguments: unsafe extern "system" fn(
        this: *mut c_void,
        argc: c_int,
        argv: *const *const c_char,
        out_session_desc: *mut SessionDesc,
        out_aux_allocation: *mut *mut c_void,
    ) -> SlangResult,
    get_session_desc_digest: unsafe extern "system" fn(
        this: *mut c_void,
        session_desc: *mut SessionDesc,
        out_blob: *mut *mut c_void,
    ) -> SlangResult,
    compile_builtin_module: unsafe extern "system" fn(
        this: *mut c_void,
        module: BuiltinModuleName,
        flags: CompileCoreModuleFlags,
    ) -> SlangResult,
    load_builtin_module: unsafe extern "system" fn(
        this: *mut c_void,
        module: BuiltinModuleName,
        module_data: *const c_void,
        size_in_bytes: usize,
    ) -> SlangResult,
    save_builtin_module: unsafe extern "system" fn(
        this: *mut c_void,
        module: BuiltinModuleName,
        archive_type: ArchiveType,
        out_blob: *mut *mut c_void,
    ) -> SlangResult,
}
pub trait IGlobalSession: IUnknown {
    fn vt(&self) -> &IGlobalSessionVTable;

    /// Create a new session for loading and compiling code.
    fn create_session(&self, desc: &SessionDesc) -> Result<ISessionPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IGlobalSession::vt(self).create_session)(self.thisptr(), desc, o.as_mut_ptr())
        })?;

        Ok(ISessionPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    /// Look up the internal ID of a profile by its `name`.
    ///
    /// Profile IDs are *not* guaranteed to be stable across versions of the Slang library,
    /// so clients are expected to look up profiles by name at runtime.
    #[inline]
    fn find_profile(&self, name: &CStr) -> ProfileID {
        unsafe { (IGlobalSession::vt(self).find_profile)(self.thisptr(), name.as_ptr()) }
    }

    /// Set the path that downstream compilers (aka back end compilers) will be looked from.
    ///
    /// For back ends that are dlls/shared libraries, it will mean the path will be prefixed with the path
    /// when calls are made out to ISlangSharedLibraryLoader.
    /// For executables - it will look for executables along the path
    ///
    /// # Arguments
    ///
    /// * `pass_through` - Identifies the downstream compiler
    /// * `path` - The path to find the downstream compiler (shared library/dll/executable)
    #[inline]
    fn set_downstream_compiler_path(&self, pass_through: PassThrough, path: &CStr) {
        unsafe {
            (IGlobalSession::vt(self).set_downstream_compiler_path)(
                self.thisptr(),
                pass_through,
                path.as_ptr(),
            )
        }
    }

    /// Set the 'prelude' for generated code for a 'downstream compiler'.
    ///
    /// That for pass-through usage, prelude is not pre-pended, preludes are for code generation only.
    ///
    /// # Arguments
    ///
    /// * `pass_through` - The downstream compiler for generated code that will have the prelude applied to it.
    /// * `prelude_text` - The text added pre-pended verbatim before the generated source
    #[deprecated = "Use set_language_prelude"]
    #[inline]
    fn set_downstream_compiler_prelude(&self, pass_through: PassThrough, prelude_text: &CStr) {
        unsafe {
            (IGlobalSession::vt(self).set_downstream_compiler_prelude)(
                self.thisptr(),
                pass_through,
                prelude_text.as_ptr(),
            )
        }
    }

    /// Get the 'prelude' for generated code for a 'downstream compiler'.
    ///
    /// # Arguments
    ///
    /// * `pass_through` - The downstream compiler for generated code that will have the prelude applied to it.
    ///
    /// # Returns
    ///
    /// A blob that holds the string of the prelude.
    #[deprecated = "Use get_language_prelude"]
    fn get_downstream_compiler_prelude(&self, pass_through: PassThrough) -> Option<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        unsafe {
            (IGlobalSession::vt(self).get_downstream_compiler_prelude)(
                self.thisptr(),
                pass_through,
                o.as_mut_ptr(),
            );
        }

        NonNull::new(unsafe { o.assume_init() }).map(IBlobPtr)
    }

    /// Get the build version 'tag' string.
    /// The string is the same as produced via `git describe --tags` for the project.
    /// If Slang is built separately from the automated build scripts the contents will by default be 'unknown'.
    /// Any string can be set by changing the contents of 'slang-tag-version.h' file and recompiling the project.
    ///
    /// This method will return exactly the same result as the free function [`spGetBuildTagString`].
    ///
    /// # Returns
    ///
    /// The build tag string
    #[inline]
    fn get_build_tag_string(&self) -> &CStr {
        unsafe {
            CStr::from_ptr((IGlobalSession::vt(self).get_build_tag_string)(
                self.thisptr(),
            ))
        }
    }

    /// For a given source language set the default compiler.
    /// If a default cannot be chosen (for example the target cannot be achieved by the default),
    /// the default will not be used.
    ///
    /// # Arguments
    ///
    /// * `source_language` - the source language
    /// * `default_compiler` - the default compiler for that language
    #[inline]
    fn set_default_downstream_compiler(
        &self,
        source_language: SourceLanguage,
        default_compiler: PassThrough,
    ) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).set_default_downstream_compiler)(
                self.thisptr(),
                source_language,
                default_compiler,
            )
        })?;

        Ok(())
    }

    /// For a source type get the default compiler
    ///
    /// # Arguments
    ///
    /// * `source_language` - the source language
    ///
    /// # Returns
    ///
    /// The downstream compiler for that source language
    #[inline]
    fn get_default_downstream_compiler(&self, source_language: SourceLanguage) -> PassThrough {
        unsafe {
            (IGlobalSession::vt(self).get_default_downstream_compiler)(
                self.thisptr(),
                source_language,
            )
        }
    }

    /// Set the 'prelude' placed before generated code for a specific language type.
    ///
    /// # Arguments
    ///
    /// * `source_language` - The language the prelude should be inserted on.
    /// * `prelude_text` - The text added pre-pended verbatim before the generated source
    ///
    /// # Note
    ///
    /// That for pass-through usage, prelude is not pre-pended, preludes are for code generation only.
    #[inline]
    fn set_language_prelude(&self, source_language: SourceLanguage, prelude_text: &CStr) {
        unsafe {
            (IGlobalSession::vt(self).set_language_prelude)(
                self.thisptr(),
                source_language,
                prelude_text.as_ptr(),
            )
        }
    }

    /// Get the 'prelude' associated with a specific source language.
    ///
    /// # Arguments
    ///
    /// * `source_language` - The language the prelude should be inserted on.
    ///
    /// # Returns
    ///
    /// A blob that hold the string of the prelude.
    fn get_language_prelude(&self, source_language: SourceLanguage) -> Option<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        unsafe {
            (IGlobalSession::vt(self).get_language_prelude)(
                self.thisptr(),
                source_language,
                o.as_mut_ptr(),
            );
        }

        NonNull::new(unsafe { o.assume_init() }).map(IBlobPtr)
    }

    /// Create a compile request.
    #[deprecated]
    fn create_compile_request(&self) -> Result<NonNull<c_void>> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IGlobalSession::vt(self).create_compile_request)(self.thisptr(), o.as_mut_ptr())
        })?;

        Ok(unsafe { NonNull::new_unchecked(o.assume_init()) })
    }

    /// Add new builtin declarations to be used in subsequent compilers.
    #[inline]
    fn add_builtins(&self, source_path: &CStr, source_string: &CStr) {
        unsafe {
            (IGlobalSession::vt(self).add_builtins)(
                self.thisptr(),
                source_path.as_ptr(),
                source_string.as_ptr(),
            )
        }
    }

    /// Set the session shared library loader. If this changes the loader,
    /// it maybe cause shared libraries to be unloaded.
    ///
    /// # Arguments
    ///
    /// * `loader` - The loader to set. Setting `None` sets the default loader.
    #[inline]
    fn set_shared_library_loader(&self, loader: Option<&impl ISharedLibraryLoader>) {
        unsafe {
            (IGlobalSession::vt(self).set_shared_library_loader)(
                self.thisptr(),
                loader.map_or_else(core::ptr::null_mut, SlangInterfacePtr::thisptr),
            )
        }
    }

    /// Gets the currently set shared library loader
    ///
    /// # Returns
    ///
    /// Gets the currently set loader. If returns `None`, it's the default loader
    #[inline]
    fn get_shared_library_loader(&self) -> Option<ISharedLibraryLoaderPtr> {
        NonNull::new(unsafe {
            (IGlobalSession::vt(self).get_shared_library_loader)(self.thisptr())
        })
        .map(ISharedLibraryLoaderPtr)
    }

    #[inline]
    fn check_compile_target_support(&self, target: CompileTarget) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).check_compile_target_support)(self.thisptr(), target)
        })?;

        Ok(())
    }

    #[inline]
    fn check_pass_through_support(&self, pass_through: PassThrough) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).check_pass_through_support)(self.thisptr(), pass_through)
        })?;

        Ok(())
    }

    #[inline]
    fn compile_core_module(&self, flags: CompileCoreModuleFlags) -> Result<()> {
        rw(unsafe { (IGlobalSession::vt(self).compile_core_module)(self.thisptr(), flags) })?;

        Ok(())
    }

    #[inline]
    fn load_core_module(
        &self,
        core_module: *const c_void,
        core_module_size_in_bytes: usize,
    ) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).load_core_module)(
                self.thisptr(),
                core_module,
                core_module_size_in_bytes,
            )
        })?;

        Ok(())
    }

    fn save_core_module(&self, archive_type: ArchiveType) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IGlobalSession::vt(self).save_core_module)(
                self.thisptr(),
                archive_type,
                o.as_mut_ptr(),
            )
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    #[inline]
    fn find_capability(&self, name: &CStr) -> CapabilityID {
        unsafe { (IGlobalSession::vt(self).find_capability)(self.thisptr(), name.as_ptr()) }
    }

    #[inline]
    fn set_downstream_compiler_for_transition(
        &self,
        source: CompileTarget,
        target: CompileTarget,
        compiler: PassThrough,
    ) {
        unsafe {
            (IGlobalSession::vt(self).set_downstream_compiler_for_transition)(
                self.thisptr(),
                source,
                target,
                compiler,
            )
        }
    }

    #[inline]
    fn get_downstream_compiler_for_transition(
        &self,
        source: CompileTarget,
        target: CompileTarget,
    ) -> PassThrough {
        unsafe {
            (IGlobalSession::vt(self).get_downstream_compiler_for_transition)(
                self.thisptr(),
                source,
                target,
            )
        }
    }

    #[inline]
    fn get_compiler_elapsed_time(
        &self,
        total_time: &mut MaybeUninit<core::ffi::c_double>,
        downstream_time: &mut MaybeUninit<core::ffi::c_double>,
    ) {
        unsafe {
            (IGlobalSession::vt(self).get_compiler_elapsed_time)(
                self.thisptr(),
                total_time.as_mut_ptr(),
                downstream_time.as_mut_ptr(),
            )
        }
    }

    #[inline]
    fn set_spirv_core_grammar(&self, json_path: &CStr) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).set_spirv_core_grammar)(self.thisptr(), json_path.as_ptr())
        })?;

        Ok(())
    }

    #[inline]
    unsafe fn parse_command_line_arguments(
        &self,
        argc: c_int,
        argv: *const *const c_char,
        session_desc: &mut MaybeUninit<SessionDesc>,
        aux_allocation: &mut MaybeUninit<IUnknownPtr>,
    ) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).parse_command_line_arguments)(
                self.thisptr(),
                argc,
                argv,
                session_desc.as_mut_ptr(),
                aux_allocation.as_mut_ptr() as _,
            )
        })?;

        Ok(())
    }

    fn get_session_desc_digest(&self, session_desc: &mut SessionDesc) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IGlobalSession::vt(self).get_session_desc_digest)(
                self.thisptr(),
                session_desc,
                o.as_mut_ptr(),
            )
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    #[inline]
    fn compile_builtin_module(
        &self,
        module: BuiltinModuleName,
        flags: CompileCoreModuleFlags,
    ) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).compile_builtin_module)(self.thisptr(), module, flags)
        })?;

        Ok(())
    }

    #[inline]
    fn load_builtin_module(
        &self,
        module: BuiltinModuleName,
        module_data: *const core::ffi::c_void,
        size_in_bytes: usize,
    ) -> Result<()> {
        rw(unsafe {
            (IGlobalSession::vt(self).load_builtin_module)(
                self.thisptr(),
                module,
                module_data,
                size_in_bytes,
            )
        })?;

        Ok(())
    }

    fn save_builtin_module(
        &self,
        module: BuiltinModuleName,
        archive_type: ArchiveType,
    ) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IGlobalSession::vt(self).save_builtin_module)(
                self.thisptr(),
                module,
                archive_type,
                o.as_mut_ptr(),
            )
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }
}

slang_interface_ptr!(IGlobalSessionPtr, GLOBAL_SESSION_IID, IGlobalSessionVTable);
impl IUnknown for IGlobalSessionPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base
    }
}
impl IGlobalSession for IGlobalSessionPtr {
    #[inline(always)]
    fn vt(&self) -> &IGlobalSessionVTable {
        self.vtable()
    }
}

pub const SESSION_IID: SlangUUID = SlangUUID::new(
    0x67618701,
    0xd116,
    0x468f,
    [0xab, 0x3b, 0x47, 0x4b, 0xed, 0xce, 0x0e, 0x3d],
);
#[repr(C)]
pub struct ISessionVTable {
    base: IUnknownVTable,
    get_global_session: unsafe extern "system" fn(this: *mut c_void) -> *mut c_void,
    load_module: unsafe extern "system" fn(
        this: *mut c_void,
        module_name: *const c_char,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut c_void,
    load_module_from_source: unsafe extern "system" fn(
        this: *mut c_void,
        module_name: *const c_char,
        path: *const c_char,
        source: *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut c_void,
    create_composite_component_type: unsafe extern "system" fn(
        this: *mut c_void,
        component_types: *const *mut c_void,
        component_type_count: SlangInt,
        out_composite_component_type: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    specialize_type: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        specialization_args: *const SpecializationArg,
        specialization_arg_count: SlangInt,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut SlangReflectionType,
    get_type_layout: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        target_index: SlangInt,
        rules: LayoutRules,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut SlangReflectionTypeLayout,
    get_container_type: unsafe extern "system" fn(
        this: *mut c_void,
        element_type: *mut SlangReflectionType,
        container_type: ContainerType,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut SlangReflectionType,
    get_dynamic_type: unsafe extern "system" fn(this: *mut c_void) -> *mut SlangReflectionType,
    get_type_rtti_mangled_name: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        out_name_blob: *mut *mut c_void,
    ) -> SlangResult,
    get_type_conformance_witness_mangled_name: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        interface_type: *mut SlangReflectionType,
        out_name_blob: *mut *mut c_void,
    ) -> SlangResult,
    get_type_conformance_witness_sequential_id: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        interface_type: *mut SlangReflectionType,
        out_id: *mut u32,
    ) -> SlangResult,
    create_compile_request: unsafe extern "system" fn(
        this: *mut c_void,
        out_compile_request: *mut *mut c_void,
    ) -> SlangResult,
    create_type_conformance_component_type: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        interface_type: *mut SlangReflectionType,
        out_conformance: *mut *mut c_void,
        conformance_id_override: SlangInt,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    load_module_from_ir_blob: unsafe extern "system" fn(
        this: *mut c_void,
        module_name: *const c_char,
        path: *const c_char,
        source: *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut c_void,
    get_loaded_module_count: unsafe extern "system" fn(this: *mut c_void) -> SlangInt,
    get_loaded_module: unsafe extern "system" fn(this: *mut c_void, index: SlangInt) -> *mut c_void,
    is_binary_module_up_to_date: unsafe extern "system" fn(
        this: *mut c_void,
        module_path: *const c_char,
        binary_module_blob: *mut c_void,
    ) -> bool,
    load_module_from_source_string: unsafe extern "system" fn(
        this: *mut c_void,
        module_name: *const c_char,
        path: *const c_char,
        string: *const c_char,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut c_void,
    get_dynamic_object_rtti_bytes: unsafe extern "system" fn(
        this: *mut c_void,
        r#type: *mut SlangReflectionType,
        interface_type: *mut SlangReflectionType,
        out_rtti_data_buffer: *mut u32,
        buffer_size_in_bytes: u32,
    ) -> SlangResult,
    load_module_info_from_ir_blob: unsafe extern "system" fn(
        this: *mut c_void,
        source: *mut c_void,
        out_module_version: *mut SlangInt,
        out_module_compiler_version: *mut *const c_char,
        out_module_name: *mut *const c_char,
    ) -> SlangResult,
}
pub trait ISession: IUnknown {
    fn vt(&self) -> &ISessionVTable;

    #[inline]
    fn get_global_session(&self) -> IGlobalSessionPtr {
        IGlobalSessionPtr(unsafe {
            NonNull::new_unchecked((ISession::vt(self).get_global_session)(self.thisptr()))
        })
    }

    #[inline]
    fn load_module(
        &self,
        module_name: &CStr,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Option<IModulePtr> {
        NonNull::new(unsafe {
            (ISession::vt(self).load_module)(
                self.thisptr(),
                module_name.as_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })
        .map(IModulePtr)
    }

    #[inline]
    fn load_module_from_source(
        &self,
        module_name: &CStr,
        path: &CStr,
        source: &impl IBlob,
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> Option<IModulePtr> {
        NonNull::new(unsafe {
            (ISession::vt(self).load_module_from_source)(
                self.thisptr(),
                module_name.as_ptr(),
                path.as_ptr(),
                source.thisptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })
        .map(IModulePtr)
    }

    fn create_composite_component_type(
        &self,
        component_types: &[IComponentTypePtr],
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IComponentTypePtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (ISession::vt(self).create_composite_component_type)(
                self.thisptr(),
                component_types.as_ptr() as _,
                component_types.len() as _,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IComponentTypePtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    #[inline]
    fn specialize_type(
        &self,
        r#type: &mut SlangReflectionType,
        specialization_args: &[SpecializationArg],
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> &mut SlangReflectionType {
        unsafe {
            &mut *(ISession::vt(self).specialize_type)(
                self.thisptr(),
                r#type,
                specialization_args.as_ptr(),
                specialization_args.len() as _,
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        }
    }

    #[inline]
    fn get_type_layout(
        &self,
        r#type: &mut SlangReflectionType,
        target_index: SlangInt,
        layout_rules: SlangLayoutRules,
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> &mut reflection::TypeLayout {
        unsafe {
            reflection::TypeLayout::from_mut_ptr((ISession::vt(self).get_type_layout)(
                self.thisptr(),
                r#type,
                target_index,
                layout_rules,
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            ))
        }
    }

    #[inline]
    fn get_container_type(
        &self,
        element_type: &mut SlangReflectionType,
        container_type: ContainerType,
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> &mut SlangReflectionType {
        unsafe {
            &mut *(ISession::vt(self).get_container_type)(
                self.thisptr(),
                element_type,
                container_type,
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        }
    }

    #[inline]
    fn get_dynamic_type(&self) -> &mut SlangReflectionType {
        unsafe { &mut *(ISession::vt(self).get_dynamic_type)(self.thisptr()) }
    }

    fn get_type_rtti_mangled_name(&self, r#type: &mut SlangReflectionType) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (ISession::vt(self).get_type_rtti_mangled_name)(self.thisptr(), r#type, o.as_mut_ptr())
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    fn get_type_conformance_witness_mangled_name(
        &self,
        r#type: &mut SlangReflectionType,
        interface_type: &mut SlangReflectionType,
    ) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (ISession::vt(self).get_type_conformance_witness_mangled_name)(
                self.thisptr(),
                r#type,
                interface_type,
                o.as_mut_ptr(),
            )
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    fn get_type_conformance_witness_sequential_id(
        &self,
        r#type: &mut SlangReflectionType,
        interface_type: &mut SlangReflectionType,
    ) -> Result<u32> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (ISession::vt(self).get_type_conformance_witness_sequential_id)(
                self.thisptr(),
                r#type,
                interface_type,
                o.as_mut_ptr(),
            )
        })?;

        Ok(unsafe { o.assume_init() })
    }

    #[allow(deprecated)]
    fn create_compile_request(&self) -> Result<ICompileRequestObject> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe { (ISession::vt(self).create_compile_request)(self.thisptr(), o.as_mut_ptr()) })?;

        Ok(ICompileRequestObject(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn create_type_conformance_component_type(
        &self,
        r#type: &mut SlangReflectionType,
        interface_type: &mut SlangReflectionType,
        out_conformance: &mut [MaybeUninit<ITypeConformancePtr>],
        conformance_id_override: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<()> {
        rw(unsafe {
            (ISession::vt(self).create_type_conformance_component_type)(
                self.thisptr(),
                r#type,
                interface_type,
                out_conformance.as_mut_ptr() as _,
                conformance_id_override,
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(())
    }

    #[inline]
    fn load_module_from_ir_blob(
        &self,
        module_name: &CStr,
        path: &CStr,
        source: &impl IBlob,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Option<IModulePtr> {
        NonNull::new(unsafe {
            (ISession::vt(self).load_module_from_ir_blob)(
                self.thisptr(),
                module_name.as_ptr(),
                path.as_ptr(),
                source.thisptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })
        .map(IModulePtr)
    }

    #[inline]
    fn get_loaded_module_count(&self) -> SlangInt {
        unsafe { (ISession::vt(self).get_loaded_module_count)(self.thisptr()) }
    }

    #[inline]
    fn get_loaded_module(&self, index: SlangInt) -> IModulePtr {
        IModulePtr(unsafe {
            NonNull::new_unchecked((ISession::vt(self).get_loaded_module)(
                self.thisptr(),
                index,
            ))
        })
    }

    #[inline]
    fn is_binary_module_up_to_date(
        &self,
        module_path: &CStr,
        binary_module_blob: &impl IBlob,
    ) -> bool {
        unsafe {
            (ISession::vt(self).is_binary_module_up_to_date)(
                self.thisptr(),
                module_path.as_ptr(),
                binary_module_blob.thisptr(),
            )
        }
    }

    #[inline]
    fn load_module_from_source_string(
        &self,
        module_name: &CStr,
        path: &CStr,
        string: &CStr,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Option<IModulePtr> {
        NonNull::new(unsafe {
            (ISession::vt(self).load_module_from_source_string)(
                self.thisptr(),
                module_name.as_ptr(),
                path.as_ptr(),
                string.as_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })
        .map(IModulePtr)
    }

    fn get_dynamic_object_rtti_bytes(
        &self,
        r#type: &mut SlangReflectionType,
        interface_type: &mut SlangReflectionType,
        out_rtti_data_buffer: &mut [MaybeUninit<u32>],
    ) -> Result<()> {
        rw(unsafe {
            (ISession::vt(self).get_dynamic_object_rtti_bytes)(
                self.thisptr(),
                r#type,
                interface_type,
                out_rtti_data_buffer.as_mut_ptr() as _,
                (out_rtti_data_buffer.len() << 2) as _,
            )
        })?;

        Ok(())
    }

    fn load_module_info_from_ir_blob(
        &self,
        source: &impl IBlob,
    ) -> Result<(SlangInt, &CStr, &CStr)> {
        let mut module_version = MaybeUninit::uninit();
        let mut module_compiler_version = MaybeUninit::uninit();
        let mut module_name = MaybeUninit::uninit();
        rw(unsafe {
            (ISession::vt(self).load_module_info_from_ir_blob)(
                self.thisptr(),
                source.thisptr(),
                module_version.as_mut_ptr(),
                module_compiler_version.as_mut_ptr(),
                module_name.as_mut_ptr(),
            )
        })?;

        Ok((
            unsafe { module_version.assume_init() },
            unsafe { CStr::from_ptr(module_compiler_version.assume_init()) },
            unsafe { CStr::from_ptr(module_name.assume_init()) },
        ))
    }
}

slang_interface_ptr!(ISessionPtr, SESSION_IID, ISessionVTable);
impl IUnknown for ISessionPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base
    }
}
impl ISession for ISessionPtr {
    #[inline(always)]
    fn vt(&self) -> &ISessionVTable {
        self.vtable()
    }
}

pub const METADATA_IID: SlangUUID = SlangUUID::new(
    0x8044a8a3,
    0xddc0,
    0x4b7f,
    [0xaf, 0x8e, 0x02, 0x6e, 0x90, 0x5d, 0x73, 0x32],
);
#[repr(C)]
pub struct IMetadataVTable {
    pub base: ICastableVTable,
    pub is_parameter_location_used: unsafe extern "system" fn(
        this: *mut c_void,
        category: SlangParameterCategory,
        space_index: SlangUInt,
        register_index: SlangUInt,
        out_used: *mut bool,
    ) -> SlangResult,
    pub get_debug_build_identifier: unsafe extern "system" fn(this: *mut c_void) -> *const c_char,
}
pub trait IMetadata: ICastable {
    fn vt(&self) -> &IMetadataVTable;

    fn is_parameter_location_used(
        &self,
        category: SlangParameterCategory,
        space_index: SlangUInt,
        register_index: SlangUInt,
    ) -> Result<bool> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IMetadata::vt(self).is_parameter_location_used)(
                self.thisptr(),
                category,
                space_index,
                register_index,
                o.as_mut_ptr(),
            )
        })?;

        Ok(unsafe { o.assume_init() })
    }

    #[inline]
    fn get_debug_build_identifier(&self) -> &CStr {
        unsafe {
            CStr::from_ptr((IMetadata::vt(self).get_debug_build_identifier)(
                self.thisptr(),
            ))
        }
    }
}

slang_interface_ptr!(IMetadataPtr, METADATA_IID, IMetadataVTable);
impl IUnknown for IMetadataPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl ICastable for IMetadataPtr {
    #[inline(always)]
    fn vt(&self) -> &ICastableVTable {
        &self.vtable().base
    }
}
impl IMetadata for IMetadataPtr {
    #[inline(always)]
    fn vt(&self) -> &IMetadataVTable {
        self.vtable()
    }
}

pub const COMPILE_RESULT_IID: SlangUUID = SlangUUID::new(
    0x5fa9380e,
    0xb62f,
    0x41e5,
    [0x9f, 0x12, 0x4b, 0xad, 0x4d, 0x9e, 0xaa, 0xe4],
);
#[repr(C)]
pub struct ICompileResultVTable {
    base: ICastableVTable,
    get_item_count: unsafe extern "system" fn(this: *mut c_void) -> u32,
    get_item_data: unsafe extern "system" fn(
        this: *mut c_void,
        index: u32,
        out_blob: *mut *mut c_void,
    ) -> SlangResult,
    get_metadata:
        unsafe extern "system" fn(this: *mut c_void, out_metadata: *mut *mut c_void) -> SlangResult,
}
pub trait ICompileResult: ICastable {
    fn vt(&self) -> &ICompileResultVTable;

    #[inline]
    fn get_item_count(&self) -> u32 {
        unsafe { (ICompileResult::vt(self).get_item_count)(self.thisptr()) }
    }

    fn get_item_data(&self, index: u32) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (ICompileResult::vt(self).get_item_data)(self.thisptr(), index, o.as_mut_ptr())
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    fn get_metadata(&self) -> Result<IMetadataPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe { (ICompileResult::vt(self).get_metadata)(self.thisptr(), o.as_mut_ptr()) })?;

        Ok(IMetadataPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }
}

slang_interface_ptr!(ICompileResultPtr, COMPILE_RESULT_IID, ICompileResultVTable);
impl IUnknown for ICompileResultPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl ICastable for ICompileResultPtr {
    #[inline(always)]
    fn vt(&self) -> &ICastableVTable {
        &self.vtable().base
    }
}
impl ICompileResult for ICompileResultPtr {
    #[inline(always)]
    fn vt(&self) -> &ICompileResultVTable {
        self.vtable()
    }
}

pub const COMPONENT_TYPE_IID: SlangUUID = SlangUUID::new(
    0x5bc42be8,
    0x5c50,
    0x4929,
    [0x9e, 0x5e, 0xd1, 0x5e, 0x7c, 0x24, 0x01, 0x5f],
);
#[repr(C)]
pub struct IComponentTypeVTable {
    base: IUnknownVTable,
    get_session: unsafe extern "system" fn(this: *mut c_void) -> *mut c_void,
    get_layout: unsafe extern "system" fn(
        this: *mut c_void,
        target_index: SlangInt,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut SlangReflection,
    get_specialization_param_count: unsafe extern "system" fn(this: *mut c_void) -> SlangInt,
    get_entry_point_code: unsafe extern "system" fn(
        this: *mut c_void,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_code: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_result_as_file_system: unsafe extern "system" fn(
        this: *mut c_void,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_file_system: *mut *mut c_void,
    ) -> SlangResult,
    get_entry_point_hash: unsafe extern "system" fn(
        this: *mut c_void,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_hash: *mut *mut c_void,
    ),
    specialize: unsafe extern "system" fn(
        this: *mut c_void,
        specialization_args: *const SpecializationArg,
        specialization_arg_count: SlangInt,
        out_specialized_component_type: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    link: unsafe extern "system" fn(
        this: *mut c_void,
        out_linked_component_type: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_entry_point_host_callable: unsafe extern "system" fn(
        this: *mut c_void,
        entry_point_index: c_int,
        target_index: c_int,
        out_shared_library: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    rename_entry_point: unsafe extern "system" fn(
        this: *mut c_void,
        new_name: *const c_char,
        out_entry_point: *mut *mut c_void,
    ) -> SlangResult,
    link_with_options: unsafe extern "system" fn(
        this: *mut c_void,
        out_linked_component_type: *mut *mut c_void,
        compiler_option_entry_count: u32,
        compiler_option_entries: *mut CompilerOptionEntry,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_target_code: unsafe extern "system" fn(
        this: *mut c_void,
        target_index: SlangInt,
        out_code: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_target_metadata: unsafe extern "system" fn(
        this: *mut c_void,
        target_index: SlangInt,
        out_metadata: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_entry_point_metadata: unsafe extern "system" fn(
        this: *mut c_void,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_metadata: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
}
pub trait IComponentType: IUnknown {
    fn vt(&self) -> &IComponentTypeVTable;

    #[inline]
    fn get_session(&self) -> ISessionPtr {
        unsafe {
            ISessionPtr(NonNull::new_unchecked((IComponentType::vt(self)
                .get_session)(
                self.thisptr()
            )))
        }
    }

    #[inline]
    fn get_layout(
        &self,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> &mut reflection::Shader {
        unsafe {
            reflection::Shader::from_mut_ptr((IComponentType::vt(self).get_layout)(
                self.thisptr(),
                target_index,
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            ))
        }
    }

    #[inline]
    fn get_specialization_param_count(&self) -> SlangInt {
        unsafe { (IComponentType::vt(self).get_specialization_param_count)(self.thisptr()) }
    }

    fn get_entry_point_code(
        &self,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).get_entry_point_code)(
                self.thisptr(),
                entry_point_index,
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(unsafe { IBlobPtr(NonNull::new_unchecked(o.assume_init())) })
    }

    fn get_result_as_file_system(
        &self,
        entry_point_index: SlangInt,
        target_index: SlangInt,
    ) -> Result<NonNull<c_void>> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).get_result_as_file_system)(
                self.thisptr(),
                entry_point_index,
                target_index,
                o.as_mut_ptr(),
            )
        })?;

        Ok(unsafe { NonNull::new_unchecked(o.assume_init()) })
    }

    fn get_entry_point_hash(
        &self,
        entry_point_index: SlangInt,
        target_index: SlangInt,
    ) -> IBlobPtr {
        let mut o = MaybeUninit::uninit();
        unsafe {
            (IComponentType::vt(self).get_entry_point_hash)(
                self.thisptr(),
                entry_point_index,
                target_index,
                o.as_mut_ptr(),
            );
        }

        IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) })
    }

    fn specialize(
        &self,
        specialization_args: &[SpecializationArg],
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IComponentTypePtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).specialize)(
                self.thisptr(),
                specialization_args.as_ptr(),
                specialization_args.len() as _,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IComponentTypePtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn link(
        &self,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IComponentTypePtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).link)(
                self.thisptr(),
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IComponentTypePtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn get_entry_point_host_callable(
        &self,
        entry_point_index: c_int,
        target_index: c_int,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<ISharedLibraryPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).get_entry_point_host_callable)(
                self.thisptr(),
                entry_point_index,
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(ISharedLibraryPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn rename_entry_point(&self, new_name: &CStr) -> Result<IComponentTypePtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).rename_entry_point)(
                self.thisptr(),
                new_name.as_ptr(),
                o.as_mut_ptr(),
            )
        })?;

        Ok(IComponentTypePtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn link_with_options(
        &self,
        compiler_option_entries: &mut [CompilerOptionEntry],
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IComponentTypePtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).link_with_options)(
                self.thisptr(),
                o.as_mut_ptr(),
                compiler_option_entries.len() as _,
                compiler_option_entries.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IComponentTypePtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn get_target_code(
        &self,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).get_target_code)(
                self.thisptr(),
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    fn get_target_metadata(
        &self,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IMetadataPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).get_target_metadata)(
                self.thisptr(),
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IMetadataPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn get_entry_point_metadata(
        &self,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<Option<IBlobPtr>>>,
    ) -> Result<IMetadataPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType::vt(self).get_entry_point_metadata)(
                self.thisptr(),
                entry_point_index,
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IMetadataPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }
}

slang_interface_ptr!(IComponentTypePtr, COMPONENT_TYPE_IID, IComponentTypeVTable);
impl IUnknown for IComponentTypePtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base
    }
}
impl IComponentType for IComponentTypePtr {
    #[inline(always)]
    fn vt(&self) -> &IComponentTypeVTable {
        self.vtable()
    }
}

pub const ENTRY_POINT_IID: SlangUUID = SlangUUID::new(
    0x8f241361,
    0xf5bd,
    0x4ca0,
    [0xa3, 0xac, 0x02, 0xf7, 0xfa, 0x24, 0x02, 0xb8],
);
#[repr(C)]
pub struct IEntryPointVTable {
    base: IComponentTypeVTable,
    get_function_reflection:
        unsafe extern "system" fn(this: *mut c_void) -> *mut SlangReflectionFunction,
}
pub trait IEntryPoint: IComponentType {
    fn vt(&self) -> &IEntryPointVTable;

    #[inline(always)]
    fn get_function_reflection(&self) -> &mut SlangReflectionFunction {
        unsafe { &mut *(IEntryPoint::vt(self).get_function_reflection)(self.thisptr()) }
    }
}

slang_interface_ptr!(IEntryPointPtr, ENTRY_POINT_IID, IEntryPointVTable);
impl IUnknown for IEntryPointPtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl IComponentType for IEntryPointPtr {
    #[inline(always)]
    fn vt(&self) -> &IComponentTypeVTable {
        &self.vtable().base
    }
}
impl IEntryPoint for IEntryPointPtr {
    #[inline(always)]
    fn vt(&self) -> &IEntryPointVTable {
        self.vtable()
    }
}

pub const TYPE_CONFORMANCE_IID: SlangUUID = SlangUUID::new(
    0x73eb3147,
    0xe544,
    0x41b5,
    [0xb8, 0xf0, 0xa2, 0x44, 0xdf, 0x21, 0x94, 0x0b],
);
#[repr(C)]
pub struct ITypeConformanceVTable {
    base: IComponentTypeVTable,
}
pub trait ITypeConformance: IComponentType {
    fn vt(&self) -> &ITypeConformanceVTable;
}

slang_interface_ptr!(
    ITypeConformancePtr,
    TYPE_CONFORMANCE_IID,
    ITypeConformanceVTable
);
impl IUnknown for ITypeConformancePtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl IComponentType for ITypeConformancePtr {
    #[inline(always)]
    fn vt(&self) -> &IComponentTypeVTable {
        &self.vtable().base
    }
}
impl ITypeConformance for ITypeConformancePtr {
    #[inline(always)]
    fn vt(&self) -> &ITypeConformanceVTable {
        self.vtable()
    }
}

pub const COMPONENT_TYPE2_IID: SlangUUID = SlangUUID::new(
    0x9c2a4b3d,
    0x7f68,
    0x4e91,
    [0xa5, 0x2c, 0x8b, 0x19, 0x3e, 0x45, 0x7a, 0x9f],
);
#[repr(C)]
pub struct IComponentType2VTable {
    base: IUnknownVTable,
    get_target_compile_result: unsafe extern "system" fn(
        this: *mut c_void,
        target_index: SlangInt,
        out_compile_result: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_entry_point_compile_result: unsafe extern "system" fn(
        this: *mut c_void,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_compile_result: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
}
pub trait IComponentType2: IUnknown {
    fn vt(&self) -> &IComponentType2VTable;

    fn get_target_compile_result(
        &self,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> Result<ICompileResultPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType2::vt(self).get_target_compile_result)(
                self.thisptr(),
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(ICompileResultPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    fn get_entry_point_compile_result(
        &self,
        entry_point_index: SlangInt,
        target_index: SlangInt,
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> Result<ICompileResultPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IComponentType2::vt(self).get_entry_point_compile_result)(
                self.thisptr(),
                entry_point_index,
                target_index,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(ICompileResultPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }
}

slang_interface_ptr!(
    IComponentType2Ptr,
    COMPONENT_TYPE2_IID,
    IComponentType2VTable
);
impl IUnknown for IComponentType2Ptr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base
    }
}
impl IComponentType2 for IComponentType2Ptr {
    #[inline(always)]
    fn vt(&self) -> &IComponentType2VTable {
        self.vtable()
    }
}

pub const MODULE_IID: SlangUUID = SlangUUID::new(
    0xc720e64,
    0x8722,
    0x4d31,
    [0x89, 0x90, 0x63, 0x8a, 0x98, 0xb1, 0xc2, 0x79],
);
#[repr(C)]
pub struct IModuleVTable {
    base: IComponentTypeVTable,
    find_entry_point_by_name: unsafe extern "system" fn(
        this: *mut c_void,
        name: *const c_char,
        out_entry_point: *mut *mut c_void,
    ) -> SlangResult,
    get_defined_entry_point_count: unsafe extern "system" fn(this: *mut c_void) -> i32,
    get_defined_entry_point: unsafe extern "system" fn(
        this: *mut c_void,
        index: i32,
        out_entry_point: *mut *mut c_void,
    ) -> SlangResult,
    serialize: unsafe extern "system" fn(
        this: *mut c_void,
        out_serialized_blob: *mut *mut c_void,
    ) -> SlangResult,
    write_to_file:
        unsafe extern "system" fn(this: *mut c_void, file_name: *const c_char) -> SlangResult,
    get_name: unsafe extern "system" fn(this: *mut c_void) -> *const c_char,
    get_file_path: unsafe extern "system" fn(this: *mut c_void) -> *const c_char,
    get_unique_identity: unsafe extern "system" fn(this: *mut c_void) -> *const c_char,
    find_and_check_entry_point: unsafe extern "system" fn(
        this: *mut c_void,
        name: *const c_char,
        stage: Stage,
        out_entry_point: *mut *mut c_void,
        out_diagnostics: *mut *mut c_void,
    ) -> SlangResult,
    get_dependency_file_count: unsafe extern "system" fn(this: *mut c_void) -> i32,
    get_dependency_file_path:
        unsafe extern "system" fn(this: *mut c_void, index: i32) -> *const c_char,
    get_module_reflection: unsafe extern "system" fn(this: *mut c_void) -> *mut SlangReflectionDecl,
    disassemble: unsafe extern "system" fn(
        this: *mut c_void,
        out_disassembled_blob: *mut *mut c_void,
    ) -> SlangResult,
}
pub trait IModule: IComponentType {
    fn vt(&self) -> &IModuleVTable;

    fn find_entry_point_by_name(&self, name: &CStr) -> Result<IEntryPointPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IModule::vt(self).find_entry_point_by_name)(
                self.thisptr(),
                name.as_ptr(),
                o.as_mut_ptr(),
            )
        })?;

        Ok(IEntryPointPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    #[inline(always)]
    fn get_defined_entry_point_count(&self) -> i32 {
        unsafe { (IModule::vt(self).get_defined_entry_point_count)(self.thisptr()) }
    }

    fn get_defined_entry_point(&self, index: i32) -> Result<IEntryPointPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IModule::vt(self).get_defined_entry_point)(self.thisptr(), index, o.as_mut_ptr())
        })?;

        Ok(IEntryPointPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    #[inline(always)]
    fn iter_defined_entry_point<'x>(&'x self) -> impl Iterator<Item = Result<IEntryPointPtr>> + 'x {
        (0..self.get_defined_entry_point_count()).map(|n| self.get_defined_entry_point(n))
    }

    fn serialize(&self) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe { (IModule::vt(self).serialize)(self.thisptr(), o.as_mut_ptr()) })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }

    fn write_to_file(&self, file_name: &CStr) -> Result<()> {
        rw(unsafe { (IModule::vt(self).write_to_file)(self.thisptr(), file_name.as_ptr()) })?;

        Ok(())
    }

    #[inline]
    fn get_name(&self) -> &CStr {
        unsafe { CStr::from_ptr((IModule::vt(self).get_name)(self.thisptr())) }
    }

    #[inline]
    fn get_file_path(&self) -> &CStr {
        unsafe { CStr::from_ptr((IModule::vt(self).get_file_path)(self.thisptr())) }
    }

    #[inline]
    fn get_unique_identity(&self) -> &CStr {
        unsafe { CStr::from_ptr((IModule::vt(self).get_unique_identity)(self.thisptr())) }
    }

    fn find_and_check_entry_point(
        &self,
        name: &CStr,
        stage: Stage,
        out_diagnostics: Option<&mut MaybeUninit<IBlobPtr>>,
    ) -> Result<IEntryPointPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe {
            (IModule::vt(self).find_and_check_entry_point)(
                self.thisptr(),
                name.as_ptr(),
                stage,
                o.as_mut_ptr(),
                out_diagnostics.map_or_else(core::ptr::null_mut, MaybeUninit::as_mut_ptr) as _,
            )
        })?;

        Ok(IEntryPointPtr(unsafe {
            NonNull::new_unchecked(o.assume_init())
        }))
    }

    #[inline]
    fn get_dependency_file_count(&self) -> i32 {
        unsafe { (IModule::vt(self).get_dependency_file_count)(self.thisptr()) }
    }

    #[inline]
    fn get_dependency_file_path(&self, index: i32) -> &CStr {
        unsafe {
            CStr::from_ptr((IModule::vt(self).get_dependency_file_path)(
                self.thisptr(),
                index,
            ))
        }
    }

    #[inline]
    fn get_module_reflection(&self) -> &mut SlangReflectionDecl {
        unsafe { &mut *(IModule::vt(self).get_module_reflection)(self.thisptr()) }
    }

    fn disassemble(&self) -> Result<IBlobPtr> {
        let mut o = MaybeUninit::uninit();
        rw(unsafe { (IModule::vt(self).disassemble)(self.thisptr(), o.as_mut_ptr()) })?;

        Ok(IBlobPtr(unsafe { NonNull::new_unchecked(o.assume_init()) }))
    }
}

slang_interface_ptr!(IModulePtr, MODULE_IID, IModuleVTable);
impl IUnknown for IModulePtr {
    #[inline(always)]
    fn vt(&self) -> &IUnknownVTable {
        &self.vtable().base.base
    }
}
impl IComponentType for IModulePtr {
    #[inline(always)]
    fn vt(&self) -> &IComponentTypeVTable {
        &self.vtable().base
    }
}
impl IModule for IModulePtr {
    #[inline(always)]
    fn vt(&self) -> &IModuleVTable {
        self.vtable()
    }
}

#[repr(C)]
pub struct TargetDesc {
    pub structure_size: usize,
    pub format: CompileTarget,
    pub profile: ProfileID,
    pub flags: TargetFlags,
    pub floating_point_mode: FloatingPointMode,
    pub line_directive_mode: LineDirectiveMode,
    pub force_glsl_scalar_buffer_layout: bool,
    pub compiler_option_entries: *mut CompilerOptionEntry,
    pub compiler_option_entry_count: u32,
}
impl Default for TargetDesc {
    fn default() -> Self {
        Self {
            structure_size: core::mem::size_of::<Self>(),
            format: SLANG_TARGET_UNKNOWN,
            profile: SLANG_PROFILE_UNKNOWN,
            flags: SLANG_TARGET_FLAG_GENERATE_SPIRV_DIRECTLY,
            floating_point_mode: SLANG_FLOATING_POINT_MODE_DEFAULT,
            line_directive_mode: SLANG_LINE_DIRECTIVE_MODE_DEFAULT,
            force_glsl_scalar_buffer_layout: false,
            compiler_option_entries: core::ptr::null_mut(),
            compiler_option_entry_count: 0,
        }
    }
}

pub type SessionFlags = u32;
pub const SESSION_FLAGS_NONE: SessionFlags = 0;

#[repr(C)]
pub struct PreprocessorMacroDesc {
    pub name: *const c_char,
    pub value: *const c_char,
}
impl PreprocessorMacroDesc {
    /// Creates a new constant value from statically-living strings
    pub const fn new_static(name: &'static CStr, value: &'static CStr) -> Self {
        Self {
            name: name.as_ptr(),
            value: value.as_ptr(),
        }
    }
}

#[repr(C)]
pub struct SessionDesc {
    pub structure_size: u32,
    pub targets: *const TargetDesc,
    pub target_count: SlangInt,
    pub flags: SessionFlags,
    pub default_matrix_layout_mode: SlangMatrixLayoutMode,
    pub search_paths: *const *const c_char,
    pub search_path_count: SlangInt,
    pub preprocessor_macros: *const PreprocessorMacroDesc,
    pub preprocessor_macro_count: SlangInt,
    pub file_system: *mut core::ffi::c_void,
    pub enable_effect_annotations: bool,
    pub allow_glsl_syntax: bool,
    pub compiler_option_entries: *mut CompilerOptionEntry,
    pub compiler_option_entry_count: u32,
    pub skip_spirv_validation: bool,
}
impl Default for SessionDesc {
    fn default() -> Self {
        Self {
            structure_size: core::mem::size_of::<Self>() as _,
            targets: core::ptr::null(),
            target_count: 0,
            flags: SESSION_FLAGS_NONE,
            default_matrix_layout_mode: SLANG_MATRIX_LAYOUT_ROW_MAJOR,
            search_paths: core::ptr::null(),
            search_path_count: 0,
            preprocessor_macros: core::ptr::null(),
            preprocessor_macro_count: 0,
            file_system: core::ptr::null_mut(),
            enable_effect_annotations: false,
            allow_glsl_syntax: false,
            compiler_option_entries: core::ptr::null_mut(),
            compiler_option_entry_count: 0,
            skip_spirv_validation: false,
        }
    }
}

#[repr(C)]
pub enum CompilerOptionName {
    MacroDefine,
    DepFile,
    EntryPointName,
    Specialize,
    Help,
    HelpStyle,
    Include, // stringValue: additional include path.
    Language,
    MatrixLayoutColumn,         // bool
    MatrixLayoutRow,            // bool
    ZeroInitialize,             // bool
    IgnoreCapabilities,         // bool
    RestrictiveCapabilityCheck, // bool
    ModuleName,                 // stringValue0: module name.
    Output,
    Profile, // intValue0: profile
    Stage,   // intValue0: stage
    Target,  // intValue0: CodeGenTarget
    Version,
    WarningsAsErrors, // stringValue0: "all" or comma separated list of warning codes or names.
    DisableWarnings,  // stringValue0: comma separated list of warning codes or names.
    EnableWarning,    // stringValue0: warning code or name.
    DisableWarning,   // stringValue0: warning code or name.
    DumpWarningDiagnostics,
    InputFilesRemain,
    EmitIr,                        // bool
    ReportDownstreamTime,          // bool
    ReportPerfBenchmark,           // bool
    ReportCheckpointIntermediates, // bool
    SkipSPIRVValidation,           // bool
    SourceEmbedStyle,
    SourceEmbedName,
    SourceEmbedLanguage,
    DisableShortCircuit,            // bool
    MinimumSlangOptimization,       // bool
    DisableNonEssentialValidations, // bool
    DisableSourceMap,               // bool
    UnscopedEnum,                   // bool
    PreserveParameters,             // bool: preserve all resource parameters in the output code.
    // Target
    Capability,                // intValue0: CapabilityName
    DefaultImageFormatUnknown, // bool
    DisableDynamicDispatch,    // bool
    DisableSpecialization,     // bool
    FloatingPointMode,         // intValue0: FloatingPointMode
    DebugInformation,          // intValue0: DebugInfoLevel
    LineDirectiveMode,
    Optimization, // intValue0: OptimizationLevel
    Obfuscate,    // bool

    VulkanBindShift, // intValue0 (higher 8 bits): kind; intValue0(lower bits): set; intValue1:
    // shift
    VulkanBindGlobals,       // intValue0: index; intValue1: set
    VulkanInvertY,           // bool
    VulkanUseDxPositionW,    // bool
    VulkanUseEntryPointName, // bool
    VulkanUseGLLayout,       // bool
    VulkanEmitReflection,    // bool

    GLSLForceScalarLayout,   // bool
    EnableEffectAnnotations, // bool

    EmitSpirvViaGLSL,     // bool (will be deprecated)
    EmitSpirvDirectly,    // bool (will be deprecated)
    SPIRVCoreGrammarJSON, // stringValue0: json path
    IncompleteLibrary,    // bool, when set, will not issue an error when the linked program has
    // unresolved extern function symbols.

    // Downstream
    CompilerPath,
    DefaultDownstreamCompiler,
    DownstreamArgs, // stringValue0: downstream compiler name. stringValue1: argument list, one
    // per line.
    PassThrough,

    // Repro
    DumpRepro,
    DumpReproOnError,
    ExtractRepro,
    LoadRepro,
    LoadReproDirectory,
    ReproFallbackDirectory,

    // Debugging
    DumpAst,
    DumpIntermediatePrefix,
    DumpIntermediates, // bool
    DumpIr,            // bool
    DumpIrIds,
    PreprocessorOutput,
    OutputIncludes,
    ReproFileSystem,
    #[deprecated = "removed"]
    SerialIR, // deprecated and removed
    SkipCodeGen, // bool
    ValidateIr,  // bool
    VerbosePaths,
    VerifyDebugSerialIr,
    NoCodeGen, // Not used.

    // Experimental
    FileSystem,
    Heterogeneous,
    NoMangle,
    NoHLSLBinding,
    NoHLSLPackConstantBufferElements,
    ValidateUniformity,
    AllowGLSL,
    EnableExperimentalPasses,
    BindlessSpaceIndex, // int

    // Internal
    ArchiveType,
    CompileCoreModule,
    Doc,

    IrCompression, //< deprecated

    LoadCoreModule,
    ReferenceModule,
    SaveCoreModule,
    SaveCoreModuleBinSource,
    TrackLiveness,
    LoopInversion, // bool, enable loop inversion optimization

    ParameterBlocksUseRegisterSpaces, // Deprecated
    LanguageVersion,                  // intValue0: SlangLanguageVersion
    TypeConformance, // stringValue0: additional type conformance to link, in the format of
    // "<TypeName>:<IInterfaceName>[=<sequentialId>]", for example
    // "Impl:IFoo=3" or "Impl:IFoo".
    EnableExperimentalDynamicDispatch, // bool, experimental
    EmitReflectionJSON,                // bool

    CountOfParsableOptions,

    // Used in parsed options only.
    DebugInformationFormat,  // intValue0: DebugInfoFormat
    VulkanBindShiftAll,      // intValue0: kind; intValue1: shift
    GenerateWholeProgram,    // bool
    UseUpToDateBinaryModule, // bool, when set, will only load
    // precompiled modules if it is up-to-date with its source.
    EmbedDownstreamIR, // bool
    ForceDXLayout,     // bool

    // Add this new option to the end of the list to avoid breaking ABI as much as possible.
    // Setting of EmitSpirvDirectly or EmitSpirvViaGLSL will turn into this option internally.
    EmitSpirvMethod, // enum SlangEmitSpirvMethod

    SaveGLSLModuleBinSource,

    SkipDownstreamLinking, // bool, experimental
    DumpModule,

    GetModuleInfo,              // Print serialized module version and name
    GetSupportedModuleVersions, // Print the min and max module versions this compiler supports

    EmitSeparateDebug, // bool

    // Floating point denormal handling modes
    DenormalModeFp16,
    DenormalModeFp32,
    DenormalModeFp64,

    // Bitfield options
    UseMSVCStyleBitfieldPacking, // bool

    ForceCLayout, // bool

    CountOf,
}

#[repr(C)]
pub enum CompilerOptionValueKind {
    Int,
    String,
}

#[repr(C)]
pub struct CompilerOptionValue {
    pub kind: CompilerOptionValueKind,
    pub int_value0: i32,
    pub int_value1: i32,
    pub string_value0: *const c_char,
    pub string_value1: *const c_char,
}

#[repr(C)]
pub struct CompilerOptionEntry {
    pub name: CompilerOptionName,
    pub value: CompilerOptionValue,
}

#[repr(C)]
pub enum ContainerType {
    None,
    UnsizedArray,
    StructuredBuffer,
    ConstantBuffer,
    ParameterBlock,
}

#[repr(C)]
pub struct SpecializationArg {
    pub kind: SpecializationArgKind,
    pub value: SpecializationArgValue,
}

#[repr(i32)]
pub enum SpecializationArgKind {
    /// An invalid specialization argument.
    Unknown,
    /// Specialize to a type.
    Type,
    /// An expression representing a type or value
    Expr,
}

#[repr(C)]
pub union SpecializationArgValue {
    pub r#type: *mut SlangReflectionType,
    pub expr: *const core::ffi::c_char,
}

pub fn create_global_session(desc: &GlobalSessionDesc) -> Result<IGlobalSessionPtr> {
    let mut o = MaybeUninit::uninit();
    rw(unsafe { ffi::slang_createGlobalSession2(desc, o.as_mut_ptr()) })?;

    Ok(IGlobalSessionPtr(unsafe {
        NonNull::new_unchecked(o.assume_init())
    }))
}
