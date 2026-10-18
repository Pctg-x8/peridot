use core::ffi::{c_char, c_int, c_uint, c_void};

#[cfg(target_pointer_width = "64")]
pub type SlangInt = i64;
#[cfg(not(target_pointer_width = "64"))]
pub type SlangInt = i32;

#[cfg(target_pointer_width = "64")]
pub type SlangUInt = u64;
#[cfg(not(target_pointer_width = "64"))]
pub type SlangUInt = u32;

pub type SlangBool = bool;

pub type SlangResult = i32;

pub type SlangFuncPtr = extern "C" fn();

/// Type that identifies how a path should be interpreted
pub type SlangPathType = c_uint;
/// Path specified specifies a directory.
pub const SLANG_PATH_TYPE_DIRECTORY: SlangPathType = 0;
/// Path specified is to a file.
pub const SLANG_PATH_TYPE_FILE: SlangPathType = 1;

pub type FileSystemContentsCallBack =
    extern "C" fn(path_type: SlangPathType, name: *const c_char, user_data: *mut c_void);

/// Identifies different types of writer target
pub type SlangWriterChannel = c_uint;
pub const SLANG_WRITER_CHANNEL_DIAGNOSTIC: SlangWriterChannel = 0;
pub const SLANG_WRITER_CHANNEL_STD_OUTPUT: SlangWriterChannel = 1;
pub const SLANG_WRITER_CHANNEL_STD_ERROR: SlangWriterChannel = 2;
pub const SLANG_WRITER_CHANNEL_COUNT_OF: SlangWriterChannel = SLANG_WRITER_CHANNEL_STD_ERROR + 1;

pub type SlangWriterMode = c_uint;
pub const SLANG_WRITER_MODE_TEXT: SlangWriterMode = 0;
pub const SLANG_WRITER_MODE_BINARY: SlangWriterMode = 1;

pub type SlangDiagnosticCallback = extern "C" fn(message: *const c_char, user_data: *mut c_void);

macro_rules! ffi_opaque_struct {
    ($name: ident) => {
        #[repr(C)]
        pub struct $name(
            [u8; 0],
            core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
        );
    };
}

ffi_opaque_struct!(SlangReflection);
ffi_opaque_struct!(SlangReflectionEntryPoint);
ffi_opaque_struct!(SlangReflectionDecl);
ffi_opaque_struct!(SlangReflectionModifier);
ffi_opaque_struct!(SlangReflectionType);
ffi_opaque_struct!(SlangReflectionTypeLayout);
ffi_opaque_struct!(SlangReflectionVariable);
ffi_opaque_struct!(SlangReflectionVariableLayout);
ffi_opaque_struct!(SlangReflectionTypeParameter);
ffi_opaque_struct!(SlangReflectionFunction);
ffi_opaque_struct!(SlangReflectionGeneric);
ffi_opaque_struct!(SlangReflectionUserAttribute);

#[repr(C)]
pub union SlangReflectionGenericArg {
    pub type_val: *mut SlangReflectionType,
    pub int_val: i64,
    pub bool_val: bool,
}

pub type SlangReflectionGenericArgType = c_int;
pub const SLANG_GENERIC_ARG_TYPE: SlangReflectionGenericArgType = 0;
pub const SLANG_GENERIC_ARG_INT: SlangReflectionGenericArgType = 1;
pub const SLANG_GENERIC_ARG_BOOL: SlangReflectionGenericArgType = 2;

pub type SlangTypeKind = c_uint;
pub const SLANG_TYPE_KIND_NONE: SlangTypeKind = 0;
pub const SLANG_TYPE_KIND_STRUCT: SlangTypeKind = 1;
pub const SLANG_TYPE_KIND_ARRAY: SlangTypeKind = 2;
pub const SLANG_TYPE_KIND_MATRIX: SlangTypeKind = 3;
pub const SLANG_TYPE_KIND_VECTOR: SlangTypeKind = 4;
pub const SLANG_TYPE_KIND_SCALAR: SlangTypeKind = 5;
pub const SLANG_TYPE_KIND_CONSTANT_BUFFER: SlangTypeKind = 6;
pub const SLANG_TYPE_KIND_RESOURCE: SlangTypeKind = 7;
pub const SLANG_TYPE_KIND_SAMPLER_STATE: SlangTypeKind = 8;
pub const SLANG_TYPE_KIND_TEXTURE_BUFFER: SlangTypeKind = 9;
pub const SLANG_TYPE_KIND_SHADER_STORAGE_BUFFER: SlangTypeKind = 10;
pub const SLANG_TYPE_KIND_PARAMETER_BLOCK: SlangTypeKind = 11;
pub const SLANG_TYPE_KIND_GENERIC_TYPE_PARAMETER: SlangTypeKind = 12;
pub const SLANG_TYPE_KIND_INTERFACE: SlangTypeKind = 13;
pub const SLANG_TYPE_KIND_OUTPUT_STREAM: SlangTypeKind = 14;
pub const SLANG_TYPE_KIND_MESH_OUTPUT: SlangTypeKind = 15;
pub const SLANG_TYPE_KIND_SPECIALIZED: SlangTypeKind = 16;
pub const SLANG_TYPE_KIND_FEEDBACK: SlangTypeKind = 17;
pub const SLANG_TYPE_KIND_POINTER: SlangTypeKind = 18;
pub const SLANG_TYPE_KIND_DYNAMIC_RESOURCE: SlangTypeKind = 19;
pub const SLANG_TYPE_KIND_COUNT: SlangTypeKind = SLANG_TYPE_KIND_DYNAMIC_RESOURCE + 1;

pub type SlangScalarType = c_uint;
pub const SLANG_SCALAR_TYPE_NONE: SlangScalarType = 0;
pub const SLANG_SCALAR_TYPE_VOID: SlangScalarType = 1;
pub const SLANG_SCALAR_TYPE_BOOL: SlangScalarType = 2;
pub const SLANG_SCALAR_TYPE_INT32: SlangScalarType = 3;
pub const SLANG_SCALAR_TYPE_UINT32: SlangScalarType = 4;
pub const SLANG_SCALAR_TYPE_INT64: SlangScalarType = 5;
pub const SLANG_SCALAR_TYPE_UINT64: SlangScalarType = 6;
pub const SLANG_SCALAR_TYPE_FLOAT16: SlangScalarType = 7;
pub const SLANG_SCALAR_TYPE_FLOAT32: SlangScalarType = 8;
pub const SLANG_SCALAR_TYPE_FLOAT64: SlangScalarType = 9;
pub const SLANG_SCALAR_TYPE_INT8: SlangScalarType = 10;
pub const SLANG_SCALAR_TYPE_UINT8: SlangScalarType = 11;
pub const SLANG_SCALAR_TYPE_INT16: SlangScalarType = 12;
pub const SLANG_SCALAR_TYPE_UINT16: SlangScalarType = 13;
pub const SLANG_SCALAR_TYPE_INTPTR: SlangScalarType = 14;
pub const SLANG_SCALAR_TYPE_UINTPT: SlangScalarType = 15;

pub type SlangDeclKind = c_uint;

pub type SlangResourceShape = c_uint;

pub type SlangResourceAccess = c_uint;

pub type SlangParameterCategory = c_uint;
pub const SLANG_PARAMETER_CATEGORY_NONE: SlangParameterCategory = 0;
pub const SLANG_PARAMETER_CATEGORY_MIXED: SlangParameterCategory = 1;
pub const SLANG_PARAMETER_CATEGORY_CONSTANT_BUFFER: SlangParameterCategory = 2;
pub const SLANG_PARAMETER_CATEGORY_SHADER_RESOURCE: SlangParameterCategory = 3;
pub const SLANG_PARAMETER_CATEGORY_UNORDERED_ACCESS: SlangParameterCategory = 4;
pub const SLANG_PARAMETER_CATEGORY_VARYING_INPUT: SlangParameterCategory = 5;
pub const SLANG_PARAMETER_CATEGORY_VARYING_OUTPUT: SlangParameterCategory = 6;
pub const SLANG_PARAMETER_CATEGORY_SAMPLER_STATE: SlangParameterCategory = 7;
pub const SLANG_PARAMETER_CATEGORY_UNIFORM: SlangParameterCategory = 8;
pub const SLANG_PARAMETER_CATEGORY_DESCRIPTOR_TABLE_SLOT: SlangParameterCategory = 9;
pub const SLANG_PARAMETER_CATEGORY_SPECIALIZATION_CONSTANT: SlangParameterCategory = 10;
pub const SLANG_PARAMETER_CATEGORY_PUSH_CONSTANT_BUFFER: SlangParameterCategory = 11;
/// HLSL register `space`, Vulkan GLSL `set`
pub const SLANG_PARAMETER_CATEGORY_REGISTER_SPACE: SlangParameterCategory = 12;
/// A parameter whose type is to be specialized by a global generic type argument
pub const SLANG_PARAMETER_CATEGORY_GENERIC: SlangParameterCategory = 13;
pub const SLANG_PARAMETER_CATEGORY_RAY_PAYLOAD: SlangParameterCategory = 14;
pub const SLANG_PARAMETER_CATEGORY_HIT_ATTRIBUTES: SlangParameterCategory = 15;
pub const SLANG_PARAMETER_CATEGORY_CALLABLE_PAYLOAD: SlangParameterCategory = 16;
pub const SLANG_PARAMETER_CATEGORY_SHADER_RECORD: SlangParameterCategory = 17;
/// An existential type parameter represents a "hole" that needs to be filled with a concrete type to enable
/// generation of specialized code.
pub const SLANG_PARAMETER_CATEGORY_EXISTENTIAL_TYPE_PARAM: SlangParameterCategory = 18;
/// An existential object parameter represents a value that needs to be passed in to provide data for
/// some interface-type shader parameter.
pub const SLANG_PARAMETER_CATEGORY_EXISTENTIAL_OBJECT_PARAM: SlangParameterCategory = 19;
/// The register space offset for the sub-elements that occupies register spaces.
pub const SLANG_PARAMETER_CATEGORY_SUB_ELEMENT_REGISTER_SPACE: SlangParameterCategory = 20;
/// The input_attachment_index subpass occupancy tracker
pub const SLANG_PARAMETER_CATEGORY_SUBPASS: SlangParameterCategory = 21;
/// Metal tier-1 argument buffer element [[id]].
pub const SLANG_PARAMETER_CATEGORY_METAL_ARGUMENT_BUFFER_ELEMENT: SlangParameterCategory = 22;
/// Metal [[attribute]] inputs.
pub const SLANG_PARAMETER_CATEGORY_METAL_ATTRIBUTE: SlangParameterCategory = 23;
/// Metal [[payload]] inputs
pub const SLANG_PARAMETER_CATEGORY_METAL_PAYLOAD: SlangParameterCategory = 24;
pub const SLANG_PARAMETER_CATEGORY_COUNT: SlangParameterCategory =
    SLANG_PARAMETER_CATEGORY_METAL_PAYLOAD + 1;

pub type SlangBindingType = u32;
pub const SLANG_BINDING_TYPE_UNKNOWN: SlangBindingType = 0;
pub const SLANG_BINDING_TYPE_SAMPLER: SlangBindingType = 1;
pub const SLANG_BINDING_TYPE_TEXTURE: SlangBindingType = 2;
pub const SLANG_BINDING_TYPE_CONSTANT_BUFFER: SlangBindingType = 3;
pub const SLANG_BINDING_TYPE_PARAMETER_BLOCK: SlangBindingType = 4;
pub const SLANG_BINDING_TYPE_TYPED_BUFFER: SlangBindingType = 5;
pub const SLANG_BINDING_TYPE_RAW_BUFFER: SlangBindingType = 6;
pub const SLANG_BINDING_TYPE_COMBINED_TEXTURE_SAMPLER: SlangBindingType = 7;
pub const SLANG_BINDING_TYPE_INPUT_RENDER_TARGET: SlangBindingType = 8;
pub const SLANG_BINDING_TYPE_INLINE_UNIFORM_DATA: SlangBindingType = 9;
pub const SLANG_BINDING_TYPE_RAY_TRACING_ACCELERATION_STRUCTURE: SlangBindingType = 10;
pub const SLANG_BINDING_TYPE_VARYING_INPUT: SlangBindingType = 11;
pub const SLANG_BINDING_TYPE_VARYING_OUTPUT: SlangBindingType = 12;
pub const SLANG_BINDING_TYPE_EXISTENTIAL_VALUE: SlangBindingType = 13;
pub const SLANG_BINDING_TYPE_PUSH_CONSTANT: SlangBindingType = 14;
pub const SLANG_BINDING_TYPE_MUTABLE_FLAG: SlangBindingType = 0x100;
pub const SLANG_BINDING_TYPE_MUTABLE_TETURE: SlangBindingType =
    SLANG_BINDING_TYPE_TEXTURE | SLANG_BINDING_TYPE_MUTABLE_FLAG;
pub const SLANG_BINDING_TYPE_MUTABLE_TYPED_BUFFER: SlangBindingType =
    SLANG_BINDING_TYPE_TYPED_BUFFER | SLANG_BINDING_TYPE_MUTABLE_FLAG;
pub const SLANG_BINDING_TYPE_MUTABLE_RAW_BUFFER: SlangBindingType =
    SLANG_BINDING_TYPE_RAW_BUFFER | SLANG_BINDING_TYPE_MUTABLE_FLAG;
pub const SLANG_BINDING_TYPE_BASE_MASK: SlangBindingType = 0x00FF;
pub const SLANG_BINDING_TYPE_EXT_MASK: SlangBindingType = 0xFF00;

pub type SlangLayoutRules = u32;
pub const SLANG_LAYOUT_RULES_DEFAULT: SlangLayoutRules = 0;
pub const SLANG_LAYOUT_RULES_METAL_ARGUMENT_BUFFER_TIER_2: SlangLayoutRules = 1;

pub type SlangModifierID = u32;

pub type SlangImageFormat = u32;

pub const SLANG_UNORDERED_SIZE: usize = !0;

pub type SlangPassThrough = core::ffi::c_int;
pub const SLANG_PASS_THROUGH_NONE: SlangPassThrough = 0;
pub const SLANG_PASS_THROUGH_FXC: SlangPassThrough = 1;
pub const SLANG_PASS_THROUGH_DXC: SlangPassThrough = 2;
pub const SLANG_PASS_THROUGH_GLSLANG: SlangPassThrough = 3;
pub const SLANG_PASS_THROUGH_SPIRV_DIS: SlangPassThrough = 4;
/// Clang C/C++ compiler
pub const SLANG_PASS_THROUGH_CLANG: SlangPassThrough = 5;
/// Visual studio C/C++ compiler
pub const SLANG_PASS_THROUGH_VISUAL_STUDIO: SlangPassThrough = 6;
/// GCC C/C++ compiler
pub const SLANG_PASS_THROUGH_GCC: SlangPassThrough = 7;
/// Generic C or C++ compiler, which is decided by the source type
pub const SLANG_PASS_THROUGH_GENERIC_C_CPP: SlangPassThrough = 8;
/// NVRTC Cuda compiler
pub const SLANG_PASS_THROGUH_NVRTC: SlangPassThrough = 9;
/// LLVM 'compiler' - includes LLVM and Clang
pub const SLANG_PASS_THROUGH_LLVM: SlangPassThrough = 10;
/// SPIRV-opt
pub const SLANG_PASS_THROUGH_SPIRV_OPT: SlangPassThrough = 11;
/// Metal compiler
pub const SLANG_PASS_THROUGH_METAL: SlangPassThrough = 12;
/// Tint WGSL compiler
pub const SLANG_PASS_THROUGH_TINT: SlangPassThrough = 13;
/// SPIRV-link
pub const SLANG_PASS_THROUGH_SPIRV_LINK: SlangPassThrough = 14;
pub const SLANG_PASS_THROUGH_COUNT_OF: SlangPassThrough = SLANG_PASS_THROUGH_SPIRV_LINK + 1;

pub type SlangArchiveType = core::ffi::c_int;
pub const SLANG_ARCHIVE_TYPE_UNDEFINED: SlangArchiveType = 0;
pub const SLANG_ARCHIVE_TYPE_ZIP: SlangArchiveType = 1;
/// Riff container with no compression
pub const SLANG_ARCHIVE_TYPE_RIFF: SlangArchiveType = 2;
pub const SLANG_ARCHIVE_TYPE_RIFF_DEFLATE: SlangArchiveType = 3;
pub const SLANG_ARCHIVE_TYPE_RIFF_LZ4: SlangArchiveType = 4;
pub const SLANG_ARCHIVE_TYPE_COUNT_OF: SlangArchiveType = SLANG_ARCHIVE_TYPE_RIFF_LZ4 + 1;

/// Flags to control code generation behavior of a compilation target
pub type SlangTargetFlags = core::ffi::c_uint;
/// When compiling for a D3D Shader Model 5.1 or higher target, allocate distinct register spaces for parameter blocks.
#[deprecated = "This behavior is not enabled unconditionally."]
pub const SLANG_TARGET_FLAG_PARAMETER_BLOCKS_USE_REGISTER_SPACES: SlangTargetFlags = 1 << 4;
/// When set, will generate target code that contains all entrypoints defined in the input source or specified via
/// `spAddEntryPoint` function in a single output module (library/source file).
pub const SLANG_TARGET_FLAG_GENERATE_WHOLE_PROGRAM: SlangTargetFlags = 1 << 8;
/// When set, will dump out the IR between intermediate compilation steps.
pub const SLANG_TARGET_FLAG_DUMP_IR: SlangTargetFlags = 1 << 9;
/// When set, will generate SPIRV directly rather than via glslang.
///
/// This flag will be deprecated, use CompilerOption instead.
pub const SLANG_TARGET_FLAG_GENERATE_SPIRV_DIRECTLY: SlangTargetFlags = 1 << 10;

/// Options to control floating-point precision guarantees for a target.
pub type SlangFloatingPointMode = core::ffi::c_uint;
pub const SLANG_FLOATING_POINT_MODE_DEFAULT: SlangFloatingPointMode = 0;
pub const SLANG_FLOATING_POINT_MODE_FAST: SlangFloatingPointMode = 1;
pub const SLANG_FLOATING_POINT_MODE_PRECISE: SlangFloatingPointMode = 2;

/// Options to control floating-point denormal handling mode for a target.
pub type SlangFpDenormalMode = core::ffi::c_uint;
pub const SLANG_FP_DENORM_MODE_ANY: SlangFpDenormalMode = 0;
pub const SLANG_FP_DENORM_MODE_PRESERVE: SlangFpDenormalMode = 1;
pub const SLANG_FP_DENORM_MODE_FTZ: SlangFpDenormalMode = 2;

/// Options to control emission of `#line` directives
pub type SlangLineDirectiveMode = core::ffi::c_uint;
/// Default behavior: pick behavior base on target.
pub const SLANG_LINE_DIRECTIVE_MODE_DEFAULT: SlangLineDirectiveMode = 0;
/// Don't emit line directives at all.
pub const SLANG_LINE_DIRECTIVE_MODE_NONE: SlangLineDirectiveMode = 1;
/// Emit standard C-style `#line` directives.
pub const SLANG_LINE_DIRECTIVE_MODE_STANDARD: SlangLineDirectiveMode = 2;
/// Emit GLSL-style directives with file *number* instead of name
pub const SLANG_LINE_DIRECTIVE_MODE_GLSL: SlangLineDirectiveMode = 3;
/// Use a source map to track line mappings (ie no #line will appear in emitting source)
pub const SLANG_LINE_DIRECTIVE_MODE_SOURCE_MAP: SlangLineDirectiveMode = 4;

pub type SlangSourceLanguage = core::ffi::c_int;
pub const SLANG_SOURCE_LANGUAGE_UNKNOWN: SlangSourceLanguage = 0;
pub const SLANG_SOURCE_LANGUAGE_SLANG: SlangSourceLanguage = 1;
pub const SLANG_SOURCE_LANGUAGE_HLSL: SlangSourceLanguage = 2;
pub const SLANG_SOURCE_LANGUAGE_GLSL: SlangSourceLanguage = 3;
pub const SLANG_SOURCE_LANGUAGE_C: SlangSourceLanguage = 4;
pub const SLANG_SOURCE_LANGUAGE_CPP: SlangSourceLanguage = 5;
pub const SLANG_SOURCE_LANGUAGE_CUDA: SlangSourceLanguage = 6;
pub const SLANG_SOURCE_LANGUAGE_SPIRV: SlangSourceLanguage = 7;
pub const SLANG_SOURCE_LANGUAGE_METAL: SlangSourceLanguage = 8;
pub const SLANG_SOURCE_LANGUAGE_WGSL: SlangSourceLanguage = 9;
pub const SLANG_SOURCE_LANGUAGE_COUNT_OF: SlangSourceLanguage = SLANG_SOURCE_LANGUAGE_WGSL + 1;

pub type SlangProfileID = c_uint;
pub const SLANG_PROFILE_UNKNOWN: SlangProfileID = 0;

pub type SlangCapabilityID = i32;
pub const SLANG_CAPABILITY_UNKNOWN: SlangCapabilityID = 0;

pub type SlangMatrixLayoutMode = c_uint;
pub const SLANG_MATRIX_LAYOUT_MODE_UNKNOWN: SlangMatrixLayoutMode = 0;
pub const SLANG_MATRIX_LAYOUT_ROW_MAJOR: SlangMatrixLayoutMode = 1;
pub const SLANG_MATRIX_LAYOUT_COLUMN_MAJOR: SlangMatrixLayoutMode = 2;

pub type SlangStage = u32;
pub const SLANG_STAGE_NONE: SlangStage = 0;
pub const SLANG_STAGE_VERTEX: SlangStage = 1;
pub const SLANG_STAGE_HULL: SlangStage = 2;
pub const SLANG_STAGE_DOMAIN: SlangStage = 3;
pub const SLANG_STAGE_GEOMETRY: SlangStage = 4;
pub const SLANG_STAGE_FRAGMENT: SlangStage = 5;
pub const SLANG_STAGE_COMPUTE: SlangStage = 6;
pub const SLANG_STAGE_RAY_GENERATION: SlangStage = 7;
pub const SLANG_STAGE_INTERSECTION: SlangStage = 8;
pub const SLANG_STAGE_ANY_HIT: SlangStage = 9;
pub const SLANG_STAGE_CLOSEST_HIT: SlangStage = 10;
pub const SLANG_STAGE_MISS: SlangStage = 11;
pub const SLANG_STAGE_CALLABLE: SlangStage = 12;
pub const SLANG_STAGE_MESH: SlangStage = 13;
pub const SLANG_STAGE_AMPLIFICATION: SlangStage = 14;
pub const SLANG_STAGE_DISPATCH: SlangStage = 15;
pub const SLANG_STAGE_COUNT: SlangStage = SLANG_STAGE_DISPATCH + 1;
// alias
pub const SLANG_STAGE_PIXEL: SlangStage = SLANG_STAGE_FRAGMENT;

pub type SlangCompileTarget = core::ffi::c_int;
pub const SLANG_TARGET_UNKNOWN: SlangCompileTarget = 0;
pub const SLANG_TARGET_NONE: SlangCompileTarget = 1;
pub const SLANG_GLSL: SlangCompileTarget = 2;
#[deprecated = "just use `SLANG_GLSL`"]
pub const SLANG_GLSL_VULKAN_DEPRECATED: SlangCompileTarget = 3;
#[deprecated]
pub const SLANG_GLSL_VULKAN_ONE_DESC_DEPRECATED: SlangCompileTarget = 4;
pub const SLANG_HLSL: SlangCompileTarget = 5;
pub const SLANG_SPIRV: SlangCompileTarget = 6;
pub const SLANG_SPIRV_ASM: SlangCompileTarget = 7;
pub const SLANG_DXBC: SlangCompileTarget = 8;
pub const SLANG_DXBC_ASM: SlangCompileTarget = 9;
pub const SLANG_DXIL: SlangCompileTarget = 10;
pub const SLANG_DXIL_ASM: SlangCompileTarget = 11;
/// The C language
pub const SLANG_C_SOURCE: SlangCompileTarget = 12;
/// C++ code for shared kernels.
pub const SLANG_CPP_SOURCE: SlangCompileTarget = 13;
/// Standalone binary executable (for hosting CPU/OS)
pub const SLANG_HOST_EXECUTABLE: SlangCompileTarget = 14;
/// A shared library/Dll for shader kernels (for hosting CPU/OS)
pub const SLANG_SHADER_SHARED_LIBRARY: SlangCompileTarget = 15;
/// A CPU target that makes the compiled shader code available to be run immediately
pub const SLANG_SHADER_HOST_CALLABLE: SlangCompileTarget = 16;
/// Cuda source
pub const SLANG_CUDA_SOURCE: SlangCompileTarget = 17;
/// PTX
pub const SLANG_PTX: SlangCompileTarget = 18;
/// Object code that contains CUDA functions.
pub const SLANG_CUDA_OBJECT_CODE: SlangCompileTarget = 19;
/// Object code that can be used for later linking
pub const SLANG_OBJECT_CODE: SlangCompileTarget = 20;
/// C++ code for host library or executable.
pub const SLANG_HOST_CPP_SOURCE: SlangCompileTarget = 21;
/// Host callable host code (ie non kernel/shader)
pub const SLANG_HOST_HOST_CALLABLE: SlangCompileTarget = 22;
/// C++ PyTorch binding code.
pub const SLANG_CPP_PYTORCH_BINDINGS: SlangCompileTarget = 23;
/// Metal shading language
pub const SLANG_METAL: SlangCompileTarget = 24;
/// Metal library
pub const SLANG_METAL_LIB: SlangCompileTarget = 25;
/// Metal library assembly
pub const SLANG_METAL_LIB_ASM: SlangCompileTarget = 26;
/// A shared library/Dll for host code (for hosting CPU/OS)
pub const SLANG_HOST_SHARED_LIBRARY: SlangCompileTarget = 27;
/// WebGPU shading language
pub const SLANG_WGSL: SlangCompileTarget = 28;
/// SPIR-V assembly via WebGPU shading language
pub const SLANG_WGSL_SPIRV_ASM: SlangCompileTarget = 29;
/// SPIR-V via WebGPU shading language
pub const SLANG_WGSL_SPIRV: SlangCompileTarget = 30;
/// Bytecode that can be interpreted by the Slang VM
pub const SLANG_HOST_VM: SlangCompileTarget = 31;
pub const SLANG_TARGET_COUNT_OF: SlangCompileTarget = SLANG_HOST_VM + 1;

// Opaque: ISession

// Helper: UserAttribute = Attribute = SlangReflectionAttribute = SlangReflectionUserAttribute
// Helper: TypeReflection = SlangReflectionType
// Helper: ParameterCategory = SlangParameterCategory
// Helper: BindingType = SlangBindingType
// Helper: TypeLayoutReflection = SlangReflectionTypeLayout
// Helper: Modifier::ID = SlangModifierID
// Helper: VariableReflection = SlangReflectionVariable
// Helper: VariableLayoutReflection = SlangReflectionVariableLayout
// Helper: FunctionReflection = SlangReflectionFunction
// Helper: GenericReflection = SlangReflectionGeneric
// Helper: EntryPointLayout = EntryPointReflection = SlangReflectionEntryPoint
// Helper: TypeParameterReflection = SlangReflectionTypeParameter
// Helper: LayoutRules = SlangLayoutRules
// Helper: ProgramLayout = ShaderReflection = SlangReflection = SlangProgramLayout
// Helper: GenericArgType = SlangReflectionGenericArgType
// Helper: DeclReflection = SlangReflectionDecl

pub type CompileCoreModuleFlags = u32;
pub const COMPILE_CORE_MODULE_FLAG_WRITE_DOCUMENTATION: CompileCoreModuleFlags = 0x01;

// Helper: IBlob = ISlangBlob
// Opaque: IComponentType
// Opaque: ITypeConformance
// Opaque: IModule
// Opaque: SessionDesc
// Opaque: SpecializationArg
// Opaque: TargetDesc

#[repr(C)]
pub enum BuiltinModuleName {
    Core,
    GLSL,
}

pub const SLANG_API_VERSION: u32 = 0;
pub const SLANG_LANGUAGE_VERSION_2025: u32 = 2025;

/// Description of a Slang global session.
#[repr(C)]
pub struct SlangGlobalSessionDesc {
    /// Size of this struct.
    pub structure_size: u32,
    /// Slang API version.
    pub api_version: u32,
    /// Specify the oldest Slang language version that any sessions will use.
    pub min_language_version: u32,
    /// Whether to enable GLSL support.
    pub enable_glsl: bool,
    /// Reserved for future use.
    pub reserved: [u32; 16],
}
impl Default for SlangGlobalSessionDesc {
    fn default() -> Self {
        Self {
            structure_size: core::mem::size_of::<Self>() as _,
            api_version: SLANG_API_VERSION,
            min_language_version: SLANG_LANGUAGE_VERSION_2025,
            enable_glsl: false,
            reserved: [0; 16],
        }
    }
}

unsafe extern "C" {
    pub fn spGetBuildTagString() -> *const c_char;
    pub fn slang_createGlobalSession2(
        desc: *const SlangGlobalSessionDesc,
        out_global_session: *mut *mut c_void,
    ) -> SlangResult;

    pub fn spReflectionUserAttribute_GetName(
        attrib: *mut SlangReflectionUserAttribute,
    ) -> *const c_char;
    pub fn spReflectionUserAttribute_GetArgumentCount(
        attrib: *mut SlangReflectionUserAttribute,
    ) -> u32;
    pub fn spReflectionUserAttribute_GetArgumentType(
        attrib: *mut SlangReflectionUserAttribute,
        index: u32,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionUserAttribute_GetArgumentValueInt(
        attrib: *mut SlangReflectionUserAttribute,
        index: u32,
        value: *mut c_int,
    ) -> SlangResult;
    pub fn spReflectionUserAttribute_GetArgumentValueFloat(
        attrib: *mut SlangReflectionUserAttribute,
        index: u32,
        value: *mut core::ffi::c_float,
    ) -> SlangResult;
    pub fn spReflectionUserAttribute_GetArgumentValueString(
        attrib: *mut SlangReflectionUserAttribute,
        index: u32,
        out_size: *mut usize,
    ) -> *const c_char;

    pub fn spReflectionType_GetKind(r#type: *mut SlangReflectionType) -> SlangTypeKind;
    pub fn spReflectionType_GetFieldCount(r#type: *mut SlangReflectionType) -> c_uint;
    pub fn spReflectionType_GetFieldByIndex(
        r#type: *mut SlangReflectionType,
        index: c_uint,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionType_GetSpecializedElementCount(
        r#type: *mut SlangReflectionType,
        reflection: *mut SlangReflection,
    ) -> usize;
    pub fn spReflectionType_GetElementType(
        r#type: *mut SlangReflectionType,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionType_GetRowCount(r#type: *mut SlangReflectionType) -> c_uint;
    pub fn spReflectionType_GetColumnCount(r#type: *mut SlangReflectionType) -> c_uint;
    pub fn spReflectionType_GetScalarType(r#type: *mut SlangReflectionType) -> SlangScalarType;
    pub fn spReflectionType_GetResourceResultType(
        r#type: *mut SlangReflectionType,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionType_GetResourceShape(
        r#type: *mut SlangReflectionType,
    ) -> SlangResourceShape;
    pub fn spReflectionType_GetResourceAccess(
        r#type: *mut SlangReflectionType,
    ) -> SlangResourceAccess;
    pub fn spReflectionType_GetName(r#type: *mut SlangReflectionType) -> *const c_char;
    pub fn spReflectionType_GetFullName(
        r#type: *mut SlangReflectionType,
        out_name_blob: *mut *mut c_void,
    ) -> SlangResult;
    pub fn spReflectionType_GetUserAttributeCount(r#type: *mut SlangReflectionType) -> c_uint;
    pub fn spReflectionType_GetUserAttribute(
        r#type: *mut SlangReflectionType,
        index: c_uint,
    ) -> *mut SlangReflectionUserAttribute;
    pub fn spReflectionType_FindUserAttributeByName(
        r#type: *mut SlangReflectionType,
        name: *const c_char,
    ) -> *mut SlangReflectionUserAttribute;
    pub fn spReflectionType_applySpecializations(
        r#type: *mut SlangReflectionType,
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionType_GetGenericContainer(
        r#type: *mut SlangReflectionType,
    ) -> *mut SlangReflectionGeneric;

    pub fn spReflectionTypeLayout_GetType(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionTypeLayout_getKind(r#type: *mut SlangReflectionTypeLayout) -> SlangTypeKind;
    pub fn spReflectionTypeLayout_GetSize(
        r#type: *mut SlangReflectionTypeLayout,
        category: SlangParameterCategory,
    ) -> usize;
    pub fn spReflectionTypeLayout_GetStride(
        r#type: *mut SlangReflectionTypeLayout,
        category: SlangParameterCategory,
    ) -> usize;
    pub fn spReflectionTypeLayout_getAlignment(
        r#type: *mut SlangReflectionTypeLayout,
        category: SlangParameterCategory,
    ) -> i32;
    pub fn spReflectionTypeLayout_GetFieldCount(r#type: *mut SlangReflectionTypeLayout) -> c_uint;
    pub fn spReflectionTypeLayout_GetFieldByIndex(
        r#type: *mut SlangReflectionTypeLayout,
        index: c_uint,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionTypeLayout_findFieldIndexByName(
        r#type: *mut SlangReflectionTypeLayout,
        name_begin: *const c_char,
        name_end: *const c_char,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_GetExplicitCounter(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionTypeLayout_GetElementStride(
        r#type: *mut SlangReflectionTypeLayout,
        category: SlangParameterCategory,
    ) -> usize;
    pub fn spReflectionTypeLayout_GetElementTypeLayout(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionTypeLayout;
    pub fn spReflectionTypeLayout_GetElementVarLayout(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionTypeLayout_getContainerVarLayout(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionTypeLayout_GetParameterCategory(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> SlangParameterCategory;
    pub fn spReflectionTypeLayout_GetCategoryCount(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> c_uint;
    pub fn spReflectionTypeLayout_GetCategoryByIndex(
        r#type: *mut SlangReflectionTypeLayout,
        index: c_uint,
    ) -> SlangParameterCategory;
    pub fn spReflectionTypeLayout_GetMatrixLayoutMode(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> SlangMatrixLayoutMode;
    pub fn spReflectionTypeLayout_getGenericParamIndex(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> c_int;
    pub fn spReflectionTypeLayout_getPendingDataTypeLayout(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionTypeLayout;
    pub fn spReflectionTypeLayout_getSpecializedTypePendingDataVarLayout(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionTypeLayout_getBindingRangeCount(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getBindingRangeType(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangBindingType;
    pub fn spReflectionTypeLayout_isBindingRangeSpecializable(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getBindingRangeBindingCount(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getFieldBindingRangeOffset(
        r#type: *mut SlangReflectionTypeLayout,
        field_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getExplicitCounterBindingRangeOffset(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getBindingRangeLeafTypeLayout(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> *mut SlangReflectionTypeLayout;
    pub fn spReflectionTypeLayout_getBindingRangeLeafVariable(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionTypeLayout_getBindingRAngeImageFormat(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangImageFormat;
    pub fn spReflectionTypeLayout_getBindingRAngeDescriptorSetIndex(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getBindingRangeFirstDescriptorRangeIndex(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getBindingRangeDescriptorRangeCount(
        r#type: *mut SlangReflectionTypeLayout,
        index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getDescriptorSetCount(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getDescriptorSetSpaceOffset(
        r#type: *mut SlangReflectionTypeLayout,
        set_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getDescriptorSetDescriptorRangeCount(
        r#type: *mut SlangReflectionTypeLayout,
        set_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getDescriptorSetDescriptorRangeIndexOffset(
        r#type: *mut SlangReflectionTypeLayout,
        set_index: SlangInt,
        range_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getDescriptorSetDescriptorRangeDescriptorCount(
        r#type: *mut SlangReflectionTypeLayout,
        set_index: SlangInt,
        range_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getDescriptorSetDescriptorRangeType(
        r#type: *mut SlangReflectionTypeLayout,
        set_index: SlangInt,
        range_index: SlangInt,
    ) -> SlangBindingType;
    pub fn spReflectionTypeLayout_getDescriptorSetDescriptorRangeCategory(
        r#type: *mut SlangReflectionTypeLayout,
        set_index: SlangInt,
        range_index: SlangInt,
    ) -> SlangParameterCategory;
    pub fn spReflectionTypeLayout_getSubObjectRangeCount(
        r#type: *mut SlangReflectionTypeLayout,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getSubObjectRangeBindingRangeIndex(
        r#type: *mut SlangReflectionTypeLayout,
        sub_object_range_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getSubObjectRangeSpaceOffset(
        r#type: *mut SlangReflectionTypeLayout,
        sub_object_range_index: SlangInt,
    ) -> SlangInt;
    pub fn spReflectionTypeLayout_getSubObjectRangeOffset(
        r#type: *mut SlangReflectionTypeLayout,
        sub_object_range_index: SlangInt,
    ) -> *mut SlangReflectionVariableLayout;

    pub fn spReflectionVariable_GetName(var: *mut SlangReflectionVariable) -> *const c_char;
    pub fn spReflectionVariable_GetType(
        var: *mut SlangReflectionVariable,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionVariable_FindModifier(
        var: *mut SlangReflectionVariable,
        id: SlangModifierID,
    ) -> *mut SlangReflectionModifier;
    pub fn spReflectionVariable_GetUserAttributeCount(var: *mut SlangReflectionVariable) -> c_uint;
    pub fn spReflectionVariable_GetUserAttribute(
        var: *mut SlangReflectionVariable,
        index: c_uint,
    ) -> *mut SlangReflectionUserAttribute;
    pub fn spReflectionVariable_FindUserAttributeByName(
        var: *mut SlangReflectionVariable,
        global_session: *mut c_void,
        name: *const c_char,
    ) -> *mut SlangReflectionUserAttribute;
    pub fn spReflectionVariable_HasDefaultValue(var: *mut SlangReflectionVariable) -> bool;
    pub fn spReflectionVariable_GetDefaultValueInt(
        var: *mut SlangReflectionVariable,
        value: *mut i64,
    ) -> SlangResult;
    pub fn spReflectionVariable_GetGenericContainer(
        var: *mut SlangReflectionVariable,
    ) -> *mut SlangReflectionGeneric;
    pub fn spReflectionVariable_applySpecializations(
        var: *mut SlangReflectionVariable,
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionVariable;

    pub fn spReflectionVariableLayout_GetVariable(
        var: *mut SlangReflectionVariableLayout,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionVariableLayout_GetTypeLayout(
        var: *mut SlangReflectionVariableLayout,
    ) -> *mut SlangReflectionTypeLayout;
    pub fn spReflectionVariableLayout_GetOffset(
        var: *mut SlangReflectionVariableLayout,
        category: SlangParameterCategory,
    ) -> usize;
    pub fn spReflectionParameter_GetBindingIndex(
        parameter: *mut SlangReflectionVariableLayout,
    ) -> c_uint;
    pub fn spReflectionParameter_GetBindingSpace(
        parameter: *mut SlangReflectionVariableLayout,
    ) -> c_uint;
    pub fn spReflectionVariableLayout_GetSpace(
        var: *mut SlangReflectionVariableLayout,
        category: SlangParameterCategory,
    ) -> usize;
    pub fn spReflectionVariableLayout_GetImageFormat(
        var: *mut SlangReflectionVariableLayout,
    ) -> SlangImageFormat;
    pub fn spReflectionVariableLayout_GetSemanticName(
        var: *mut SlangReflectionVariableLayout,
    ) -> *const c_char;
    pub fn spReflectionVariableLayout_GetSemanticIndex(
        var: *mut SlangReflectionVariableLayout,
    ) -> usize;
    pub fn spReflectionVariableLayout_getStage(
        var: *mut SlangReflectionVariableLayout,
    ) -> SlangStage;
    pub fn spReflectionVariableLayout_getPendingDataLayout(
        var: *mut SlangReflectionVariableLayout,
    ) -> *mut SlangReflectionVariableLayout;

    pub fn spReflectionFunction_GetName(inFunc: *mut SlangReflectionFunction) -> *const c_char;
    pub fn spReflectionFunction_GetResultType(
        func: *mut SlangReflectionFunction,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionFunction_GetParameterCount(func: *mut SlangReflectionFunction) -> c_uint;
    pub fn spReflectionFunction_GetParameter(
        func: *mut SlangReflectionFunction,
        index: c_uint,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionFunction_GetUserAttributeCount(func: *mut SlangReflectionFunction)
    -> c_uint;
    pub fn spReflectionFunction_GetUserAttribute(
        func: *mut SlangReflectionFunction,
        index: c_uint,
    ) -> *mut SlangReflectionUserAttribute;
    pub fn spReflectionFunction_FindUserAttributeByName(
        func: *mut SlangReflectionFunction,
        global_session: *mut c_void,
        name: *const c_char,
    ) -> *mut SlangReflectionUserAttribute;
    pub fn spReflectionFunction_FindModifier(
        func: *mut SlangReflectionFunction,
        id: SlangModifierID,
    ) -> *mut SlangReflectionModifier;
    pub fn spReflectionFunction_GetGenericContainer(
        func: *mut SlangReflectionFunction,
    ) -> *mut SlangReflectionGeneric;
    pub fn spReflectionFunction_applySpecializations(
        func: *mut SlangReflectionFunction,
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionFunction;
    pub fn spReflectionFunction_specializeWithArgTypes(
        func: *mut SlangReflectionFunction,
        arg_count: c_uint,
        types: *const *mut SlangReflectionType,
    ) -> *mut SlangReflectionFunction;
    pub fn spReflectionFunction_isOverloaded(func: *mut SlangReflectionFunction) -> bool;
    pub fn spReflectionFunction_getOverloadCount(func: *mut SlangReflectionFunction) -> c_uint;
    pub fn spReflectionFunction_getOverload(
        func: *mut SlangReflectionFunction,
        index: c_uint,
    ) -> *mut SlangReflectionFunction;

    pub fn spReflectionGeneric_asDecl(
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionDecl;
    pub fn spReflectionGeneric_GetName(generic: *mut SlangReflectionGeneric) -> *const c_char;
    pub fn spReflectionGeneric_GetTypeParameterCount(
        generic: *mut SlangReflectionGeneric,
    ) -> c_uint;
    pub fn spReflectionGeneric_GetTypeParameter(
        generic: *mut SlangReflectionGeneric,
        index: c_uint,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionGeneric_GetValueParameterCount(
        generic: *mut SlangReflectionGeneric,
    ) -> c_uint;
    pub fn spReflectionGeneric_GetValueParameter(
        generic: *mut SlangReflectionGeneric,
        index: c_uint,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionGeneric_GetTypeParameterConstraintCount(
        generic: *mut SlangReflectionGeneric,
        type_param: *mut SlangReflectionVariable,
    ) -> c_uint;
    pub fn spReflectionGeneric_GetTypeParameterConstraintType(
        generic: *mut SlangReflectionGeneric,
        type_param: *mut SlangReflectionVariable,
        index: c_uint,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionGeneric_GetInnerDecl(
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionDecl;
    pub fn spReflectionGeneric_GetInnerKind(generic: *mut SlangReflectionGeneric) -> SlangDeclKind;
    pub fn spReflectionGeneric_GetOuterGenericContainer(
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionGeneric;
    pub fn spReflectionGeneric_GetConcreteType(
        generic: *mut SlangReflectionGeneric,
        type_param: *mut SlangReflectionVariable,
    ) -> *mut SlangReflectionType;
    pub fn spReflectionGeneric_GetConcreteIntVal(
        generic: *mut SlangReflectionGeneric,
        value_param: *mut SlangReflectionVariable,
    ) -> i64;
    pub fn spReflectionGeneric_applySpecializations(
        generic: *mut SlangReflectionGeneric,
        generic: *mut SlangReflectionGeneric,
    ) -> *mut SlangReflectionGeneric;

    pub fn spReflectionEntryPoint_getName(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> *const c_char;
    pub fn spReflectionEntryPoint_getNameOverride(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> *const c_char;
    pub fn spReflectionEntryPoint_getParameterCount(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> c_uint;
    pub fn spReflectionEntryPoint_getFunction(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> *mut SlangReflectionFunction;
    pub fn spReflectionEntryPoint_getParameterByIndex(
        entry_point: *mut SlangReflectionEntryPoint,
        index: c_uint,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionEntryPoint_getStage(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> SlangStage;
    pub fn spReflectionEntryPoint_getComputeThreadGroupSize(
        entry_point: *mut SlangReflectionEntryPoint,
        axis_count: SlangUInt,
        out_size_along_axis: *mut SlangUInt,
    );
    pub fn spReflectionEntryPoint_getComputeWaveSize(
        entry_point: *mut SlangReflectionEntryPoint,
        out_wave_size: *mut SlangUInt,
    );
    pub fn spReflectionEntryPoint_usesAnySampleRateInput(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> c_int;
    pub fn spReflectionEntryPoint_getVarLayout(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionEntryPoint_getResultVarLayout(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflectionEntryPoint_hasDefaultConstantBuffer(
        entry_point: *mut SlangReflectionEntryPoint,
    ) -> c_int;

    pub fn spReflectionTypeParameter_GetName(
        type_param: *mut SlangReflectionTypeParameter,
    ) -> *const c_char;
    pub fn spReflectionTypeParameter_GetIndex(
        type_param: *mut SlangReflectionTypeParameter,
    ) -> c_uint;
    pub fn spReflectionTypeParameter_GetConstraintCount(
        type_param: *mut SlangReflectionTypeParameter,
    ) -> c_uint;
    pub fn spReflectionTypeParameter_GetConstraintByIndex(
        type_param: *mut SlangReflectionTypeParameter,
        index: c_uint,
    ) -> *mut SlangReflectionType;

    pub fn spReflection_GetParameterCount(reflection: *mut SlangReflection) -> c_uint;
    pub fn spReflection_GetTypeParameterCount(reflection: *mut SlangReflection) -> c_uint;
    pub fn spReflection_GetSession(reflection: *mut SlangReflection) -> *mut c_void;
    pub fn spReflection_GetTypeParameterByIndex(
        reflection: *mut SlangReflection,
        index: c_uint,
    ) -> *mut SlangReflectionTypeParameter;
    pub fn spReflection_FindTypeParameter(
        reflection: *mut SlangReflection,
        name: *const c_char,
    ) -> *mut SlangReflectionTypeParameter;
    pub fn spReflection_GetParameterByIndex(
        reflection: *mut SlangReflection,
        index: c_uint,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflection_getEntryPointCount(reflection: *mut SlangReflection) -> SlangUInt;
    pub fn spReflection_getEntryPointByIndex(
        reflection: *mut SlangReflection,
        index: SlangUInt,
    ) -> *mut SlangReflectionEntryPoint;
    pub fn spReflection_getGlobalConstantBufferBinding(
        reflection: *mut SlangReflection,
    ) -> SlangUInt;
    pub fn spReflection_getGlobalConstantBufferSize(reflection: *mut SlangReflection) -> usize;
    pub fn spReflection_FindTypeByName(
        reflection: *mut SlangReflection,
        name: *const c_char,
    ) -> *mut SlangReflectionType;
    pub fn spReflection_FindFunctionByName(
        reflection: *mut SlangReflection,
        name: *const c_char,
    ) -> *mut SlangReflectionFunction;
    pub fn spReflection_FindFunctionByNameInType(
        reflection: *mut SlangReflection,
        r#type: *mut SlangReflectionType,
        name: *const c_char,
    ) -> *mut SlangReflectionFunction;
    pub fn spReflection_FindVarByNameInType(
        reflection: *mut SlangReflection,
        r#type: *mut SlangReflectionType,
        name: *const c_char,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflection_GetTypeLayout(
        reflection: *mut SlangReflection,
        r#type: *mut SlangReflectionType,
        rules: SlangLayoutRules,
    ) -> *mut SlangReflectionTypeLayout;
    pub fn spReflection_findEntryPointByName(
        reflection: *mut SlangReflection,
        name: *const c_char,
    ) -> *mut SlangReflectionEntryPoint;
    pub fn spReflection_specializeType(
        reflection: *mut SlangReflection,
        r#type: *mut SlangReflectionType,
        specialization_arg_count: SlangInt,
        specialization_args: *const *mut SlangReflectionType,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut SlangReflectionType;
    pub fn spReflection_specializeGeneric(
        reflection: *mut SlangReflection,
        generic: *mut SlangReflectionGeneric,
        specialization_arg_count: SlangInt,
        specialization_arg_types: *const SlangReflectionGenericArgType,
        specialization_arg_vals: *const SlangReflectionGenericArg,
        out_diagnostics: *mut *mut c_void,
    ) -> *mut SlangReflectionGeneric;
    pub fn spReflection_isSubType(
        reflection: *mut SlangReflection,
        sub_type: *mut SlangReflectionType,
        super_type: *mut SlangReflectionType,
    ) -> bool;
    pub fn spReflection_getHashedStringCount(reflection: *mut SlangReflection) -> SlangUInt;
    pub fn spReflection_getHashedString(
        reflection: *mut SlangReflection,
        index: SlangUInt,
        out_count: *mut usize,
    ) -> *const c_char;
    pub fn spReflection_getGlobalParamsTypeLayout(
        reflection: *mut SlangReflection,
    ) -> *mut SlangReflectionTypeLayout;
    pub fn spReflection_getGlobalParamsVarLayout(
        reflection: *mut SlangReflection,
    ) -> *mut SlangReflectionVariableLayout;
    pub fn spReflection_ToJson(
        reflection: *mut SlangReflection,
        request: *mut c_void, /* SlangCompileRequest */
        out_blob: *mut *mut c_void,
    ) -> SlangResult;

    pub fn spReflectionDecl_getName(decl: *mut SlangReflectionDecl) -> *const c_char;
    pub fn spReflectionDecl_getKind(decl: *mut SlangReflectionDecl) -> SlangDeclKind;
    pub fn spReflectionDecl_getChildrenCount(decl: *mut SlangReflectionDecl) -> c_uint;
    pub fn spReflectionDecl_getChild(
        decl: *mut SlangReflectionDecl,
        index: c_uint,
    ) -> *mut SlangReflectionDecl;
    pub fn spReflection_getTypeFromDecl(decl: *mut SlangReflectionDecl)
    -> *mut SlangReflectionType;
    pub fn spReflectionDecl_castToVariable(
        decl: *mut SlangReflectionDecl,
    ) -> *mut SlangReflectionVariable;
    pub fn spReflectionDecl_castToFunction(
        decl: *mut SlangReflectionDecl,
    ) -> *mut SlangReflectionFunction;
    pub fn spReflectionDecl_castToGeneric(
        decl: *mut SlangReflectionDecl,
    ) -> *mut SlangReflectionGeneric;
    pub fn spReflectionDecl_getParent(decl: *mut SlangReflectionDecl) -> *mut SlangReflectionDecl;
}
