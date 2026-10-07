#![allow(non_camel_case_types, non_upper_case_globals)]

use core::ffi::*;

pub type spv_result_t = i32;
pub const SPV_SUCCESS: spv_result_t = 0;
pub const SPV_UNSUPPORTED: spv_result_t = 1;
pub const SPV_END_OF_STREAM: spv_result_t = 2;
pub const SPV_WARNING: spv_result_t = 3;
pub const SPV_FAILED_MATCH: spv_result_t = 4;
pub const SPV_REQUESTED_TERMINATION: spv_result_t = 5;
pub const SPV_ERROR_INTERNAL: spv_result_t = -1;
pub const SPV_ERROR_OUT_OF_MEMORY: spv_result_t = -2;
pub const SPV_ERROR_INVALID_POINTER: spv_result_t = -3;
pub const SPV_ERROR_INVALID_BINARY: spv_result_t = -4;
pub const SPV_ERROR_INVALID_TEXT: spv_result_t = -5;
pub const SPV_ERROR_INVALID_TABLE: spv_result_t = -6;
pub const SPV_ERROR_INVALID_VALUE: spv_result_t = -7;
pub const SPV_ERROR_INVALID_DIAGNOSTIC: spv_result_t = -8;
pub const SPV_ERROR_INVALID_LOOKUP: spv_result_t = -9;
pub const SPV_ERROR_INVALID_ID: spv_result_t = -10;
pub const SPV_ERROR_INVALID_CFG: spv_result_t = -11;
pub const SPV_ERROR_INVALID_LAYOUT: spv_result_t = -12;
pub const SPV_ERROR_INVALID_CAPABILITY: spv_result_t = -13;
pub const SPV_ERROR_INVALID_DATA: spv_result_t = -14;
pub const SPV_ERROR_MISSING_EXTENSION: spv_result_t = -15;
pub const SPV_ERROR_WRONG_VERSION: spv_result_t = -16;
pub const SPV_ERROR_FNVAR: spv_result_t = -17;

pub type spv_message_level_t = c_int;
pub const SPV_MSG_FATAL: spv_message_level_t = 0;
pub const SPV_MSG_INTERNAL_ERROR: spv_message_level_t = 1;
pub const SPV_MSG_ERROR: spv_message_level_t = 2;
pub const SPV_MSG_WARNING: spv_message_level_t = 3;
pub const SPV_MSG_INFO: spv_message_level_t = 4;
pub const SPV_MSG_DEBUG: spv_message_level_t = 5;

pub type spv_endianness_t = i32;
pub const SPV_ENDIANNESS_LITTLE: spv_endianness_t = 0;
pub const SPV_ENDIANNESS_BIG: spv_endianness_t = 1;

pub type spv_operand_type_t = u32;

unsafe extern "C" {
    pub fn spvOperandIsConcrete(r#type: spv_operand_type_t) -> bool;
    pub fn spvOperandIsConcreteMask(r#type: spv_operand_type_t) -> bool;
}

pub type spv_ext_inst_type_t = u32;

pub type spv_number_kind_t = c_int;
pub const SPV_NUMBER_NONE: spv_number_kind_t = 0;
pub const SPV_NUMBER_UNSIGNED_INT: spv_number_kind_t = 1;
pub const SPV_NUMBER_SIGNED_INT: spv_number_kind_t = 2;
pub const SPV_NUMBER_FLOATING: spv_number_kind_t = 3;

pub type spv_fp_encoding_t = c_int;
pub const SPV_FP_ENCODING_UNKNOWN: spv_fp_encoding_t = 0;
pub const SPV_FP_ENCODING_IEEE754_BINARY16: spv_fp_encoding_t = 1;
pub const SPV_FP_ENCODING_IEEE754_BINARY32: spv_fp_encoding_t = 2;
pub const SPV_FP_ENCODING_IEEE754_BINARY64: spv_fp_encoding_t = 3;
pub const SPV_FP_ENCODING_BFLOAT16: spv_fp_encoding_t = 4;
pub const SPV_FP_ENCODING_FLOAT8_E4M3: spv_fp_encoding_t = 5;
pub const SPV_FP_ENCODING_FLOAT8_E5M2: spv_fp_encoding_t = 6;

pub type spv_text_to_binary_options_t = u32;
pub const SPV_TEXT_TO_BINARY_OPTION_NONE: spv_text_to_binary_options_t = 1 << 0;
pub const SPV_TEXT_TO_BINARY_OPTION_PRESERVE_NUMERIC_IDS: spv_text_to_binary_options_t = 1 << 1;

pub type spv_binary_to_text_options_t = u32;
pub const SPV_BINARY_TO_TEXT_OPTION_NONE: spv_binary_to_text_options_t = 1 << 0;
pub const SPV_BINARY_TO_TEXT_OPTION_PRINT: spv_binary_to_text_options_t = 1 << 1;
pub const SPV_BINARY_TO_TEXT_OPTION_COLOR: spv_binary_to_text_options_t = 1 << 2;
pub const SPV_BINARY_TO_TEXT_OPTION_INDENT: spv_binary_to_text_options_t = 1 << 3;
pub const SPV_BINARY_TO_TEXT_OPTION_SHOW_BYTE_OFFSET: spv_binary_to_text_options_t = 1 << 4;
pub const SPV_BINARY_TO_TEXT_OPTION_NO_HEADER: spv_binary_to_text_options_t = 1 << 5;
pub const SPV_BINARY_TO_TEXT_OPTION_FRIENDLY_NAMES: spv_binary_to_text_options_t = 1 << 6;
pub const SPV_BINARY_TO_TEXT_OPTION_COMMENT: spv_binary_to_text_options_t = 1 << 7;
pub const SPV_BINARY_TO_TEXT_OPTION_NESTED_INDENT: spv_binary_to_text_options_t = 1 << 8;
pub const SPV_BINARY_TO_TEXT_OPTION_REORDER_BLOCKS: spv_binary_to_text_options_t = 1 << 9;

pub const kDefaultMaxIdBound: u32 = 0x3fffff;

#[repr(C)]
pub struct spv_parsed_operand_t {
    pub offset: u16,
    pub num_words: u16,
    pub r#type: spv_operand_type_t,
    pub number_kind: spv_number_kind_t,
    pub number_bit_width: u32,
    pub fp_encoding: spv_fp_encoding_t,
}

#[repr(C)]
pub struct spv_parsed_instruction_t {
    pub words: *const u32,
    pub num_words: u16,
    pub opcode: u16,
    pub ext_inst_type: spv_ext_inst_type_t,
    pub type_id: u32,
    pub result_id: u32,
    pub operands: *const spv_parsed_operand_t,
    pub num_operands: u16,
}

#[repr(C)]
pub struct spv_parsed_header_t {
    pub magic: u32,
    pub version: u32,
    pub generator: u32,
    pub bound: u32,
    pub reserved: u32,
}

#[repr(C)]
pub struct spv_const_binary_t {
    pub code: *const u32,
    pub word_count: usize,
}

#[repr(C)]
pub struct spv_binary_t {
    pub code: *mut u32,
    pub word_count: usize,
}

#[repr(C)]
pub struct spv_text_t {
    pub str: *const c_char,
    pub length: usize,
}

#[repr(C)]
pub struct spv_position_t {
    pub line: usize,
    pub column: usize,
    pub index: usize,
}

#[repr(C)]
pub struct spv_diagnostic_t {
    pub position: spv_position_t,
    pub error: *mut c_char,
    pub is_text_source: bool,
}

#[repr(C)]
struct OpaqueFFIStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct spv_context_t(OpaqueFFIStruct);

#[repr(C)]
pub struct spv_validator_options_t(OpaqueFFIStruct);

#[repr(C)]
pub struct spv_optimizer_options_t(OpaqueFFIStruct);

#[repr(C)]
pub struct spv_reducer_options_t(OpaqueFFIStruct);

#[repr(C)]
pub struct spv_fuzzer_options_t(OpaqueFFIStruct);

#[repr(C)]
pub struct spv_optimizer_t(OpaqueFFIStruct);

unsafe extern "C" {
    pub fn spvSoftwareVersionString() -> *const c_char;
    pub fn spvSoftwareVersionDetailsString() -> *const c_char;
}

pub type spv_target_env = c_int;
pub const SPV_ENV_UNIVERSAL_1_0: spv_target_env = 0;
pub const SPV_ENV_VULKAN_1_0: spv_target_env = 1;
pub const SPV_ENV_UNIVERSAL_1_1: spv_target_env = 2;
pub const SPV_ENV_OPENCL_2_1: spv_target_env = 3;
pub const SPV_ENV_OPENCL_2_2: spv_target_env = 4;
pub const SPV_ENV_OPENGL_4_0: spv_target_env = 5;
pub const SPV_ENV_OPENGL_4_1: spv_target_env = 6;
pub const SPV_ENV_OPENGL_4_2: spv_target_env = 7;
pub const SPV_ENV_OPENGL_4_3: spv_target_env = 8;
pub const SPV_ENV_OPENGL_4_5: spv_target_env = 9;
pub const SPV_ENV_UNIVERSAL_1_2: spv_target_env = 10;
pub const SPV_ENV_OPENCL_1_2: spv_target_env = 11;
pub const SPV_ENV_OPENCL_EMBEDDED_1_2: spv_target_env = 12;
pub const SPV_ENV_OPENCL_2_0: spv_target_env = 13;
pub const SPV_ENV_OPENCL_EMBEDDED_2_0: spv_target_env = 14;
pub const SPV_ENV_OPENCL_EMBEDDED_2_1: spv_target_env = 15;
pub const SPV_ENV_OPENCL_EMBEDDED_2_2: spv_target_env = 16;
pub const SPV_ENV_UNIVERSAL_1_3: spv_target_env = 17;
pub const SPV_ENV_VULKAN_1_1: spv_target_env = 18;
pub const SPV_ENV_WEBGPU_0: spv_target_env = 19;
pub const SPV_ENV_UNIVERSAL_1_4: spv_target_env = 20;
pub const SPV_ENV_VULKAN_1_1_SPIRV_1_4: spv_target_env = 21;
pub const SPV_ENV_UNIVERSAL_1_5: spv_target_env = 22;
pub const SPV_ENV_VULKAN_1_2: spv_target_env = 23;
pub const SPV_ENV_UNIVERSAL_1_6: spv_target_env = 24;
pub const SPV_ENV_VULKAN_1_3: spv_target_env = 25;
pub const SPV_ENV_VULKAN_1_4: spv_target_env = 26;

pub type spv_validator_limit = c_int;
pub const spv_validator_limit_max_struct_members: spv_validator_limit = 0;
pub const spv_validator_limit_max_struct_depth: spv_validator_limit = 1;
pub const spv_validator_limit_max_local_variables: spv_validator_limit = 2;
pub const spv_validator_limit_max_global_variables: spv_validator_limit = 3;
pub const spv_validator_limit_max_switch_branches: spv_validator_limit = 4;
pub const spv_validator_limit_max_function_args: spv_validator_limit = 5;
pub const spv_validator_limit_max_control_flow_nesting_depth: spv_validator_limit = 6;
pub const spv_validator_limit_max_access_chain_indexes: spv_validator_limit = 7;
pub const spv_validator_limit_max_id_bound: spv_validator_limit = 8;

unsafe extern "C" {
    pub fn spvTargetEnvDescription(env: spv_target_env) -> *const c_char;
    pub fn spvParseTargetEnv(s: *const c_char, env: *mut spv_target_env) -> bool;
    pub fn spvParseVulkanEnv(vulkan_ver: u32, spirv_ver: u32, env: *mut spv_target_env) -> bool;
    pub fn spvContextCreate(env: spv_target_env) -> *mut spv_context_t;
    pub fn spvContextDestroy(context: *mut spv_context_t);

    pub fn spvValidatorOptionsCreate() -> *mut spv_validator_options_t;
    pub fn spvValidatorOptionsDestroy(options: *mut spv_validator_options_t);
    pub fn spvValidatorOptionsSetUniversalLimit(
        options: *mut spv_validator_options_t,
        limit_type: spv_validator_limit,
        limit: u32,
    );
    pub fn spvValidatorOptionsSetRelaxStoreStruct(options: *mut spv_validator_options_t, val: bool);
    pub fn spvValidatorOptionsSetRelaxLogicalPointer(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetBeforeHlslLegalization(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetRelaxBlockLayout(options: *mut spv_validator_options_t, val: bool);
    pub fn spvValidatorOptionsSetUniformBufferStandardLayout(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetScalarBlockLayout(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetWorkgroupScalarBlockLayout(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetSkipBlockLayout(options: *mut spv_validator_options_t, val: bool);
    pub fn spvValidatorOptionsSetAllowLocalSizeId(options: *mut spv_validator_options_t, val: bool);
    pub fn spvValidatorOptionsSetAllowOffsetTextureOperand(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetAllowVulkan32BitBitwise(
        options: *mut spv_validator_options_t,
        val: bool,
    );
    pub fn spvValidatorOptionsSetFriendlyNames(options: *mut spv_validator_options_t, val: bool);

    pub fn spvOptimizerOptionsCreate() -> *mut spv_optimizer_options_t;
    pub fn spvOptimizerOptionsDestroy(options: *mut spv_optimizer_options_t);
    pub fn spvOptimizerOptionsSetRunValidator(options: *mut spv_optimizer_options_t, val: bool);
    pub fn spvOptimizerOptionsSetValidatorOptions(
        options: *mut spv_optimizer_options_t,
        val: *mut spv_validator_options_t,
    );
    pub fn spvOptimizerOptionsSetMaxIdBound(options: *mut spv_optimizer_options_t, val: u32);
    pub fn spvoptimizerOptionsSetPreserveBindings(options: *mut spv_optimizer_options_t, val: bool);
    pub fn spvOptimizerOptionsSetPreserveSpecConstants(
        options: *mut spv_optimizer_options_t,
        val: bool,
    );

    pub fn spvReducerOptionsCreate() -> *mut spv_reducer_options_t;
    pub fn spvReducerOptionsDestroy(options: *mut spv_reducer_options_t);
    pub fn spvReducerOptionsSetStepLimit(options: *mut spv_reducer_options_t, step_limit: u32);
    pub fn spvReducerOptionsSetFailOnValidationError(
        options: *mut spv_reducer_options_t,
        fail_on_validation_error: bool,
    );
    pub fn spvReducerOptionsSetTargetFunction(
        options: *mut spv_reducer_options_t,
        target_function: u32,
    );

    pub fn spvFuzzerOptionsCreate() -> *mut spv_fuzzer_options_t;
    pub fn spvFuzzerOptionsDestroy(options: *mut spv_fuzzer_options_t);
    pub fn spvFuzzerOptionsEnableReplayValidation(options: *mut spv_fuzzer_options_t);
    pub fn spvFuzzerOptionsSetRandomSeed(options: *mut spv_fuzzer_options_t, seed: u32);
    pub fn spvFuzzerOptionsSetReplayRange(options: *mut spv_fuzzer_options_t, replay_range: i32);
    pub fn spvFuzzerOptionsSetShrinkerStepLimit(
        options: *mut spv_fuzzer_options_t,
        shrinker_step_limit: u32,
    );
    pub fn spvFuzzerOptionsEnableFuzzerPassValidation(options: *mut spv_fuzzer_options_t);
    pub fn spvFuzzerOptionsEnableAllPasses(options: *mut spv_fuzzer_options_t);

    pub fn spvTextToBinary(
        context: *const spv_context_t,
        text: *const c_char,
        length: usize,
        binary: *mut *mut spv_binary_t,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;
    pub fn spvTextToBinaryWithOptions(
        context: *const spv_context_t,
        text: *const c_char,
        length: usize,
        options: u32,
        binary: *mut *mut spv_binary_t,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;
    pub fn spvTextDestroy(text: *mut spv_text_t);
    pub fn spvBinaryToText(
        context: *const spv_context_t,
        binary: *const u32,
        word_count: usize,
        options: u32,
        text: *mut *mut spv_text_t,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;
    pub fn spvBinaryDestroy(binary: *mut spv_binary_t);
    pub fn spvValidate(
        context: *const spv_context_t,
        binary: *const spv_binary_t,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;
    pub fn spvValidateWithOptions(
        context: *const spv_context_t,
        options: *const spv_validator_options_t,
        binary: *const spv_binary_t,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;
    pub fn spvValidateBinary(
        context: *const spv_context_t,
        words: *const u32,
        num_words: usize,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;

    pub fn spvDiagnosticCreate(
        position: *mut spv_position_t,
        message: *const c_char,
    ) -> *mut spv_diagnostic_t;
    pub fn spvDiagnosticDestroy(diagnostic: *mut spv_diagnostic_t);
    pub fn spvDiagnosticPrint(diagnostic: *mut spv_diagnostic_t) -> spv_result_t;

    pub fn spvOpcodeString(opcode: u32) -> *const c_char;
}

pub type spv_parsed_header_fn_t = extern "C" fn(
    user_data: *mut c_void,
    endian: spv_endianness_t,
    magic: u32,
    version: u32,
    generator: u32,
    id_bound: u32,
    reserved: u32,
) -> spv_result_t;

pub type spv_parsed_instruction_fn_t = extern "C" fn(
    user_data: *mut c_void,
    parsed_instruction: *const spv_parsed_instruction_t,
) -> spv_result_t;

unsafe extern "C" {
    pub fn spvBinaryParse(
        context: *const spv_context_t,
        user_data: *mut c_void,
        words: *const u32,
        num_words: usize,
        parse_header: Option<spv_parsed_header_fn_t>,
        parse_instruction: Option<spv_parsed_instruction_fn_t>,
        diagnostic: *mut *mut spv_diagnostic_t,
    ) -> spv_result_t;
}

pub type spv_message_consumer =
    extern "C" fn(spv_message_level_t, *const c_char, *const spv_position_t, *const c_char);

unsafe extern "C" {
    pub fn spvOptimizerCreate(env: spv_target_env) -> *mut spv_optimizer_t;
    pub fn spvOptimizerDestroy(optimizer: *mut spv_optimizer_t);
    pub fn spvOptimizerSetMessageConsumer(
        optimizer: *mut spv_optimizer_t,
        consumer: spv_message_consumer,
    );
    pub fn spvOptimizerRegisterLegalizationPasses(optimizer: *mut spv_optimizer_t);
    pub fn spvOptimizerRegisterPerformancePasses(optimizer: *mut spv_optimizer_t);
    pub fn spvOptimizerRegisterSizePasses(optimiezr: *mut spv_optimizer_t);
    pub fn spvOptimizerRegisterPassFromFlag(
        optimizer: *mut spv_optimizer_t,
        flag: *const c_char,
    ) -> bool;
    pub fn spvOptimizerRegisterPassesFromFlags(
        optimizer: *mut spv_optimizer_t,
        flags: *mut *const c_char,
        flag_count: usize,
    ) -> bool;
    pub fn spvOptimizerRegisterPassesFromFlagsWhilePreservingTheInterface(
        optimizer: *mut spv_optimizer_t,
        flags: *mut *const c_char,
        flag_count: usize,
    ) -> bool;
    pub fn spvOptimizerRun(
        optimizer: *mut spv_optimizer_t,
        binary: *const u32,
        word_count: usize,
        optimized_binary: *mut *mut spv_binary_t,
        options: *mut spv_optimizer_options_t,
    ) -> spv_result_t;
}
