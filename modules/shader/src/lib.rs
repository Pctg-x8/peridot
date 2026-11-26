use std::{
    collections::{BTreeMap, HashMap, HashSet, hash_map},
    ffi::CString,
    io::{BufRead, Read, Write},
    num::NonZeroUsize,
    path::Path,
};

use bedrock as br;
use peridot::VertexInputSemantic;
use peridot_serialization_utils::{VariableUInt, VariableULong};
use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession};
use vectored_io_helper::{mkioslice, mkioslicem, read_vectored_all, write_vectored_all};

pub mod asset;

#[cfg(feature = "with-asset-processing")]
mod asset_processing;
use crate::asset::{
    AssetDescriptorPointer, AssetDescriptorSlotFieldVk, AssetInputVariableLayoutVk,
    AssetInputVariableType, AssetMaterialPropertyVk, AssetVertexInput, AssetVk,
};

#[cfg(feature = "with-asset-processing")]
pub use self::asset_processing::*;

pub mod engine_params;

pub struct Environment {
    pub peridot_stdlib_search_path: CString,
}

#[derive(thiserror::Error, Debug)]
pub enum BuildError {
    #[error("Failed to load source: {0}")]
    SourceLoad(std::io::Error),
    #[error("failed with some reason")]
    GenericFailure,
}

pub fn build(source_path: impl AsRef<Path>, env: &Environment) -> Result<AssetVk, BuildError> {
    let source_path = source_path.as_ref();

    let session = slang::create_global_session(&slang::GlobalSessionDesc {
        ..Default::default()
    })
    .expect("slang.create_global_session");
    let targets = [slang::TargetDesc {
        format: slang::ffi::SLANG_SPIRV,
        profile: session.find_profile(c"spirv_1_3"),
        ..Default::default()
    }];
    let search_paths = [env.peridot_stdlib_search_path.as_ptr()];

    let local_session = session
        .create_session(&slang::SessionDesc {
            targets: targets.as_ptr(),
            target_count: targets.len() as _,
            search_paths: search_paths.as_ptr(),
            search_path_count: search_paths.len() as _,
            ..Default::default()
        })
        .expect("slang.create_local_session");
    let mut diag = core::mem::MaybeUninit::new(None);
    let module = local_session.load_module_from_source_string(
        c"main",
        &CString::new(source_path.to_str().expect("invalid path str")).expect("nul in path"),
        &CString::new(std::fs::read_to_string(source_path).map_err(BuildError::SourceLoad)?)
            .expect("nul contained in source"),
        Some(&mut diag),
    );
    print_slang_diag(unsafe { diag.assume_init() });
    let module = module.expect("failed to load module");

    let entrypoints = module
        .iter_defined_entry_point()
        .collect::<Result<Vec<_>, _>>()
        .expect("entrypoints");
    let program_components = core::iter::once(&**module)
        .chain(entrypoints.iter().map(|e| &***e))
        .collect::<Vec<&slang::IComponentTypePtr>>();
    let mut diag = core::mem::MaybeUninit::new(None);
    let program =
        local_session.create_composite_component_type(&program_components, Some(&mut diag));
    print_slang_diag(unsafe { diag.assume_init() });
    let program = program.expect("program.create");

    let mut diag = core::mem::MaybeUninit::new(None);
    let linked = program.link(Some(&mut diag));
    print_slang_diag(unsafe { diag.assume_init() });
    let linked = linked.expect("program.link");

    let mut diag = core::mem::MaybeUninit::new(None);
    let code = linked.get_target_code(0, Some(&mut diag));
    print_slang_diag(unsafe { diag.assume_init() });
    let code = code.expect("linked.get_code");
    assert_eq!(code.get_buffer_size() & 0x03, 0, "misaligned spirv stream");
    let mut code_binary = Vec::<u32>::with_capacity(code.get_buffer_size() >> 2);
    unsafe {
        core::ptr::copy_nonoverlapping(
            code.get_buffer_pointer().cast::<u8>(),
            code_binary.spare_capacity_mut().as_mut_ptr().cast::<u8>(),
            code.get_buffer_size(),
        );
        code_binary.set_len(code.get_buffer_size() >> 2);
    }

    let mut diag = core::mem::MaybeUninit::new(None);
    let rl = linked.get_layout(0, Some(&mut diag));
    print_slang_diag(unsafe { diag.assume_init() });
    let mut vertex_inputs = Vec::new();
    let mut vertex_entry_point_name = None;
    let mut fragment_entry_point_name = None;
    let mut fragment_shader_blending_configuration = FragmentShaderBlendingConfiguration::default();
    for n in 0..rl.entry_point_count() {
        let Some(e) = rl.entry_point(n) else {
            continue;
        };

        if e.stage() == slang::ffi::SLANG_STAGE_VERTEX {
            // vertex shader func
            vertex_entry_point_name = Some(e.name());
            let vl = e.var_layout();
            let tl = vl.type_layout();
            assert_eq!(tl.kind(), slang::reflection::TypeKind::Struct);
            for f in tl.iter_field() {
                visit_vsh_input(f, &*session, 0, &mut vertex_inputs);
            }
        }
        if e.stage() == slang::ffi::SLANG_STAGE_FRAGMENT {
            fragment_entry_point_name = Some(e.name());
            let additional_attributes = e
                .function()
                .iter_user_attribute()
                .filter_map(|a| FragmentShaderAdditionalAttribute::parse(a))
                .collect::<Vec<_>>();
            println!("{additional_attributes:#?}");
            fragment_shader_blending_configuration =
                FragmentShaderBlendingConfiguration::collect(additional_attributes.iter());
        }
    }
    let vertex_entry_point_name = vertex_entry_point_name
        .expect("no vertex shader defined")
        .to_str()
        .expect("invalid function name")
        .to_owned();
    let fragment_entry_point_name =
        fragment_entry_point_name.map(|x| x.to_str().expect("invalid function name").to_owned());

    let object_parameters_ty = rl
        .find_type_by_name(c"peridot::ObjectParameters")
        .expect("no object parameters type?");
    println!("object parameters ty: {object_parameters_ty:p}");

    if let Some(gv) = rl.global_params_var_layout() {
        println!("---");
        recursive_dump_global_input(gv, 0);
        println!("---ty---");
        recursive_dump_ty_layout(gv.type_layout(), 0);
        println!("---");
    }

    let mut collected = GlobalInputCollected {
        input_vars: Vec::new(),
        object_parameters_var: None,
        camera_parameters_var: None,
    };
    let mut var_name_pool = VarNamePool::new();
    let mut accumulated_offset_history_arena = AccumulatedOffsetHistoryArena::new();
    if let Some(gv) = rl.global_params_var_layout() {
        let tl = gv.type_layout();
        println!("global var: {:?}", tl.kind());
        if tl.kind() == slang::reflection::TypeKind::ConstantBuffer {
            let cvl = tl.container_var_layout().expect("no container var");
            println!(
                "cvl {} {:?} {} {}",
                cvl.binding_index(),
                cvl.category(),
                cvl.offset(cvl.category()),
                cvl.space(cvl.category())
            );
            let evl = tl.element_var_layout().expect("no element var");
            println!(
                "evl {} {:?} {} {}",
                evl.binding_index(),
                evl.category(),
                evl.offset(evl.category()),
                evl.space(evl.category())
            );
            assert_eq!(
                evl.type_layout().kind(),
                slang::reflection::TypeKind::Struct
            );
            for n in 0..evl.type_layout().field_count() {
                visit_global_input(
                    evl.type_layout().field(n).expect("no field"),
                    0,
                    &*session,
                    &mut VisitContext {
                        accumulated_offset_history_arena: &mut accumulated_offset_history_arena,
                        accumulated_offset_history_parent: None,
                        var_name_pool: &mut var_name_pool,
                        parent_name_ref: None,
                        is_material_property: false,
                    },
                    &mut collected,
                );
            }
        } else {
            assert_eq!(tl.kind(), slang::reflection::TypeKind::Struct);
            for f in tl.iter_field() {
                visit_global_input(
                    f,
                    0,
                    &*session,
                    &mut VisitContext {
                        accumulated_offset_history_arena: &mut accumulated_offset_history_arena,
                        accumulated_offset_history_parent: None,
                        var_name_pool: &mut var_name_pool,
                        parent_name_ref: None,
                        is_material_property: false,
                    },
                    &mut collected,
                );
            }
        }
    }

    println!("vertex inputs");
    for v in vertex_inputs.iter() {
        println!(
            "  {:?}{} = {}",
            v.semantic_name, v.semantic_index, v.location
        );
    }
    println!("input variables");
    for v in collected.input_vars.iter() {
        println!(
            "  {:?}: {:?}({:?}) at Descriptor(set={}, binding={})+{} size={} is_material_property={} propertyName={:?}",
            var_name_pool.query_chained(v.path),
            v.r#type.r#type().expect("no type?").name(),
            v.category,
            v.placement.descriptor_set,
            v.placement.descriptor_binding,
            v.placement.byte_offset,
            v.size,
            v.is_material_property,
            v.property_name_override
        );
    }

    // retain only leaf params
    let mut non_root_paths = HashSet::new();
    for v in collected.input_vars.iter() {
        let f = var_name_pool.query_chained(v.path);
        let mut nr = &f[..f.len() - 1];
        while !nr.is_empty() {
            non_root_paths.insert(nr.to_vec());
            nr = &nr[..nr.len() - 1];
        }
    }
    for p in non_root_paths.into_iter() {
        collected
            .input_vars
            .retain(|x| var_name_pool.query_chained(x.path) != p);
    }

    let mut material_properties = Vec::new();
    let mut input_variable_layout = HashMap::new();
    let mut failed = false;
    for v in collected.input_vars {
        if v.is_material_property {
            let Some(prop) = construct_asset_material_property_vk(&v, &var_name_pool) else {
                failed = true;
                continue;
            };
            material_properties.push(prop);

            let Some(input_variable_layout_key) = try_make_asset_descriptor_pointer(&v.placement)
            else {
                failed = true;
                continue;
            };
            let slot = match v.category {
                slang::reflection::ParameterCategory::DescriptorTableSlot => {
                    // single descriptor
                    let sl_type = v.r#type;
                    match sl_type.kind() {
                        slang::reflection::TypeKind::ConstantBuffer
                        | slang::reflection::TypeKind::ParameterBlock
                        | slang::reflection::TypeKind::Struct => {
                            // no support for compound objects
                            unimplemented!();
                        }
                        slang::reflection::TypeKind::Array => unimplemented!("single array slot"),
                        slang::reflection::TypeKind::Vector => unimplemented!("single vector slot"),
                        slang::reflection::TypeKind::Matrix => unimplemented!("single matrix slot"),
                        slang::reflection::TypeKind::Resource => {
                            let shape = sl_type.r#type().expect("no type?").resource_shape();
                            let result_type = sl_type
                                .r#type()
                                .expect("no type?")
                                .resource_result_type()
                                .expect("no result type");
                            match (
                                shape & slang::ffi::SLANG_RESOURCE_BASE_SHAPE_MASK,
                                shape & slang::ffi::SLANG_RESOURCE_EXT_SHAPE_MASK,
                                result_type.kind(),
                            ) {
                                (
                                    slang::ffi::SLANG_TEXTURE_2D,
                                    ext_mask,
                                    slang::reflection::TypeKind::Vector,
                                ) if (ext_mask & slang::ffi::SLANG_TEXTURE_COMBINED_FLAG) != 0 => {
                                    let element_count = result_type.element_count(None);
                                    let element_type =
                                        result_type.element_type().expect("no vector element type");
                                    let element_scalar_type = element_type.scalar_type();
                                    match (element_scalar_type, element_count) {
                                        (slang::reflection::ScalarType::Float32, 4) => {
                                            AssetInputVariableLayoutVk::CombinedImageSampler
                                        }
                                        (t, c) => {
                                            todo!(
                                                "unhandled resource result vector combination: {t:?} x {c}"
                                            );
                                        }
                                    }
                                }
                                (s, x, k) => {
                                    todo!(
                                        "unhandled resource shape/result combination: {s:?}/0x{x:x}/{k:?}"
                                    );
                                }
                            }
                        }
                        _ => unimplemented!("unknown type kind"),
                    }
                }
                slang::reflection::ParameterCategory::Uniform => {
                    AssetInputVariableLayoutVk::UniformBuffer {
                        fields: vec![AssetDescriptorSlotFieldVk {
                            offset: v.var.offset(slang::reflection::ParameterCategory::Uniform)
                                as _,
                            size: v.r#type.size(slang::reflection::ParameterCategory::Uniform) as _,
                        }],
                    }
                }
                cat => unreachable!("unsupported parameter category? {cat:?}"),
            };

            match input_variable_layout.entry(input_variable_layout_key) {
                hash_map::Entry::Vacant(e) => {
                    e.insert(slot);
                }
                hash_map::Entry::Occupied(e) => match (slot, e.into_mut()) {
                    (
                        AssetInputVariableLayoutVk::UniformBuffer { fields },
                        AssetInputVariableLayoutVk::UniformBuffer { fields: e_fields },
                    ) => {
                        // can be merged
                        e_fields.extend(fields);
                    }
                    _ => {
                        tracing::error!(
                            set = input_variable_layout_key.set,
                            binding = input_variable_layout_key.binding,
                            "conflicting descriptor content"
                        );
                        failed = true;
                    }
                },
            }
        }
    }
    if failed {
        return Err(BuildError::GenericFailure);
    }

    let mut asset_vertex_inputs = HashMap::with_capacity(vertex_inputs.len());
    let mut failed = false;
    for x in vertex_inputs {
        let semantic = match extract_vertex_semantic(&x.semantic_name, x.semantic_index) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "invalid semantic");
                failed = true;
                continue;
            }
        };
        let input = AssetVertexInput {
            location: match x.location.try_into() {
                Ok(x) => x,
                Err(_) => {
                    tracing::error!(location = x.location, "location is out of range(0..=255)");
                    failed = true;
                    continue;
                }
            },
        };

        match asset_vertex_inputs.entry(semantic) {
            hash_map::Entry::Vacant(e) => {
                e.insert(input);
            }
            hash_map::Entry::Occupied(mut e) => {
                tracing::warn!(
                    ?semantic,
                    old_location = e.get().location,
                    new_location = input.location,
                    "conflicting semantic"
                );
                e.insert(input);
            }
        }
    }
    if failed {
        return Err(BuildError::GenericFailure);
    }

    let object_parameters_placement = collected
        .object_parameters_var
        .map(|x| {
            assert_eq!(
                x.placement.byte_offset, 0,
                "engine parameters placed on non-zero offset"
            );

            try_make_asset_descriptor_pointer(&x.placement).ok_or(())
        })
        .transpose();
    let camera_parameters_placement = collected
        .camera_parameters_var
        .map(|x| {
            assert_eq!(
                x.placement.byte_offset, 0,
                "engine parameters placed on non-zero offset"
            );

            try_make_asset_descriptor_pointer(&x.placement).ok_or(())
        })
        .transpose();
    let (Ok(object_parameters_placement), Ok(camera_parameters_placement)) =
        (object_parameters_placement, camera_parameters_placement)
    else {
        return Err(BuildError::GenericFailure);
    };

    Ok(AssetVk {
        code_binary,
        vertex_entry_point_name,
        fragment_entry_point_name,
        vertex_inputs: asset_vertex_inputs,
        material_properties,
        input_variable_layout,
        object_parameters_placement,
        camera_parameters_placement,
        blending: fragment_shader_blending_configuration,
    })
}

fn try_make_asset_descriptor_pointer(placement: &VarPlacement) -> Option<AssetDescriptorPointer> {
    let set = placement
        .descriptor_set
        .try_into()
        .inspect_err(|_| {
            tracing::error!(
                value = placement.descriptor_set,
                "descriptor set is out of range"
            )
        })
        .ok();
    let binding = placement
        .descriptor_binding
        .try_into()
        .inspect_err(|_| {
            tracing::error!(
                value = placement.descriptor_binding,
                "descriptor binding is out of range"
            )
        })
        .ok();

    if let (Some(set), Some(binding)) = (set, binding) {
        Some(AssetDescriptorPointer { set, binding })
    } else {
        None
    }
}

fn construct_asset_material_property_vk(
    v: &InputVariable,
    var_name_pool: &VarNamePool,
) -> Option<AssetMaterialPropertyVk> {
    let r#type = AssetInputVariableType::from_slang_type(v.r#type.r#type().expect("no type?"));
    let mut name = String::new();
    let mut failed = false;
    for x in var_name_pool.query_chained(v.path).iter().flatten() {
        let Ok(x) = x
            .to_str()
            .inspect_err(|e| tracing::error!(reason = %e, text = ?x, "invalid path str"))
        else {
            failed = true;
            continue;
        };

        if !name.is_empty() {
            name.push('.');
        }

        name.push_str(x);
    }

    let property_name_override = v
        .property_name_override
        .as_deref()
        .map(|x| {
            x.to_str().inspect_err(
                |e| tracing::error!(reason = %e, text = ?x, "invalid property name override"),
            )
        })
        .transpose();
    let descriptor = try_make_asset_descriptor_pointer(&v.placement);
    let offset = v.placement.byte_offset.try_into().inspect_err(|_| {
        tracing::error!(
            value = v.placement.byte_offset,
            name,
            "byte offset is out of range"
        )
    });
    let size = v
        .size
        .try_into()
        .inspect_err(|_| tracing::error!(value = v.size, name, "too large variable size"));

    let (Some(r#type), Ok(property_name_override), Some(descriptor), Ok(offset), Ok(size)) =
        (r#type, property_name_override, descriptor, offset, size)
    else {
        return None;
    };
    if failed {
        // name building failed
        return None;
    }

    Some(AssetMaterialPropertyVk {
        name,
        property_name_override: property_name_override.map(ToOwned::to_owned),
        r#type,
        descriptor,
        offset,
        size,
    })
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BlendFactor {
    SrcAlpha = 0,
    DstAlpha = 1,
    InvSrcAlpha = 2,
    InvDstAlpha = 3,
    SrcColor = 4,
    DstColor = 5,
    InvSrcColor = 6,
    InvDstColor = 7,
    Constant = 8,
    ConstantOne = 9,
    ConstantZero = 10,
}
impl BlendFactor {
    #[tracing::instrument(skip(attr))]
    fn from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        if Self::SrcAlpha as i32 <= value && value <= Self::ConstantZero as i32 {
            Some(unsafe { core::mem::transmute(value as u8) })
        } else {
            tracing::warn!(value, "value out of range");
            None
        }
    }

    pub const fn into_vk(self) -> br::vk::VkBlendFactor {
        match self {
            Self::SrcAlpha => br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
            Self::DstAlpha => br::vk::VK_BLEND_FACTOR_DST_ALPHA,
            Self::InvSrcAlpha => br::vk::VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
            Self::InvDstAlpha => br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
            Self::SrcColor => br::vk::VK_BLEND_FACTOR_SRC_COLOR,
            Self::DstColor => br::vk::VK_BLEND_FACTOR_DST_COLOR,
            Self::InvSrcColor => br::vk::VK_BLEND_FACTOR_ONE_MINUS_SRC_COLOR,
            Self::InvDstColor => br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,
            // TODO: should split alpha/color?
            Self::Constant => br::vk::VK_BLEND_FACTOR_CONSTANT_ALPHA,
            Self::ConstantOne => br::vk::VK_BLEND_FACTOR_ONE,
            Self::ConstantZero => br::vk::VK_BLEND_FACTOR_ZERO,
        }
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[*self as u8])
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut byte = [0u8];
        r.read_exact(&mut byte)?;
        match byte[0] {
            v if Self::SrcAlpha as u8 <= v && v <= Self::ConstantZero as u8 => {
                Ok(unsafe { core::mem::transmute(v) })
            }
            v => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("byte out of range: {v}"),
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BlendOp {
    Add = 0,
    Min = 1,
    Max = 2,
}
impl BlendOp {
    #[tracing::instrument(skip(attr))]
    fn from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        if Self::Add as i32 <= value && value <= Self::Max as i32 {
            Some(unsafe { core::mem::transmute(value as u8) })
        } else {
            tracing::warn!(value, "value out of range");
            None
        }
    }

    pub const fn into_vk(self) -> br::vk::VkBlendOp {
        match self {
            Self::Add => br::vk::VK_BLEND_OP_ADD,
            Self::Min => br::vk::VK_BLEND_OP_MIN,
            Self::Max => br::vk::VK_BLEND_OP_MAX,
        }
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[*self as u8])
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut byte = [0u8];
        r.read_exact(&mut byte)?;
        match byte[0] {
            v if Self::Add as u8 <= v && v <= Self::Max as u8 => {
                Ok(unsafe { core::mem::transmute(v) })
            }
            v => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("byte out of range: {v}"),
            )),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum BlendPreset {
    Disabled = 0,
    StraightAlpha = 1,
    PremultipliedAlpha = 2,
}
impl BlendPreset {
    #[tracing::instrument(skip(attr))]
    fn from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        if Self::Disabled as i32 <= value && value <= Self::PremultipliedAlpha as i32 {
            Some(unsafe { core::mem::transmute(value as u8) })
        } else {
            tracing::warn!(value, "value out of range");
            None
        }
    }

    pub fn make_content(&self) -> PresetBlendingConfigurationContent {
        match self {
            Self::Disabled => PresetBlendingConfigurationContent {
                enabled: false,
                ..Self::PremultipliedAlpha.make_content()
            },
            Self::StraightAlpha => PresetBlendingConfigurationContent {
                enabled: true,
                src_color_factor: BlendFactor::SrcAlpha,
                dst_color_factor: BlendFactor::InvSrcAlpha,
                color_op: BlendOp::Add,
                src_alpha_factor: BlendFactor::SrcAlpha,
                dst_alpha_factor: BlendFactor::InvSrcAlpha,
                alpha_op: BlendOp::Add,
            },
            Self::PremultipliedAlpha => PresetBlendingConfigurationContent {
                enabled: true,
                src_color_factor: BlendFactor::ConstantOne,
                dst_color_factor: BlendFactor::InvSrcAlpha,
                color_op: BlendOp::Add,
                src_alpha_factor: BlendFactor::ConstantOne,
                dst_alpha_factor: BlendFactor::InvSrcAlpha,
                alpha_op: BlendOp::Add,
            },
        }
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[*self as u8])
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut byte = [0u8];
        r.read_exact(&mut byte)?;
        match byte[0] {
            v if Self::Disabled as u8 <= v && v <= Self::PremultipliedAlpha as u8 => {
                Ok(unsafe { core::mem::transmute(v) })
            }
            v => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("byte out of range: {v}"),
            )),
        }
    }
}

pub struct PresetBlendingConfigurationContent {
    pub enabled: bool,
    pub src_color_factor: BlendFactor,
    pub dst_color_factor: BlendFactor,
    pub color_op: BlendOp,
    pub src_alpha_factor: BlendFactor,
    pub dst_alpha_factor: BlendFactor,
    pub alpha_op: BlendOp,
}
impl PresetBlendingConfigurationContent {
    pub fn apply_overrides(&mut self, overrides: BlendingConfigurationContentOverrides) {
        self.enabled = overrides.enabled.unwrap_or(self.enabled);
        self.src_color_factor = overrides.src_color_factor.unwrap_or(self.src_color_factor);
        self.dst_color_factor = overrides.dst_color_factor.unwrap_or(self.dst_color_factor);
        self.color_op = overrides.color_op.unwrap_or(self.color_op);
        self.src_alpha_factor = overrides.src_alpha_factor.unwrap_or(self.src_alpha_factor);
        self.dst_alpha_factor = overrides.dst_alpha_factor.unwrap_or(self.dst_alpha_factor);
        self.alpha_op = overrides.alpha_op.unwrap_or(self.alpha_op);
    }
}

pub struct BlendingConfigurationContentOverrides {
    pub enabled: Option<bool>,
    pub src_color_factor: Option<BlendFactor>,
    pub dst_color_factor: Option<BlendFactor>,
    pub color_op: Option<BlendOp>,
    pub src_alpha_factor: Option<BlendFactor>,
    pub dst_alpha_factor: Option<BlendFactor>,
    pub alpha_op: Option<BlendOp>,
}
impl BlendingConfigurationContentOverrides {
    pub fn merge_lower_cascade(&mut self, lower: Self) {
        self.enabled = self.enabled.or(lower.enabled);
        self.src_color_factor = self.src_color_factor.or(lower.src_color_factor);
        self.dst_color_factor = self.dst_color_factor.or(lower.dst_color_factor);
        self.color_op = self.color_op.or(lower.color_op);
        self.src_alpha_factor = self.src_alpha_factor.or(lower.src_alpha_factor);
        self.dst_alpha_factor = self.dst_alpha_factor.or(lower.dst_alpha_factor);
        self.alpha_op = self.alpha_op.or(lower.alpha_op);
    }
}
impl<'a> From<&'a PartialBlendingConfiguration> for BlendingConfigurationContentOverrides {
    fn from(value: &'a PartialBlendingConfiguration) -> Self {
        let preset = value.preset.map(|x| x.make_content());
        let color = value.color.as_ref().or(value.base.as_ref());
        let alpha = value.alpha.as_ref().or(value.base.as_ref());

        Self {
            enabled: if color.is_some() || alpha.is_some() {
                // force true if there are some overrides
                Some(true)
            } else {
                preset.as_ref().map(|x| x.enabled)
            },
            src_color_factor: color
                .map(|x| x.src)
                .or_else(|| preset.as_ref().map(|x| x.src_color_factor)),
            dst_color_factor: color
                .map(|x| x.dst)
                .or_else(|| preset.as_ref().map(|x| x.dst_color_factor)),
            color_op: color
                .map(|x| x.op)
                .or_else(|| preset.as_ref().map(|x| x.color_op)),
            src_alpha_factor: alpha
                .map(|x| x.src)
                .or_else(|| preset.as_ref().map(|x| x.src_alpha_factor)),
            dst_alpha_factor: alpha
                .map(|x| x.dst)
                .or_else(|| preset.as_ref().map(|x| x.dst_alpha_factor)),
            alpha_op: alpha
                .map(|x| x.op)
                .or_else(|| preset.as_ref().map(|x| x.alpha_op)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlendingAttachment {
    Default,
    Specified(u32),
}
impl BlendingAttachment {
    #[tracing::instrument(skip(attr))]
    fn specified_from_attr_arg(attr: &slang::reflection::Attribute, index: u32) -> Option<Self> {
        let value = match attr.argument_value_int(index) {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, "attr.argument_value_int");
                return None;
            }
        };

        match u32::try_from(value) {
            Ok(x) => Some(Self::Specified(x)),
            Err(_) => {
                tracing::warn!(value, "attachment index is out of range");
                None
            }
        }
    }
}

#[derive(Debug)]
pub enum Blending {
    Preset(BlendPreset),
    Specified {
        src_factor: BlendFactor,
        dst_factor: BlendFactor,
        op: BlendOp,
    },
    Color {
        src_factor: BlendFactor,
        dst_factor: BlendFactor,
        op: BlendOp,
    },
    Alpha {
        src_factor: BlendFactor,
        dst_factor: BlendFactor,
        op: BlendOp,
    },
}

#[derive(Debug)]
pub enum FragmentShaderAdditionalAttribute {
    Blending(BlendingAttachment, Blending),
    BlendingConstant([f32; 4]),
}
impl FragmentShaderAdditionalAttribute {
    #[tracing::instrument(skip(a), fields(name = ?a.name()))]
    fn parse(a: &slang::reflection::Attribute) -> Option<Self> {
        fn validate_argument_count(a: &slang::reflection::Attribute, count: u32) -> bool {
            if a.argument_count() != count {
                tracing::warn!(count = a.argument_count(), "argument count mismatched");
                false
            } else {
                true
            }
        }

        match a.name() {
            x if x == c"peridot_blending" => {
                if !validate_argument_count(a, 3) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Specified {
                        src_factor: BlendFactor::from_attr_arg(a, 0)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 1)?,
                        op: BlendOp::from_attr_arg(a, 2)?,
                    },
                ))
            }
            x if x == c"peridot_blending_preset" => {
                if !validate_argument_count(a, 1) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Preset(BlendPreset::from_attr_arg(a, 0)?),
                ))
            }
            x if x == c"peridot_blending_preset_for" => {
                if !validate_argument_count(a, 2) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Preset(BlendPreset::from_attr_arg(a, 1)?),
                ))
            }
            x if x == c"peridot_blending_for" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Specified {
                        src_factor: BlendFactor::from_attr_arg(a, 1)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 2)?,
                        op: BlendOp::from_attr_arg(a, 3)?,
                    },
                ))
            }
            x if x == c"peridot_blending_color" => {
                if !validate_argument_count(a, 3) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Color {
                        src_factor: BlendFactor::from_attr_arg(a, 0)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 1)?,
                        op: BlendOp::from_attr_arg(a, 2)?,
                    },
                ))
            }
            x if x == c"peridot_blending_color_for" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Color {
                        src_factor: BlendFactor::from_attr_arg(a, 1)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 2)?,
                        op: BlendOp::from_attr_arg(a, 3)?,
                    },
                ))
            }
            x if x == c"peridot_blending_alpha" => {
                if !validate_argument_count(a, 3) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::Default,
                    Blending::Alpha {
                        src_factor: BlendFactor::from_attr_arg(a, 0)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 1)?,
                        op: BlendOp::from_attr_arg(a, 2)?,
                    },
                ))
            }
            x if x == c"peridot_blending_alpha_for" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::Blending(
                    BlendingAttachment::specified_from_attr_arg(a, 0)?,
                    Blending::Alpha {
                        src_factor: BlendFactor::from_attr_arg(a, 1)?,
                        dst_factor: BlendFactor::from_attr_arg(a, 2)?,
                        op: BlendOp::from_attr_arg(a, 3)?,
                    },
                ))
            }
            x if x == c"peridot_blending_const" => {
                if !validate_argument_count(a, 4) {
                    return None;
                }

                Some(Self::BlendingConstant([
                    a.argument_value_float(0)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                    a.argument_value_float(1)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                    a.argument_value_float(2)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                    a.argument_value_float(3)
                        .inspect_err(|e| tracing::warn!(reason = ?e, "argument_value_float"))
                        .ok()?,
                ]))
            }
            name => {
                tracing::warn!(?name, "unknown fragment shader attribute");
                None
            }
        }
    }
}

#[derive(Debug)]
pub struct PartialBlendingConfigurationCascadeLayer {
    pub src: BlendFactor,
    pub dst: BlendFactor,
    pub op: BlendOp,
}

/// 上から下にオーバーライドできる構成
/// (presetでベーズパラメータ決定->baseのパラメータで上書き->colorのパラメータでcolor関係を上書き->alphaのパラメータでalpha関係を上書き)
///
/// Blendingの有効/無効だけ特殊で、base/color/alphaいずれかの上書きがあればpresetがDisabledでもブレンディング有効になる
#[derive(Debug, Default)]
pub struct PartialBlendingConfiguration {
    pub preset: Option<BlendPreset>,
    pub base: Option<PartialBlendingConfigurationCascadeLayer>,
    pub color: Option<PartialBlendingConfigurationCascadeLayer>,
    pub alpha: Option<PartialBlendingConfigurationCascadeLayer>,
}
impl PartialBlendingConfiguration {
    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        let mut content_flags = PartialBlendingConfigurationContentFlags::empty();
        if self.preset.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_PRESET;
        }
        if self.base.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_BASE;
        }
        if self.color.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_COLOR;
        }
        if self.alpha.is_some() {
            content_flags |= PartialBlendingConfigurationContentFlags::HAS_ALPHA;
        }

        w.write_all(&[content_flags.bits()])?;
        if let Some(p) = self.preset {
            w.write_all(&[p as u8])?;
        }
        if let Some(ref p) = self.base {
            w.write_all(&[p.src as u8, p.dst as u8, p.op as u8])?;
        }
        if let Some(ref p) = self.color {
            w.write_all(&[p.src as u8, p.dst as u8, p.op as u8])?;
        }
        if let Some(ref p) = self.alpha {
            w.write_all(&[p.src as u8, p.dst as u8, p.op as u8])?;
        }

        Ok(())
    }

    pub fn read(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut content_flags = [0u8];
        r.read_exact(&mut content_flags)?;
        let content_flags =
            PartialBlendingConfigurationContentFlags::from_bits_retain(content_flags[0]);

        let preset = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_PRESET)
        {
            Some(BlendPreset::read(r)?)
        } else {
            None
        };
        let base = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_BASE) {
            let src = BlendFactor::read(r)?;
            let dst = BlendFactor::read(r)?;
            let op = BlendOp::read(r)?;

            Some(PartialBlendingConfigurationCascadeLayer { src, dst, op })
        } else {
            None
        };
        let color = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_COLOR) {
            let src = BlendFactor::read(r)?;
            let dst = BlendFactor::read(r)?;
            let op = BlendOp::read(r)?;

            Some(PartialBlendingConfigurationCascadeLayer { src, dst, op })
        } else {
            None
        };
        let alpha = if content_flags.contains(PartialBlendingConfigurationContentFlags::HAS_ALPHA) {
            let src = BlendFactor::read(r)?;
            let dst = BlendFactor::read(r)?;
            let op = BlendOp::read(r)?;

            Some(PartialBlendingConfigurationCascadeLayer { src, dst, op })
        } else {
            None
        };

        Ok(Self {
            preset,
            base,
            color,
            alpha,
        })
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    struct PartialBlendingConfigurationContentFlags : u8 {
        const HAS_PRESET = 0x01;
        const HAS_BASE = 0x02;
        const HAS_COLOR = 0x04;
        const HAS_ALPHA = 0x08;
    }
}

pub fn resolve_attachment_blending_cascade(
    target: &mut PresetBlendingConfigurationContent,
    per_attachment: &PartialBlendingConfiguration,
    default: &PartialBlendingConfiguration,
) {
    let mut overrides = BlendingConfigurationContentOverrides::from(per_attachment);
    overrides.merge_lower_cascade(BlendingConfigurationContentOverrides::from(default));

    target.apply_overrides(overrides);
}

#[derive(Debug, Default)]
pub struct FragmentShaderBlendingConfiguration {
    pub default: PartialBlendingConfiguration,
    pub per_attachment: BTreeMap<u32, PartialBlendingConfiguration>,
    pub constant: Option<[f32; 4]>,
}
impl FragmentShaderBlendingConfiguration {
    pub fn collect<'a>(attrs: impl Iterator<Item = &'a FragmentShaderAdditionalAttribute>) -> Self {
        let mut sink = Self::default();

        for a in attrs {
            match a {
                FragmentShaderAdditionalAttribute::BlendingConstant(xs) => {
                    if sink.constant.is_some() {
                        tracing::warn!("overwriting blending constant");
                    }

                    sink.constant = Some(*xs);
                }
                FragmentShaderAdditionalAttribute::Blending(attachment_index, blending) => {
                    let sink = match attachment_index {
                        BlendingAttachment::Default => &mut sink.default,
                        &BlendingAttachment::Specified(index) => {
                            sink.per_attachment.entry(index).or_default()
                        }
                    };

                    match *blending {
                        Blending::Preset(preset) => {
                            if sink.preset.is_some() {
                                tracing::warn!("overwriting blending preset");
                            }

                            sink.preset = Some(preset);
                        }
                        Blending::Specified {
                            src_factor,
                            dst_factor,
                            op,
                        } => {
                            if sink.base.is_some() {
                                tracing::warn!("overwriting blending base override");
                            }

                            sink.base = Some(PartialBlendingConfigurationCascadeLayer {
                                src: src_factor,
                                dst: dst_factor,
                                op,
                            });
                        }
                        Blending::Color {
                            src_factor,
                            dst_factor,
                            op,
                        } => {
                            if sink.color.is_some() {
                                tracing::warn!("overwriting blending color override");
                            }

                            sink.color = Some(PartialBlendingConfigurationCascadeLayer {
                                src: src_factor,
                                dst: dst_factor,
                                op,
                            });
                        }
                        Blending::Alpha {
                            src_factor,
                            dst_factor,
                            op,
                        } => {
                            if sink.alpha.is_some() {
                                tracing::warn!("overwriting blending alpha override");
                            }

                            sink.alpha = Some(PartialBlendingConfigurationCascadeLayer {
                                src: src_factor,
                                dst: dst_factor,
                                op,
                            });
                        }
                    }
                }
                _ => {}
            }
        }

        sink
    }

    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        self.default.write(w)?;
        VariableULong(self.per_attachment.len() as _).write(w)?;
        for (&a, x) in self.per_attachment.iter() {
            VariableUInt(a).write(w)?;
            x.write(w)?;
        }

        match self.constant {
            None => {
                w.write_all(&[0x00])?;
            }
            Some(ref xs) => {
                write_vectored_all(
                    w,
                    &mut [
                        mkioslice(&0x01u8),
                        mkioslice(&xs[0]),
                        mkioslice(&xs[1]),
                        mkioslice(&xs[2]),
                        mkioslice(&xs[3]),
                    ],
                )?;
            }
        }

        Ok(())
    }

    pub fn read(r: &mut (impl BufRead + ?Sized), swap_bytes: bool) -> std::io::Result<Self> {
        let default = PartialBlendingConfiguration::read(r)?;
        let per_attachment_count = VariableULong::read(r)?;
        let mut per_attachment = BTreeMap::new();
        for _ in 0..per_attachment_count.0 {
            let a = VariableUInt::read(r)?;
            let x = PartialBlendingConfiguration::read(r)?;

            per_attachment.insert(a.0, x);
        }

        let mut has_constant = [0u8];
        r.read_exact(&mut has_constant)?;
        let constant = if has_constant[0] == 0x00 {
            None
        } else {
            let mut x = core::mem::MaybeUninit::<f32>::uninit();
            let mut y = core::mem::MaybeUninit::<f32>::uninit();
            let mut z = core::mem::MaybeUninit::<f32>::uninit();
            let mut w = core::mem::MaybeUninit::<f32>::uninit();
            read_vectored_all(
                r,
                &mut [
                    mkioslicem(&mut x),
                    mkioslicem(&mut y),
                    mkioslicem(&mut z),
                    mkioslicem(&mut w),
                ],
            )?;
            if swap_bytes {
                x.write(f32::from_bits(
                    unsafe { x.assume_init() }.to_bits().swap_bytes(),
                ));
                y.write(f32::from_bits(
                    unsafe { y.assume_init() }.to_bits().swap_bytes(),
                ));
                z.write(f32::from_bits(
                    unsafe { z.assume_init() }.to_bits().swap_bytes(),
                ));
                w.write(f32::from_bits(
                    unsafe { w.assume_init() }.to_bits().swap_bytes(),
                ));
            }

            Some([
                unsafe { x.assume_init() },
                unsafe { y.assume_init() },
                unsafe { z.assume_init() },
                unsafe { w.assume_init() },
            ])
        };

        Ok(Self {
            default,
            per_attachment,
            constant,
        })
    }
}

#[derive(Debug, thiserror::Error)]
enum VertexSemanticExtractionError<'s> {
    #[error("invalid semantic name bytes({0:?}): {1}")]
    InvalidNameBytes(&'s core::ffi::CStr, core::str::Utf8Error),
    #[error("unsupported semantic name: {0}")]
    Unsupported(&'s str),
}

fn extract_vertex_semantic<'s>(
    semantic_name: &'s core::ffi::CStr,
    semantic_index: usize,
) -> Result<VertexInputSemantic, VertexSemanticExtractionError<'s>> {
    let semantic_name = match semantic_name.to_str() {
        Ok(x) => x,
        Err(e) => {
            return Err(VertexSemanticExtractionError::InvalidNameBytes(
                semantic_name,
                e,
            ));
        }
    };

    if semantic_name.eq_ignore_ascii_case("position") {
        return Ok(VertexInputSemantic::Position(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("normal") {
        return Ok(VertexInputSemantic::Normal(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("tangent") {
        return Ok(VertexInputSemantic::Tangent(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("binormal") {
        return Ok(VertexInputSemantic::Binormal(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("texcoord") {
        return Ok(VertexInputSemantic::Texcoord(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("color") {
        return Ok(VertexInputSemantic::Color(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("blendindex") {
        return Ok(VertexInputSemantic::BlendIndex(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("blendweight") {
        return Ok(VertexInputSemantic::BlendWeight(semantic_index as _));
    }
    if semantic_name.eq_ignore_ascii_case("misc") {
        return Ok(VertexInputSemantic::Misc(semantic_index as _));
    }

    Err(VertexSemanticExtractionError::Unsupported(semantic_name))
}

pub struct VertexInput {
    semantic_name: CString,
    semantic_index: usize,
    location: usize,
}

fn dump_vl_space_offset_forall_category(vl: &slang::reflection::VariableLayout, prefix: &str) {
    for cat in vl.iter_category() {
        println!(
            "{prefix}cat {cat:?} space={} offset={} size={}",
            vl.space(cat),
            vl.offset(cat),
            vl.type_layout().size(cat),
        );
    }
}

fn dump_user_attrs(vl: &slang::reflection::VariableLayout, prefix: &str) {
    if let Some(v) = vl.variable() {
        for a in v.iter_user_attribute() {
            println!("{prefix}var user attr: {:?}", a.name());
        }

        for a in v.r#type().iter_user_attribute() {
            println!("{prefix}type user attr: {:?}", a.name());
        }
    }
}

fn visit_vsh_input(
    v: &slang::reflection::VariableLayout,
    global_session: &impl slang::IGlobalSession,
    indent: usize,
    collected: &mut Vec<VertexInput>,
) {
    let ty = v.type_layout();
    println!("{}vsh param: {:?}", "  ".repeat(indent), v.name());
    dump_vl_space_offset_forall_category(v, &"  ".repeat(indent));
    dump_user_attrs(v, &"  ".repeat(indent + 1));
    dump_ty_layout(ty, indent + 1);

    if let Some(t) = ty.r#type()
        && t.kind() == slang::reflection::TypeKind::Struct
    {
        // recurse members
        for f in ty.iter_field() {
            visit_vsh_input(f, global_session, indent + 1, collected);
        }
        return;
    }

    if v.variable()
        .and_then(|v| v.find_user_attribute_by_name(global_session, c"peridot_vertex"))
        .is_some()
    {
        assert!(
            v.iter_category()
                .any(|x| x == slang::reflection::ParameterCategory::VaryingInput)
        );

        collected.push(VertexInput {
            semantic_name: v.semantic_name().expect("no semantic decorated").to_owned(),
            semantic_index: v.semantic_index(),
            location: v.offset(slang::reflection::ParameterCategory::VaryingInput),
        });
    }
}

fn recursive_dump_global_input(vl: &slang::reflection::VariableLayout, indent: usize) {
    let ty = vl.type_layout();
    println!("{}g param: {:?}", "  ".repeat(indent), vl.name());
    dump_ty_layout(ty, indent + 1);
    dump_vl_space_offset_forall_category(vl, &"  ".repeat(indent + 1));
    dump_user_attrs(vl, &"  ".repeat(indent + 1));

    if let Some(c) = ty.container_var_layout() {
        println!("{}Container:", "  ".repeat(indent + 1));
        recursive_dump_global_input(c, indent + 2);
    }

    if let Some(c) = ty.content_var_layout() {
        println!("{}Content:", "  ".repeat(indent + 1));
        recursive_dump_global_input(c, indent + 2);
    }

    if let Some(e) = ty.element_var_layout() {
        println!("{}Element:", "  ".repeat(indent + 1));
        recursive_dump_global_input(e, indent + 2);
    }

    for f in ty.iter_field() {
        recursive_dump_global_input(f, indent + 1);
    }
}

pub struct VarName {
    name: Option<CString>,
    parent: Option<VarNameRef>,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct VarNameRef(NonZeroUsize);
impl VarNameRef {
    #[inline(always)]
    const fn from_array_index(v: usize) -> Self {
        Self(unsafe { NonZeroUsize::new_unchecked(v + 1) })
    }

    #[inline(always)]
    const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

pub struct VarNamePool(Vec<VarName>);
impl VarNamePool {
    #[inline(always)]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    #[inline(always)]
    pub fn add(&mut self, name: VarName) -> VarNameRef {
        self.0.push(name);
        VarNameRef::from_array_index(self.0.len() - 1)
    }

    #[inline(always)]
    pub fn get(&self, r: VarNameRef) -> &VarName {
        &self.0[r.into_array_index()]
    }

    pub fn query_chained(&self, leaf: VarNameRef) -> Vec<Option<&core::ffi::CStr>> {
        let mut sink = Vec::new();
        let mut ptr = Some(leaf);
        while let Some(p) = ptr {
            let x = &self.0[p.into_array_index()];
            sink.push(x.name.as_deref());
            ptr = x.parent;
        }
        sink.reverse();

        sink
    }
}

#[derive(Debug)]
struct VarPlacement {
    descriptor_binding: usize,
    descriptor_set: usize,
    byte_offset: usize,
}
impl VarPlacement {
    const ZERO: Self = Self {
        descriptor_binding: 0,
        descriptor_set: 0,
        byte_offset: 0,
    };
}

#[derive(Debug)]
struct ParameterCategoryValues {
    space: usize,
    offset: usize,
    size: usize,
}

#[derive(Debug)]
pub struct AccumulatedOffsetHistoryEntry {
    values: HashMap<slang::reflection::ParameterCategory, ParameterCategoryValues>,
    container_sub_element_register_space_size: usize,
    is_container: bool,
    parent: Option<AccumulatedOffsetHistoryHandle>,
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct AccumulatedOffsetHistoryHandle(NonZeroUsize);
impl AccumulatedOffsetHistoryHandle {
    #[inline(always)]
    const fn from_array_index(v: usize) -> Self {
        Self(unsafe { NonZeroUsize::new_unchecked(v + 1) })
    }

    #[inline(always)]
    const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

pub struct AccumulatedOffsetHistoryArena {
    entries: Vec<AccumulatedOffsetHistoryEntry>,
}
impl AccumulatedOffsetHistoryArena {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    pub fn create(&mut self, e: AccumulatedOffsetHistoryEntry) -> AccumulatedOffsetHistoryHandle {
        self.entries.push(e);
        AccumulatedOffsetHistoryHandle::from_array_index(self.entries.len() - 1)
    }

    pub fn get(&self, handle: AccumulatedOffsetHistoryHandle) -> &AccumulatedOffsetHistoryEntry {
        &self.entries[handle.into_array_index()]
    }
}

struct VisitContext<'env> {
    accumulated_offset_history_arena: &'env mut AccumulatedOffsetHistoryArena,
    accumulated_offset_history_parent: Option<AccumulatedOffsetHistoryHandle>,
    var_name_pool: &'env mut VarNamePool,
    parent_name_ref: Option<VarNameRef>,
    is_material_property: bool,
}

pub struct InputVariable<'a> {
    path: VarNameRef,
    var: &'a slang::reflection::VariableLayout,
    r#type: &'a slang::reflection::TypeLayout,
    category: slang::reflection::ParameterCategory,
    placement: VarPlacement,
    size: usize,
    is_material_property: bool,
    property_name_override: Option<CString>,
}

pub struct EngineInputVariable {
    placement: VarPlacement,
}

pub struct GlobalInputCollected<'a> {
    pub input_vars: Vec<InputVariable<'a>>,
    pub object_parameters_var: Option<EngineInputVariable>,
    pub camera_parameters_var: Option<EngineInputVariable>,
}

enum InputVarType {
    Unknown,
    MaterialProperty,
    EngineObjectParameters,
    EngineCameraParameters,
}

// accumulation algorithm deformed from official docs: https://docs.shader-slang.org/en/latest/external/slang/docs/user-guide/09-reflection.html#handling-single-element-containers
fn accumulate_var_placement(
    leaf: AccumulatedOffsetHistoryHandle,
    arena: &AccumulatedOffsetHistoryArena,
    is_uniform: bool,
) -> VarPlacement {
    if is_uniform {
        let mut accumulate_uniform_offset = true;
        let mut accumulate_descriptor_binding_offset = true;
        let mut accumulated = VarPlacement::ZERO;
        let mut ptr = Some(leaf);
        while let Some(p) = ptr {
            let h = arena.get(p);

            if accumulate_uniform_offset {
                accumulated.byte_offset += h
                    .values
                    .get(&slang::reflection::ParameterCategory::Uniform)
                    .map_or(0, |x| x.offset);

                if h.is_container {
                    // container boundary
                    accumulate_uniform_offset = false;

                    // accumulate descriptor locations from here
                    accumulated.descriptor_binding = 0;
                    accumulated.descriptor_set = 0;
                }
            }
            if accumulate_descriptor_binding_offset {
                accumulated.descriptor_binding += h
                    .values
                    .get(&slang::reflection::ParameterCategory::DescriptorTableSlot)
                    .map_or(0, |x| x.offset);
                accumulated.descriptor_set += h
                    .values
                    .get(&slang::reflection::ParameterCategory::DescriptorTableSlot)
                    .map_or(0, |x| x.space);

                if h.container_sub_element_register_space_size > 0 {
                    // sub space boundary
                    accumulate_descriptor_binding_offset = false;
                }
            }
            accumulated.descriptor_set += h
                .values
                .get(&slang::reflection::ParameterCategory::SubElementRegisterSpace)
                .map_or(0, |x| x.offset);

            ptr = h.parent;
        }

        accumulated
    } else {
        let mut accumulate_descriptor_binding_offset = true;
        let mut accumulated = VarPlacement::ZERO;
        let mut ptr = Some(leaf);
        while let Some(p) = ptr {
            let h = arena.get(p);

            if accumulate_descriptor_binding_offset {
                accumulated.descriptor_binding += h
                    .values
                    .get(&slang::reflection::ParameterCategory::DescriptorTableSlot)
                    .map_or(0, |x| x.offset);
                accumulated.descriptor_set += h
                    .values
                    .get(&slang::reflection::ParameterCategory::DescriptorTableSlot)
                    .map_or(0, |x| x.space);
            }
            accumulated.descriptor_set += h
                .values
                .get(&slang::reflection::ParameterCategory::SubElementRegisterSpace)
                .map_or(0, |x| x.offset);

            if h.container_sub_element_register_space_size > 0 {
                // sub space boundary
                accumulate_descriptor_binding_offset = false;
            }

            ptr = h.parent;
        }

        accumulated
    }
}

fn visit_global_input<'a>(
    v: &'a slang::reflection::VariableLayout,
    indent: usize,
    global_session: &impl slang::IGlobalSession,
    ctx: &mut VisitContext,
    collected: &mut GlobalInputCollected<'a>,
) {
    let is_material_property = ctx.is_material_property
        || v.variable()
            .and_then(|v| {
                v.find_user_attribute_by_name(global_session, c"peridot_material_property")
            })
            .is_some();
    let constant_buffer_content_full_type_name = 'try_constant_buffer_content_full_type_name: {
        match v.type_layout().kind() {
            slang::reflection::TypeKind::ConstantBuffer => {
                let Some(content_var) = v.type_layout().content_var_layout() else {
                    tracing::error!("contant buffer have no content var?");
                    break 'try_constant_buffer_content_full_type_name None;
                };
                match content_var
                    .type_layout()
                    .r#type()
                    .expect("no type?")
                    .full_name()
                {
                    Ok(x) => Some(x),
                    Err(e) => {
                        tracing::error!(reason = ?e, "failed to query full name of content type");
                        break 'try_constant_buffer_content_full_type_name None;
                    }
                }
            }
            _ => None,
        }
    };
    let is_engine_object_parameters = constant_buffer_content_full_type_name
        .as_ref()
        .is_some_and(|x| x.as_cstr() == c"peridot.ObjectParameters");
    let is_engine_camera_parameters = constant_buffer_content_full_type_name
        .as_ref()
        .is_some_and(|x| x.as_cstr() == c"peridot.CameraParameters");
    let input_var_type = match (
        is_material_property,
        is_engine_object_parameters,
        is_engine_camera_parameters,
    ) {
        (true, false, false) => InputVarType::MaterialProperty,
        (false, true, false) => InputVarType::EngineObjectParameters,
        (false, false, true) => InputVarType::EngineCameraParameters,
        (false, false, false) => {
            tracing::warn!("cannot determine variable type");
            InputVarType::Unknown
        }
        _ => {
            tracing::warn!("cannot determine varaible type (role conflicting)");
            InputVarType::Unknown
        }
    };
    let property_name_override = v
        .variable()
        .and_then(|v| v.find_user_attribute_by_name(global_session, c"peridot_property_name"))
        .map(|x| {
            assert_eq!(x.argument_count(), 1);
            x.argument_value_string(0)
                .expect("missing argument")
                .to_owned()
        });

    let ty = v.type_layout();
    println!("{}g param: {:?}", "  ".repeat(indent), v.name());
    dump_ty_layout(ty, indent + 1);
    dump_vl_space_offset_forall_category(v, &"  ".repeat(indent + 1));
    dump_user_attrs(v, &"  ".repeat(indent + 1));

    let var_name_ref = ctx.var_name_pool.add(VarName {
        name: v.name().map(ToOwned::to_owned),
        parent: ctx.parent_name_ref,
    });
    let accumulated_offset_history_self =
        ctx.accumulated_offset_history_arena
            .create(AccumulatedOffsetHistoryEntry {
                values: v
                    .iter_category()
                    .map(|cat| {
                        (
                            cat,
                            ParameterCategoryValues {
                                space: v.space(cat),
                                offset: v.offset(cat),
                                size: v.type_layout().size(cat),
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>(),
                is_container: matches!(
                    ty.kind(),
                    slang::reflection::TypeKind::ConstantBuffer
                        | slang::reflection::TypeKind::ParameterBlock
                        | slang::reflection::TypeKind::TextureBuffer
                        | slang::reflection::TypeKind::ShaderStorageBuffer
                ),
                container_sub_element_register_space_size: ty.container_var_layout().map_or(
                    0,
                    |x| {
                        x.type_layout()
                            .size(slang::reflection::ParameterCategory::SubElementRegisterSpace)
                    },
                ),
                parent: ctx.accumulated_offset_history_parent,
            });
    println!(
        "{}parameter category value history:",
        "  ".repeat(indent + 1)
    );
    let mut ptr = Some(accumulated_offset_history_self);
    while let Some(p) = ptr {
        let h = ctx.accumulated_offset_history_arena.get(p);
        println!("{}{h:?}", "  ".repeat(indent + 2));
        ptr = h.parent;
    }

    let variable_placement = accumulate_var_placement(
        accumulated_offset_history_self,
        ctx.accumulated_offset_history_arena,
        v.category() == slang::reflection::ParameterCategory::Uniform,
    );
    println!(
        "{}variable placed: {variable_placement:?}",
        "  ".repeat(indent + 1),
    );

    match input_var_type {
        InputVarType::MaterialProperty if v.category_count() == 1 => {
            let cat = v.category_by_index(0);
            collected.input_vars.push(InputVariable {
                path: var_name_ref,
                var: v,
                r#type: ty,
                category: cat,
                placement: variable_placement,
                size: ty.size(cat),
                is_material_property,
                property_name_override,
            });
        }
        InputVarType::EngineObjectParameters => {
            if collected.object_parameters_var.is_some() {
                tracing::warn!("multiple object parameters variables found");
            }
            collected.object_parameters_var = Some(EngineInputVariable {
                placement: variable_placement,
            });
        }
        InputVarType::EngineCameraParameters => {
            if collected.camera_parameters_var.is_some() {
                tracing::warn!("multiple camera parameters variables found");
            }
            collected.camera_parameters_var = Some(EngineInputVariable {
                placement: variable_placement,
            });
        }
        _ => {
            let mut variable_path = Vec::new();
            let mut ptr = Some(var_name_ref);
            while let Some(p) = ptr {
                let x = ctx.var_name_pool.get(p);
                variable_path.push(x.name.as_deref());
                ptr = x.parent;
            }
            variable_path.reverse();

            tracing::warn!(path = ?variable_path, "ignoring variable...");
        }
    }

    let mut child_ctx = VisitContext {
        var_name_pool: ctx.var_name_pool,
        parent_name_ref: Some(var_name_ref),
        is_material_property,
        accumulated_offset_history_arena: ctx.accumulated_offset_history_arena,
        accumulated_offset_history_parent: Some(accumulated_offset_history_self),
    };

    //     if let Some(cvl) = ty.container_var_layout() {
    //         // println!(
    //         //     "{}- cvl: sub element register space size: {}",
    //         //     "  ".repeat(indent),
    //         //     cvl.type_layout()
    //         //         .size(slang::reflection::ParameterCategory::SubElementRegisterSpace)
    //         // );
    //         // for cat in cvl.iter_category() {
    //         //     println!(
    //         //         "{}- cvl({:?}) {cat:?}: offset={}",
    //         //         "  ".repeat(indent),
    //         //         cvl.type_layout().kind(),
    //         //         cvl.offset(cat)
    //         //     );
    //         // }
    //
    //         // containerはparameter category value stackにのせなくていい(Slang公式も含めてないっぽい)
    //     }

    // Note: getElementTypeはgetContentTypeのサブセットっぽい(cpp見る感じでは)のでElementVarのほうは見なくてもよさそう

    match ty.kind() {
        slang::reflection::TypeKind::Struct => {
            // recurse members
            for f in ty.iter_field() {
                visit_global_input(f, indent + 1, global_session, &mut child_ctx, collected);
            }
        }
        slang::reflection::TypeKind::ParameterBlock
        | slang::reflection::TypeKind::ConstantBuffer
        | slang::reflection::TypeKind::ShaderStorageBuffer => {
            visit_global_input(
                ty.content_var_layout().expect("no element var"),
                indent + 1,
                global_session,
                &mut child_ctx,
                collected,
            );
        }
        slang::reflection::TypeKind::Scalar
        | slang::reflection::TypeKind::Vector
        | slang::reflection::TypeKind::Matrix
        | slang::reflection::TypeKind::Resource => {}
        t => tracing::warn!("unhandled type: {t:?}"),
    }
}

fn dump_ty_layout(tl: &slang::reflection::TypeLayout, indent: usize) {
    let prespace = "  ".repeat(indent);
    let kind = tl.kind();

    match tl.r#type() {
        None => println!("{prespace}<Unknown Type>: {kind:?}"),
        Some(ty) => println!("{prespace}{:?}({:?}): {kind:?}", ty.name(), ty.full_name()),
    }
}

fn recursive_dump_ty_layout(tl: &slang::reflection::TypeLayout, indent: usize) {
    dump_ty_layout(tl, indent);

    let prespace = "  ".repeat(indent);
    match tl.kind() {
        slang::reflection::TypeKind::Vector | slang::reflection::TypeKind::Array => {
            println!(
                "{prespace}- element count: {}",
                tl.r#type().expect("no type?").element_count(None)
            );
        }
        slang::reflection::TypeKind::Matrix => {
            let ty = tl.r#type().expect("no type?");
            println!(
                "{prespace}- element count: {}x{}",
                ty.row_count(),
                ty.column_count()
            );
        }
        slang::reflection::TypeKind::Resource => {
            let ty = tl.r#type().expect("no type?");
            let shape = ty.resource_shape();
            println!(
                "{prespace}- resource: shape_base={} shape_ext=0x{:x} access={} result_ty={:?}",
                shape & slang::ffi::SLANG_RESOURCE_BASE_SHAPE_MASK,
                shape & slang::ffi::SLANG_RESOURCE_EXT_SHAPE_MASK,
                ty.resource_access(),
                ty.resource_result_type().expect("no result type").name()
            );

            println!("{prespace}  ResultType:");
            recursive_dump_ty(
                ty.resource_result_type().expect("no result type"),
                indent + 2,
            );
        }
        _ => {}
    }

    if let Some(x) = tl.container_var_layout() {
        println!("{prespace}  Container:");
        recursive_dump_ty_layout(x.type_layout(), indent + 2);
    }

    if let Some(x) = tl.content_var_layout() {
        println!("{prespace}  Content:");
        recursive_dump_ty_layout(x.type_layout(), indent + 2);
    }

    if let Some(x) = tl.element_var_layout() {
        println!("{prespace}  Element:");
        recursive_dump_ty_layout(x.type_layout(), indent + 2);
    }

    for f in tl.iter_field() {
        recursive_dump_ty_layout(f.type_layout(), indent + 1);
    }
}
fn recursive_dump_ty(ty: &slang::reflection::Type, indent: usize) {
    let prespace = "  ".repeat(indent);
    let kind = ty.kind();

    println!("{prespace}{:?}({:?}): {kind:?}", ty.name(), ty.full_name());

    match kind {
        slang::reflection::TypeKind::Vector | slang::reflection::TypeKind::Array => {
            println!("{prespace}- element count: {}", ty.element_count(None));
        }
        slang::reflection::TypeKind::Matrix => {
            println!(
                "{prespace}- element count: {}x{}",
                ty.row_count(),
                ty.column_count()
            );
        }
        slang::reflection::TypeKind::Resource => {
            let shape = ty.resource_shape();
            println!(
                "{prespace}- resource: shape_base={} shape_ext=0x{:x} access={}",
                shape & slang::ffi::SLANG_RESOURCE_BASE_SHAPE_MASK,
                shape & slang::ffi::SLANG_RESOURCE_EXT_SHAPE_MASK,
                ty.resource_access(),
            );

            println!("{prespace}  ResultType:");
            recursive_dump_ty(
                ty.resource_result_type().expect("no result type"),
                indent + 2,
            );
        }
        _ => {}
    }

    if let Some(x) = ty.element_type() {
        println!("{prespace}  Element:");
        recursive_dump_ty(x, indent + 2);
    }

    for f in ty.iter_field() {
        recursive_dump_ty(f.r#type(), indent + 1);
    }
}

fn print_slang_diag(diag: Option<slang::Owned<slang::IBlobPtr>>) {
    let Some(d) = diag else {
        return;
    };

    let str = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer().cast()) };
    match str.to_str() {
        Err(x) => {
            tracing::warn!(target: "libslang-diag", to_str_err = ?x, msg = ?str);
        }
        Ok(x) => {
            for l in x.lines() {
                eprintln!("[libslang] {l}");
            }
        }
    }
}
