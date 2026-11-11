use std::{
    collections::{HashMap, HashSet},
    ffi::CString,
    io::{BufRead, IoSlice, IoSliceMut, Write},
    num::NonZeroUsize,
    path::Path,
};

use bitflags::bitflags;
use peridot::VertexInputSemantic;
use peridot_serialization_utils::{PascalStr, PascalString, VariableULong};
use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession};
use vectored_io_helper::{read_vectored_all, write_vectored_all};

#[cfg(feature = "with-asset-processing")]
mod asset_processing;
#[cfg(feature = "with-asset-processing")]
pub use self::asset_processing::*;

pub mod engine_params;

pub struct AssetCore;
impl peridot::LogicalAssetData for AssetCore {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SHADER;
}

pub struct Environment {
    pub peridot_stdlib_search_path: CString,
}

pub fn build(source_path: impl AsRef<Path>, env: &Environment) -> AssetVk {
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
        &CString::new(std::fs::read_to_string(source_path).expect("file.read"))
            .expect("nui in source"),
        Some(&mut diag),
    );
    print_slang_diag(unsafe { diag.assume_init() });
    let module = module.expect("failed to load module");

    let entrypoints = module
        .iter_defined_entry_point()
        .collect::<Result<Vec<_>, _>>()
        .expect("entrypoints");
    let mut program_components = Vec::<core::ptr::NonNull<core::ffi::c_void>>::with_capacity(
        1 + module.get_defined_entry_point_count() as usize,
    );
    // Note: ここでclone_cast(というよりおそらくquery_interface)してはいけない(詳しくは不明だが二重解放がおきるらしい create_composite_component_typeには参照の配列で渡す必要がある)
    // ロード済みmoduleをlocal_sessionに記録してるっぽいので、Moduleをcloneすると同じModuleが二重に登録されることになるのかもしれない
    program_components.push(unsafe {
        core::mem::transmute::<slang::IModulePtr, core::ptr::NonNull<core::ffi::c_void>>(module)
    });
    program_components.extend(entrypoints.iter().map(|e| unsafe {
        *core::mem::transmute::<&slang::IEntryPointPtr, &core::ptr::NonNull<core::ffi::c_void>>(e)
    }));
    let mut diag = core::mem::MaybeUninit::new(None);
    let program = local_session.create_composite_component_type(
        unsafe {
            core::mem::transmute::<&[core::ptr::NonNull<core::ffi::c_void>], &[_]>(
                &program_components,
            )
        },
        Some(&mut diag),
    );
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
                visit_vsh_input(f, &session, 0, &mut vertex_inputs);
            }
        }
        if e.stage() == slang::ffi::SLANG_STAGE_FRAGMENT {
            fragment_entry_point_name = Some(e.name());
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
                    &session,
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
                    &session,
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
    for v in collected.input_vars {
        if v.is_material_property {
            let r#type =
                AssetInputVariableType::from_slang_type(v.r#type.r#type().expect("no type?"))
                    .expect("unsupported input variable type");
            material_properties.push(AssetMaterialPropertyVk {
                name: var_name_pool
                    .query_chained(v.path)
                    .iter()
                    .flatten()
                    .map(|x| x.to_str().expect("invalid path str"))
                    .collect::<Vec<_>>()
                    .join("."),
                property_name_override: v.property_name_override.map(|x| {
                    x.to_str()
                        .expect("invalid property name override")
                        .to_owned()
                }),
                r#type,
                descriptor_binding: v.placement.descriptor_binding as _,
                descriptor_set: v.placement.descriptor_set as _,
                offset: v.placement.byte_offset as _,
                size: v.size as _,
            });

            let input_variable_layout_key = (
                v.placement.descriptor_set as u32,
                v.placement.descriptor_binding as u32,
            );
            match v.category {
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
                                            match input_variable_layout
                                                .entry(input_variable_layout_key)
                                            {
                                                std::collections::hash_map::Entry::Vacant(
                                                    entry,
                                                ) => {
                                                    entry.insert(AssetInputVariableLayoutVk::CombinedImageSampler);
                                                }
                                                std::collections::hash_map::Entry::Occupied(_) => {
                                                    panic!("conflicting descriptor content");
                                                }
                                            }
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
                    match input_variable_layout.entry(input_variable_layout_key) {
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(AssetInputVariableLayoutVk::UniformBuffer {
                                fields: vec![AssetDescriptorSlotFieldVk {
                                    offset: v
                                        .var
                                        .offset(slang::reflection::ParameterCategory::Uniform)
                                        as _,
                                    size: v
                                        .r#type
                                        .size(slang::reflection::ParameterCategory::Uniform)
                                        as _,
                                }],
                            });
                        }
                        std::collections::hash_map::Entry::Occupied(entry) => {
                            match entry.into_mut() {
                                AssetInputVariableLayoutVk::UniformBuffer { fields } => {
                                    fields.push(AssetDescriptorSlotFieldVk {
                                        offset: v
                                            .var
                                            .offset(slang::reflection::ParameterCategory::Uniform)
                                            as _,
                                        size: v
                                            .r#type
                                            .size(slang::reflection::ParameterCategory::Uniform)
                                            as _,
                                    });
                                }
                                AssetInputVariableLayoutVk::CombinedImageSampler => {
                                    panic!("conflicting descriptor content");
                                }
                            }
                        }
                    }
                }
                cat => unreachable!("unsupported parameter category? {cat:?}"),
            }
        }
    }

    AssetVk {
        code_binary,
        vertex_entry_point_name,
        fragment_entry_point_name,
        vertex_inputs: vertex_inputs
            .into_iter()
            .map(|x| {
                (
                    extract_vertex_semantic(&x.semantic_name, x.semantic_index)
                        .expect("invalid semantic"),
                    AssetVertexInput {
                        location: x.location as _,
                    },
                )
            })
            .collect(),
        material_properties,
        input_variable_layout,
        object_parameters_placement: collected.object_parameters_var.map(|x| {
            assert_eq!(
                x.placement.byte_offset, 0,
                "engine parameters placed on nonzero offset"
            );
            (
                x.placement.descriptor_set as u32,
                x.placement.descriptor_binding as u32,
            )
        }),
        camera_parameters_placement: collected.camera_parameters_var.map(|x| {
            assert_eq!(
                x.placement.byte_offset, 0,
                "engine parameters placed on nonzero offset"
            );
            (
                x.placement.descriptor_set as u32,
                x.placement.descriptor_binding as u32,
            )
        }),
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssetInputVariableType {
    Float4,
    Texture2D,
}
impl AssetInputVariableType {
    pub fn from_slang_type(t: &slang::reflection::Type) -> Option<Self> {
        match t.kind() {
            slang::reflection::TypeKind::ConstantBuffer
            | slang::reflection::TypeKind::ParameterBlock
            | slang::reflection::TypeKind::Struct => {
                // no support for compound objects
                None
            }
            slang::reflection::TypeKind::Array => todo!("array type property"),
            slang::reflection::TypeKind::Vector => {
                let element_count = t.element_count(None);
                let element_type = t.element_type().expect("no vector element type");
                let element_scalar_type = element_type.scalar_type();
                match (element_scalar_type, element_count) {
                    (slang::reflection::ScalarType::Float32, 4) => Some(Self::Float4),
                    (t, c) => {
                        eprintln!("unknown vector element combination: {t:?} x {c}");
                        None
                    }
                }
            }
            slang::reflection::TypeKind::Matrix => todo!("matrix type property"),
            slang::reflection::TypeKind::Resource => {
                let shape = t.resource_shape();
                let result_type = t.resource_result_type().expect("no result type");
                match (
                    shape & slang::ffi::SLANG_RESOURCE_BASE_SHAPE_MASK,
                    result_type.kind(),
                ) {
                    (slang::ffi::SLANG_TEXTURE_2D, slang::reflection::TypeKind::Vector) => {
                        let element_count = result_type.element_count(None);
                        let element_type =
                            result_type.element_type().expect("no vector element type");
                        let element_scalar_type = element_type.scalar_type();
                        match (element_scalar_type, element_count) {
                            (slang::reflection::ScalarType::Float32, 4) => Some(Self::Texture2D),
                            (t, c) => {
                                eprintln!(
                                    "unknown resource result vector combination: {t:?} x {c}"
                                );
                                None
                            }
                        }
                    }
                    (s, k) => {
                        eprintln!("unknown resource shape/result combination: {s:?}/{k:?}");
                        None
                    }
                }
            }
            _ => unimplemented!("unknown type kind"),
        }
    }
}

#[derive(Debug)]
pub struct AssetVertexInput {
    pub location: u8,
}

#[inline(always)]
fn mkioslicem<'a, T>(data: &'a mut core::mem::MaybeUninit<T>) -> IoSliceMut<'a> {
    IoSliceMut::new(unsafe {
        core::slice::from_raw_parts_mut(data.as_mut_ptr().cast(), size_of::<T>())
    })
}

#[derive(Debug)]
pub struct AssetMaterialPropertyVk {
    pub name: String,
    pub property_name_override: Option<String>,
    pub r#type: AssetInputVariableType,
    pub descriptor_binding: u32,
    pub descriptor_set: u32,
    pub offset: u32,
    pub size: u32,
}
impl AssetMaterialPropertyVk {
    fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        PascalStr(&self.name).write(w)?;
        PascalStr(self.property_name_override.as_deref().unwrap_or("")).write(w)?;
        write_vectored_all(
            w,
            &mut [
                IoSlice::new(&[self.r#type as u8]),
                IoSlice::new(&self.descriptor_set.to_ne_bytes()),
                IoSlice::new(&self.descriptor_binding.to_ne_bytes()),
                IoSlice::new(&self.offset.to_ne_bytes()),
                IoSlice::new(&self.size.to_ne_bytes()),
            ],
        )?;

        Ok(())
    }

    fn read(r: &mut (impl BufRead + ?Sized), is_native_endian: bool) -> std::io::Result<Self> {
        let name = PascalString::read(r)?;
        let property_name_override = PascalString::read(r)?;
        let property_name_override = if property_name_override.0.is_empty() {
            None
        } else {
            Some(property_name_override.0)
        };
        let mut r#type = core::mem::MaybeUninit::<AssetInputVariableType>::uninit();
        let mut descriptor_set = core::mem::MaybeUninit::<u32>::uninit();
        let mut descriptor_binding = core::mem::MaybeUninit::<u32>::uninit();
        let mut offset = core::mem::MaybeUninit::<u32>::uninit();
        let mut size = core::mem::MaybeUninit::<u32>::uninit();
        read_vectored_all(
            r,
            &mut [
                mkioslicem(&mut r#type),
                mkioslicem(&mut descriptor_set),
                mkioslicem(&mut descriptor_binding),
                mkioslicem(&mut offset),
                mkioslicem(&mut size),
            ],
        )?;
        let r#type = unsafe { r#type.assume_init() };
        let descriptor_set = unsafe { descriptor_set.assume_init() };
        let descriptor_binding = unsafe { descriptor_binding.assume_init() };
        let offset = unsafe { offset.assume_init() };
        let size = unsafe { size.assume_init() };

        if is_native_endian {
            Ok(Self {
                name: name.0,
                property_name_override,
                r#type,
                descriptor_set,
                descriptor_binding,
                offset,
                size,
            })
        } else {
            Ok(Self {
                name: name.0,
                property_name_override,
                r#type,
                descriptor_set: descriptor_set.swap_bytes(),
                descriptor_binding: descriptor_binding.swap_bytes(),
                offset: offset.swap_bytes(),
                size: size.swap_bytes(),
            })
        }
    }
}

#[derive(Debug)]
pub struct AssetDescriptorSlotFieldVk {
    pub offset: u32,
    pub size: u32,
}

#[derive(Debug)]
pub enum AssetInputVariableLayoutVk {
    CombinedImageSampler,
    UniformBuffer {
        fields: Vec<AssetDescriptorSlotFieldVk>,
    },
}
impl AssetInputVariableLayoutVk {
    fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::CombinedImageSampler => {
                w.write_all(&[0])?;
            }
            Self::UniformBuffer { fields } => {
                w.write_all(&[128])?;
                VariableULong(fields.len() as _).write(w)?;
                let mut fields = fields.iter().collect::<Vec<_>>();
                fields.sort_by_key(|v| v.offset);
                for f in fields {
                    write_vectored_all(
                        w,
                        &mut [
                            IoSlice::new(&f.offset.to_ne_bytes()),
                            IoSlice::new(&f.size.to_ne_bytes()),
                        ],
                    )?;
                }
            }
        }

        Ok(())
    }

    fn read(r: &mut (impl BufRead + ?Sized), is_native_endian: bool) -> std::io::Result<Self> {
        let mut discriminant = [0u8];
        r.read_exact(&mut discriminant)?;
        match discriminant[0] {
            0 => Ok(Self::CombinedImageSampler),
            128 => {
                let field_count = VariableULong::read(r)?.0 as usize;
                let mut fields = Vec::with_capacity(field_count);
                for _ in 0..field_count {
                    let mut offset = core::mem::MaybeUninit::<u32>::uninit();
                    let mut size = core::mem::MaybeUninit::<u32>::uninit();
                    read_vectored_all(r, &mut [mkioslicem(&mut offset), mkioslicem(&mut size)])?;
                    let offset = unsafe { offset.assume_init() };
                    let size = unsafe { size.assume_init() };

                    fields.push(AssetDescriptorSlotFieldVk {
                        offset: if is_native_endian {
                            offset
                        } else {
                            offset.swap_bytes()
                        },
                        size: if is_native_endian {
                            size
                        } else {
                            size.swap_bytes()
                        },
                    });
                }

                Ok(Self::UniformBuffer { fields })
            }
            v => unreachable!("invalid discriminant byte: {v}"),
        }
    }
}

pub struct AssetVk {
    pub code_binary: Vec<u32>,
    pub vertex_entry_point_name: String,
    pub fragment_entry_point_name: Option<String>,
    pub vertex_inputs: HashMap<VertexInputSemantic, AssetVertexInput>,
    pub material_properties: Vec<AssetMaterialPropertyVk>,
    pub input_variable_layout: HashMap<(u32, u32), AssetInputVariableLayoutVk>,
    pub object_parameters_placement: Option<(u32, u32)>,
    pub camera_parameters_placement: Option<(u32, u32)>,
}
impl peridot::LogicalAssetData for AssetVk {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SHADER;
}
impl peridot::FromAssetBlob for AssetVk {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob<'a, Blob: peridot::AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        Self::read(&mut std::io::BufReader::new(
            peridot_native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
    }
}
impl AssetVk {
    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        let mut content_flags = AssetVkContentFlags::empty();
        if self.object_parameters_placement.is_some() {
            content_flags.insert(AssetVkContentFlags::HAS_OBJECT_PARAMETER);
        }
        if self.camera_parameters_placement.is_some() {
            content_flags.insert(AssetVkContentFlags::HAS_CAMERA_PARAMETER);
        }
        if self.fragment_entry_point_name.is_some() {
            content_flags.insert(AssetVkContentFlags::HAS_FRAGMENT_ENTRY_POINT);
        }

        // head
        write_vectored_all(
            w,
            &mut [
                IoSlice::new(&0x1234u16.to_ne_bytes()),
                IoSlice::new(&[content_flags.bits()]),
            ],
        )?;

        // material props
        VariableULong(self.material_properties.len() as _).write(w)?;
        for p in self.material_properties.iter() {
            p.write(w)?;
        }

        // input layouts
        VariableULong(self.vertex_inputs.len() as _).write(w)?;
        for (s, vi) in self.vertex_inputs.iter() {
            s.write(w)?;
            w.write_all(&[vi.location])?;
        }

        VariableULong(self.input_variable_layout.len() as _).write(w)?;
        let mut input_variable_layouts = self.input_variable_layout.iter().collect::<Vec<_>>();
        input_variable_layouts.sort_by_key(|&(k, _)| k);
        for (k, v) in input_variable_layouts {
            write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&k.0.to_ne_bytes()),
                    IoSlice::new(&k.1.to_ne_bytes()),
                ],
            )?;
            v.write(w)?;
        }

        // shader props
        PascalStr(&self.vertex_entry_point_name).write(w)?;
        if let Some(ref x) = self.fragment_entry_point_name {
            PascalStr(x).write(w)?;
        }
        if let Some(ref x) = self.object_parameters_placement {
            write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&x.0.to_ne_bytes()),
                    IoSlice::new(&x.1.to_ne_bytes()),
                ],
            )?;
        }
        if let Some(ref x) = self.camera_parameters_placement {
            write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&x.0.to_ne_bytes()),
                    IoSlice::new(&x.1.to_ne_bytes()),
                ],
            )?;
        }

        // shader code
        VariableULong(self.code_binary.len() as _).write(w)?;
        w.write_all(unsafe {
            core::slice::from_raw_parts(
                self.code_binary.as_ptr().cast(),
                self.code_binary.len() << 2,
            )
        })?;

        Ok(())
    }

    pub fn read(r: &mut (impl BufRead + ?Sized)) -> std::io::Result<Self> {
        // head
        let mut endian_mark = core::mem::MaybeUninit::<u16>::uninit();
        let mut content_flags = core::mem::MaybeUninit::<AssetVkContentFlags>::uninit();
        read_vectored_all(
            r,
            &mut [mkioslicem(&mut endian_mark), mkioslicem(&mut content_flags)],
        )?;
        let endian_mark = unsafe { endian_mark.assume_init() };
        let is_native_endian = if endian_mark == 0x1234 {
            true
        } else if endian_mark.swap_bytes() == 0x1234 {
            false
        } else {
            panic!("invalid endian mark: 0x{endian_mark:04x}")
        };
        let content_flags = unsafe { content_flags.assume_init() };

        // material props
        let material_property_count = VariableULong::read(r)?.0 as usize;
        let mut material_properties = Vec::with_capacity(material_property_count);
        for _ in 0..material_property_count {
            material_properties.push(AssetMaterialPropertyVk::read(r, is_native_endian)?);
        }

        // input layouts
        let vertex_input_count = VariableULong::read(r)?.0 as usize;
        let mut vertex_inputs = HashMap::with_capacity(vertex_input_count);
        for _ in 0..vertex_input_count {
            let semantic = VertexInputSemantic::read(r)?;
            let mut location = [0u8];
            r.read_exact(&mut location)?;

            vertex_inputs.insert(
                semantic,
                AssetVertexInput {
                    location: location[0],
                },
            );
        }

        let input_variable_layout_count = VariableULong::read(r)?.0 as usize;
        let mut input_variable_layout = HashMap::with_capacity(input_variable_layout_count);
        for _ in 0..input_variable_layout_count {
            let mut set = core::mem::MaybeUninit::<u32>::uninit();
            let mut binding = core::mem::MaybeUninit::<u32>::uninit();
            read_vectored_all(r, &mut [mkioslicem(&mut set), mkioslicem(&mut binding)])?;
            let set = unsafe { set.assume_init() };
            let binding = unsafe { binding.assume_init() };
            let content = AssetInputVariableLayoutVk::read(r, is_native_endian)?;

            if is_native_endian {
                input_variable_layout.insert((set, binding), content);
            } else {
                input_variable_layout.insert((set.swap_bytes(), binding.swap_bytes()), content);
            }
        }

        // shader props
        let vertex_entry_point_name = PascalString::read(r)?.0;
        let fragment_entry_point_name =
            if content_flags.contains(AssetVkContentFlags::HAS_FRAGMENT_ENTRY_POINT) {
                Some(PascalString::read(r)?.0)
            } else {
                None
            };
        let object_parameters_placement =
            if content_flags.contains(AssetVkContentFlags::HAS_OBJECT_PARAMETER) {
                let mut set = core::mem::MaybeUninit::<u32>::uninit();
                let mut binding = core::mem::MaybeUninit::<u32>::uninit();
                read_vectored_all(r, &mut [mkioslicem(&mut set), mkioslicem(&mut binding)])?;
                let set = unsafe { set.assume_init() };
                let binding = unsafe { binding.assume_init() };

                Some(if is_native_endian {
                    (set, binding)
                } else {
                    (set.swap_bytes(), binding.swap_bytes())
                })
            } else {
                None
            };
        let camera_parameters_placement =
            if content_flags.contains(AssetVkContentFlags::HAS_CAMERA_PARAMETER) {
                let mut set = core::mem::MaybeUninit::<u32>::uninit();
                let mut binding = core::mem::MaybeUninit::<u32>::uninit();
                read_vectored_all(r, &mut [mkioslicem(&mut set), mkioslicem(&mut binding)])?;
                let set = unsafe { set.assume_init() };
                let binding = unsafe { binding.assume_init() };

                Some(if is_native_endian {
                    (set, binding)
                } else {
                    (set.swap_bytes(), binding.swap_bytes())
                })
            } else {
                None
            };

        // shader code
        let code_binary_words = VariableULong::read(r)?.0 as usize;
        let mut code_binary = Vec::<u32>::with_capacity(code_binary_words);
        r.read_exact(unsafe {
            core::slice::from_raw_parts_mut(
                code_binary.spare_capacity_mut().as_mut_ptr().cast(),
                code_binary_words << 2,
            )
        })?;
        unsafe {
            code_binary.set_len(code_binary_words);
        }
        if !is_native_endian {
            for x in code_binary.iter_mut() {
                *x = x.swap_bytes();
            }
        }

        Ok(Self {
            code_binary,
            vertex_entry_point_name,
            fragment_entry_point_name,
            vertex_inputs,
            material_properties,
            input_variable_layout,
            object_parameters_placement,
            camera_parameters_placement,
        })
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct AssetVkContentFlags : u8 {
        const HAS_OBJECT_PARAMETER = 0x01;
        const HAS_CAMERA_PARAMETER = 0x02;
        const HAS_FRAGMENT_ENTRY_POINT = 0x04;
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
        InputVarType::MaterialProperty => {
            if v.category_count() == 1 {
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
        InputVarType::Unknown => {
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
        | slang::reflection::TypeKind::ConstantBuffer => {
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

fn print_slang_diag(diag: Option<slang::IBlobPtr>) {
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
