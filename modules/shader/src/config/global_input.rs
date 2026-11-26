use std::{collections::HashMap, ffi::CString, num::NonZeroUsize};

use crate::{
    dump_ty_layout, dump_user_attrs, dump_vl_space_offset_forall_category,
    slang_bridge::{
        CAMERA_PARAMETERS_FULL_TYPE_NAME, MATERIAL_PROPERTY_ATTR_NAME,
        OBJECT_PARAMETERS_FULL_TYPE_NAME, PROPERTY_NAME_ATTR_NAME,
    },
};

pub(crate) struct VisitContext<'env> {
    pub accumulated_offset_history_arena: &'env mut AccumulatedOffsetHistoryArena,
    pub var_name_pool: &'env mut VarNamePool,
}

pub(crate) struct OuterContainerState {
    is_material_property: bool,
    name_ref: Option<VarNameRef>,
    offset_history_tail: Option<AccumulatedOffsetHistoryHandle>,
}
impl OuterContainerState {
    pub(crate) fn root() -> Self {
        Self {
            is_material_property: false,
            name_ref: None,
            offset_history_tail: None,
        }
    }
}

pub(crate) struct GlobalInputCollected<'a> {
    pub input_vars: Vec<InputVariable<'a>>,
    pub object_parameters_var: Option<EngineInputVariable>,
    pub camera_parameters_var: Option<EngineInputVariable>,
}
impl<'a> GlobalInputCollected<'a> {
    pub(crate) fn new() -> Self {
        Self {
            input_vars: Vec::new(),
            object_parameters_var: None,
            camera_parameters_var: None,
        }
    }
}

pub(crate) fn visit_global_input<'a>(
    v: &'a slang::reflection::VariableLayout,
    indent: usize,
    global_session: &impl slang::IGlobalSession,
    outer_container: &OuterContainerState,
    ctx: &mut VisitContext,
    collected: &mut GlobalInputCollected<'a>,
) {
    let is_material_property = outer_container.is_material_property
        || variable_is_material_property_block(v, global_session);
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
        .is_some_and(|x| x == OBJECT_PARAMETERS_FULL_TYPE_NAME);
    let is_engine_camera_parameters = constant_buffer_content_full_type_name
        .as_ref()
        .is_some_and(|x| x == CAMERA_PARAMETERS_FULL_TYPE_NAME);
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
    let property_name_override = variable_property_name_override(v, global_session);

    let ty = v.type_layout();
    println!("{}g param: {:?}", "  ".repeat(indent), v.name());
    dump_ty_layout(ty, indent + 1);
    dump_vl_space_offset_forall_category(v, &"  ".repeat(indent + 1));
    dump_user_attrs(v, &"  ".repeat(indent + 1));

    let var_name_ref = ctx.var_name_pool.add(VarName {
        name: v.name().map(ToOwned::to_owned),
        parent: outer_container.name_ref,
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
                parent: outer_container.offset_history_tail,
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
                property_name_override: property_name_override.map(ToOwned::to_owned),
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

    let container_state = OuterContainerState {
        is_material_property,
        name_ref: Some(var_name_ref),
        offset_history_tail: Some(accumulated_offset_history_self),
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
                visit_global_input(
                    f,
                    indent + 1,
                    global_session,
                    &container_state,
                    ctx,
                    collected,
                );
            }
        }
        slang::reflection::TypeKind::ParameterBlock
        | slang::reflection::TypeKind::ConstantBuffer
        | slang::reflection::TypeKind::ShaderStorageBuffer => {
            visit_global_input(
                ty.content_var_layout().expect("no element var"),
                indent + 1,
                global_session,
                &container_state,
                ctx,
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

fn variable_is_material_property_block(
    v: &slang::reflection::VariableLayout,
    session: &impl slang::IGlobalSession,
) -> bool {
    v.variable()
        .and_then(|v| v.find_user_attribute_by_name(session, MATERIAL_PROPERTY_ATTR_NAME))
        .is_some()
}

fn variable_property_name_override<'a>(
    v: &'a slang::reflection::VariableLayout,
    session: &impl slang::IGlobalSession,
) -> Option<&'a core::ffi::CStr> {
    let Some(var) = v.variable() else {
        tracing::warn!("variable layout does not have the variable");
        return None;
    };
    // none expected(no override specified by the user)
    let attr = var.find_user_attribute_by_name(session, PROPERTY_NAME_ATTR_NAME)?;

    if attr.argument_count() != 1 {
        tracing::warn!(
            actual_count = attr.argument_count(),
            "argument count mismatch"
        );
    }

    match attr.argument_value_string(0) {
        Some(x) => Some(x),
        None => {
            tracing::warn!("missing argument");
            None
        }
    }
}

#[derive(Debug)]
pub(crate) struct VarPlacement {
    pub descriptor_binding: usize,
    pub descriptor_set: usize,
    pub byte_offset: usize,
}
impl VarPlacement {
    const ZERO: Self = Self {
        descriptor_binding: 0,
        descriptor_set: 0,
        byte_offset: 0,
    };
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

struct VarName {
    name: Option<CString>,
    parent: Option<VarNameRef>,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub(crate) struct VarNameRef(NonZeroUsize);
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

pub(crate) struct VarNamePool(Vec<VarName>);
impl VarNamePool {
    #[inline(always)]
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    #[inline(always)]
    fn add(&mut self, name: VarName) -> VarNameRef {
        self.0.push(name);
        VarNameRef::from_array_index(self.0.len() - 1)
    }

    #[inline(always)]
    fn get(&self, r: VarNameRef) -> &VarName {
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
struct ParameterCategoryValues {
    space: usize,
    offset: usize,
}

#[derive(Debug)]
struct AccumulatedOffsetHistoryEntry {
    values: HashMap<slang::reflection::ParameterCategory, ParameterCategoryValues>,
    container_sub_element_register_space_size: usize,
    is_container: bool,
    parent: Option<AccumulatedOffsetHistoryHandle>,
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
struct AccumulatedOffsetHistoryHandle(NonZeroUsize);
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

pub(crate) struct AccumulatedOffsetHistoryArena {
    entries: Vec<AccumulatedOffsetHistoryEntry>,
}
impl AccumulatedOffsetHistoryArena {
    pub(crate) fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    fn create(&mut self, e: AccumulatedOffsetHistoryEntry) -> AccumulatedOffsetHistoryHandle {
        self.entries.push(e);
        AccumulatedOffsetHistoryHandle::from_array_index(self.entries.len() - 1)
    }

    fn get(&self, handle: AccumulatedOffsetHistoryHandle) -> &AccumulatedOffsetHistoryEntry {
        &self.entries[handle.into_array_index()]
    }
}

pub(crate) struct InputVariable<'a> {
    pub path: VarNameRef,
    pub var: &'a slang::reflection::VariableLayout,
    pub r#type: &'a slang::reflection::TypeLayout,
    pub category: slang::reflection::ParameterCategory,
    pub placement: VarPlacement,
    pub size: usize,
    pub is_material_property: bool,
    pub property_name_override: Option<CString>,
}

pub(crate) struct EngineInputVariable {
    pub placement: VarPlacement,
}

enum InputVarType {
    Unknown,
    MaterialProperty,
    EngineObjectParameters,
    EngineCameraParameters,
}
