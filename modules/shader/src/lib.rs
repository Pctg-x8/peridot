use std::{collections::HashMap, ffi::CString, path::Path};

use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession, IUnknown};

pub struct AssetCore;
impl peridot::LogicalAssetData for AssetCore {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SHADER;
}

pub struct Environment {
    pub peridot_stdlib_search_path: CString,
}

pub fn build(source_path: impl AsRef<Path>, env: &Environment) {
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

    let mut program_components = Vec::<slang::IComponentTypePtr>::with_capacity(
        1 + module.get_defined_entry_point_count() as usize,
    );
    program_components.push(module.clone_cast().expect("module.clone_cast"));
    program_components.extend(module.iter_defined_entry_point().map(|e| {
        e.expect("Faield to iterate entry points")
            .clone_cast()
            .expect("module.entry_point.cast")
    }));
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

    std::fs::write("generated.spv", unsafe {
        core::slice::from_raw_parts(
            code.get_buffer_pointer().cast::<u8>(),
            code.get_buffer_size(),
        )
    })
    .expect("generated.write");

    let mut diag = core::mem::MaybeUninit::new(None);
    let rl = linked.get_layout(0, Some(&mut diag));
    print_slang_diag(unsafe { diag.assume_init() });
    let mut vertex_inputs = Vec::new();
    for n in 0..rl.entry_point_count() {
        let Some(e) = rl.entry_point(n) else {
            continue;
        };

        if e.stage() == slang::ffi::SLANG_STAGE_VERTEX {
            // vertex shader func
            let vl = e.var_layout();
            let tl = vl.type_layout();
            assert_eq!(tl.kind(), slang::reflection::TypeKind::Struct);
            for n in 0..tl.field_count() {
                visit_vsh_input(tl.field(n).expect("no field"), 0, &mut vertex_inputs);
            }
        }
    }

    let mut input_variables = Vec::new();
    if let Some(gv) = rl.global_params_var_layout() {
        let tl = gv.type_layout();
        println!("global var: {:?}", tl.kind());
        if tl.kind() == slang::reflection::TypeKind::ConstantBuffer {
            let cvl = tl.container_var_layout().expect("no container var");
            println!(
                "cvl {} {} {:?} {} {}",
                cvl.binding_index(),
                cvl.binding_space(),
                cvl.category(),
                cvl.offset(cvl.category()),
                cvl.space(cvl.category())
            );
            let evl = tl.element_var_layout().expect("no element var");
            println!(
                "evl {} {} {:?} {} {}",
                evl.binding_index(),
                evl.binding_space(),
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
                    VisitContext {
                        per_category_accum_value: HashMap::new(),
                        incoming_variable_placement: AccumulatedCounter::ZERO,
                        parent_path: Vec::new(),
                        is_material_property: false,
                    },
                    &mut input_variables,
                );
            }
        } else {
            assert_eq!(tl.kind(), slang::reflection::TypeKind::Struct);
            for n in 0..tl.field_count() {
                visit_global_input(
                    tl.field(n).expect("no field"),
                    0,
                    &session,
                    VisitContext {
                        per_category_accum_value: HashMap::new(),
                        incoming_variable_placement: AccumulatedCounter::ZERO,
                        parent_path: Vec::new(),
                        is_material_property: false,
                    },
                    &mut input_variables,
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
    for v in input_variables.iter() {
        println!(
            "  {:?}: {:?}({:?}) at Descriptor(set={}, binding={})+{} size={} is_material_property={} propertyName={:?}",
            v.path,
            v.r#type.name(),
            v.category,
            v.placement.descriptor_set_base,
            v.placement.descriptor_binding_base,
            v.placement.offset_base,
            v.size,
            v.is_material_property,
            v.property_name_override
        );
    }
}

pub struct VertexInput {
    semantic_name: CString,
    semantic_index: usize,
    location: usize,
}

fn visit_vsh_input(
    v: &slang::reflection::VariableLayout,
    indent: usize,
    collected: &mut Vec<VertexInput>,
) {
    let ty = v.type_layout();
    println!(
        "{}vsh param: {:?}: {:?}",
        "  ".repeat(indent),
        v.name(),
        ty.name()
    );
    for n in 0..v.category_count() {
        let cat = v.category_by_index(n);

        println!(
            "{}- {cat:?}: set={} offset={}",
            "  ".repeat(indent),
            v.space(cat),
            v.offset(cat)
        );
    }

    if ty.kind() == slang::reflection::TypeKind::Struct {
        // recurse members
        for n in 0..ty.field_count() {
            visit_vsh_input(ty.field(n).expect("no field"), indent + 1, collected);
        }
        return;
    }

    for n in 0..v.variable().user_attribute_count() {
        let Some(a) = v.variable().user_attribute(n) else {
            continue;
        };

        println!("{}user attr: {:?}", "  ".repeat(indent + 1), a.name());

        if a.name() == c"peridot_vertex" {
            assert!(
                (0..v.category_count()).any(|n| v.category_by_index(n)
                    == slang::reflection::ParameterCategory::VaryingInput)
            );

            collected.push(VertexInput {
                semantic_name: v.semantic_name().expect("no semantic decorated").to_owned(),
                semantic_index: v.semantic_index(),
                location: v.offset(slang::reflection::ParameterCategory::VaryingInput),
            });

            println!(
                "{}vertex input: {:?} {} = {}",
                "  ".repeat(indent + 2),
                v.semantic_name(),
                v.semantic_index(),
                v.offset(slang::reflection::ParameterCategory::VaryingInput)
            );
        }
    }
}

#[derive(Clone, Debug)]
struct AccumulatedCounter {
    descriptor_binding_base: usize,
    descriptor_set_base: usize,
    offset_base: usize,
}
impl AccumulatedCounter {
    const ZERO: Self = Self {
        descriptor_binding_base: 0,
        descriptor_set_base: 0,
        offset_base: 0,
    };
}

#[derive(Clone, Debug)]
struct VisitContext {
    per_category_accum_value: HashMap<slang::reflection::ParameterCategory, AccumulatedCounter>,
    incoming_variable_placement: AccumulatedCounter,
    parent_path: Vec<Option<CString>>,
    is_material_property: bool,
}

pub struct InputVariable<'a> {
    path: Vec<Option<CString>>,
    r#type: &'a slang::reflection::TypeLayout,
    category: slang::reflection::ParameterCategory,
    placement: AccumulatedCounter,
    size: usize,
    is_material_property: bool,
    property_name_override: Option<CString>,
}

fn visit_global_input<'a>(
    v: &'a slang::reflection::VariableLayout,
    indent: usize,
    global_session: &impl slang::IGlobalSession,
    ctx: VisitContext,
    collected: &mut Vec<InputVariable<'a>>,
) {
    let is_material_property = ctx.is_material_property
        || v.variable()
            .find_user_attribute_by_name(global_session, c"peridot_material_property")
            .is_some();
    let property_name_override = v
        .variable()
        .find_user_attribute_by_name(global_session, c"peridot_property_name")
        .map(|x| {
            assert_eq!(x.argument_count(), 1);
            x.argument_value_string(0)
                .expect("missing argument")
                .to_owned()
        });
    let ty = v.type_layout();
    let variable_path = ctx
        .parent_path
        .iter()
        .cloned()
        .chain(core::iter::once(v.name().map(|x| x.to_owned())))
        .collect::<Vec<_>>();
    println!(
        "{}g param: {:?}: {:?}",
        "  ".repeat(indent),
        v.name(),
        ty.name(),
    );
    assert_eq!(v.category_count(), 1, "mixed category variable");
    let cat = v.category_by_index(0);

    assert_eq!(v.space(cat), 0);
    println!(
        "{}- {cat:?}: set={} offset={} size={}",
        "  ".repeat(indent),
        v.space(cat),
        v.offset(cat),
        ty.size(cat),
    );

    let mut variable_placement = ctx
        .per_category_accum_value
        .get(&cat)
        .unwrap_or(&ctx.incoming_variable_placement)
        .clone();
    match cat {
        slang::reflection::ParameterCategory::DescriptorTableSlot => {
            assert_eq!(v.space(cat), 0);
            variable_placement.descriptor_binding_base += v.offset(cat);
        }
        slang::reflection::ParameterCategory::SubElementRegisterSpace => {
            assert_eq!(v.space(cat), 0);
            variable_placement.descriptor_set_base += v.offset(cat);
        }
        slang::reflection::ParameterCategory::Uniform => {
            assert_eq!(v.space(cat), 0);
            variable_placement.offset_base += v.offset(cat);
        }
        _ => (),
    }

    println!(
        "{}- variable placed: {variable_placement:?}",
        "  ".repeat(indent + 1),
    );
    collected.push(InputVariable {
        path: variable_path.clone(),
        r#type: ty,
        category: cat,
        placement: variable_placement.clone(),
        size: ty.size(cat),
        is_material_property,
        property_name_override,
    });
    let mut ctx = VisitContext {
        parent_path: variable_path,
        incoming_variable_placement: variable_placement,
        is_material_property,
        per_category_accum_value: HashMap::new(),
    };
    ctx.per_category_accum_value
        .insert(cat, ctx.incoming_variable_placement.clone());

    if let Some(cvl) = ty.container_var_layout() {
        for n in 0..cvl.category_count() {
            let cat = cvl.category_by_index(n);

            assert_eq!(cvl.space(cat), 0);
            println!(
                "{}- cvl({:?}) {cat:?}: offset={}",
                "  ".repeat(indent),
                cvl.type_layout().kind(),
                cvl.offset(cat)
            );

            match cat {
                slang::reflection::ParameterCategory::DescriptorTableSlot => {
                    ctx.per_category_accum_value
                        .entry(cat)
                        .or_insert(ctx.incoming_variable_placement.clone())
                        .descriptor_binding_base += cvl.offset(cat);
                }
                slang::reflection::ParameterCategory::SubElementRegisterSpace => {
                    ctx.per_category_accum_value
                        .entry(cat)
                        .or_insert(ctx.incoming_variable_placement.clone())
                        .descriptor_set_base += cvl.offset(cat);
                }
                slang::reflection::ParameterCategory::Uniform => {
                    ctx.per_category_accum_value
                        .entry(cat)
                        .or_insert(ctx.incoming_variable_placement.clone())
                        .offset_base += cvl.offset(cat);
                }
                _ => (),
            }
        }
    }

    if let Some(cnvl) = ty.content_var_layout() {
        for n in 0..cnvl.category_count() {
            let cat = cnvl.category_by_index(n);

            assert_eq!(cnvl.space(cat), 0);
            println!(
                "{}- cnvl({:?}) {cat:?}: offset={}",
                "  ".repeat(indent),
                cnvl.type_layout().kind(),
                cnvl.offset(cat),
            );

            match cat {
                slang::reflection::ParameterCategory::DescriptorTableSlot => {
                    ctx.per_category_accum_value
                        .entry(cat)
                        .or_insert(ctx.incoming_variable_placement.clone())
                        .descriptor_binding_base += cnvl.offset(cat);
                }
                slang::reflection::ParameterCategory::SubElementRegisterSpace => {
                    ctx.per_category_accum_value
                        .entry(cat)
                        .or_insert(ctx.incoming_variable_placement.clone())
                        .descriptor_set_base += cnvl.offset(cat);
                }
                slang::reflection::ParameterCategory::Uniform => {
                    ctx.per_category_accum_value
                        .entry(cat)
                        .or_insert(ctx.incoming_variable_placement.clone())
                        .offset_base += cnvl.offset(cat);
                }
                _ => (),
            }
        }
    }

    if let Some(evl) = ty.element_var_layout() {
        for n in 0..evl.category_count() {
            let cat = evl.category_by_index(n);

            println!(
                "{}- evl({:?}) {cat:?}: set={} offset={}",
                "  ".repeat(indent),
                evl.type_layout().kind(),
                evl.space(cat),
                evl.offset(cat)
            );
        }
    }

    if ty.kind() == slang::reflection::TypeKind::Struct {
        // recurse members
        for n in 0..ty.field_count() {
            visit_global_input(
                ty.field(n).expect("no field"),
                indent + 1,
                global_session,
                ctx.clone(),
                collected,
            );
        }
        return;
    }

    if ty.kind() == slang::reflection::TypeKind::ParameterBlock
        || ty.kind() == slang::reflection::TypeKind::ConstantBuffer
    {
        let content_vl = ty.content_var_layout().expect("no element var");
        if content_vl.type_layout().kind() == slang::reflection::TypeKind::Struct {
            for n in 0..content_vl.type_layout().field_count() {
                visit_global_input(
                    content_vl.type_layout().field(n).expect("no field"),
                    indent + 1,
                    global_session,
                    ctx.clone(),
                    collected,
                );
            }
        }

        return;
    }

    for n in 0..v.variable().user_attribute_count() {
        let Some(a) = v.variable().user_attribute(n) else {
            continue;
        };

        println!("{}user attr: {:?}", "  ".repeat(indent + 1), a.name());

        if a.name() == c"peridot_material_property" {
            println!(
                "{}material property input: {}",
                "  ".repeat(indent + 2),
                a.argument_count(),
            );
        }
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
