use std::{collections::HashMap, ffi::CString};

mod codegen;
mod syntax;
mod tokenizer;

use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession, IUnknown};

use crate::{
    Code, CompiledRenderingConfigurationVk, DescriptorTypeVk, InstancingSupport, ShadingPassVk,
    VariantKey,
    compilation::{
        codegen::RenderingConfiguration,
        syntax::{ParserState, ToplevelElement},
    },
};

#[tracing::instrument(skip(src))]
pub fn compile(src: &str) -> Option<CompiledRenderingConfigurationVk> {
    let ctx = tokenizer::Context::new(src);
    let mut state = ParserState::new(ctx);
    let mut toplevel_elements = Vec::new();
    while !state.is_finished() {
        match ToplevelElement::parse(&mut state) {
            Ok(Some(top)) => toplevel_elements.push(top),
            Ok(None) => break,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to parse rendering configuration source");
                return None;
            }
        }
    }

    let rc = RenderingConfiguration::new(toplevel_elements);

    let slang_session = match slang::create_global_session(&slang::GlobalSessionDesc {
        ..Default::default()
    }) {
        Ok(x) => x,
        Err(e) => {
            tracing::error!(reason = ?e, "Failed to create libslang global session");
            return None;
        }
    };
    let targets = [slang::TargetDesc {
        format: slang::ffi::SLANG_SPIRV,
        profile: slang_session.find_profile(c"glsl_450"),
        ..Default::default()
    }];

    let mut asset = CompiledRenderingConfigurationVk {
        property_mappings: HashMap::new(),
        passes: HashMap::new(),
    };
    let mut has_failure = false;
    for (n, p) in rc.passes.iter() {
        let tracing_span = tracing::span!(tracing::Level::TRACE, "compile_pass", name = %n);
        let _tracing_span_enter = tracing_span.enter();

        if p.shader_code.is_none() && p.option_overrides.is_none() {
            // simple derive
            let deriving = p
                .deriving
                .clone()
                .expect("no deriving specified (completely empty?)");
            asset.passes.insert(
                n.clone(),
                ShadingPassVk::SimpleDeriveBuiltinPass { name: deriving },
            );

            continue;
        }

        let mut target_variants = Vec::new();
        match p.instancing_support {
            InstancingSupport::None => target_variants.push(VariantKey { instancing: false }),
            InstancingSupport::Allowed => target_variants.extend([
                VariantKey { instancing: false },
                VariantKey { instancing: true },
            ]),
            InstancingSupport::Only => target_variants.push(VariantKey { instancing: true }),
        };

        let mut variants = HashMap::with_capacity(target_variants.len());
        for v in target_variants {
            let (prelude, property_mapping, descriptor_set_bindings) =
                rc.gen_vk_prelude(v.instancing);
            asset.property_mappings = property_mapping;
            let mut descriptor_set_bindings = descriptor_set_bindings;

            let code = p.gen_vk_code();
            let generated_code = format!("{prelude}\n{code}");

            #[cfg(feature = "debug-dumps")]
            println!("{generated_code}");

            let session = match slang_session.create_session(&slang::SessionDesc {
                targets: targets.as_ptr(),
                target_count: targets.len() as _,
                ..Default::default()
            }) {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to create slang local session");
                    has_failure = true;
                    continue;
                }
            };

            let mut diag = core::mem::MaybeUninit::new(None);
            let module = session.load_module_from_source_string(
                c"main",
                c"main",
                &CString::new(generated_code).expect("invalid code generated"),
                Some(&mut diag),
            );
            if let Some(d) = unsafe { diag.assume_init() } {
                print_slang_diag(&d);
            }
            let Some(module) = module else {
                tracing::error!("Failed to load generated slang module");
                has_failure = true;
                continue;
            };

            let mut program_components = Vec::<slang::IComponentTypePtr>::with_capacity(
                1 + module.get_defined_entry_point_count() as usize,
            );
            program_components.push(match module.clone_cast() {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to cast module to IComponentType");
                    has_failure = true;
                    continue;
                }
            });
            program_components.extend(
                module.iter_defined_entry_point().filter_map(|e| {
                    e.inspect_err(|e| {
                        tracing::error!(reason = ?e, "Failed to iterate entry points");
                        has_failure = true;
                    }).ok()?.clone_cast().inspect_err(|e| {
                        tracing::error!(reason = ?e, "Failed to cast entry point object to IComponentType");
                        has_failure = true;
                    }).ok()
                })
            );
            let mut diag = core::mem::MaybeUninit::new(None);
            let program =
                session.create_composite_component_type(&program_components, Some(&mut diag));
            if let Some(d) = unsafe { diag.assume_init() } {
                print_slang_diag(&d);
            }
            let program = match program {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to composite components");
                    has_failure = true;
                    continue;
                }
            };

            let mut diag = core::mem::MaybeUninit::new(None);
            let linked = program.link(Some(&mut diag));
            if let Some(d) = unsafe { diag.assume_init() } {
                print_slang_diag(&d);
            }
            let linked = match linked {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to link program");
                    has_failure = true;
                    continue;
                }
            };

            let mut diag = core::mem::MaybeUninit::new(None);
            let code = linked.get_target_code(0, Some(&mut diag));
            if let Some(d) = unsafe { diag.assume_init() } {
                print_slang_diag(&d);
            }
            let code = match code {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to get target code");
                    has_failure = true;
                    continue;
                }
            };

            assert_eq!(code.get_buffer_size() & 0x03, 0, "not a valid spirv stream");
            let mut aligned_code = Vec::with_capacity(code.get_buffer_size() >> 2);
            unsafe {
                core::ptr::copy_nonoverlapping(
                    code.get_buffer_pointer() as *const u8,
                    aligned_code.spare_capacity_mut().as_mut_ptr() as *mut u8,
                    code.get_buffer_size(),
                );
                aligned_code.set_len(aligned_code.capacity());
            }

            #[cfg(feature = "debug-dumps")]
            dump_spv_disasm(&aligned_code);

            let refl = program.get_layout(0, None);
            let push_constant_buffer_size_bytes = if let Some(t) =
                refl.find_type_by_name(c"Peridot.MaterialParameters.PerDrawCall")
            {
                let tl = refl
                    .type_layout(t, slang::ffi::SLANG_LAYOUT_RULES_DEFAULT)
                    .expect("no type layout for uniform block");

                tl.size(slang::reflection::ParameterCategory::PushConstantBuffer)
            } else {
                0
            };
            if let Some(t) =
                refl.find_type_by_name(c"Peridot.MaterialParameters.UniformPropertyBlock")
            {
                let tl = refl
                    .type_layout(t, slang::ffi::SLANG_LAYOUT_RULES_DEFAULT)
                    .expect("no type layout for uniform property block");

                descriptor_set_bindings.push(DescriptorTypeVk::UniformBuffer {
                    size_bytes: tl.size(slang::reflection::ParameterCategory::Uniform),
                });
            }
            if let Some(t) =
                refl.find_type_by_name(c"Peridot.MaterialParameters.InstancedPropertyBlock")
            {
                let tl = refl
                    .type_layout(t, slang::ffi::SLANG_LAYOUT_RULES_DEFAULT)
                    .expect("no type layout for uniform property block");

                descriptor_set_bindings.push(DescriptorTypeVk::StorageBuffer {
                    size_bytes: tl.size(slang::reflection::ParameterCategory::Uniform),
                });
            }
            if let Some(t) = refl.find_type_by_name(c"Peridot.MaterialParameters.RealtimeBuffer") {
                let tl = refl
                    .type_layout(t, slang::ffi::SLANG_LAYOUT_RULES_DEFAULT)
                    .expect("no type layout for realtime buffer");

                descriptor_set_bindings.push(DescriptorTypeVk::UniformBuffer {
                    size_bytes: tl.size(slang::reflection::ParameterCategory::Uniform),
                });
            }
            let mut vertex_semantic_to_location = HashMap::new();
            let mut vertex_entry_point_name = None;
            let mut fragment_entry_point_name = None;
            for ep in refl.iter_entry_point() {
                let stage = ep.stage();

                if stage == slang::ffi::SLANG_STAGE_VERTEX {
                    if let Some(ref x) = vertex_entry_point_name {
                        tracing::error!(before = x, "conflicting entry point for vertex stage");
                        has_failure = true;
                    } else {
                        vertex_entry_point_name =
                            Some(ep.name().to_str().expect("invalid entry name").into());

                        println!("vertex inputs");
                        for x in ep.iter_parameter() {
                            let is_vertex_input = x
                                .variable()
                                .iter_user_attribute()
                                .any(|a| a.name() == c"Peridot_VertexInput")
                                || x.r#type()
                                    .iter_user_attribute()
                                    .any(|a| a.name() == c"Peridot_VertexInput");
                            if !is_vertex_input {
                                continue;
                            }

                            rec(x, &mut vertex_semantic_to_location, &mut has_failure);
                            fn rec(
                                v: &slang::reflection::VariableLayout,
                                vertex_semantic_to_location: &mut HashMap<
                                    peridot_semantic_shader::VertexInputSemantic,
                                    u32,
                                >,
                                has_failure: &mut bool,
                            ) {
                                let tl = v.type_layout();

                                if tl.kind() == slang::reflection::TypeKind::Struct {
                                    // process each fields recursively
                                    for m in tl.iter_field() {
                                        rec(m, vertex_semantic_to_location, has_failure);
                                    }

                                    return;
                                }

                                let Some(semantic_name) = v.semantic_name() else {
                                    tracing::error!(var_name = ?v.name(), "vertex input variables should have semantic");
                                    *has_failure = true;
                                    return;
                                };
                                let semantic_name = match semantic_name.to_str() {
                                    Ok(x) => x,
                                    Err(e) => {
                                        tracing::error!(reason = ?e, var_name = ?v.name(), ?semantic_name, "invalid semantic_name bytes");
                                        *has_failure = true;
                                        return;
                                    }
                                };

                                let semantic = if semantic_name.eq_ignore_ascii_case("position") {
                                    peridot_semantic_shader::VertexInputSemantic::Position(
                                        v.semantic_index() as _,
                                    )
                                } else if semantic_name.eq_ignore_ascii_case("normal") {
                                    peridot_semantic_shader::VertexInputSemantic::Normal(
                                        v.semantic_index() as _,
                                    )
                                } else if semantic_name.eq_ignore_ascii_case("tangent") {
                                    peridot_semantic_shader::VertexInputSemantic::Tangent(
                                        v.semantic_index() as _,
                                    )
                                } else if semantic_name.eq_ignore_ascii_case("binormal") {
                                    peridot_semantic_shader::VertexInputSemantic::Binormal(
                                        v.semantic_index() as _,
                                    )
                                } else if semantic_name.eq_ignore_ascii_case("texcoord") {
                                    peridot_semantic_shader::VertexInputSemantic::Texcoord(
                                        v.semantic_index() as _,
                                    )
                                } else if semantic_name.eq_ignore_ascii_case("color") {
                                    peridot_semantic_shader::VertexInputSemantic::Color(
                                        v.semantic_index() as _,
                                    )
                                } else if semantic_name.eq_ignore_ascii_case("misc") {
                                    peridot_semantic_shader::VertexInputSemantic::Misc(
                                        v.semantic_index() as _,
                                    )
                                } else {
                                    tracing::warn!(
                                        var_name = ?v.name(),
                                        semantic_name,
                                        "unsupported semantic name, skipping"
                                    );
                                    return;
                                };

                                vertex_semantic_to_location
                                    .insert(semantic, v.binding_index() as _);
                            }
                        }
                    }
                } else if stage == slang::ffi::SLANG_STAGE_FRAGMENT {
                    if let Some(ref x) = fragment_entry_point_name {
                        tracing::error!(before = x, "conflicting entry point for fragment stage");
                        has_failure = true;
                    } else {
                        fragment_entry_point_name =
                            Some(ep.name().to_str().expect("invalid entry name").into());
                    }
                } else {
                    tracing::warn!(stage, "unimplemented entry point stage");
                }
            }

            // TODO: slang v2025.17だとISession由来のオブジェクトをreleaseするとISession::releaseでおちるので、他オブジェクトはあえてreleaseしない(どうせSessionが消えたらこれらも消えるはず)
            // ただ解放するのが正解だとはおもう......(slang側のバグのようにみえるが、詳細なドキュメントがないので不明)
            core::mem::forget(linked);
            core::mem::forget(program);
            core::mem::forget(module);

            variants.insert(
                v,
                Code {
                    push_constant_buffer_size_bytes,
                    descriptor_set_bindings,
                    vertex_semantic_to_location,
                    vertex_entry_point_name,
                    fragment_entry_point_name,
                    words: aligned_code,
                },
            );
        }

        asset.passes.insert(
            n.clone(),
            ShadingPassVk::Custom {
                option_overrides: p.option_overrides.clone().unwrap_or_default(),
                variants,
            },
        );
    }

    (!has_failure).then_some(asset)
}

fn print_slang_diag(diag: &(impl slang::IBlob + ?Sized)) {
    let str = unsafe { core::ffi::CStr::from_ptr(diag.get_buffer_pointer().cast()) };
    match str.to_str() {
        Err(x) => {
            tracing::warn!(target: "libslang diag", to_str_err = ?x, msg = ?str);
        }
        Ok(x) => {
            for l in x.lines() {
                eprintln!("[libslang] {l}");
            }
        }
    }
}

#[cfg(feature = "debug-dumps")]
fn dump_spv_disasm(code: &[u32]) {
    let st_context = spirv_tools::Context::new(spirv_tools::ffi::SPV_ENV_VULKAN_1_4);
    let text = st_context
        .binary_to_text(
            code,
            spirv_tools::ffi::SPV_BINARY_TO_TEXT_OPTION_FRIENDLY_NAMES,
            None,
        )
        .expect("spvBinaryToText");
    let cstr = text.as_cstr();
    match cstr.to_str() {
        Err(_) => {
            tracing::warn!(target: "spirv-tools disasm", code = ?cstr);
        }
        Ok(x) => {
            for l in x.lines() {
                eprintln!("[disasm] {l}");
            }
        }
    }
}
