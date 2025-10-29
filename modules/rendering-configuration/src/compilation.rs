use std::{collections::HashMap, ffi::CString};

mod codegen;
mod syntax;
mod tokenizer;

use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession, IUnknown};

use crate::{
    CompiledRenderingConfigurationVk, DescriptorTypeVk, ShadingPassVk,
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
        descriptor_set_bindings: Vec::new(),
        push_constant_buffer_size_bytes: 0,
        passes: HashMap::new(),
    };
    let (prelude, property_mapping, descriptor_set_bindings) = rc.gen_vk_prelude();
    asset.property_mappings = property_mapping;
    asset.descriptor_set_bindings = descriptor_set_bindings;
    let mut has_failure = false;
    for (n, p) in rc.passes {
        let tracing_span = tracing::span!(tracing::Level::TRACE, "compile_pass", name = %n);
        let _tracing_span_enter = tracing_span.enter();

        if p.shader_code.is_none() && p.vertex_bindings.is_empty() && p.option_overrides.is_none() {
            // simple derive
            let deriving = p
                .deriving
                .expect("no deriving specified (completely empty?)");
            asset
                .passes
                .insert(n, ShadingPassVk::SimpleDeriveBuiltinPass { name: deriving });

            continue;
        }

        let (code, semantic_to_location) = p.gen_vk_code();
        let generated_code = format!("{prelude}\n{code}");

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
            tracing::warn!(target: "libslang diag", msg = ?unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) });
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
        for e in module.iter_defined_entry_point() {
            let e = match e {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to iterate entry points");
                    has_failure = true;
                    continue;
                }
            };

            program_components.push(match e.clone_cast() {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to cast entry point object to IComponentType");
                    has_failure = true;
                    continue;
                }
            });
        }
        let mut diag = core::mem::MaybeUninit::new(None);
        let program = session.create_composite_component_type(&program_components, Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init() } {
            tracing::warn!(target: "libslang diag", msg = ?unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) });
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
            tracing::warn!(target: "libslang diag", msg = ?unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) });
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
            tracing::warn!(target: "libslang diag", msg = ?unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) });
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

        let refl = program.get_layout(0, None);
        if let Some(t) = refl.find_type_by_name(c"PeridotMaterialParameters.PerDrawCall") {
            let tl = refl
                .type_layout(t, slang::ffi::SLANG_LAYOUT_RULES_DEFAULT)
                .expect("no type layout for uniform block");
            asset.push_constant_buffer_size_bytes =
                tl.size(slang::reflection::ParameterCategory::PushConstantBuffer);
        }
        if let Some(t) = refl.find_type_by_name(c"PeridotMaterialParameters.RealtimeBuffer") {
            let tl = refl
                .type_layout(t, slang::ffi::SLANG_LAYOUT_RULES_DEFAULT)
                .expect("no type layout for realtime buffer");
            // Realtime Bufferのbindingは一番最後に生える(ようにcodegenではなってる)
            asset
                .descriptor_set_bindings
                .push(DescriptorTypeVk::UniformBuffer {
                    size_bytes: tl.size(slang::reflection::ParameterCategory::Uniform),
                });
        }
        let mut vertex_entry_point_name = None::<String>;
        let mut fragment_entry_point_name = None::<String>;
        for ep in refl.iter_entry_point() {
            let stage = ep.stage();

            if stage == slang::ffi::SLANG_STAGE_VERTEX {
                if let Some(ref x) = vertex_entry_point_name {
                    tracing::error!(before = x, "conflicting entry point for vertex stage");
                    has_failure = true;
                } else {
                    vertex_entry_point_name =
                        Some(ep.name().to_str().expect("invalid entry name").into());
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

        asset.passes.insert(
            n,
            ShadingPassVk::Custom {
                option_overrides: p.option_overrides.unwrap_or_default(),
                vertex_semantic_to_location: semantic_to_location,
                vertex_entry_point_name,
                fragment_entry_point_name,
                code: aligned_code,
            },
        );

        // TODO: slang v2025.17だとISession由来のオブジェクトをreleaseするとISession::releaseでおちるので、他オブジェクトはあえてreleaseしない(どうせSessionが消えたらこれらも消えるはず)
        // ただ解放するのが正解だとはおもう......(slang側のバグのようにみえるが、詳細なドキュメントがないので不明)
        core::mem::forget(linked);
        core::mem::forget(program);
        core::mem::forget(module);
    }

    (!has_failure).then_some(asset)
}
