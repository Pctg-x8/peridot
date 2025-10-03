use std::{collections::HashMap, ffi::CString, path::PathBuf};

mod codegen;
mod syntax;
mod tokenizer;

use clap::Parser;
use peridot_rendering_configuration as prc;
use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession, IUnknown};

use crate::{
    codegen::RenderingConfiguration,
    syntax::{ParserState, ToplevelElement},
};

#[derive(Parser)]
pub struct App {
    input: PathBuf,
    #[arg(long, short = 'o')]
    output: Option<PathBuf>,
}

fn main() {
    let args = App::parse();

    let content = std::fs::read_to_string(&args.input).expect("failed to read input");

    let ctx = tokenizer::Context::new(&content);
    let mut state = ParserState::new(ctx);
    let mut toplevel_elements = Vec::new();
    while !state.is_finished() {
        let Some(top) = ToplevelElement::parse(&mut state) else {
            break;
        };

        toplevel_elements.push(top);
    }

    let rc = RenderingConfiguration::new(toplevel_elements);

    let slang_session = slang::create_global_session(&slang::GlobalSessionDesc {
        ..Default::default()
    })
    .expect("slang::create_global_session failed");
    let targets = [slang::TargetDesc {
        format: slang::ffi::SLANG_SPIRV,
        profile: slang_session.find_profile(c"glsl_450"),
        ..Default::default()
    }];

    let mut asset = prc::CompiledRenderingConfigurationVk {
        property_mappings: HashMap::new(),
        descriptor_set_bindings: Vec::new(),
        push_constant_buffer_size_bytes: 0,
        passes: HashMap::new(),
    };
    let (prelude, property_mapping, descriptor_set_bindings) = rc.gen_vk_prelude();
    asset.property_mappings = property_mapping;
    asset.descriptor_set_bindings = descriptor_set_bindings;
    for (n, p) in rc.passes {
        if p.shader_code.is_none() && p.vertex_bindings.is_empty() && p.option_overrides.is_none() {
            // simple derive
            let deriving = p
                .deriving
                .expect("no deriving specified (completely empty?)");
            asset.passes.insert(
                n,
                prc::ShadingPassVk::SimpleDeriveBuiltinPass { name: deriving },
            );
            continue;
        }

        let (code, semantic_to_location) = p.gen_vk_code();

        let session = slang_session
            .create_session(&slang::SessionDesc {
                targets: targets.as_ptr(),
                target_count: targets.len() as _,
                ..Default::default()
            })
            .expect("slang_session.create_session failed");
        let mut diag = core::mem::MaybeUninit::new(None);
        let module = session.load_module_from_source_string(
            c"main",
            c"main",
            &CString::new(format!("{prelude}\n{code}")).expect("invalid code generated"),
            Some(&mut diag),
        );
        if let Some(d) = unsafe { diag.assume_init() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag: {d}");
            }
        }
        let module = module.expect("session.load_module_from_source_string failed");

        let mut program_components = Vec::<slang::IComponentTypePtr>::with_capacity(
            1 + module.get_defined_entry_point_count() as usize,
        );
        program_components.push(
            module
                .clone_cast()
                .expect("module.clone_cast to IComponentType failed"),
        );
        program_components.extend(module.iter_defined_entry_point().map(|e| {
            e.expect("module.get_defined_entry_point failed")
                .clone_cast()
                .expect("entry_point.clone_cast to IComponentType failed")
        }));
        let mut diag = core::mem::MaybeUninit::new(None);
        let program = session.create_composite_component_type(&program_components, Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag[program]: {d}");
            }
        }
        let program = program.expect("session.create_composite_component_type failed");

        let mut diag = core::mem::MaybeUninit::new(None);
        let linked = program.link(Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag[link]: {d}");
            }
        }
        let linked = linked.expect("program.link failed");

        let mut diag = core::mem::MaybeUninit::new(None);
        let code = linked.get_target_code(0, Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag[codegen]: {d}");
            }
        }
        let code = code.expect("linked.get_target_code failed");

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
                .push(prc::DescriptorTypeVk::UniformBuffer {
                    size_bytes: tl.size(slang::reflection::ParameterCategory::Uniform),
                });
        }
        let mut vertex_entry_point_name = None::<String>;
        let mut fragment_entry_point_name = None::<String>;
        for ep in refl.iter_entry_point() {
            let stage = ep.stage();

            if stage == slang::ffi::SLANG_STAGE_VERTEX {
                if let Some(ref x) = vertex_entry_point_name {
                    panic!("conflicting entry point for vertex stage: {x:?}");
                }

                vertex_entry_point_name =
                    Some(ep.name().to_str().expect("invalid entry name").into());
            } else if stage == slang::ffi::SLANG_STAGE_FRAGMENT {
                if let Some(ref x) = fragment_entry_point_name {
                    panic!("conflicting entry point for fragment stage: {x:?}");
                }

                fragment_entry_point_name =
                    Some(ep.name().to_str().expect("invalid entry name").into());
            } else {
                eprintln!("warn: unimplemented entry point stage: {stage}");
            }
        }

        asset.passes.insert(
            n,
            prc::ShadingPassVk::Custom {
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

    let mut total_time = core::mem::MaybeUninit::uninit();
    let mut downstream_time = core::mem::MaybeUninit::uninit();
    slang_session.get_compiler_elapsed_time(&mut total_time, &mut downstream_time);
    println!(
        "{}: compilation done! total={} downstream={}",
        args.input.display(),
        unsafe { total_time.assume_init() },
        unsafe { downstream_time.assume_init() }
    );

    let opath = args
        .output
        .unwrap_or_else(|| args.input.with_extension("prcc"));
    let mut o = std::fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(opath)
        .expect("Failed to open write file");
    let writes = prc::write(&mut o, asset).expect("failed to write asset");
    println!("asset write {writes} bytes");
}
