use std::{collections::HashMap, ffi::CString};

use rendering_configuration::{
    CompiledRenderingConfigurationVk, ShadingPassVk,
    codegen::RenderingConfiguration,
    syntax::{ParserState, ToplevelElement},
    tokenizer,
};
use slang::{IBlob, IComponentType, IGlobalSession, IModule, ISession, IUnknown};

fn main() {
    let content = std::fs::read_to_string(&std::env::args_os().nth(1).expect("missing arg"))
        .expect("failed to read input");

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

    let mut asset = CompiledRenderingConfigurationVk {
        property_mappings: HashMap::new(),
        passes: HashMap::new(),
    };
    let (prelude, property_mapping) = rc.gen_vk_prelude();
    eprintln!("property mapping: {property_mapping:#?}");
    asset.property_mappings.extend(property_mapping);
    for (n, p) in rc.passes {
        if p.shader_code.is_none() && p.vertex_bindings.is_empty() {
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
        eprintln!("semantic -> vertex location: {semantic_to_location:#?}");
        eprintln!("gencode: {prelude}\n{code}");

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
        if let Some(d) = unsafe { diag.assume_init_ref() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag: {d}");
            }
        }

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
        if let Some(d) = unsafe { diag.assume_init_ref() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag[program]: {d}");
            }
        }
        let program = program.expect("session.create_composite_component_type failed");

        let mut diag = core::mem::MaybeUninit::new(None);
        let layout = program.get_layout(0, Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init_ref() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag[reflection]: {d}");
            }
        }
        for p in layout.iter_parameter() {
            let ty = p.r#type();
            let fn_blob = ty.full_name().expect("ty.full_name failed");

            println!("param {:?}", unsafe {
                core::ffi::CStr::from_ptr(fn_blob.get_buffer_pointer() as _)
            });
        }

        let mut diag = core::mem::MaybeUninit::new(None);
        let linked = program.link(Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init_ref() } {
            let ds = unsafe { core::ffi::CStr::from_ptr(d.get_buffer_pointer() as _) };
            for d in ds.to_string_lossy().lines() {
                eprintln!("diag[link]: {d}");
            }
        }
        let linked = linked.expect("program.link failed");

        let mut diag = core::mem::MaybeUninit::new(None);
        let code = linked.get_target_code(0, Some(&mut diag));
        if let Some(d) = unsafe { diag.assume_init_ref() } {
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

        asset.passes.insert(
            n,
            ShadingPassVk::Custom {
                vertex_semantic_to_location: semantic_to_location,
                code: aligned_code,
            },
        );
    }

    let mut total_time = core::mem::MaybeUninit::uninit();
    let mut downstream_time = core::mem::MaybeUninit::uninit();
    slang_session.get_compiler_elapsed_time(&mut total_time, &mut downstream_time);
    println!(
        "compilation done! total={} downstream={}",
        unsafe { total_time.assume_init() },
        unsafe { downstream_time.assume_init() }
    );

    let mut o = std::fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open("out.prcc")
        .expect("Failed to open write file");
    let writes = rendering_configuration::write(&mut o, asset).expect("failed to write asset");
    println!("asset write {writes} bytes");
}
