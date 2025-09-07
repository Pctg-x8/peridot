use pbxproj::{Decodable, PBXObject, PBXProjectFile, ParserState, Value, parse_value};

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).expect("no args"))
        .expect("Failed to load file");
    let mut ps = ParserState::new(&src);
    ps.skip_spaces();
    let v = parse_value(&mut ps).unwrap();
    let mut pbxproj = PBXProjectFile::decode(v).expect("Failed to parse pbxproj");
    eprintln!("pbxproj: {pbxproj:#?}");
    let root_project = pbxproj.root_project();
    eprintln!("rootObject: {root_project:#?}");
    let main_group = root_project
        .main_group
        .entity(&pbxproj)
        .expect("invalid mainGroup");
    eprintln!("main group: {main_group:#?}");
    for c in main_group.children.iter() {
        eprintln!("main: {:#?}", pbxproj.object_ref(c));
    }
    eprintln!(
        "product ref group: {:#?}",
        root_project.product_ref_group.entity(&pbxproj)
    );
    let build_configuration_list = root_project
        .build_configuration_list
        .entity(&pbxproj)
        .expect("invalid buildConfiguration");
    eprintln!("build configuration list: {build_configuration_list:#?}");
    for c in build_configuration_list.build_configurations.iter() {
        eprintln!("build cfg: {:#?}", c.entity(&pbxproj));
    }
    for c in root_project.targets.clone().into_iter() {
        eprintln!("target: {:#?}", c.entity(&pbxproj));
        match c.entity(&pbxproj).expect("no target object found") {
            PBXObject::NativeTarget(nt) => {
                let build_cfg_list = nt
                    .build_configuration_list
                    .entity(&pbxproj)
                    .expect("invalid buildConfigurationList");
                eprintln!("target build cfg list: {build_cfg_list:#?}");

                for c in build_cfg_list.build_configurations.clone().into_iter() {
                    let cfg = c.entity(&pbxproj).expect("invalid buildConfiguration");
                    eprintln!("target build cfg: {cfg:#?}");

                    c.entity_mut(&mut pbxproj)
                        .expect("invalid buildConfiguration")
                        .build_settings
                        .insert(
                            "VULKAN_SDK",
                            Value::Single("/home/pctgx8/VulkanSDK/1.3.283.0/macOS".into()),
                        );
                }
            }
            _ => (),
        }
    }

    let mut serialized = std::io::Cursor::new(Vec::with_capacity(src.len()));
    let p1 = pbxproj.clone();
    pbxproj
        .encode()
        .write_oneline(&mut serialized)
        .expect("Failed to re-serialize pbxproj");
    let serialized = unsafe { String::from_utf8_unchecked(serialized.into_inner()) };
    println!("{serialized}");
    let mut ps = ParserState::new(&serialized);
    ps.skip_spaces();
    let v = parse_value(&mut ps).unwrap();
    let pbxproj2 = PBXProjectFile::decode(v).expect("Failed to parse pbxproj");
    for (o, v) in p1.objects.iter() {
        match pbxproj2.objects.get(o) {
            None => {
                eprintln!("not found in pbxproj2: {o}");
            }
            Some(x) if x != v => {
                eprintln!("mismatch: {o}");
                eprintln!("  {x:?}");
                eprintln!("  {v:?}");
                match (x, v) {
                    (PBXObject::BuildConfiguration(x), PBXObject::BuildConfiguration(v)) => {
                        for (o, v) in v.build_settings.iter() {
                            match x.build_settings.get(o) {
                                None => eprintln!("missing build_settings: {o}"),
                                Some(x) if x != v => {
                                    eprintln!("    mismatch {o}: {x:?} {v:?}");
                                }
                                _ => (),
                            }
                        }
                    }
                    _ => (),
                }
            }
            _ => (),
        }
    }
    /*parse_object(&mut ps, |key, st| {
        println!("object entry {key:?}");

        match key {
            "archiveVersion" => {
                println!("archive version: {}", parse_single_val(st)?);
            }
            "classes" => {
                parse_object(st, |key, _| unimplemented!("unknown classes key: {key:?}"))?;
                println!("classes");
            }
            "objectVersion" => {
                println!("object version: {}", parse_single_val(st)?);
            }
            "objects" => parse_object(st, |key, st| {
                println!("object key: {key}");

                let mut object_entries = HashMap::new();
                parse_object(st, |key, st| match object_entries.entry(key) {
                    std::collections::hash_map::Entry::Vacant(v) => {
                        v.insert(parse_single_val(st)?);
                        Ok(())
                    }
                    std::collections::hash_map::Entry::Occupied(o) => {
                        panic!("conflicting entry: {}", o.key());
                    }
                })?;

                println!("object entry: {object_entries:?}");
                Ok(())
            })?,
            _ => unreachable!("unknown key"),
        }

        Ok(())

        // match ValueStarting::determine(st) {
        //     ValueStarting::Array => parse_array(st, |st| {
        //         unimplemented!("Array");
        //     }),
        //     ValueStarting::Object => parse_object(st, |key, st| {
        //         unimplemented!("nested object entry: {key:?}");
        //     }),
        //     ValueStarting::AnyVal => {
        //         let v = parse_single_val(st)?;
        //         println!("val: {v:?}");
        //         Ok(())
        //     }
        // }
    })
    .unwrap();*/
}
