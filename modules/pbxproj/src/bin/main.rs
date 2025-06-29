use std::collections::HashMap;

use pbxproj::{
    ParserState, Value, ValueStarting, parse_array, parse_object, parse_single_val, parse_value,
};

#[derive(Debug)]
pub struct PBXProjectFile<'s> {
    pub objects: HashMap<&'s str, PBXObject<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}

#[derive(Debug)]
pub enum PBXObject<'s> {
    BuildFile(PBXBuildFile<'s>),
    FileReference(PBXFileReference<'s>),
    FrameworksBuildPhase(PBXFrameworksBuildPhase<'s>),
    Group(PBXGroup<'s>),
    NativeTarget(PBXNativeTarget<'s>),
    Project(PBXProject<'s>),
    ReosurcesBuildPhase(PBXResourcesBuildPhase<'s>),
    SourcesBuildPhase(PBXSourcesBuildPhase<'s>),
    VariantGroup(PBXVariantGroup<'s>),
    BuildConfiguration(XCBuildConfiguration<'s>),
    ConfigurationList(XCConfigurationList<'s>),
    CopyFilesBuildPhase(PBXCopyFilesBuildPhase<'s>),
}

#[derive(Debug)]
pub struct PBXBuildFile<'s> {
    pub file_ref: &'s str,
    pub settings: Option<PBXBuildFileSettings<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXBuildFile<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let file_ref = match xs.remove("fileRef").expect("fileRef required") {
            Value::Single(x) => x,
            x => unreachable!("invalid PBXBuildFile.fileRef value: {x:?}"),
        };
        let settings = xs.remove("settings").map(PBXBuildFileSettings::decode);

        Self {
            file_ref,
            settings,
            extras: xs,
        }
    }
}

#[derive(Debug)]
pub struct PBXBuildFileSettings<'s> {
    pub attributes: Vec<&'s str>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXBuildFileSettings<'s> {
    pub fn decode(v: Value<'s>) -> Self {
        match v {
            Value::Map(mut xs) => {
                let attributes = match xs.remove("ATTRIBUTES") {
                    None => Vec::new(),
                    Some(Value::Array(xs)) => xs
                        .into_iter()
                        .map(|x| match x {
                            Value::Single(x) => x,
                            x => panic!("invalid PBXBuildFileSettings.ATTRIBUTE value: {x:?}"),
                        })
                        .collect::<Vec<_>>(),
                    Some(x) => panic!("invalid PBXBuildFileSettings.ATTRIBUTE: {x:?}"),
                };

                Self {
                    attributes,
                    extras: xs,
                }
            }
            x => unreachable!("invalid PBXBuildFileSettings: {x:?}"),
        }
    }
}

#[derive(Debug)]
pub struct PBXFileReference<'s> {
    pub last_known_file_type: Option<&'s str>,
    pub name: Option<&'s str>,
    pub path: &'s str,
    pub source_tree: &'s str,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXFileReference<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let last_known_file_type = match xs.remove("lastKnownFileType") {
            Some(Value::Single(x)) => Some(x),
            Some(x) => unreachable!("invalid PBXFileReference.lastKnownFileType value: {x:?}"),
            None => None,
        };
        let name = match xs.remove("name") {
            Some(Value::Single(x)) => Some(x),
            Some(x) => unreachable!("invalid PBXFileReference.name value: {x:?}"),
            None => None,
        };
        let path = match xs.remove("path") {
            Some(Value::Single(x)) => x,
            Some(x) => unreachable!("invalid PBXFileReference.path value: {x:?}"),
            None => unreachable!("PBXFileReference.path missing"),
        };
        let source_tree = match xs.remove("sourceTree") {
            Some(Value::Single(x)) => x,
            Some(x) => unreachable!("invalid PBXFileReference.sourceTree value: {x:?}"),
            None => unreachable!("PBXFileReference.sourceTree missing"),
        };

        Self {
            last_known_file_type,
            name,
            path,
            source_tree,
            extras: xs,
        }
    }
}

#[derive(Debug)]
pub struct PBXFrameworksBuildPhase<'s> {
    pub build_action_mask: u32,
    pub files: Vec<&'s str>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXFrameworksBuildPhase<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let build_action_mask = match xs.remove("buildActionMask") {
            Some(Value::Single(x)) => x.parse::<u32>().expect("cannot parse as u32"),
            Some(x) => unreachable!("invalid PBXFrameworksBuildPhase.buildActionMask: {x:?}"),
            None => unreachable!("missing PBXFrameworkBuildPhase.buildActionMask"),
        };
        let files = match xs.remove("files") {
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| match x {
                    Value::Single(x) => x,
                    x => unreachable!("invalid PBXFrameworksBuildPhase.files element: {x:?}"),
                })
                .collect::<Vec<_>>(),
            Some(x) => unreachable!("invalid PBXFrameworksBuildPhase.files: {x:?}"),
            None => panic!("missing PBXFrameworksBuildPhase.files"),
        };

        Self {
            build_action_mask,
            files,
            extras: xs,
        }
    }
}

#[derive(Debug)]
pub struct PBXGroup<'s> {
    pub children: Vec<&'s str>,
    pub name: Option<&'s str>,
    pub source_tree: &'s str,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXGroup<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let children = match xs.remove("children") {
            None => Vec::new(),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| match x {
                    Value::Single(x) => x,
                    x => unreachable!("invalid PBXGroup.children entry: {x:?}"),
                })
                .collect::<Vec<_>>(),
            Some(x) => panic!("invalid PBXGroup.children: {x:?}"),
        };
        let source_tree = match xs.remove("sourceTree") {
            Some(Value::Single(x)) => x,
            Some(x) => panic!("invalid PBXGroup.sourceTree value: {x:?}"),
            None => panic!("missing PBXGroup.sourceTree"),
        };
        let name = match xs.remove("name") {
            Some(Value::Single(x)) => Some(x),
            Some(x) => panic!("invalid PBXGroup.name value: {x:?}"),
            None => None,
        };

        Self {
            children,
            source_tree,
            name,
            extras: xs,
        }
    }
}

#[derive(Debug)]
pub struct PBXNativeTarget<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXNativeTarget<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct PBXProject<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXProject<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct PBXResourcesBuildPhase<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXResourcesBuildPhase<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct PBXSourcesBuildPhase<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXSourcesBuildPhase<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct PBXVariantGroup<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXVariantGroup<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct XCBuildConfiguration<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> XCBuildConfiguration<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct XCConfigurationList<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> XCConfigurationList<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

#[derive(Debug)]
pub struct PBXCopyFilesBuildPhase<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXCopyFilesBuildPhase<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).expect("no args"))
        .expect("Failed to load file");
    let mut ps = ParserState::new(&src);
    ps.skip_spaces();
    let v = parse_value(&mut ps).unwrap();
    let object = match v {
        Value::Map(mut xs) => {
            let objects = match xs.remove("objects").expect("no objects defined on root") {
                Value::Map(xs) => xs
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            k,
                            match v {
                                Value::Map(mut xs) => {
                                    let isa =
                                        xs.remove("isa").expect("cannot determine object type");
                                    match isa {
                                        Value::Single("PBXBuildFile") => {
                                            PBXObject::BuildFile(PBXBuildFile::decode(xs))
                                        }
                                        Value::Single("PBXFileReference") => {
                                            PBXObject::FileReference(PBXFileReference::decode(xs))
                                        }
                                        Value::Single("PBXFrameworksBuildPhase") => {
                                            PBXObject::FrameworksBuildPhase(
                                                PBXFrameworksBuildPhase::decode(xs),
                                            )
                                        }
                                        Value::Single("PBXGroup") => {
                                            PBXObject::Group(PBXGroup::decode(xs))
                                        }
                                        Value::Single("PBXNativeTarget") => {
                                            PBXObject::NativeTarget(PBXNativeTarget::decode(xs))
                                        }
                                        Value::Single("PBXProject") => {
                                            PBXObject::Project(PBXProject::decode(xs))
                                        }
                                        Value::Single("PBXResourcesBuildPhase") => {
                                            PBXObject::ReosurcesBuildPhase(
                                                PBXResourcesBuildPhase::decode(xs),
                                            )
                                        }
                                        Value::Single("PBXSourcesBuildPhase") => {
                                            PBXObject::SourcesBuildPhase(
                                                PBXSourcesBuildPhase::decode(xs),
                                            )
                                        }
                                        Value::Single("PBXVariantGroup") => {
                                            PBXObject::VariantGroup(PBXVariantGroup::decode(xs))
                                        }
                                        Value::Single("XCBuildConfiguration") => {
                                            PBXObject::BuildConfiguration(
                                                XCBuildConfiguration::decode(xs),
                                            )
                                        }
                                        Value::Single("XCConfigurationList") => {
                                            PBXObject::ConfigurationList(
                                                XCConfigurationList::decode(xs),
                                            )
                                        }
                                        Value::Single("PBXCopyFilesBuildPhase") => {
                                            PBXObject::CopyFilesBuildPhase(
                                                PBXCopyFilesBuildPhase::decode(xs),
                                            )
                                        }
                                        k => unreachable!("unknwon isa: {k:?}"),
                                    }
                                }
                                x => unreachable!("unknown {x:?}"),
                            },
                        )
                    })
                    .collect::<HashMap<_, _>>(),
                x => unreachable!("unknown {x:?}"),
            };

            PBXProjectFile {
                objects,
                extras: xs,
            }
        }
        x => unreachable!("invalid pbxproj root type: {x:?}"),
    };
    println!("pbxproj: {object:#?}");
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
