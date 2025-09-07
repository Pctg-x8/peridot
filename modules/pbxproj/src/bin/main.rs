use std::{borrow::Cow, collections::HashMap};

use pbxproj::{ParserState, Value, parse_value};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXProjectFile<'s> {
    pub objects: HashMap<&'s str, PBXObject<'s>>,
    pub root_object: PBXObjectIDRef<'s>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXProjectFile<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let objects = match xs.remove("objects").expect("no objects defined on root") {
            Value::Map(xs) => xs
                .into_iter()
                .map(|(k, v)| {
                    (
                        k,
                        match v {
                            Value::Map(xs) => {
                                PBXObject::decode(xs).expect("Failed decoding PBXObject")
                            }
                            x => unreachable!("unknown {x:?}"),
                        },
                    )
                })
                .collect::<HashMap<_, _>>(),
            x => unreachable!("unknown {x:?}"),
        };
        let root_object = PBXObjectIDRef::decode(
            xs.remove("rootObject")
                .expect("no rootObject defined on root"),
        )
        .expect("invalid rootObject value");

        Self {
            objects,
            root_object,
            extras: xs,
        }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(2 + self.extras.len());
        xs.insert(
            "objects",
            Value::Map(
                self.objects
                    .into_iter()
                    .map(|(k, v)| (k, v.encode()))
                    .collect(),
            ),
        );
        xs.insert("rootObject", self.root_object.encode());
        xs.extend(self.extras);

        Value::Map(xs)
    }

    #[inline(always)]
    pub fn object_ref(&self, id: &PBXObjectIDRef<'_>) -> Option<&PBXObject<'s>> {
        self.objects.get(&id.0 as &str)
    }

    #[inline(always)]
    pub fn object_ref_mut(&mut self, id: &PBXObjectIDRef<'_>) -> Option<&mut PBXObject<'s>> {
        self.objects.get_mut(&id.0 as &str)
    }

    #[inline(always)]
    pub fn object_ref_of<T>(&self, id: &PBXObjectIDRef<'_>) -> Option<&T>
    where
        T: PBXObjectType<'s>,
    {
        self.object_ref(id)?.downcast_ref()
    }

    #[inline(always)]
    pub fn object_ref_mut_of<T>(&mut self, id: &PBXObjectIDRef<'_>) -> Option<&mut T>
    where
        T: PBXObjectType<'s>,
    {
        self.object_ref_mut(id)?.downcast_ref_mut()
    }

    pub fn root_project(&self) -> &PBXProject<'s> {
        match self.object_ref(&self.root_object) {
            None => panic!(
                "corrupted pbxproj: no root object found at {:?}",
                self.root_object.0
            ),
            Some(PBXObject::Project(x)) => x,
            Some(x) => panic!("root object does not point a PBXProject object: {x:?}"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ValueDecodeError<'s> {
    #[error("unexpected value: {0:?}")]
    Unexpected(Value<'s>),
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXObjectIDRef<'s>(Cow<'s, str>);
impl<'s> PBXObjectIDRef<'s> {
    pub fn decode(x: Value<'s>) -> Result<Self, ValueDecodeError<'s>> {
        match x {
            Value::Single(x) => Ok(Self(x)),
            x => Err(ValueDecodeError::Unexpected(x)),
        }
    }

    #[inline(always)]
    pub fn encode(self) -> Value<'s> {
        Value::Single(self.0)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ObjectDecodeError<'s> {
    #[error("missing `{0}`")]
    MissingRequiredAttr(&'static str),
    #[error("Failed parsing `{0}`: {1}")]
    FailedParsingTypedObject(&'static str, Box<ObjectDecodeError<'s>>),
    #[error("invalid `{0}` value: {1}")]
    InvalidAttributeValue(&'static str, ValueDecodeError<'s>),
    #[error("invalid `{0}` element value: {1}")]
    InvalidAttributeElementValue(&'static str, ValueDecodeError<'s>),
}
impl<'s> ObjectDecodeError<'s> {
    #[inline(always)]
    pub const fn failed_parsing_typed_object(isa: &'static str) -> impl FnOnce(Self) -> Self {
        move |e| Self::FailedParsingTypedObject(isa, Box::new(e))
    }

    #[inline(always)]
    pub const fn invalid_attr_value(
        attr_name: &'static str,
    ) -> impl FnOnce(ValueDecodeError<'s>) -> Self {
        move |e| Self::InvalidAttributeValue(attr_name, e)
    }

    #[inline(always)]
    pub const fn invalid_attr_element_value(
        attr_name: &'static str,
    ) -> impl FnOnce(ValueDecodeError<'s>) -> Self {
        move |e| Self::InvalidAttributeElementValue(attr_name, e)
    }

    #[inline(always)]
    pub const fn unexpected_attr_value(attr_name: &'static str, value: Value<'s>) -> Self {
        Self::InvalidAttributeValue(attr_name, ValueDecodeError::Unexpected(value))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PBXObject<'s> {
    BuildFile(PBXBuildFile<'s>),
    FileReference(PBXFileReference<'s>),
    FrameworksBuildPhase(PBXFrameworksBuildPhase<'s>),
    Group(PBXGroup<'s>),
    NativeTarget(PBXNativeTarget<'s>),
    Project(PBXProject<'s>),
    ResourcesBuildPhase(PBXResourcesBuildPhase<'s>),
    SourcesBuildPhase(PBXSourcesBuildPhase<'s>),
    VariantGroup(PBXVariantGroup<'s>),
    BuildConfiguration(XCBuildConfiguration<'s>),
    ConfigurationList(XCConfigurationList<'s>),
    CopyFilesBuildPhase(PBXCopyFilesBuildPhase<'s>),
    Unknown {
        isa: Value<'s>,
        attributes: HashMap<&'s str, Value<'s>>,
    },
}
impl<'s> PBXObject<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, ObjectDecodeError<'s>> {
        let isa = match xs.remove("isa") {
            None => panic!("cannot determine object type"),
            Some(Value::Single(x)) => x,
            Some(x) => panic!("isa is not an value: {x:?}"),
        };

        Ok(match &isa as &str {
            "PBXBuildFile" => Self::BuildFile(PBXBuildFile::decode(xs)),
            "PBXFileReference" => Self::FileReference(PBXFileReference::decode(xs)),
            "PBXFrameworksBuildPhase" => {
                Self::FrameworksBuildPhase(PBXFrameworksBuildPhase::decode(xs))
            }
            "PBXGroup" => Self::Group(PBXGroup::decode(xs)),
            "PBXNativeTarget" => Self::NativeTarget(PBXNativeTarget::decode(xs).map_err(
                ObjectDecodeError::failed_parsing_typed_object("PBXNativeTarget"),
            )?),
            "PBXProject" => Self::Project(PBXProject::decode(xs)),
            "PBXResourcesBuildPhase" => {
                Self::ResourcesBuildPhase(PBXResourcesBuildPhase::decode(xs))
            }
            "PBXSourcesBuildPhase" => Self::SourcesBuildPhase(PBXSourcesBuildPhase::decode(xs)),
            "PBXVariantGroup" => PBXObject::VariantGroup(PBXVariantGroup::decode(xs)),
            "XCBuildConfiguration" => Self::BuildConfiguration(XCBuildConfiguration::decode(xs)),
            "XCConfigurationList" => Self::ConfigurationList(XCConfigurationList::decode(xs)),
            "PBXCopyFilesBuildPhase" => {
                Self::CopyFilesBuildPhase(PBXCopyFilesBuildPhase::decode(xs))
            }
            _ => Self::Unknown {
                isa: Value::Single(isa),
                attributes: xs,
            },
        })
    }

    pub fn encode(self) -> Value<'s> {
        match self {
            Self::BuildFile(x) => x.encode(),
            Self::FileReference(x) => x.encode(),
            Self::FrameworksBuildPhase(x) => x.encode(),
            Self::Group(x) => x.encode(),
            Self::NativeTarget(x) => x.encode(),
            Self::Project(x) => x.encode(),
            Self::ResourcesBuildPhase(x) => x.encode(),
            Self::SourcesBuildPhase(x) => x.encode(),
            Self::VariantGroup(x) => x.encode(),
            Self::BuildConfiguration(x) => x.encode(),
            Self::ConfigurationList(x) => x.encode(),
            Self::CopyFilesBuildPhase(x) => x.encode(),
            Self::Unknown { isa, attributes } => {
                let mut xs = HashMap::with_capacity(1 + attributes.len());
                xs.insert("isa", isa);
                xs.extend(attributes);

                Value::Map(xs)
            }
        }
    }

    #[inline(always)]
    pub fn downcast_ref<T>(&self) -> Option<&T>
    where
        T: PBXObjectType<'s>,
    {
        T::from_object_enum(self)
    }

    #[inline(always)]
    pub fn downcast_ref_mut<T>(&mut self) -> Option<&mut T>
    where
        T: PBXObjectType<'s>,
    {
        T::from_object_enum_mut(self)
    }
}

pub trait PBXObjectType<'s> {
    fn from_object_enum<'e>(e: &'e PBXObject<'s>) -> Option<&'e Self>;
    fn from_object_enum_mut<'e>(e: &'e mut PBXObject<'s>) -> Option<&'e mut Self>;
}
impl<'s> PBXObjectType<'s> for XCBuildConfiguration<'s> {
    fn from_object_enum<'e>(e: &'e PBXObject<'s>) -> Option<&'e Self> {
        if let PBXObject::BuildConfiguration(x) = e {
            Some(x)
        } else {
            None
        }
    }

    fn from_object_enum_mut<'e>(e: &'e mut PBXObject<'s>) -> Option<&'e mut Self> {
        if let PBXObject::BuildConfiguration(x) = e {
            Some(x)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXBuildFile<'s> {
    pub file_ref: PBXObjectIDRef<'s>,
    pub settings: Option<PBXBuildFileSettings<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXBuildFile<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let file_ref = PBXObjectIDRef::decode(xs.remove("fileRef").expect("fileRef required"))
            .map_err(ObjectDecodeError::invalid_attr_value("fileRef"))
            .unwrap();
        let settings = xs.remove("settings").map(PBXBuildFileSettings::decode);

        Self {
            file_ref,
            settings,
            extras: xs,
        }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 2 + self.extras.len());
        xs.insert("isa", Value::Single("PBXBuildFile".into()));
        xs.insert("fileRef", self.file_ref.encode());
        xs.extend(self.settings.map(|x| ("settings", x.encode())));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXBuildFileSettings<'s> {
    pub attributes: Vec<Cow<'s, str>>,
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

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + self.extras.len());
        if !self.attributes.is_empty() {
            xs.insert(
                "ATTRIBUTES",
                Value::Array(self.attributes.into_iter().map(Value::Single).collect()),
            );
        }
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXFileReference<'s> {
    pub last_known_file_type: Option<Cow<'s, str>>,
    pub name: Option<Cow<'s, str>>,
    pub path: Cow<'s, str>,
    pub source_tree: Cow<'s, str>,
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

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 4 + self.extras.len());
        xs.insert("isa", Value::Single("PBXFileReference".into()));
        xs.extend(
            self.last_known_file_type
                .map(|x| ("lastKnownFileType", Value::Single(x))),
        );
        xs.extend(self.name.map(|x| ("name", Value::Single(x))));
        xs.insert("path", Value::Single(self.path));
        xs.insert("sourceTree", Value::Single(self.source_tree));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXFrameworksBuildPhase<'s> {
    pub build_action_mask: u32,
    pub files: Vec<Cow<'s, str>>,
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

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 2 + self.extras.len());
        xs.insert("isa", Value::Single("PBXFrameworksBuildPhase".into()));
        xs.insert(
            "buildActionMask",
            Value::Single(self.build_action_mask.to_string().into()),
        );
        xs.insert(
            "files",
            Value::Array(self.files.into_iter().map(Value::Single).collect()),
        );
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXGroup<'s> {
    pub children: Vec<PBXObjectIDRef<'s>>,
    pub name: Option<Cow<'s, str>>,
    pub source_tree: Cow<'s, str>,
    pub path: Option<Cow<'s, str>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXGroup<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let children = match xs.remove("children") {
            None => Vec::new(),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x).expect("invalid PBXGroup.children element value")
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
        let path = xs.remove("path").map(|x| match x {
            Value::Single(x) => x,
            x => unreachable!("invalid PBXGroup.path value: {x:?}"),
        });

        Self {
            children,
            source_tree,
            name,
            path,
            extras: xs,
        }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 4 + self.extras.len());
        xs.insert("isa", Value::Single("PBXGroup".into()));
        xs.insert(
            "children",
            Value::Array(self.children.into_iter().map(|x| x.encode()).collect()),
        );
        xs.insert("sourceTree", Value::Single(self.source_tree));
        xs.extend(self.name.map(|x| ("name", Value::Single(x))));
        xs.extend(self.path.map(|x| ("path", Value::Single(x))));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXNativeTarget<'s> {
    pub name: Cow<'s, str>,
    pub product_reference: PBXObjectIDRef<'s>,
    pub product_type: Cow<'s, str>,
    pub build_phases: Vec<PBXObjectIDRef<'s>>,
    pub build_rules: Vec<PBXObjectIDRef<'s>>,
    pub build_configuration_list: PBXObjectIDRef<'s>,
    pub product_name: Cow<'s, str>,
    pub dependencies: Vec<PBXObjectIDRef<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXNativeTarget<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, ObjectDecodeError<'s>> {
        let name = match xs.remove("name") {
            None => Err(ObjectDecodeError::MissingRequiredAttr("name")),
            Some(Value::Single(x)) => Ok(x),
            Some(x) => Err(ObjectDecodeError::unexpected_attr_value("name", x)),
        }?;
        let product_reference = PBXObjectIDRef::decode(
            xs.remove("productReference")
                .ok_or(ObjectDecodeError::MissingRequiredAttr("productReference"))?,
        )
        .map_err(ObjectDecodeError::invalid_attr_value("productReference"))?;
        let product_type = match xs.remove("productType") {
            None => Err(ObjectDecodeError::MissingRequiredAttr("productType")),
            Some(Value::Single(x)) => Ok(x),
            Some(x) => Err(ObjectDecodeError::unexpected_attr_value("productType", x)),
        }?;
        let build_phases = match xs.remove("buildPhases") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(ObjectDecodeError::invalid_attr_element_value("buildPhases"))
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(ObjectDecodeError::unexpected_attr_value("buildPhaess", x)),
        }?;
        let build_rules = match xs.remove("buildRules") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(ObjectDecodeError::invalid_attr_element_value("buildRules"))
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(ObjectDecodeError::unexpected_attr_value("buildRules", x)),
        }?;
        let build_configuration_list =
            PBXObjectIDRef::decode(xs.remove("buildConfigurationList").ok_or(
                ObjectDecodeError::MissingRequiredAttr("buildConfigurationList"),
            )?)
            .map_err(ObjectDecodeError::invalid_attr_value(
                "buildConfigurationList",
            ))?;
        let product_name = match xs.remove("productName") {
            None => Err(ObjectDecodeError::MissingRequiredAttr("productName")),
            Some(Value::Single(x)) => Ok(x),
            Some(x) => Err(ObjectDecodeError::unexpected_attr_value("productName", x)),
        }?;
        let dependencies = match xs.remove("dependencies") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x).map_err(
                        ObjectDecodeError::invalid_attr_element_value("dependencies"),
                    )
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(ObjectDecodeError::unexpected_attr_value("dependencies", x)),
        }?;

        Ok(Self {
            name,
            product_reference,
            product_type,
            build_phases,
            build_rules,
            build_configuration_list,
            product_name,
            dependencies,
            extras: xs,
        })
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 8 + self.extras.len());
        xs.insert("isa", Value::Single("PBXNativeTarget".into()));
        xs.insert("name", Value::Single(self.name));
        xs.insert("productReference", self.product_reference.encode());
        xs.insert("productType", Value::Single(self.product_type));
        xs.insert(
            "buildPhases",
            Value::Array(self.build_phases.into_iter().map(|x| x.encode()).collect()),
        );
        xs.insert(
            "buildRules",
            Value::Array(self.build_rules.into_iter().map(|x| x.encode()).collect()),
        );
        xs.insert(
            "buildConfigurationList",
            self.build_configuration_list.encode(),
        );
        xs.insert("productName", Value::Single(self.product_name));
        xs.insert(
            "dependencies",
            Value::Array(self.dependencies.into_iter().map(|x| x.encode()).collect()),
        );
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXProject<'s> {
    pub build_configuration_list: PBXObjectIDRef<'s>,
    pub targets: Vec<PBXObjectIDRef<'s>>,
    pub project_dir_path: Option<Cow<'s, str>>,
    pub main_group: PBXObjectIDRef<'s>,
    pub development_region: Option<Cow<'s, str>>,
    pub product_ref_group: PBXObjectIDRef<'s>,
    pub project_root: Option<Cow<'s, str>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXProject<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let build_configuration_list = xs
            .remove("buildConfigurationList")
            .map(|x| PBXObjectIDRef::decode(x).expect("invalid buildConfigurationList value"))
            .expect("no buildConfigurationList");
        let targets = match xs.remove("targets") {
            None => Vec::new(),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| PBXObjectIDRef::decode(x).expect("invalid target element value"))
                .collect(),
            Some(x) => unreachable!("invalid targets value: {x:?}"),
        };
        let project_dir_path = xs.remove("projectDirPath").map(|x| match x {
            Value::Single(x) => x,
            x => unreachable!("invalid projectDirPath value: {x:?}"),
        });
        let main_group = xs
            .remove("mainGroup")
            .map(|x| PBXObjectIDRef::decode(x).expect("invalid mainGroup value"))
            .expect("no mainGroup");
        let development_region = xs.remove("developmentRegion").map(|x| match x {
            Value::Single(x) => x,
            x => unreachable!("invalid developmentRegion value: {x:?}"),
        });
        let product_ref_group = xs
            .remove("productRefGroup")
            .map(|x| PBXObjectIDRef::decode(x).expect("invalid productRefGroup value"))
            .expect("no projectRefGroup");
        let project_root = xs.remove("projectRoot").map(|x| match x {
            Value::Single(x) => x,
            x => unreachable!("invalid projectRoot value: {x:?}"),
        });

        Self {
            build_configuration_list,
            targets,
            project_dir_path,
            main_group,
            development_region,
            product_ref_group,
            project_root,
            extras: xs,
        }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 7 + self.extras.len());
        xs.insert("isa", Value::Single("PBXProject".into()));
        xs.insert(
            "buildConfigurationList",
            self.build_configuration_list.encode(),
        );
        xs.insert(
            "targets",
            Value::Array(self.targets.into_iter().map(|x| x.encode()).collect()),
        );
        xs.extend(
            self.project_dir_path
                .map(|x| ("projectDirPath", Value::Single(x))),
        );
        xs.insert("mainGroup", self.main_group.encode());
        xs.extend(
            self.development_region
                .map(|x| ("developmentRegion", Value::Single(x))),
        );
        xs.insert("productRefGroup", self.product_ref_group.encode());
        xs.extend(self.project_root.map(|x| ("projectRoot", Value::Single(x))));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXResourcesBuildPhase<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXResourcesBuildPhase<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + self.extras.len());
        xs.insert("isa", Value::Single("PBXResourcesBuildPhase".into()));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXSourcesBuildPhase<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXSourcesBuildPhase<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + self.extras.len());
        xs.insert("isa", Value::Single("PBXSourcesBuildPhase".into()));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXVariantGroup<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXVariantGroup<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + self.extras.len());
        xs.insert("isa", Value::Single("PBXVariantGroup".into()));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct XCBuildConfiguration<'s> {
    pub name: Cow<'s, str>,
    pub build_settings: HashMap<&'s str, Value<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> XCBuildConfiguration<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let name = match xs.remove("name").expect("no name") {
            Value::Single(x) => x,
            x => unreachable!("invalid XCBuildConfiguration.name value: {x:?}"),
        };
        let build_settings = match xs.remove("buildSettings") {
            None => HashMap::new(),
            Some(Value::Map(xs)) => xs,
            Some(x) => unreachable!("invalid XCBuildConfiguration.buildSettings value: {x:?}"),
        };

        Self {
            name,
            build_settings,
            extras: xs,
        }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 2 + self.extras.len());
        xs.insert("isa", Value::Single("XCBuildConfiguration".into()));
        xs.insert("name", Value::Single(self.name));
        xs.insert("buildSettings", Value::Map(self.build_settings));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct XCConfigurationList<'s> {
    pub default_configuration_name: Cow<'s, str>,
    pub build_configurations: Vec<PBXObjectIDRef<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> XCConfigurationList<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Self {
        let default_configuration_name = match xs
            .remove("defaultConfigurationName")
            .expect("no defaultConfigurationName")
        {
            Value::Single(x) => x,
            x => unreachable!("invalid defaultConfigurationName value: {x:?}"),
        };
        let build_configurations = match xs.remove("buildConfigurations") {
            None => Vec::new(),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| PBXObjectIDRef::decode(x).expect("invalid buildConfigurations element"))
                .collect(),
            Some(x) => unreachable!("invalid buildConfigurations value: {x:?}"),
        };

        Self {
            default_configuration_name,
            build_configurations,
            extras: xs,
        }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + 2 + self.extras.len());
        xs.insert("isa", Value::Single("XCConfigurationList".into()));
        xs.insert(
            "defaultConfigurationName",
            Value::Single(self.default_configuration_name),
        );
        xs.insert(
            "buildConfigurations",
            Value::Array(
                self.build_configurations
                    .into_iter()
                    .map(|x| x.encode())
                    .collect(),
            ),
        );
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXCopyFilesBuildPhase<'s> {
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXCopyFilesBuildPhase<'s> {
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Self {
        Self { extras: xs }
    }

    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + self.extras.len());
        xs.insert("isa", Value::Single("PBXCopyFilesBuildPhase".into()));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}

fn main() {
    let src = std::fs::read_to_string(std::env::args().nth(1).expect("no args"))
        .expect("Failed to load file");
    let mut ps = ParserState::new(&src);
    ps.skip_spaces();
    let v = parse_value(&mut ps).unwrap();
    let mut pbxproj = match v {
        Value::Map(xs) => PBXProjectFile::decode(xs),
        x => unreachable!("invalid pbxproj root type: {x:?}"),
    };
    eprintln!("pbxproj: {pbxproj:#?}");
    let root_project = pbxproj.root_project();
    eprintln!("rootObject: {root_project:#?}");
    let PBXObject::Group(main_group) = pbxproj
        .object_ref(&root_project.main_group)
        .expect("missing main group object")
    else {
        unreachable!(
            "invalid mainGroup: {:?}",
            pbxproj.object_ref(&root_project.main_group)
        );
    };
    eprintln!("main group: {main_group:#?}");
    for c in main_group.children.iter() {
        eprintln!("main: {:#?}", pbxproj.object_ref(c));
    }
    eprintln!(
        "product ref group: {:#?}",
        pbxproj.object_ref(&root_project.product_ref_group)
    );
    let PBXObject::ConfigurationList(build_configuration_list) = pbxproj
        .object_ref(&root_project.build_configuration_list)
        .expect("no buildConfigurationList")
    else {
        unreachable!(
            "invalid buildConfigurationList: {:?}",
            pbxproj.object_ref(&root_project.build_configuration_list)
        );
    };
    eprintln!("build configuration list: {build_configuration_list:#?}");
    for c in build_configuration_list.build_configurations.iter() {
        eprintln!("build cfg: {:#?}", pbxproj.object_ref(c));
    }
    for c in root_project.targets.clone().into_iter() {
        eprintln!("target: {:#?}", pbxproj.object_ref(&c));
        match pbxproj.object_ref(&c).expect("no target object found") {
            PBXObject::NativeTarget(nt) => {
                let PBXObject::ConfigurationList(build_cfg_list) = pbxproj
                    .object_ref(&nt.build_configuration_list)
                    .expect("no buildConfigurationList")
                else {
                    unreachable!(
                        "expected cfg list: {:?}",
                        pbxproj.object_ref(&nt.build_configuration_list)
                    );
                };
                eprintln!("target build cfg list: {build_cfg_list:#?}");

                for c in build_cfg_list.build_configurations.clone().into_iter() {
                    let PBXObject::BuildConfiguration(cfg) =
                        pbxproj.object_ref(&c).expect("missing buildConfiguration")
                    else {
                        unreachable!("expected build cfg: {:?}", pbxproj.object_ref(&c));
                    };
                    eprintln!("target build cfg: {cfg:#?}");

                    pbxproj
                        .object_ref_mut_of::<XCBuildConfiguration>(&c)
                        .unwrap()
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
    let pbxproj2 = match v {
        Value::Map(xs) => PBXProjectFile::decode(xs),
        x => unreachable!("invalid pbxproj root type: {x:?}"),
    };
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
