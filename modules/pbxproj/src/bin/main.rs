use std::{borrow::Cow, collections::HashMap};

use pbxproj::{ParserState, Value, parse_value};

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXObjectIDRef<'s>(Cow<'s, str>);
impl<'s> PBXObjectIDRef<'s> {
    pub fn decode(x: Value<'s>) -> Result<Self, DecodeError<'s>> {
        match x {
            Value::Single(x) => Ok(Self(x)),
            x => Err(DecodeError::Unexpected(x)),
        }
    }

    #[inline(always)]
    pub fn encode(self) -> Value<'s> {
        Value::Single(self.0)
    }

    #[inline(always)]
    pub fn entity<'f>(&self, file: &'f PBXProjectFile<'s>) -> Option<&'f PBXObject<'s>> {
        file.object_ref(self)
    }

    #[inline(always)]
    pub fn entity_mut<'f>(
        &self,
        file: &'f mut PBXProjectFile<'s>,
    ) -> Option<&'f mut PBXObject<'s>> {
        file.object_ref_mut(self)
    }

    #[inline(always)]
    pub fn entity_of<'id, 'f, T>(
        &'id self,
        file: &'f PBXProjectFile<'s>,
    ) -> Result<&'f T, TypedObjectReferenceError<'id, 'f, 's>>
    where
        T: PBXObjectType<'s>,
    {
        file.object_ref_of(self)
    }

    #[inline(always)]
    pub fn entity_mut_of<'id, 'f, T>(
        &'id self,
        file: &'f mut PBXProjectFile<'s>,
    ) -> Result<&'f mut T, TypedObjectMutableReferenceError<'id, 'f, 's>>
    where
        T: PBXObjectType<'s>,
    {
        file.object_ref_mut_of(self)
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXTypedObjectIDRef<'s, T>(PBXObjectIDRef<'s>, core::marker::PhantomData<T>);
impl<'s, T> PBXTypedObjectIDRef<'s, T> {
    #[inline(always)]
    pub fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>> {
        Ok(Self(PBXObjectIDRef::decode(v)?, core::marker::PhantomData))
    }

    #[inline(always)]
    pub fn encode(self) -> Value<'s> {
        self.0.encode()
    }

    #[inline(always)]
    pub fn entity<'id, 'f>(
        &'id self,
        file: &'f PBXProjectFile<'s>,
    ) -> Result<&'f T, TypedObjectReferenceError<'id, 'f, 's>>
    where
        T: PBXObjectType<'s>,
    {
        self.0.entity_of::<T>(file)
    }

    #[inline(always)]
    pub fn entity_mut<'id, 'f>(
        &'id self,
        file: &'f mut PBXProjectFile<'s>,
    ) -> Result<&'f mut T, TypedObjectMutableReferenceError<'id, 'f, 's>>
    where
        T: PBXObjectType<'s>,
    {
        self.0.entity_mut_of::<T>(file)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DecodeError<'s> {
    #[error("unexpected value: {0:?}")]
    Unexpected(Value<'s>),
    #[error("missing `{0}`")]
    MissingRequiredAttr(&'static str),
    #[error("Failed parsing `{0}`: {1}")]
    FailedParsingTypedObject(&'static str, Box<DecodeError<'s>>),
    #[error("invalid `{0}` value: {1}")]
    InvalidAttributeValue(&'static str, Box<DecodeError<'s>>),
    #[error("invalid `{0}` element value: {1}")]
    InvalidAttributeElementValue(&'static str, Box<DecodeError<'s>>),
}
impl<'s> DecodeError<'s> {
    #[inline(always)]
    pub const fn failed_parsing_typed_object(isa: &'static str) -> impl FnOnce(Self) -> Self {
        move |e| Self::FailedParsingTypedObject(isa, Box::new(e))
    }

    #[inline(always)]
    pub const fn invalid_attr_value(attr_name: &'static str) -> impl FnOnce(Self) -> Self {
        move |e| Self::InvalidAttributeValue(attr_name, Box::new(e))
    }

    #[inline(always)]
    pub const fn invalid_attr_element_value(attr_name: &'static str) -> impl FnOnce(Self) -> Self {
        move |e| Self::InvalidAttributeElementValue(attr_name, Box::new(e))
    }

    #[inline(always)]
    pub fn unexpected_attr_value(attr_name: &'static str, value: Value<'s>) -> Self {
        Self::InvalidAttributeValue(attr_name, Box::new(Self::Unexpected(value)))
    }

    #[inline(always)]
    pub fn unexpected_attr_element_value(attr_name: &'static str, value: Value<'s>) -> Self {
        Self::InvalidAttributeElementValue(attr_name, Box::new(Self::Unexpected(value)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXProjectFile<'s> {
    pub objects: HashMap<&'s str, PBXObject<'s>>,
    pub root_object: PBXObjectIDRef<'s>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXProjectFile<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let objects = match xs
            .remove("objects")
            .ok_or(DecodeError::MissingRequiredAttr("objects"))?
        {
            Value::Map(xs) => xs
                .into_iter()
                .map(|(k, v)| {
                    let v = match v {
                        Value::Map(xs) => PBXObject::decode(xs)
                            .map_err(DecodeError::invalid_attr_element_value("objects")),
                        x => Err(DecodeError::unexpected_attr_element_value("objects", x)),
                    }?;

                    Ok((k, v))
                })
                .collect::<Result<_, _>>()?,
            x => unreachable!("unknown {x:?}"),
        };
        let root_object = PBXObjectIDRef::decode(
            xs.remove("rootObject")
                .ok_or(DecodeError::MissingRequiredAttr("rootObject"))?,
        )
        .map_err(DecodeError::invalid_attr_value("rootObject"))?;

        Ok(Self {
            objects,
            root_object,
            extras: xs,
        })
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
    pub fn object_ref_of<'id, 'x, T>(
        &'x self,
        id: &'id PBXObjectIDRef<'s>,
    ) -> Result<&'x T, TypedObjectReferenceError<'id, 'x, 's>>
    where
        T: PBXObjectType<'s>,
    {
        match self.object_ref(id) {
            None => Err(TypedObjectReferenceError::Missing(id)),
            Some(x) => match x.downcast_ref() {
                None => Err(TypedObjectReferenceError::Mismatch(x)),
                Some(t) => Ok(t),
            },
        }
    }

    #[inline(always)]
    pub fn object_ref_mut_of<'id, 'x, T>(
        &'x mut self,
        id: &'id PBXObjectIDRef<'s>,
    ) -> Result<&'x mut T, TypedObjectMutableReferenceError<'id, 'x, 's>>
    where
        T: PBXObjectType<'s>,
    {
        match self.object_ref_mut(id) {
            None => Err(TypedObjectMutableReferenceError::Missing(id)),
            Some(x) => match x.downcast_ref_mut() {
                Err(x) => Err(TypedObjectMutableReferenceError::Mismatch(x)),
                Ok(t) => Ok(t),
            },
        }
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
pub enum TypedObjectReferenceError<'id, 'x, 's> {
    #[error("missing object for id {:?}", .0 .0)]
    Missing(&'id PBXObjectIDRef<'s>),
    #[error("object type is not as expected: found {0:?}")]
    Mismatch(&'x PBXObject<'s>),
}

#[derive(Debug, thiserror::Error)]
pub enum TypedObjectMutableReferenceError<'id, 'x, 's> {
    #[error("missing object for id {:?}", .0 .0)]
    Missing(&'id PBXObjectIDRef<'s>),
    #[error("object type is not as expected: found {0:?}")]
    Mismatch(&'x mut PBXObject<'s>),
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
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let isa = match xs.remove("isa") {
            Some(Value::Single(x)) => x,
            None => return Err(DecodeError::MissingRequiredAttr("isa")),
            Some(x) => return Err(DecodeError::unexpected_attr_value("isa", x)),
        };

        Ok(match &isa as &str {
            "PBXBuildFile" => Self::BuildFile(
                PBXBuildFile::decode(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXBuildFile"))?,
            ),
            "PBXFileReference" => Self::FileReference(
                PBXFileReference::decode(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXFileReference"))?,
            ),
            "PBXFrameworksBuildPhase" => {
                Self::FrameworksBuildPhase(PBXFrameworksBuildPhase::decode(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXFrawmeworksBuildPhase"),
                )?)
            }
            "PBXGroup" => Self::Group(
                PBXGroup::decode(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXGroup"))?,
            ),
            "PBXNativeTarget" => Self::NativeTarget(
                PBXNativeTarget::decode(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXNativeTarget"))?,
            ),
            "PBXProject" => Self::Project(
                PBXProject::decode(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXProject"))?,
            ),
            "PBXResourcesBuildPhase" => {
                Self::ResourcesBuildPhase(PBXResourcesBuildPhase::decode(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXResourcesBuildPhase"),
                )?)
            }
            "PBXSourcesBuildPhase" => {
                Self::SourcesBuildPhase(PBXSourcesBuildPhase::decode(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXSourcesBuildPhase"),
                )?)
            }
            "PBXVariantGroup" => PBXObject::VariantGroup(
                PBXVariantGroup::decode(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXVariantGroup"))?,
            ),
            "XCBuildConfiguration" => {
                Self::BuildConfiguration(XCBuildConfiguration::decode(xs).map_err(
                    DecodeError::failed_parsing_typed_object("XCBuildConfiguration"),
                )?)
            }
            "XCConfigurationList" => {
                Self::ConfigurationList(XCConfigurationList::decode(xs).map_err(
                    DecodeError::failed_parsing_typed_object("XCConfigurationList"),
                )?)
            }
            "PBXCopyFilesBuildPhase" => {
                Self::CopyFilesBuildPhase(PBXCopyFilesBuildPhase::decode(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXCopyFilesBuildPhase"),
                )?)
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
    pub fn downcast_ref_mut<T>(&mut self) -> Result<&mut T, &mut Self>
    where
        T: PBXObjectType<'s>,
    {
        T::from_object_enum_mut(self)
    }
}

pub trait PBXObjectType<'s> {
    fn from_object_enum<'e>(e: &'e PBXObject<'s>) -> Option<&'e Self>;
    fn from_object_enum_mut<'e>(
        e: &'e mut PBXObject<'s>,
    ) -> Result<&'e mut Self, &'e mut PBXObject<'s>>;
}
macro_rules! DefinePBXObjectType {
    (for $t: ty, $extraction: pat => $take: expr) => {
        impl<'s> PBXObjectType<'s> for $t {
            fn from_object_enum<'e>(e: &'e PBXObject<'s>) -> Option<&'e Self> {
                if let $extraction = e {
                    Some($take)
                } else {
                    None
                }
            }

            fn from_object_enum_mut<'e>(
                e: &'e mut PBXObject<'s>,
            ) -> Result<&'e mut Self, &'e mut PBXObject<'s>> {
                if let $extraction = e {
                    Ok($take)
                } else {
                    Err(e)
                }
            }
        }
    };
}
DefinePBXObjectType!(for PBXGroup<'s>, PBXObject::Group(x) => x);
DefinePBXObjectType!(for PBXNativeTarget<'s>, PBXObject::NativeTarget(x) => x);
DefinePBXObjectType!(for XCConfigurationList<'s>, PBXObject::ConfigurationList(x) => x);
DefinePBXObjectType!(for XCBuildConfiguration<'s>, PBXObject::BuildConfiguration(x) => x);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXBuildFile<'s> {
    pub file_ref: PBXObjectIDRef<'s>,
    pub settings: Option<PBXBuildFileSettings<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXBuildFile<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let file_ref = PBXObjectIDRef::decode(xs.remove("fileRef").expect("fileRef required"))
            .map_err(DecodeError::invalid_attr_value("fileRef"))?;
        let settings = xs
            .remove("settings")
            .map(PBXBuildFileSettings::decode)
            .transpose()?;

        Ok(Self {
            file_ref,
            settings,
            extras: xs,
        })
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
    pub fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>> {
        match v {
            Value::Map(mut xs) => {
                let attributes = match xs.remove("ATTRIBUTES") {
                    None => Ok(Vec::new()),
                    Some(Value::Array(xs)) => xs
                        .into_iter()
                        .map(|x| match x {
                            Value::Single(x) => Ok(x),
                            x => Err(DecodeError::unexpected_attr_element_value("ATTRIBUTES", x)),
                        })
                        .collect::<Result<_, _>>(),
                    Some(x) => Err(DecodeError::unexpected_attr_value("ATTRIBUTES", x)),
                }?;

                Ok(Self {
                    attributes,
                    extras: xs,
                })
            }
            x => Err(DecodeError::unexpected_attr_value(
                "PBXBuildFile.settings",
                x,
            )),
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
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let last_known_file_type = match xs.remove("lastKnownFileType") {
            Some(Value::Single(x)) => Some(x),
            Some(x) => return Err(DecodeError::unexpected_attr_value("lastKnownFileType", x)),
            None => None,
        };
        let name = match xs.remove("name") {
            Some(Value::Single(x)) => Some(x),
            Some(x) => return Err(DecodeError::unexpected_attr_value("name", x)),
            None => None,
        };
        let path = match xs.remove("path") {
            Some(Value::Single(x)) => x,
            Some(x) => return Err(DecodeError::unexpected_attr_value("path", x)),
            None => return Err(DecodeError::MissingRequiredAttr("path")),
        };
        let source_tree = match xs.remove("sourceTree") {
            Some(Value::Single(x)) => x,
            Some(x) => return Err(DecodeError::unexpected_attr_value("sourceTree", x)),
            None => return Err(DecodeError::MissingRequiredAttr("sourceTree")),
        };

        Ok(Self {
            last_known_file_type,
            name,
            path,
            source_tree,
            extras: xs,
        })
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
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let build_action_mask = match xs.remove("buildActionMask") {
            Some(Value::Single(x)) => x.parse::<u32>().expect("cannot parse as u32"),
            Some(x) => return Err(DecodeError::unexpected_attr_value("buildActionMask", x)),
            None => return Err(DecodeError::MissingRequiredAttr("buildActionMask")),
        };
        let files = match xs.remove("files") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| match x {
                    Value::Single(x) => Ok(x),
                    x => Err(DecodeError::unexpected_attr_element_value("files", x)),
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(DecodeError::unexpected_attr_value("files", x)),
        }?;

        Ok(Self {
            build_action_mask,
            files,
            extras: xs,
        })
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
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let children = match xs.remove("children") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(DecodeError::invalid_attr_element_value("children"))
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(DecodeError::unexpected_attr_value("children", x)),
        }?;
        let source_tree = match xs.remove("sourceTree") {
            Some(Value::Single(x)) => Ok(x),
            Some(x) => Err(DecodeError::unexpected_attr_value("sourceTree", x)),
            None => Err(DecodeError::MissingRequiredAttr("sourceTree")),
        }?;
        let name = match xs.remove("name") {
            Some(Value::Single(x)) => Ok(Some(x)),
            None => Ok(None),
            Some(x) => Err(DecodeError::unexpected_attr_value("name", x)),
        }?;
        let path = xs
            .remove("path")
            .map(|x| match x {
                Value::Single(x) => Ok(x),
                x => Err(DecodeError::unexpected_attr_value("path", x)),
            })
            .transpose()?;

        Ok(Self {
            children,
            source_tree,
            name,
            path,
            extras: xs,
        })
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
    pub build_configuration_list: PBXTypedObjectIDRef<'s, XCConfigurationList<'s>>,
    pub product_name: Cow<'s, str>,
    pub dependencies: Vec<PBXObjectIDRef<'s>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXNativeTarget<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let name = match xs.remove("name") {
            Some(Value::Single(x)) => Ok(x),
            None => Err(DecodeError::MissingRequiredAttr("name")),
            Some(x) => Err(DecodeError::unexpected_attr_value("name", x)),
        }?;
        let product_reference = PBXObjectIDRef::decode(
            xs.remove("productReference")
                .ok_or(DecodeError::MissingRequiredAttr("productReference"))?,
        )
        .map_err(DecodeError::invalid_attr_value("productReference"))?;
        let product_type = match xs.remove("productType") {
            Some(Value::Single(x)) => Ok(x),
            None => Err(DecodeError::MissingRequiredAttr("productType")),
            Some(x) => Err(DecodeError::unexpected_attr_value("productType", x)),
        }?;
        let build_phases = match xs.remove("buildPhases") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(DecodeError::invalid_attr_element_value("buildPhases"))
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(DecodeError::unexpected_attr_value("buildPhaess", x)),
        }?;
        let build_rules = match xs.remove("buildRules") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(DecodeError::invalid_attr_element_value("buildRules"))
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(DecodeError::unexpected_attr_value("buildRules", x)),
        }?;
        let build_configuration_list = PBXTypedObjectIDRef::decode(
            xs.remove("buildConfigurationList")
                .ok_or(DecodeError::MissingRequiredAttr("buildConfigurationList"))?,
        )
        .map_err(DecodeError::invalid_attr_value("buildConfigurationList"))?;
        let product_name = match xs.remove("productName") {
            None => Err(DecodeError::MissingRequiredAttr("productName")),
            Some(Value::Single(x)) => Ok(x),
            Some(x) => Err(DecodeError::unexpected_attr_value("productName", x)),
        }?;
        let dependencies = match xs.remove("dependencies") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(DecodeError::invalid_attr_element_value("dependencies"))
                })
                .collect::<Result<_, _>>(),
            Some(x) => Err(DecodeError::unexpected_attr_value("dependencies", x)),
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
    pub main_group: PBXTypedObjectIDRef<'s, PBXGroup<'s>>,
    pub development_region: Option<Cow<'s, str>>,
    pub product_ref_group: PBXTypedObjectIDRef<'s, PBXGroup<'s>>,
    pub project_root: Option<Cow<'s, str>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> PBXProject<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let build_configuration_list = PBXObjectIDRef::decode(
            xs.remove("buildConfigurationList")
                .ok_or(DecodeError::MissingRequiredAttr("buildConfigurationList"))?,
        )
        .map_err(DecodeError::invalid_attr_value("buildConfigurationList"))?;
        let targets = match xs.remove("targets") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXObjectIDRef::decode(x)
                        .map_err(DecodeError::invalid_attr_element_value("targets"))
                })
                .collect(),
            Some(x) => Err(DecodeError::unexpected_attr_value("targets", x)),
        }?;
        let project_dir_path = xs
            .remove("projectDirPath")
            .map(|x| match x {
                Value::Single(x) => Ok(x),
                x => Err(DecodeError::unexpected_attr_value("projectDirPath", x)),
            })
            .transpose()?;
        let main_group = PBXTypedObjectIDRef::decode(
            xs.remove("mainGroup")
                .ok_or(DecodeError::MissingRequiredAttr("mainGroup"))?,
        )
        .map_err(DecodeError::invalid_attr_value("mainGroup"))?;
        let development_region = xs
            .remove("developmentRegion")
            .map(|x| match x {
                Value::Single(x) => Ok(x),
                x => Err(DecodeError::unexpected_attr_value("developmentRegion", x)),
            })
            .transpose()?;
        let product_ref_group = PBXTypedObjectIDRef::decode(
            xs.remove("productRefGroup")
                .ok_or(DecodeError::MissingRequiredAttr("productRefGroup"))?,
        )
        .map_err(DecodeError::invalid_attr_value("productRefGroup"))?;
        let project_root = xs
            .remove("projectRoot")
            .map(|x| match x {
                Value::Single(x) => Ok(x),
                x => Err(DecodeError::unexpected_attr_value("projectRoot value", x)),
            })
            .transpose()?;

        Ok(Self {
            build_configuration_list,
            targets,
            project_dir_path,
            main_group,
            development_region,
            product_ref_group,
            project_root,
            extras: xs,
        })
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
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
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
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
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
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
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
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let name = match xs
            .remove("name")
            .ok_or(DecodeError::MissingRequiredAttr("name"))?
        {
            Value::Single(x) => Ok(x),
            x => Err(DecodeError::unexpected_attr_value("name", x)),
        }?;
        let build_settings = match xs.remove("buildSettings") {
            None => Ok(HashMap::new()),
            Some(Value::Map(xs)) => Ok(xs),
            Some(x) => Err(DecodeError::unexpected_attr_value("buildSettings", x)),
        }?;

        Ok(Self {
            name,
            build_settings,
            extras: xs,
        })
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
    pub build_configurations: Vec<PBXTypedObjectIDRef<'s, XCBuildConfiguration<'s>>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> XCConfigurationList<'s> {
    pub fn decode(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        let default_configuration_name = match xs
            .remove("defaultConfigurationName")
            .ok_or(DecodeError::MissingRequiredAttr("defaultConfigurationName"))?
        {
            Value::Single(x) => Ok(x),
            x => Err(DecodeError::unexpected_attr_value(
                "defaultConfigurationName",
                x,
            )),
        }?;
        let build_configurations = match xs.remove("buildConfigurations") {
            None => Ok(Vec::new()),
            Some(Value::Array(xs)) => xs
                .into_iter()
                .map(|x| {
                    PBXTypedObjectIDRef::decode(x).map_err(DecodeError::invalid_attr_element_value(
                        "buildConfigurations",
                    ))
                })
                .collect(),
            Some(x) => Err(DecodeError::unexpected_attr_value("buildConfigurations", x)),
        }?;

        Ok(Self {
            default_configuration_name,
            build_configurations,
            extras: xs,
        })
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
    pub fn decode(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
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
        Value::Map(xs) => PBXProjectFile::decode(xs).expect("Failed to parse pbxproj"),
        x => unreachable!("invalid pbxproj root type: {x:?}"),
    };
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
        .entity_of::<XCConfigurationList>(&pbxproj)
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
    let pbxproj2 = match v {
        Value::Map(xs) => PBXProjectFile::decode(xs).expect("Failed to parse pbxproj"),
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
