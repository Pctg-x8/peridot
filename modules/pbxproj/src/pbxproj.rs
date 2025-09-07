use std::{borrow::Cow, collections::HashMap};

use crate::Value;

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

pub trait Decodable<'s>: Sized {
    fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>>;
}
pub trait DecodableMap<'s>: Sized {
    fn decode_map(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>>;
}

trait ValueChainHelper<'s> {
    fn decode<T>(self) -> Result<T, DecodeError<'s>>
    where
        T: Decodable<'s>;

    fn decode_single_as_str(self) -> Result<Cow<'s, str>, DecodeError<'s>>;
}
impl<'s> ValueChainHelper<'s> for Value<'s> {
    #[inline(always)]
    fn decode<T>(self) -> Result<T, DecodeError<'s>>
    where
        T: Decodable<'s>,
    {
        T::decode(self)
    }

    #[inline(always)]
    fn decode_single_as_str(self) -> Result<Cow<'s, str>, DecodeError<'s>> {
        match self {
            Self::Single(v) => Ok(v),
            x => Err(DecodeError::Unexpected(x)),
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXObjectIDRef<'s>(Cow<'s, str>);
impl<'s> Decodable<'s> for PBXObjectIDRef<'s> {
    #[inline(always)]
    fn decode(x: Value<'s>) -> Result<Self, DecodeError<'s>> {
        Ok(Self(x.decode_single_as_str()?))
    }
}
impl<'s> PBXObjectIDRef<'s> {
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
impl<'s, T> Decodable<'s> for PBXTypedObjectIDRef<'s, T> {
    #[inline(always)]
    fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>> {
        Ok(Self(v.decode()?, core::marker::PhantomData))
    }
}
impl<'s, T> PBXTypedObjectIDRef<'s, T> {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PBXProjectFile<'s> {
    pub objects: HashMap<&'s str, PBXObject<'s>>,
    pub root_object: PBXObjectIDRef<'s>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> Decodable<'s> for PBXProjectFile<'s> {
    fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>> {
        let Value::Map(mut xs) = v else {
            return Err(DecodeError::Unexpected(v));
        };

        Ok(Self {
            objects: match xs.remove("objects") {
                None => Ok(HashMap::new()),
                Some(Value::Map(xs)) => xs
                    .into_iter()
                    .map(|(k, v)| {
                        Ok((
                            k,
                            v.decode()
                                .map_err(DecodeError::invalid_attr_element_value("objects"))?,
                        ))
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("objects", x)),
            }?,
            root_object: xs
                .remove("rootObject")
                .ok_or(DecodeError::MissingRequiredAttr("rootObject"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("rootObject"))?,
            extras: xs,
        })
    }
}
impl<'s> PBXProjectFile<'s> {
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
impl<'s> Decodable<'s> for PBXObject<'s> {
    #[inline(always)]
    fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>> {
        let Value::Map(mut xs) = v else {
            return Err(DecodeError::Unexpected(v));
        };

        let isa = xs
            .remove("isa")
            .ok_or(DecodeError::MissingRequiredAttr("isa"))?
            .decode_single_as_str()
            .map_err(DecodeError::invalid_attr_value("isa"))?;

        Ok(match &isa as &str {
            "PBXBuildFile" => Self::BuildFile(
                PBXBuildFile::decode_map(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXBuildFile"))?,
            ),
            "PBXFileReference" => Self::FileReference(
                PBXFileReference::decode_map(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXFileReference"))?,
            ),
            "PBXFrameworksBuildPhase" => {
                Self::FrameworksBuildPhase(PBXFrameworksBuildPhase::decode_map(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXFrawmeworksBuildPhase"),
                )?)
            }
            "PBXGroup" => Self::Group(
                PBXGroup::decode_map(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXGroup"))?,
            ),
            "PBXNativeTarget" => Self::NativeTarget(
                PBXNativeTarget::decode_map(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXNativeTarget"))?,
            ),
            "PBXProject" => Self::Project(
                PBXProject::decode_map(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXProject"))?,
            ),
            "PBXResourcesBuildPhase" => {
                Self::ResourcesBuildPhase(PBXResourcesBuildPhase::decode_map(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXResourcesBuildPhase"),
                )?)
            }
            "PBXSourcesBuildPhase" => {
                Self::SourcesBuildPhase(PBXSourcesBuildPhase::decode_map(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXSourcesBuildPhase"),
                )?)
            }
            "PBXVariantGroup" => PBXObject::VariantGroup(
                PBXVariantGroup::decode_map(xs)
                    .map_err(DecodeError::failed_parsing_typed_object("PBXVariantGroup"))?,
            ),
            "XCBuildConfiguration" => {
                Self::BuildConfiguration(XCBuildConfiguration::decode_map(xs).map_err(
                    DecodeError::failed_parsing_typed_object("XCBuildConfiguration"),
                )?)
            }
            "XCConfigurationList" => {
                Self::ConfigurationList(XCConfigurationList::decode_map(xs).map_err(
                    DecodeError::failed_parsing_typed_object("XCConfigurationList"),
                )?)
            }
            "PBXCopyFilesBuildPhase" => {
                Self::CopyFilesBuildPhase(PBXCopyFilesBuildPhase::decode_map(xs).map_err(
                    DecodeError::failed_parsing_typed_object("PBXCopyFilesBuildPhase"),
                )?)
            }
            _ => Self::Unknown {
                isa: Value::Single(isa),
                attributes: xs,
            },
        })
    }
}
impl<'s> PBXObject<'s> {
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
impl<'s> DecodableMap<'s> for PBXBuildFile<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            file_ref: xs
                .remove("fileRef")
                .ok_or(DecodeError::MissingRequiredAttr("fileRef"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("fileRef"))?,
            settings: xs
                .remove("settings")
                .map(PBXBuildFileSettings::decode)
                .transpose()?,
            extras: xs,
        })
    }
}
impl<'s> PBXBuildFile<'s> {
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
impl<'s> Decodable<'s> for PBXBuildFileSettings<'s> {
    fn decode(v: Value<'s>) -> Result<Self, DecodeError<'s>> {
        let Value::Map(mut xs) = v else {
            return Err(DecodeError::Unexpected(v));
        };

        Ok(Self {
            attributes: match xs.remove("ATTRIBUTES") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode_single_as_str()
                            .map_err(DecodeError::invalid_attr_element_value("ATTRIBUTES"))
                    })
                    .collect::<Result<_, _>>(),
                Some(x) => Err(DecodeError::unexpected_attr_value("ATTRIBUTES", x)),
            }?,
            extras: xs,
        })
    }
}
impl<'s> PBXBuildFileSettings<'s> {
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
impl<'s> DecodableMap<'s> for PBXFileReference<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            last_known_file_type: xs
                .remove("lastKnownFileType")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("lastKnownFileType"))
                })
                .transpose()?,
            name: xs
                .remove("name")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("name"))
                })
                .transpose()?,
            path: xs
                .remove("path")
                .ok_or(DecodeError::MissingRequiredAttr("path"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("path"))?,
            source_tree: xs
                .remove("sourceTree")
                .ok_or(DecodeError::MissingRequiredAttr("sourceTree"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("sourceTree"))?,
            extras: xs,
        })
    }
}
impl<'s> PBXFileReference<'s> {
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
impl<'s> DecodableMap<'s> for PBXFrameworksBuildPhase<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            build_action_mask: match xs.remove("buildActionMask") {
                Some(Value::Single(x)) => Ok(x.parse::<u32>().expect("cannot parse as u32")),
                Some(x) => Err(DecodeError::unexpected_attr_value("buildActionMask", x)),
                None => Err(DecodeError::MissingRequiredAttr("buildActionMask")),
            }?,
            files: match xs.remove("files") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| match x {
                        Value::Single(x) => Ok(x),
                        x => Err(DecodeError::unexpected_attr_element_value("files", x)),
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("files", x)),
            }?,
            extras: xs,
        })
    }
}
impl<'s> PBXFrameworksBuildPhase<'s> {
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
impl<'s> DecodableMap<'s> for PBXGroup<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            children: match xs.remove("children") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode()
                            .map_err(DecodeError::invalid_attr_element_value("children"))
                    })
                    .collect::<Result<_, _>>(),
                Some(x) => Err(DecodeError::unexpected_attr_value("children", x)),
            }?,
            source_tree: xs
                .remove("sourceTree")
                .ok_or(DecodeError::MissingRequiredAttr("sourceTree"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("sourceTree"))?,
            name: xs
                .remove("name")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("name"))
                })
                .transpose()?,
            path: xs
                .remove("path")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("path"))
                })
                .transpose()?,
            extras: xs,
        })
    }
}
impl<'s> PBXGroup<'s> {
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
impl<'s> DecodableMap<'s> for PBXNativeTarget<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            name: xs
                .remove("name")
                .ok_or(DecodeError::MissingRequiredAttr("name"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("name"))?,
            product_reference: xs
                .remove("productReference")
                .ok_or(DecodeError::MissingRequiredAttr("productReference"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("productReference"))?,
            product_type: xs
                .remove("productType")
                .ok_or(DecodeError::MissingRequiredAttr("productType"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("productType"))?,
            build_phases: match xs.remove("buildPhases") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode()
                            .map_err(DecodeError::invalid_attr_element_value("buildPhases"))
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("buildPhaess", x)),
            }?,
            build_rules: match xs.remove("buildRules") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode()
                            .map_err(DecodeError::invalid_attr_element_value("buildRules"))
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("buildRules", x)),
            }?,
            build_configuration_list: xs
                .remove("buildConfigurationList")
                .ok_or(DecodeError::MissingRequiredAttr("buildConfigurationList"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("buildConfigurationList"))?,
            product_name: xs
                .remove("productName")
                .ok_or(DecodeError::MissingRequiredAttr("productName"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("productName"))?,
            dependencies: match xs.remove("dependencies") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode()
                            .map_err(DecodeError::invalid_attr_element_value("dependencies"))
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("dependencies", x)),
            }?,
            extras: xs,
        })
    }
}
impl<'s> PBXNativeTarget<'s> {
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
    pub build_configuration_list: PBXTypedObjectIDRef<'s, XCConfigurationList<'s>>,
    pub targets: Vec<PBXObjectIDRef<'s>>,
    pub project_dir_path: Option<Cow<'s, str>>,
    pub main_group: PBXTypedObjectIDRef<'s, PBXGroup<'s>>,
    pub development_region: Option<Cow<'s, str>>,
    pub product_ref_group: PBXTypedObjectIDRef<'s, PBXGroup<'s>>,
    pub project_root: Option<Cow<'s, str>>,
    pub extras: HashMap<&'s str, Value<'s>>,
}
impl<'s> DecodableMap<'s> for PBXProject<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            build_configuration_list: xs
                .remove("buildConfigurationList")
                .ok_or(DecodeError::MissingRequiredAttr("buildConfigurationList"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("buildConfigurationList"))?,
            targets: match xs.remove("targets") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode()
                            .map_err(DecodeError::invalid_attr_element_value("targets"))
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("targets", x)),
            }?,
            project_dir_path: xs
                .remove("projectDirPath")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("projectDirPath"))
                })
                .transpose()?,
            main_group: xs
                .remove("mainGroup")
                .ok_or(DecodeError::MissingRequiredAttr("mainGroup"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("mainGroup"))?,
            development_region: xs
                .remove("developmentRegion")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("developmentRegion"))
                })
                .transpose()?,
            product_ref_group: xs
                .remove("productRefGroup")
                .ok_or(DecodeError::MissingRequiredAttr("productRefGroup"))?
                .decode()
                .map_err(DecodeError::invalid_attr_value("productRefGroup"))?,
            project_root: xs
                .remove("projectRoot")
                .map(|x| {
                    x.decode_single_as_str()
                        .map_err(DecodeError::invalid_attr_value("projectRoot value"))
                })
                .transpose()?,
            extras: xs,
        })
    }
}
impl<'s> PBXProject<'s> {
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
impl<'s> DecodableMap<'s> for PBXResourcesBuildPhase<'s> {
    fn decode_map(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
    }
}
impl<'s> PBXResourcesBuildPhase<'s> {
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
impl<'s> DecodableMap<'s> for PBXSourcesBuildPhase<'s> {
    fn decode_map(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
    }
}
impl<'s> PBXSourcesBuildPhase<'s> {
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
impl<'s> DecodableMap<'s> for PBXVariantGroup<'s> {
    fn decode_map(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
    }
}
impl<'s> PBXVariantGroup<'s> {
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
impl<'s> DecodableMap<'s> for XCBuildConfiguration<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            name: xs
                .remove("name")
                .ok_or(DecodeError::MissingRequiredAttr("name"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("name"))?,
            build_settings: match xs.remove("buildSettings") {
                None => Ok(HashMap::new()),
                Some(Value::Map(xs)) => Ok(xs),
                Some(x) => Err(DecodeError::unexpected_attr_value("buildSettings", x)),
            }?,
            extras: xs,
        })
    }
}
impl<'s> XCBuildConfiguration<'s> {
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
impl<'s> DecodableMap<'s> for XCConfigurationList<'s> {
    fn decode_map(mut xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self {
            default_configuration_name: xs
                .remove("defaultConfigurationName")
                .ok_or(DecodeError::MissingRequiredAttr("defaultConfigurationName"))?
                .decode_single_as_str()
                .map_err(DecodeError::invalid_attr_value("defaultConfigurationName"))?,
            build_configurations: match xs.remove("buildConfigurations") {
                None => Ok(Vec::new()),
                Some(Value::Array(xs)) => xs
                    .into_iter()
                    .map(|x| {
                        x.decode().map_err(DecodeError::invalid_attr_element_value(
                            "buildConfigurations",
                        ))
                    })
                    .collect(),
                Some(x) => Err(DecodeError::unexpected_attr_value("buildConfigurations", x)),
            }?,
            extras: xs,
        })
    }
}
impl<'s> XCConfigurationList<'s> {
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
impl<'s> DecodableMap<'s> for PBXCopyFilesBuildPhase<'s> {
    #[inline(always)]
    fn decode_map(xs: HashMap<&'s str, Value<'s>>) -> Result<Self, DecodeError<'s>> {
        Ok(Self { extras: xs })
    }
}
impl<'s> PBXCopyFilesBuildPhase<'s> {
    pub fn encode(self) -> Value<'s> {
        let mut xs = HashMap::with_capacity(1 + self.extras.len());
        xs.insert("isa", Value::Single("PBXCopyFilesBuildPhase".into()));
        xs.extend(self.extras);

        Value::Map(xs)
    }
}
