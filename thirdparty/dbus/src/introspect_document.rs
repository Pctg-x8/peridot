use std::borrow::Cow;

use quick_xml::events::{BytesStart, Event as XmlEvent};

pub enum InterfaceElementContent<'a> {
    Method {
        name: Cow<'a, [u8]>,
        empty: bool,
    },
    Signal {
        name: Cow<'a, [u8]>,
        empty: bool,
    },
    Property {
        name: Cow<'a, [u8]>,
        r#type: Cow<'a, [u8]>,
        access: Cow<'a, [u8]>,
    },
}
pub enum MethodSignalElementContent<'a> {
    Arg {
        name: Cow<'a, [u8]>,
        r#type: Cow<'a, [u8]>,
        direction: Option<Cow<'a, [u8]>>,
    },
    Annotation {
        name: Cow<'a, [u8]>,
        value: Cow<'a, [u8]>,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum ReadError<'a> {
    #[error("read_event failed: {0}")]
    ReadEventFailed(quick_xml::Error),
    #[error("no {name} attr in {tag} tag")]
    MissingRequiredAttr {
        name: &'static str,
        tag: &'static str,
    },
    #[error("unexpected {phase} element: {event:?}")]
    UnexpectedElement {
        phase: &'static str,
        event: quick_xml::events::Event<'a>,
    },
    #[error("invalid pairing of </{tag}>")]
    InvalidTagPairing { tag: &'static str },
    #[error("invalid pairing of {tag:?}")]
    InvalidTagPairingString {
        tag: Result<String, std::string::FromUtf8Error>,
    },
    #[error("invalid nested tag {tag}")]
    InvalidNestedTag { tag: &'static str },
    #[error("attribute error: {0}")]
    Attribute(#[from] quick_xml::events::attributes::AttrError),
}

pub fn read_toplevel<'a>(
    reader: &mut quick_xml::Reader<&'a [u8]>,
    mut callback: impl for<'b> FnMut(
        &[Option<Result<String, quick_xml::Error>>],
        Cow<'b, [u8]>,
        &mut quick_xml::Reader<&'a [u8]>,
    ) -> Result<(), ReadError<'a>>,
) -> Result<(), ReadError<'a>> {
    let mut node_paths = Vec::new();

    loop {
        match reader.read_event().map_err(ReadError::ReadEventFailed)? {
            XmlEvent::DocType(x) => {
                tracing::debug!(doctype = ?x.xml_content(), "TODO: parse head doctype");
                break;
            }
            XmlEvent::Comment(_) => (),
            e => {
                return Err(ReadError::UnexpectedElement {
                    phase: "introspection heading",
                    event: e,
                });
            }
        }
    }

    loop {
        match reader.read_event().map_err(ReadError::ReadEventFailed)? {
            XmlEvent::Start(x) if x.name().0 == b"node" => {
                let mut name = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.unescape_value().map(Into::into));
                    }
                }

                node_paths.push(name);
            }
            XmlEvent::Start(x) if x.name().0 == b"interface" => {
                let mut name = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.value);
                    }
                }

                callback(
                    &node_paths,
                    name.ok_or(ReadError::MissingRequiredAttr {
                        name: "name",
                        tag: "interface",
                    })?,
                    reader,
                )?;
            }
            XmlEvent::End(x) if x.name().0 == b"node" => {
                if node_paths.pop().is_none() {
                    return Err(ReadError::InvalidTagPairing { tag: "node" });
                }
            }
            XmlEvent::Text(x) if x.trim_ascii().is_empty() => (),
            XmlEvent::Comment(_) => (),
            XmlEvent::Eof => break,
            e => {
                return Err(ReadError::UnexpectedElement {
                    phase: "toplevel",
                    event: e,
                });
            }
        }
    }

    Ok(())
}

pub fn skip_read_interface_tag_contents<'a>(
    reader: &mut quick_xml::Reader<&'a [u8]>,
) -> Result<(), ReadError<'a>> {
    loop {
        match reader.read_event().map_err(ReadError::ReadEventFailed)? {
            XmlEvent::Start(x) if x.name().0 == b"interface" => {
                return Err(ReadError::InvalidNestedTag { tag: "interface" });
            }
            XmlEvent::End(x) if x.name().0 == b"interface" => return Ok(()),
            XmlEvent::End(x) => {
                return Err(ReadError::InvalidTagPairingString {
                    tag: String::from_utf8(x.name().0.into()),
                });
            }
            e @ XmlEvent::Eof => {
                return Err(ReadError::UnexpectedElement {
                    phase: "interface(skipping)",
                    event: e,
                });
            }
            _ => (),
        }
    }
}

pub fn read_interface_tag_content<'a>(
    reader: &mut quick_xml::Reader<&'a [u8]>,
    mut callback: impl for<'b> FnMut(
        InterfaceElementContent<'b>,
        &mut quick_xml::Reader<&'a [u8]>,
    ) -> Result<(), ReadError<'a>>,
) -> Result<(), ReadError<'a>> {
    loop {
        match reader.read_event().map_err(ReadError::ReadEventFailed)? {
            XmlEvent::Start(x) if x.name().0 == b"interface" => {
                return Err(ReadError::InvalidNestedTag { tag: "interface" });
            }
            XmlEvent::Start(x) if x.name().0 == b"method" => {
                let mut name = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.value);
                    }
                }

                callback(
                    InterfaceElementContent::Method {
                        name: name.ok_or(ReadError::MissingRequiredAttr {
                            name: "name",
                            tag: "method",
                        })?,
                        empty: false,
                    },
                    reader,
                )?;
            }
            XmlEvent::Start(x) if x.name().0 == b"signal" => {
                let mut name = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.value);
                    }
                }

                callback(
                    InterfaceElementContent::Signal {
                        name: name.ok_or(ReadError::MissingRequiredAttr {
                            name: "name",
                            tag: "signal",
                        })?,
                        empty: false,
                    },
                    reader,
                )?;
            }
            XmlEvent::Empty(x) if x.name().0 == b"method" => {
                let mut name = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.value);
                    }
                }

                callback(
                    InterfaceElementContent::Method {
                        name: name.ok_or(ReadError::MissingRequiredAttr {
                            name: "name",
                            tag: "method",
                        })?,
                        empty: true,
                    },
                    reader,
                )?;
            }
            XmlEvent::Empty(x) if x.name().0 == b"signal" => {
                let mut name = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.value);
                    }
                }

                callback(
                    InterfaceElementContent::Signal {
                        name: name.ok_or(ReadError::MissingRequiredAttr {
                            name: "name",
                            tag: "signal",
                        })?,
                        empty: true,
                    },
                    reader,
                )?;
            }
            XmlEvent::Empty(x) if x.name().0 == b"property" => {
                let mut name = None;
                let mut r#type = None;
                let mut access = None;
                for a in x.attributes() {
                    let a = a?;

                    if a.key.0 == b"name" {
                        name = Some(a.value);
                    } else if a.key.0 == b"type" {
                        r#type = Some(a.value);
                    } else if a.key.0 == b"access" {
                        access = Some(a.value);
                    }
                }

                callback(
                    InterfaceElementContent::Property {
                        name: name.ok_or(ReadError::MissingRequiredAttr {
                            name: "name",
                            tag: "property",
                        })?,
                        r#type: r#type.ok_or(ReadError::MissingRequiredAttr {
                            name: "type",
                            tag: "property",
                        })?,
                        access: access.ok_or(ReadError::MissingRequiredAttr {
                            name: "access",
                            tag: "property",
                        })?,
                    },
                    reader,
                )?;
            }
            XmlEvent::End(x) if x.name().0 == b"interface" => return Ok(()),
            XmlEvent::End(x) => {
                return Err(ReadError::InvalidTagPairingString {
                    tag: String::from_utf8(x.name().0.into()),
                });
            }
            XmlEvent::Text(x) if x.trim_ascii().is_empty() => (),
            e => {
                return Err(ReadError::UnexpectedElement {
                    phase: "interface",
                    event: e,
                });
            }
        }
    }
}

pub fn read_method_tag_content<'a>(
    reader: &mut quick_xml::Reader<&'a [u8]>,
    mut callback: impl for<'b> FnMut(
        MethodSignalElementContent<'b>,
        &mut quick_xml::Reader<&'a [u8]>,
    ) -> Result<(), ReadError<'a>>,
) -> Result<(), ReadError<'a>> {
    loop {
        match reader.read_event().map_err(ReadError::ReadEventFailed)? {
            XmlEvent::Empty(x) if x.name().0 == b"arg" => {
                let content = ArgTag::parse(&x)?;

                callback(
                    MethodSignalElementContent::Arg {
                        name: content.name,
                        r#type: content.r#type,
                        direction: content.direction,
                    },
                    reader,
                )?;
            }
            XmlEvent::Empty(x) if x.name().0 == b"annotation" => {
                let content = AnnotationTag::parse(&x)?;

                callback(
                    MethodSignalElementContent::Annotation {
                        name: content.name,
                        value: content.value,
                    },
                    reader,
                )?;
            }
            XmlEvent::End(x) => {
                if x.name().0 == b"method" {
                    break;
                }

                return Err(ReadError::InvalidTagPairingString {
                    tag: String::from_utf8(x.name().0.into()),
                });
            }
            XmlEvent::Text(x) if x.trim_ascii().is_empty() => (),
            e => {
                return Err(ReadError::UnexpectedElement {
                    phase: "method",
                    event: e,
                });
            }
        }
    }

    Ok(())
}

pub fn read_signal_tag_content<'a>(
    reader: &mut quick_xml::Reader<&'a [u8]>,
    mut callback: impl for<'b> FnMut(
        MethodSignalElementContent<'b>,
        &mut quick_xml::Reader<&'a [u8]>,
    ) -> Result<(), ReadError<'a>>,
) -> Result<(), ReadError<'a>> {
    loop {
        match reader.read_event().map_err(ReadError::ReadEventFailed)? {
            XmlEvent::Empty(x) if x.name().0 == b"arg" => {
                let content = ArgTag::parse(&x)?;

                callback(
                    MethodSignalElementContent::Arg {
                        name: content.name,
                        r#type: content.r#type,
                        direction: content.direction,
                    },
                    reader,
                )?;
            }
            XmlEvent::Empty(x) if x.name().0 == b"annotation" => {
                let content = AnnotationTag::parse(&x)?;

                callback(
                    MethodSignalElementContent::Annotation {
                        name: content.name,
                        value: content.value,
                    },
                    reader,
                )?;
            }
            XmlEvent::End(x) => {
                if x.name().0 == b"signal" {
                    break;
                }

                return Err(ReadError::InvalidTagPairingString {
                    tag: String::from_utf8(x.name().0.into()),
                });
            }
            XmlEvent::Text(x) if x.trim_ascii().is_empty() => (),
            e => {
                return Err(ReadError::UnexpectedElement {
                    phase: "signal",
                    event: e,
                });
            }
        }
    }

    Ok(())
}

#[derive(Debug, Clone)]
pub struct ArgTag<'a> {
    pub name: Cow<'a, [u8]>,
    pub r#type: Cow<'a, [u8]>,
    pub direction: Option<Cow<'a, [u8]>>,
}
impl<'b> ArgTag<'b> {
    pub fn parse<'a>(content: &'b BytesStart<'a>) -> Result<Self, ReadError<'a>> {
        let mut name = None;
        let mut r#type = None;
        let mut direction = None;
        for a in content.attributes() {
            let a = a?;

            if a.key.0 == b"name" {
                name = Some(a.value);
            } else if a.key.0 == b"type" {
                r#type = Some(a.value);
            } else if a.key.0 == b"direction" {
                direction = Some(a.value);
            }
        }

        Ok(Self {
            name: name.ok_or(ReadError::MissingRequiredAttr {
                name: "name",
                tag: "arg",
            })?,
            r#type: r#type.ok_or(ReadError::MissingRequiredAttr {
                name: "type",
                tag: "arg",
            })?,
            direction,
        })
    }
}

#[derive(Debug, Clone)]
pub struct AnnotationTag<'a> {
    pub name: Cow<'a, [u8]>,
    pub value: Cow<'a, [u8]>,
}
impl<'b> AnnotationTag<'b> {
    pub fn parse<'a>(content: &'b BytesStart<'a>) -> Result<Self, ReadError<'a>> {
        let mut name = None;
        let mut r#value = None;
        for a in content.attributes() {
            let a = a?;

            if a.key.0 == b"name" {
                name = Some(a.value);
            } else if a.key.0 == b"value" {
                value = Some(a.value);
            }
        }

        Ok(Self {
            name: name.ok_or(ReadError::MissingRequiredAttr {
                name: "name",
                tag: "annotation",
            })?,
            value: value.ok_or(ReadError::MissingRequiredAttr {
                name: "value",
                tag: "annotation",
            })?,
        })
    }
}
