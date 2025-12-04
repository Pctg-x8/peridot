use std::{collections::HashMap, ops::Range, path::PathBuf, str::FromStr};

pub struct SourceAssetSprite {
    pub left: u32,
    pub top: u32,
    pub border_left: u32,
    pub border_top: u32,
    pub border_right: u32,
    pub border_bottom: u32,
    pub source_file_path: PathBuf,
    pub name: String,
}

pub struct SpriteAtlasSourceAsset {
    pub width: u32,
    pub height: u32,
    pub sprites: HashMap<String, SourceAssetSprite>,
}

#[derive(Debug)]
pub enum Record {
    Configuration(ConfigurationRecord),
    Sprite(SpriteRecord),
}

#[derive(Debug, Clone)]
pub struct ConfigurationRecord {
    pub width: u32,
    pub height: u32,
}

#[derive(Debug, Clone)]
pub struct SpriteRecord {
    pub id: String,
    pub left: u32,
    pub top: u32,
    pub border_left: u32,
    pub border_top: u32,
    pub border_right: u32,
    pub border_bottom: u32,
    pub source_file_path: PathBuf,
    pub name: String,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("unexpected eof reached")]
    UnexpectedEof,
    #[error("unexpected newline")]
    UnexpectedNewline,
    #[error("invalid value")]
    InvalidValue,
    #[error("missing configuration: {0}")]
    MissingConfiguration(&'static str),
    #[error("missing sprite parameter: {0}")]
    MissingSpriteParam(&'static str),
}

enum ParseState {
    Start {
        content_buffer: String,
    },
    ReadConfiguration {
        content_byte_ranges: Vec<Range<usize>>,
        content_head_bytes: usize,
    },
    ReadSprite {
        id: String,
        content_byte_ranges: Vec<Range<usize>>,
        content_head_bytes: usize,
    },
}

pub struct Parser<'s> {
    source: &'s str,
    source_chars: std::str::Chars<'s>,
    pointer_bytes: usize,
    state: ParseState,
}
impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            source_chars: source.chars(),
            pointer_bytes: 0,
            state: ParseState::Start {
                content_buffer: String::new(),
            },
        }
    }
}
impl<'s> Iterator for Parser<'s> {
    type Item = Result<Record, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            ParseState::Start {
                ref mut content_buffer,
            } => match self.source_chars.next() {
                Some(c @ '=') => {
                    self.pointer_bytes += c.len_utf8();
                    let id = core::mem::replace(content_buffer, String::new());
                    if id == "?" {
                        self.state = ParseState::ReadConfiguration {
                            content_byte_ranges: Vec::with_capacity(2),
                            content_head_bytes: self.pointer_bytes,
                        };
                    } else {
                        self.state = ParseState::ReadSprite {
                            id,
                            content_byte_ranges: Vec::with_capacity(8),
                            content_head_bytes: self.pointer_bytes,
                        };
                    }

                    self.next()
                }
                Some(c @ '\n') => {
                    self.pointer_bytes += c.len_utf8();
                    content_buffer.clear();

                    Some(Err(ParseError::UnexpectedNewline))
                }
                Some(c) => {
                    self.pointer_bytes += c.len_utf8();
                    content_buffer.push(c);
                    self.next()
                }
                None if content_buffer.is_empty() => None,
                None => Some(Err(ParseError::UnexpectedEof)),
            },
            ParseState::ReadConfiguration {
                ref mut content_byte_ranges,
                ref mut content_head_bytes,
            } => match self.source_chars.next() {
                Some(c @ '\n') => {
                    if self.pointer_bytes > *content_head_bytes {
                        content_byte_ranges.push(*content_head_bytes..self.pointer_bytes);
                    }
                    self.pointer_bytes += c.len_utf8();
                    let ParseState::ReadConfiguration {
                        content_byte_ranges,
                        ..
                    } = core::mem::replace(
                        &mut self.state,
                        ParseState::Start {
                            content_buffer: String::new(),
                        },
                    )
                    else {
                        unreachable!()
                    };

                    let mut contents_iter = content_byte_ranges.into_iter();
                    let width = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingConfiguration("width"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let height = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingConfiguration("height"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };

                    Some(Ok(Record::Configuration(ConfigurationRecord {
                        width,
                        height,
                    })))
                }
                Some(c @ ',') => {
                    content_byte_ranges.push(*content_head_bytes..self.pointer_bytes);
                    self.pointer_bytes += c.len_utf8();
                    *content_head_bytes = self.pointer_bytes;
                    self.next()
                }
                Some(c) => {
                    self.pointer_bytes += c.len_utf8();
                    self.next()
                }
                None => {
                    if self.pointer_bytes > *content_head_bytes {
                        content_byte_ranges.push(*content_head_bytes..self.pointer_bytes);
                    }
                    let ParseState::ReadConfiguration {
                        content_byte_ranges,
                        ..
                    } = core::mem::replace(
                        &mut self.state,
                        ParseState::Start {
                            content_buffer: String::new(),
                        },
                    )
                    else {
                        unreachable!()
                    };

                    let mut contents_iter = content_byte_ranges.into_iter();
                    let width = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingConfiguration("width"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let height = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingConfiguration("height"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };

                    Some(Ok(Record::Configuration(ConfigurationRecord {
                        width,
                        height,
                    })))
                }
            },
            ParseState::ReadSprite {
                ref mut content_byte_ranges,
                ref mut content_head_bytes,
                ..
            } => match self.source_chars.next() {
                Some(c @ '\n') => {
                    if self.pointer_bytes > *content_head_bytes {
                        content_byte_ranges.push(*content_head_bytes..self.pointer_bytes);
                    }
                    self.pointer_bytes += c.len_utf8();
                    let ParseState::ReadSprite {
                        id,
                        content_byte_ranges,
                        ..
                    } = core::mem::replace(
                        &mut self.state,
                        ParseState::Start {
                            content_buffer: String::new(),
                        },
                    )
                    else {
                        unreachable!()
                    };

                    let mut contents_iter = content_byte_ranges.into_iter();
                    let border_left = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_left"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let border_top = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_top"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let border_right = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_right"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let border_bottom = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_bottom"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let left = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("left"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let top = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("top"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let source_file_path = match PathBuf::from_str(
                        self.source[match contents_iter.next() {
                            Some(x) => x,
                            None => {
                                return Some(Err(ParseError::MissingSpriteParam("source_path")));
                            }
                        }]
                        .trim(),
                    ) {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let name = self.source[match contents_iter.next() {
                        Some(x) => x,
                        None => return Some(Err(ParseError::MissingSpriteParam("name"))),
                    }]
                    .to_owned();

                    Some(Ok(Record::Sprite(SpriteRecord {
                        id,
                        border_left,
                        border_top,
                        border_right,
                        border_bottom,
                        left,
                        top,
                        source_file_path,
                        name,
                    })))
                }
                Some(c @ ',') => {
                    content_byte_ranges.push(*content_head_bytes..self.pointer_bytes);
                    self.pointer_bytes += c.len_utf8();
                    *content_head_bytes = self.pointer_bytes;
                    self.next()
                }
                Some(c) => {
                    self.pointer_bytes += c.len_utf8();
                    self.next()
                }
                None => {
                    if self.pointer_bytes > *content_head_bytes {
                        content_byte_ranges.push(*content_head_bytes..self.pointer_bytes);
                    }
                    let ParseState::ReadSprite {
                        id,
                        content_byte_ranges,
                        ..
                    } = core::mem::replace(
                        &mut self.state,
                        ParseState::Start {
                            content_buffer: String::new(),
                        },
                    )
                    else {
                        unreachable!()
                    };

                    let mut contents_iter = content_byte_ranges.into_iter();
                    let border_left = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_left"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let border_top = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_top"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let border_right = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_right"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let border_bottom = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("border_bottom"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let left = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("left"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let top = match self.source[match contents_iter
                        .next()
                        .ok_or(ParseError::MissingSpriteParam("top"))
                    {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    }]
                    .trim()
                    .parse()
                    {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let source_file_path = match PathBuf::from_str(
                        self.source[match contents_iter.next() {
                            Some(x) => x,
                            None => {
                                return Some(Err(ParseError::MissingSpriteParam("source_path")));
                            }
                        }]
                        .trim(),
                    ) {
                        Ok(x) => x,
                        Err(_) => return Some(Err(ParseError::InvalidValue)),
                    };
                    let name = self.source[match contents_iter.next() {
                        Some(x) => x,
                        None => return Some(Err(ParseError::MissingSpriteParam("name"))),
                    }]
                    .to_owned();

                    Some(Ok(Record::Sprite(SpriteRecord {
                        id,
                        border_left,
                        border_top,
                        border_right,
                        border_bottom,
                        left,
                        top,
                        source_file_path,
                        name,
                    })))
                }
            },
        }
    }
}
