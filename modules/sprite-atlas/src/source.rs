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
                    self.state = if id == "?" {
                        ParseState::ReadConfiguration {
                            content_byte_ranges: Vec::with_capacity(2),
                            content_head_bytes: self.pointer_bytes,
                        }
                    } else {
                        ParseState::ReadSprite {
                            id,
                            content_byte_ranges: Vec::with_capacity(8),
                            content_head_bytes: self.pointer_bytes,
                        }
                    };

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

                    Some(
                        self.build_configuration_record(content_byte_ranges)
                            .map(Record::Configuration),
                    )
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

                    Some(
                        self.build_configuration_record(content_byte_ranges)
                            .map(Record::Configuration),
                    )
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

                    Some(
                        self.build_sprite_record(id, content_byte_ranges)
                            .map(Record::Sprite),
                    )
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

                    Some(
                        self.build_sprite_record(id, content_byte_ranges)
                            .map(Record::Sprite),
                    )
                }
            },
        }
    }
}
impl<'s> Parser<'s> {
    fn build_configuration_record(
        &self,
        content_byte_ranges: Vec<Range<usize>>,
    ) -> Result<ConfigurationRecord, ParseError> {
        let mut contents_iter = content_byte_ranges.into_iter();

        let width = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingConfiguration("width"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;
        let height = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingConfiguration("height"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;

        Ok(ConfigurationRecord { width, height })
    }

    fn build_sprite_record(
        &self,
        id: String,
        content_byte_ranges: Vec<Range<usize>>,
    ) -> Result<SpriteRecord, ParseError> {
        let mut contents_iter = content_byte_ranges.into_iter();

        let source_file_path = PathBuf::from_str(
            self.source[contents_iter
                .next()
                .ok_or(ParseError::MissingSpriteParam("source_path"))?]
            .trim(),
        )
        .map_err(|_| ParseError::InvalidValue)?;
        let name = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("name"))?]
        .to_owned();
        let left = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("left"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;
        let top = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("top"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;
        let border_left = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("border_left"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;
        let border_top = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("border_top"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;
        let border_right = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("border_right"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;
        let border_bottom = self.source[contents_iter
            .next()
            .ok_or(ParseError::MissingSpriteParam("border_bottom"))?]
        .trim()
        .parse()
        .map_err(|_| ParseError::InvalidValue)?;

        Ok(SpriteRecord {
            id,
            border_left,
            border_top,
            border_right,
            border_bottom,
            left,
            top,
            source_file_path,
            name,
        })
    }
}
