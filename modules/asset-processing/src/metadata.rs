use std::{ops::Range, str::Chars};

#[derive(Clone, Copy)]
enum ParserState {
    Key,
    Value,
    Finished,
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("no key(at line {line})")]
    NoKey { line: usize },
}

pub struct Parser<'s> {
    content: &'s str,
    content_chars: Chars<'s>,
    state: ParserState,
    key_buffer: String,
    pointer_bytes: usize,
    pointer_line: usize,
    value_byte_range: Range<usize>,
}
impl<'s> Parser<'s> {
    pub fn new(content: &'s str) -> Self {
        Self {
            content,
            content_chars: content.chars(),
            state: ParserState::Key,
            key_buffer: String::new(),
            pointer_bytes: 0,
            pointer_line: 1,
            value_byte_range: 0..0,
        }
    }
}
impl<'s> Iterator for Parser<'s> {
    type Item = Result<(String, &'s str), ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.state {
            ParserState::Finished => None,
            ParserState::Key => match self.content_chars.next() {
                Some(c @ '=') => {
                    self.pointer_bytes += c.len_utf8();
                    self.state = ParserState::Value;
                    self.value_byte_range = self.pointer_bytes..self.pointer_bytes;

                    self.next()
                }
                Some(c @ '\n') if self.key_buffer.is_empty() => {
                    // empty line
                    self.pointer_bytes += c.len_utf8();
                    self.pointer_line += 1;
                    self.state = ParserState::Value;

                    self.next()
                }
                Some(c @ '\n') => {
                    // recover to next line
                    self.pointer_bytes += c.len_utf8();
                    self.pointer_line += 1;
                    self.state = ParserState::Key;
                    self.key_buffer.clear();

                    Some(Err(ParseError::NoKey {
                        line: self.pointer_line,
                    }))
                }
                Some(c) => {
                    self.pointer_bytes += c.len_utf8();
                    self.key_buffer.push(c);

                    self.next()
                }
                None if self.key_buffer.is_empty() => {
                    // empty final line
                    self.state = ParserState::Finished;

                    None
                }
                None => {
                    self.state = ParserState::Finished;

                    Some(Err(ParseError::NoKey {
                        line: self.pointer_line,
                    }))
                }
            },
            ParserState::Value => match self.content_chars.next() {
                Some(c @ '\n') => {
                    self.pointer_bytes += c.len_utf8();
                    self.pointer_line += 1;
                    self.state = ParserState::Value;

                    let new_buffer_cap = self.key_buffer.capacity();
                    let key = core::mem::replace(
                        &mut self.key_buffer,
                        String::with_capacity(new_buffer_cap),
                    );
                    let value = &self.content[self.value_byte_range.clone()];

                    Some(Ok((key, value)))
                }
                Some(c) => {
                    self.pointer_bytes += c.len_utf8();
                    self.value_byte_range.end += c.len_utf8();

                    self.next()
                }
                None => {
                    self.state = ParserState::Finished;

                    let new_buffer_cap = self.key_buffer.capacity();
                    let key = core::mem::replace(
                        &mut self.key_buffer,
                        String::with_capacity(new_buffer_cap),
                    );
                    let value = &self.content[self.value_byte_range.clone()];

                    Some(Ok((key, value)))
                }
            },
        }
    }
}
