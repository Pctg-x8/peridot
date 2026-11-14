use std::{borrow::Cow, collections::HashMap};

mod pbxproj;
pub use self::pbxproj::*;

pub struct ParserState<'s> {
    source: &'s [u8],
    line: usize,
    col: usize,
}

impl<'s> ParserState<'s> {
    pub fn new(source_str: &'s str) -> Self {
        Self {
            source: source_str.as_bytes(),
            line: 1,
            col: 1,
        }
    }

    pub fn skip_spaces(&mut self) {
        loop {
            if self
                .source
                .first()
                .is_some_and(|&x| x.is_ascii_whitespace() && x != b'\n')
            {
                self.consume_bytes(1);
                self.forward(1);
                continue;
            }

            if self.source.first().is_some_and(|&x| x == b'\n') {
                self.consume_bytes(1);
                self.forward_line(1);
                continue;
            }

            if self.is_starting(b"//") {
                parse_oneline_comment(self);
                continue;
            }

            if self.is_starting(b"/*") {
                parse_multiline_comment(self);
                continue;
            }

            break;
        }
    }

    pub fn is_head_char(&self, ch: u8) -> bool {
        self.source.first() == Some(&ch)
    }

    pub fn is_head(&self, pred: impl FnOnce(u8) -> bool) -> bool {
        self.source.first().is_some_and(|&x| pred(x))
    }

    pub fn expect_head_char(&mut self, ch: u8) -> Result<(), ParseError> {
        if self.is_head_char(ch) {
            self.consume_bytes(1);
            self.forward(1);
            Ok(())
        } else {
            Err(ParseError::UnexpectedCharacter {
                expected: ch,
                line: self.line,
                col: self.col,
            })
        }
    }

    pub fn is_starting(&self, bytes: &[u8]) -> bool {
        self.source.starts_with(bytes)
    }

    pub fn consume_bytes(&mut self, count: usize) {
        self.source = &self.source[count..];
    }

    pub fn forward(&mut self, count: usize) {
        self.col += count;
    }

    pub fn forward_line(&mut self, count: usize) {
        self.col = 1;
        self.line += count;
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedCharacter {
        expected: u8,
        line: usize,
        col: usize,
    },
    ExpectedObjectKey {
        line: usize,
        col: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'s> {
    Single(Cow<'s, str>),
    Array(Vec<Value<'s>>),
    Map(HashMap<&'s str, Value<'s>>),
}
impl<'s> ElementWrite for Value<'s> {
    fn write(&self, w: &mut Writer<impl std::io::Write>) -> std::io::Result<()> {
        match self {
            Self::Single(v) => w.emit_single(v),
            Self::Array(xs) => {
                w.begin_array()?;
                for x in xs {
                    x.write(w)?;
                }
                w.end_array()?;
                Ok(())
            }
            Self::Map(xs) => {
                w.begin_map()?;
                for (k, v) in xs {
                    w.emit_single(k)?;
                    v.write(w)?;
                }
                w.end_map()?;
                Ok(())
            }
        }
    }
}
impl<'s> Value<'s> {
    #[inline(always)]
    pub fn try_into_single_str(self) -> Result<Cow<'s, str>, Self> {
        match self {
            Self::Single(x) => Ok(x),
            x => Err(x),
        }
    }

    fn single_requires_quoted(v: &str) -> bool {
        v.is_empty()
            || v.contains([
                '"', '(', ')', '{', '}', '/', '*', '<', '>', '-', ' ', ';', '=', ',', '+',
            ])
    }

    pub fn write_oneline(&self, sink: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::Single(v) if Self::single_requires_quoted(v) => {
                sink.write_all(b"\"")?;
                for c in v.chars() {
                    if c == '"' {
                        sink.write_all(b"\"")?;
                    }

                    write!(sink, "{c}")?
                }
                sink.write_all(b"\"")?;
            }
            Self::Single(v) => sink.write_all(v.as_bytes())?,
            Self::Array(xs) => {
                sink.write_all(b"(")?;
                for x in xs {
                    x.write_oneline(sink)?;
                    sink.write_all(b", ")?;
                }
                sink.write_all(b")")?;
            }
            Self::Map(xs) => {
                sink.write_all(b"{")?;
                for (k, v) in xs {
                    sink.write_all(k.as_bytes())?;
                    sink.write_all(b" = ")?;
                    v.write_oneline(sink)?;
                    sink.write_all(b"; ")?;
                }
                sink.write_all(b"}")?;
            }
        }

        Ok(())
    }
}

pub fn parse_oneline_comment<'s>(state: &mut ParserState<'s>) -> &'s str {
    let input_slice = state.source;
    if !state.is_starting(b"//") {
        return "";
    }

    state.consume_bytes(2);
    state.forward(2);
    let byte_count = state.source.iter().take_while(|&&c| c != b'\n').count();
    state.consume_bytes(byte_count);
    state.forward(byte_count);

    unsafe { core::str::from_utf8_unchecked(&input_slice[..2 + byte_count]) }
}

pub fn parse_multiline_comment<'s>(state: &mut ParserState<'s>) -> &'s str {
    let input_slice = state.source;
    if !state.is_starting(b"/*") {
        return "";
    }

    state.consume_bytes(2);
    state.forward(2);
    let mut byte_count = 2;
    loop {
        match state.source.get(..2) {
            Some(b"*/") => {
                // end
                state.consume_bytes(2);
                state.forward(2);
                byte_count += 2;
                break;
            }
            Some(xs) if xs.get(1) == Some(&b'*') => {
                // maybe end at +1 pos
                state.consume_bytes(1);
                if xs[0] == b'\n' {
                    state.forward_line(1);
                } else {
                    state.forward(1);
                }
                byte_count += 1;
            }
            Some(xs) => {
                // text
                state.consume_bytes(xs.len());
                byte_count += xs.len();
                match (xs[0], xs.get(1)) {
                    (b'\n', Some(b'\n')) => {
                        // dual line feed
                        state.forward_line(2);
                    }
                    (b'\n', None) | (_, Some(b'\n')) => {
                        // one newline at end
                        state.forward_line(1);
                    }
                    (b'\n', Some(_)) => {
                        // newline followed char
                        state.forward_line(1);
                        state.forward(1);
                    }
                    _ => {
                        // both normal char
                        state.forward(xs.len());
                    }
                }
            }
            None => break,
        }
    }

    unsafe { core::str::from_utf8_unchecked(&input_slice[..byte_count]) }
}

pub fn parse_string<'s>(state: &mut ParserState<'s>) -> Result<&'s str, ParseError> {
    let input_slice = state.source;
    state.expect_head_char(b'"')?;

    let mut slice_length = 1;
    loop {
        match state.source.get(..2) {
            Some(b"\\r") | Some(b"\\n") | Some(b"\\t") | Some(b"\\b") => {
                state.consume_bytes(2);
                slice_length += 2;
                state.forward(2);
            }
            Some(b"\\u") => {
                unimplemented!("unicode char");
            }
            Some(x) if x[0] == b'"' => {
                // end
                state.consume_bytes(1);
                slice_length += 1;
                state.forward(1);

                break Ok(unsafe {
                    core::str::from_utf8_unchecked(&input_slice[1..slice_length - 1])
                });
            }
            Some(x) if x.get(1) == Some(&b'\\') || x.get(1) == Some(&b'"') => {
                state.consume_bytes(1);
                slice_length += 1;
                state.forward(1);
            }
            Some(_) => {
                state.consume_bytes(2);
                slice_length += 2;
                state.forward(2);
            }
            None => {
                break Err(ParseError::UnexpectedCharacter {
                    expected: b'"',
                    line: state.line,
                    col: state.col,
                });
            }
        }
    }
}

pub fn parse_single_val<'s>(state: &mut ParserState<'s>) -> Result<&'s str, ParseError> {
    if state.is_head_char(b'"') {
        return parse_string(state);
    }

    let byte_count = state
        .source
        .iter()
        .take_while(|&x| !x.is_ascii_whitespace() && !b"-;,{}()=".contains(&x))
        .count();
    if byte_count == 0 {
        return Err(ParseError::ExpectedObjectKey {
            line: state.line,
            col: state.col,
        });
    }

    let x = &state.source[..byte_count];
    state.consume_bytes(byte_count);
    state.forward(byte_count);
    Ok(unsafe { core::str::from_utf8_unchecked(x) })
}

pub enum ValueStarting {
    Array,
    Object,
    AnyVal,
}
impl ValueStarting {
    pub const fn determine(state: &ParserState) -> Self {
        match state.source.first() {
            Some(b'(') => Self::Array,
            Some(b'{') => Self::Object,
            _ => Self::AnyVal,
        }
    }
}

pub fn parse_value<'s>(state: &mut ParserState<'s>) -> Result<Value<'s>, ParseError> {
    match state.source.first() {
        Some(b'(') => {
            let mut xs = Vec::new();
            parse_array(state, |st| {
                xs.push(parse_value(st)?);
                Ok(())
            })?;

            Ok(Value::Array(xs))
        }
        Some(b'{') => {
            let mut xs = HashMap::new();
            parse_object(state, |k, state| match xs.entry(k) {
                std::collections::hash_map::Entry::Vacant(v) => {
                    v.insert(parse_value(state)?);
                    Ok(())
                }
                std::collections::hash_map::Entry::Occupied(o) => {
                    panic!("conflicting map key: {}", o.key());
                }
            })?;

            Ok(Value::Map(xs))
        }
        _ => Ok(Value::Single(parse_single_val(state)?.into())),
    }
}

pub fn parse_array<'s>(
    state: &mut ParserState<'s>,
    mut entry_parser: impl FnMut(&mut ParserState<'s>) -> Result<(), ParseError>,
) -> Result<(), ParseError> {
    state.expect_head_char(b'(')?;

    state.skip_spaces();
    while !state.is_head_char(b')') {
        entry_parser(state)?;
        state.skip_spaces();
        // optional comma
        match state.source.first() {
            Some(b')') => break,
            Some(b',') => {
                state.consume_bytes(1);
                state.forward(1);
            }
            _ => {
                return Err(ParseError::UnexpectedCharacter {
                    expected: b',',
                    line: state.line,
                    col: state.col,
                });
            }
        }
        state.skip_spaces();
    }

    state.expect_head_char(b')')?;
    Ok(())
}

pub fn parse_object<'s>(
    state: &mut ParserState<'s>,
    mut entry_parser: impl FnMut(&'s str, &mut ParserState<'s>) -> Result<(), ParseError>,
) -> Result<(), ParseError> {
    state.expect_head_char(b'{')?;

    state.skip_spaces();
    while !state.is_head_char(b'}') {
        let key = parse_single_val(state)?;
        state.skip_spaces();
        state.expect_head_char(b'=')?;
        state.skip_spaces();
        entry_parser(key, state)?;
        state.skip_spaces();
        state.expect_head_char(b';')?;
        state.skip_spaces();
    }

    state.expect_head_char(b'}')?;
    Ok(())
}

pub trait ElementWrite {
    fn write(&self, w: &mut Writer<impl std::io::Write>) -> std::io::Result<()>;
}
impl<'x, T> ElementWrite for &'x T
where
    T: ElementWrite + ?Sized + 'x,
{
    #[inline(always)]
    fn write(&self, w: &mut Writer<impl std::io::Write>) -> std::io::Result<()> {
        T::write(*self, w)
    }
}
impl ElementWrite for str {
    #[inline(always)]
    fn write(&self, w: &mut Writer<impl std::io::Write>) -> std::io::Result<()> {
        w.emit_single(self)
    }
}
impl ElementWrite for String {
    #[inline(always)]
    fn write(&self, w: &mut Writer<impl std::io::Write>) -> std::io::Result<()> {
        w.emit_single(self)
    }
}
impl<'x> ElementWrite for Cow<'x, str> {
    #[inline(always)]
    fn write(&self, w: &mut Writer<impl std::io::Write>) -> std::io::Result<()> {
        w.emit_single(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WritingContext {
    Map,
    Array,
    MapValue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FinalEmit {
    Single,
    Token,
}

pub struct Writer<D> {
    dest: D,
    final_emit: FinalEmit,
    context_stack: Vec<WritingContext>,
}
impl<D> Writer<D> {
    pub fn new(dest: D) -> Self {
        Self {
            dest,
            final_emit: FinalEmit::Token,
            context_stack: Vec::new(),
        }
    }

    fn current_context(&self) -> Option<WritingContext> {
        self.context_stack.last().copied()
    }

    fn push_context(&mut self, ctx: WritingContext) {
        self.context_stack.push(ctx);
    }

    fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    #[inline(always)]
    pub fn emit(&mut self, e: &(impl ElementWrite + ?Sized)) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        e.write(self)
    }

    pub fn emit_value(&mut self, v: &Value<'_>) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        match v {
            Value::Single(x) => self.emit_single(&x),
            Value::Array(xs) => {
                self.begin_array()?;
                for x in xs {
                    self.emit_value(x)?;
                }
                self.end_array()?;
                Ok(())
            }
            Value::Map(xs) => {
                self.begin_map()?;
                for (k, v) in xs {
                    self.emit_single(k)?;
                    self.emit_value(v)?;
                }
                self.end_map()?;
                Ok(())
            }
        }
    }

    #[inline(always)]
    pub fn emit_raw_map_entries<'m, 'v>(
        &mut self,
        entries: impl IntoIterator<Item = (&'m &'v str, &'m Value<'v>)>,
    ) -> std::io::Result<()>
    where
        D: std::io::Write,
        'v: 'm,
    {
        for (k, v) in entries {
            self.emit_map_entry(k, v)?;
        }

        Ok(())
    }

    pub fn begin_map(&mut self) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        if self.current_context() == Some(WritingContext::MapValue) {
            self.dest.write_all(b" = ")?;
        }

        self.dest.write_all(b"{")?;
        self.final_emit = FinalEmit::Token;
        self.push_context(WritingContext::Map);
        Ok(())
    }

    pub fn end_map(&mut self) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        assert_eq!(
            self.current_context(),
            Some(WritingContext::Map),
            "not in a map context"
        );

        self.dest.write_all(b"}")?;
        self.final_emit = FinalEmit::Token;
        self.pop_context();
        if self.current_context() == Some(WritingContext::MapValue) {
            self.dest.write_all(b";")?;
            self.pop_context();
        }
        Ok(())
    }

    pub fn begin_array(&mut self) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        if self.current_context() == Some(WritingContext::MapValue) {
            self.dest.write_all(b" = ")?;
        }

        self.dest.write_all(b"(")?;
        self.final_emit = FinalEmit::Token;
        self.push_context(WritingContext::Array);
        Ok(())
    }

    pub fn end_array(&mut self) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        assert_eq!(
            self.current_context(),
            Some(WritingContext::Array),
            "not in a array context"
        );

        self.dest.write_all(b")")?;
        self.final_emit = FinalEmit::Token;
        self.pop_context();
        if self.current_context() == Some(WritingContext::MapValue) {
            self.dest.write_all(b";")?;
            self.pop_context();
        }
        Ok(())
    }

    #[inline]
    pub fn emit_array<'v>(
        &mut self,
        xs: impl IntoIterator<Item = &'v (impl ElementWrite + ?Sized + 'v)>,
    ) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        self.begin_array()?;
        for x in xs {
            x.write(self)?;
        }
        self.end_array()?;
        Ok(())
    }

    #[inline]
    pub fn emit_map_entry(
        &mut self,
        key: &str,
        value: &(impl ElementWrite + ?Sized),
    ) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        self.emit_single(key)?;
        self.emit(value)?;
        Ok(())
    }

    #[inline]
    pub fn emit_some_map_entry(
        &mut self,
        key: &str,
        v: Option<&(impl ElementWrite + ?Sized)>,
    ) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        let Some(v) = v else {
            return Ok(());
        };

        self.emit_single(key)?;
        self.emit(v)?;
        Ok(())
    }

    pub fn emit_singles(&mut self, vs: &[&str]) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        for x in vs {
            self.emit_single(x)?;
        }

        Ok(())
    }

    pub fn emit_single(&mut self, v: &str) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        match self.current_context() {
            None => panic!("single cannot emit on toplevel"),
            Some(WritingContext::Array) => {
                if self.final_emit == FinalEmit::Single {
                    self.dest.write_all(b", ")?;
                }
                self.emit_single_core(v)?;
                self.final_emit = FinalEmit::Single;
            }
            Some(WritingContext::Map) => {
                self.emit_single_core(v)?;
                self.final_emit = FinalEmit::Single;
                self.push_context(WritingContext::MapValue);
            }
            Some(WritingContext::MapValue) => {
                assert_eq!(
                    self.final_emit,
                    FinalEmit::Single,
                    "an key single should be emitted preceding"
                );

                self.dest.write_all(b" = ")?;
                self.emit_single_core(v)?;
                self.dest.write_all(b";")?;
                self.final_emit = FinalEmit::Token;
                self.pop_context();
            }
        }
        Ok(())
    }

    fn emit_single_core(&mut self, v: &str) -> std::io::Result<()>
    where
        D: std::io::Write,
    {
        let requires_quoted = Self::single_requires_quoted(v);

        if requires_quoted {
            self.dest.write_all(b"\"")?;
        }
        self.dest.write_all(v.as_bytes())?;
        if requires_quoted {
            self.dest.write_all(b"\"")?;
        }

        Ok(())
    }

    fn single_requires_quoted(v: &str) -> bool {
        v.is_empty()
            || v.contains([
                '"', '(', ')', '{', '}', '/', '*', '<', '>', '-', ' ', ';', '=', ',', '+',
            ])
    }
}
