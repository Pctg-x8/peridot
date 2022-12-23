
pub struct Position {
    pub column: usize,
    pub line: usize
}
impl Position {
    pub fn from_str_and_pos(src: &str, pos: usize) -> Self {
        let lines = src[..pos].chars().filter(|&c| c == '\n').count();
        let cols = src[..pos].chars().rev().take_while(|&c| c == '\n').count();

        Self { column: cols + 1, line: lines + 1 }
    }
}
impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.column, self.line)
    }
}

pub struct Span<'s> {
    pub src: &'s str,
    pub range: std::ops::Range<usize>
}
impl<'s> Span<'s> {
    pub fn as_str(&self) -> &'s str {
        &self.src[self.range.clone()]
    }

    pub fn calc_pos(&self) -> Position {
        Position::from_str_and_pos(self.src, self.range.start)
    }
}
impl std::fmt::Debug for Span<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}({})", self.as_str(), self.calc_pos())
    }
}

pub enum Token<'s> {
    Parameter(Span<'s>),
    Switch(Span<'s>),
    VertexShader(Span<'s>),
    FragmentShader(Span<'s>),
    GeometryShader(Span<'s>),
    RenderMode(Span<'s>),
    VertexInput(Span<'s>),
    Binding(Span<'s>),
    PerVertex(Span<'s>),
    PerInstance(Span<'s>),
    Cull(Span<'s>),
    Front(Span<'s>),
    If(Span<'s>),
    True(Span<'s>),
    False(Span<'s>),
    Bool(Span<'s>),
    Int(Span<'s>),
    Float(Span<'s>),
    Identifier(Span<'s>),
    BlockStart(Span<'s>),
    BlockEnd(Span<'s>),
    OpenParenthese(Span<'s>),
    CloseParenthese(Span<'s>),
    OpenBracket(Span<'s>),
    CloseBracket(Span<'s>)
}

pub struct Tokenizer<'s> {
    pub src: &'s str,
    pub pos: usize,
}
impl<'s> Tokenizer<'s> {
    pub fn new(src: &'s str) -> Self {
        Self { src, pos: 0 }
    }

    pub fn save(&self) -> Self {
        Self { src: self.src, pos: self.pos }
    }

    pub fn rewind(&mut self, saved: Self) {
        self.src = saved.src;
        self.pos = saved.pos;
    }
}
