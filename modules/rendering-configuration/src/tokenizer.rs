#[derive(Debug, Clone)]
pub struct Identifier<'s>(&'s str, Location);
impl<'s> Identifier<'s> {
    pub(crate) const fn as_str(&self) -> &'s str {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct StrLit<'s>(&'s str, Location);
impl<'s> StrLit<'s> {
    pub(crate) const fn as_str(&self) -> &'s str {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct NumLit<'s>(&'s str, Location);
impl<'s> NumLit<'s> {
    pub(crate) const fn as_str(&self) -> &'s str {
        self.0
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct KwUse(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct KwEnd(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct KwProperties(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct KwPass(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct KwShader(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct KwVertexBindings(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct OpenBracket(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct CloseBracket(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct OpenParen(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct CloseParen(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Comma(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Equal(Location);

#[derive(Debug, Clone)]
#[repr(transparent)]
pub struct Colon(Location);

#[derive(Debug)]
pub enum Token<'s> {
    Keyword(Keyword),
    Identifier(Identifier<'s>),
    NumLiteral(NumLit<'s>),
    StringLiteral(StrLit<'s>),
    OpenBracket(OpenBracket),
    CloseBracket(CloseBracket),
    OpenParenthese(OpenParen),
    CloseParenthese(CloseParen),
    Comma(Comma),
    Equal(Equal),
    Colon(Colon),
    EndOfInput(Location),
}

#[derive(Debug, Clone)]
pub enum Keyword {
    End(KwEnd),
    Properties(KwProperties),
    Pass(KwPass),
    Use(KwUse),
    Shader(KwShader),
    VertexBindings(KwVertexBindings),
}

#[derive(Debug, Clone)]
pub struct Location {
    pub col: usize,
    pub line: usize,
}

pub struct Context<'s> {
    src: &'s str,
    loc: Location,
}
impl<'s> Context<'s> {
    pub fn new(src: &'s str) -> Self {
        Self {
            src,
            loc: Location { col: 1, line: 1 },
        }
    }

    pub const fn is_finished(&self) -> bool {
        self.src.is_empty()
    }

    pub const fn src(&self) -> &'s str {
        self.src
    }

    pub const fn loc(&self) -> &Location {
        &self.loc
    }

    pub fn save(&self) -> ContextRestorePoint<'s> {
        ContextRestorePoint {
            src: self.src,
            loc: self.loc.clone(),
        }
    }

    pub fn restore(&mut self, p: ContextRestorePoint<'s>) {
        self.src = p.src;
        self.loc = p.loc;
    }
}

#[derive(Clone)]
pub struct ContextRestorePoint<'s> {
    src: &'s str,
    loc: Location,
}

fn strip_spaces<'s>(ctx: &mut Context<'s>) {
    while let Some(c) = ctx.src.chars().next() {
        if c == '\n' {
            ctx.src = &ctx.src[1..];
            ctx.loc.line += 1;
            ctx.loc.col = 1;
        } else if c.is_whitespace() {
            ctx.src = &ctx.src[c.len_utf8()..];
            ctx.loc.col += 1;
        } else {
            break;
        }
    }
}

pub fn read_until_next_end<'s>(ctx: &mut Context<'s>) -> Option<(&'s str, KwEnd)> {
    let mut read_bytes = 0;
    let mut nloc = ctx.loc.clone();
    loop {
        if ctx.src[read_bytes..].starts_with("End")
            && ctx.src.get(read_bytes + 3..).is_none_or(|s| {
                s.starts_with(|c: char| {
                    c.is_whitespace() || matches!(c, '=' | ':' | ',' | '(' | ')' | '[' | ']' | '"')
                })
            })
        {
            // end keyword
            let content_slice = &ctx.src[..read_bytes];
            read_bytes += 3;
            let e = KwEnd(nloc.clone());
            nloc.col += 3;
            ctx.src = &ctx.src[read_bytes..];
            ctx.loc = nloc;
            return Some((content_slice, e));
        }

        let Some(c) = ctx.src[read_bytes..].chars().next() else {
            // no corresponding end
            return None;
        };
        read_bytes += c.len_utf8();
        if c == '\n' {
            nloc.line += 1;
            nloc.col = 1;
        } else {
            nloc.col += 1;
        }
    }
}

pub fn next_token<'s>(ctx: &mut Context<'s>) -> Option<Token<'s>> {
    strip_spaces(ctx);

    if ctx.src.is_empty() {
        return Some(Token::EndOfInput(ctx.loc.clone()));
    }

    if ctx.src.starts_with('#') {
        // comment
        let (b, c) = ctx
            .src
            .chars()
            .take_while(|&c| c != '\n')
            .fold((0, 0), |(b, c), x| (b + x.len_utf8(), c + 1));
        ctx.src = &ctx.src[b..];
        ctx.loc.col += c;
        return next_token(ctx);
    }

    if ctx.src.starts_with('[') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::OpenBracket(OpenBracket(loc)));
    }

    if ctx.src.starts_with(']') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::CloseBracket(CloseBracket(loc)));
    }

    if ctx.src.starts_with('(') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::OpenParenthese(OpenParen(loc)));
    }

    if ctx.src.starts_with(')') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::CloseParenthese(CloseParen(loc)));
    }

    if ctx.src.starts_with('=') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::Equal(Equal(loc)));
    }

    if ctx.src.starts_with(':') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::Colon(Colon(loc)));
    }

    if ctx.src.starts_with(',') {
        let loc = ctx.loc.clone();
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token::Comma(Comma(loc)));
    }

    if ctx.src.starts_with('"') {
        // string literal
        let mut chars = ctx.src.chars();
        let mut nloc = ctx.loc.clone();
        let _ = chars.next();
        let mut read_bytes = 1;
        nloc.col += 1;
        let mut escaping = false;
        loop {
            match chars.next() {
                None => return None,
                Some(c @ '"') => {
                    read_bytes += c.len_utf8();
                    nloc.col += 1;

                    if !escaping {
                        break;
                    } else {
                        escaping = false;
                    }
                }
                Some(c @ '\\') => {
                    read_bytes += c.len_utf8();
                    nloc.col += 1;

                    if !escaping {
                        escaping = true;
                    } else {
                        escaping = false;
                    }
                }
                Some(c) => {
                    read_bytes += c.len_utf8();
                    if c == '\n' {
                        nloc.line += 1;
                        nloc.col = 1;
                    } else {
                        nloc.col += 1;
                    }
                }
            }
        }

        let t = Token::StringLiteral(StrLit(&ctx.src[1..read_bytes - 1], ctx.loc.clone()));
        ctx.src = &ctx.src[read_bytes..];
        ctx.loc = nloc;

        return Some(t);
    }

    if ctx.src.starts_with(|c: char| c.is_digit(10)) {
        let (b, c) = ctx
            .src
            .chars()
            .take_while(|&c| c.is_digit(10))
            .fold((0, 0), |(b, c), x| (b + x.len_utf8(), c + 1));

        let t = Token::NumLiteral(NumLit(&ctx.src[..b], ctx.loc.clone()));
        ctx.src = &ctx.src[b..];
        ctx.loc.col += c;

        return Some(t);
    }

    let (b, c) = ctx
        .src
        .chars()
        .take_while(|&c| {
            !c.is_whitespace() && !matches!(c, '=' | ':' | ',' | '(' | ')' | '[' | ']' | '"')
        })
        .fold((0, 0), |(b, c), x| (b + x.len_utf8(), c + 1));
    if c == 0 {
        return None;
    }
    let t = match &ctx.src[..b] {
        "Properties" => Token::Keyword(Keyword::Properties(KwProperties(ctx.loc.clone()))),
        "End" => Token::Keyword(Keyword::End(KwEnd(ctx.loc.clone()))),
        "Pass" => Token::Keyword(Keyword::Pass(KwPass(ctx.loc.clone()))),
        "Use" => Token::Keyword(Keyword::Use(KwUse(ctx.loc.clone()))),
        "Shader" => Token::Keyword(Keyword::Shader(KwShader(ctx.loc.clone()))),
        "VertexBindings" => {
            Token::Keyword(Keyword::VertexBindings(KwVertexBindings(ctx.loc.clone())))
        }
        t => Token::Identifier(Identifier(t, ctx.loc.clone())),
    };

    ctx.src = &ctx.src[b..];
    ctx.loc.col += c;

    Some(t)
}
