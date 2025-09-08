#[derive(Debug)]
pub struct Token<'s> {
    pub kind: TokenKind<'s>,
    pub loc: Location,
}

#[derive(Debug)]
pub enum TokenKind<'s> {
    Keyword(Keyword),
    Identifier(&'s str),
    NumLiteral(&'s str),
    StringLiteral(&'s str),
    OpenBracket,
    CloseBracket,
    OpenParenthese,
    CloseParenthese,
    Comma,
    Equal,
    Colon,
}

#[derive(Debug, Clone, Copy)]
pub enum Keyword {
    End,
    Properties,
    Pass,
    Use,
    Shader,
    VertexBindings,
}

#[derive(Debug, Clone, Copy)]
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

pub fn next_token<'s>(ctx: &mut Context<'s>) -> Option<Token<'s>> {
    strip_spaces(ctx);

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
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::OpenBracket,
            loc,
        });
    }

    if ctx.src.starts_with(']') {
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::CloseBracket,
            loc,
        });
    }

    if ctx.src.starts_with('(') {
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::OpenParenthese,
            loc,
        });
    }

    if ctx.src.starts_with(')') {
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::CloseParenthese,
            loc,
        });
    }

    if ctx.src.starts_with('=') {
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::Equal,
            loc,
        });
    }

    if ctx.src.starts_with(':') {
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::Colon,
            loc,
        });
    }

    if ctx.src.starts_with(',') {
        let loc = ctx.loc;
        ctx.src = &ctx.src[1..];
        ctx.loc.col += 1;
        return Some(Token {
            kind: TokenKind::Comma,
            loc,
        });
    }

    if ctx.src.starts_with('"') {
        // string literal
        let mut chars = ctx.src.chars();
        let mut nloc = ctx.loc;
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

        let t = Token {
            kind: TokenKind::StringLiteral(&ctx.src[1..read_bytes - 1]),
            loc: ctx.loc,
        };
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

        let t = Token {
            kind: TokenKind::NumLiteral(&ctx.src[..b]),
            loc: ctx.loc,
        };
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
        "Properties" => Token {
            kind: TokenKind::Keyword(Keyword::Properties),
            loc: ctx.loc,
        },
        "End" => Token {
            kind: TokenKind::Keyword(Keyword::End),
            loc: ctx.loc,
        },
        "Pass" => Token {
            kind: TokenKind::Keyword(Keyword::Pass),
            loc: ctx.loc,
        },
        "Use" => Token {
            kind: TokenKind::Keyword(Keyword::Use),
            loc: ctx.loc,
        },
        "Shader" => Token {
            kind: TokenKind::Keyword(Keyword::Shader),
            loc: ctx.loc,
        },
        "VertexBindings" => Token {
            kind: TokenKind::Keyword(Keyword::VertexBindings),
            loc: ctx.loc,
        },
        t => Token {
            kind: TokenKind::Identifier(t),
            loc: ctx.loc,
        },
    };

    ctx.src = &ctx.src[b..];
    ctx.loc.col += c;

    Some(t)
}
