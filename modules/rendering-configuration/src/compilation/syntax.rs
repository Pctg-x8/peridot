use crate::compilation::tokenizer::{Located, Location};

use super::tokenizer::{
    self, CloseBracket, CloseParen, Colon, Comma, Equal, Identifier, Keyword, KwEnd, KwPass,
    KwProperties, KwRenderOption, KwShader, KwUse, NumLit, OpenBracket, OpenParen, StrLit, Token,
};

pub struct ParserState<'s> {
    tok: tokenizer::Context<'s>,
}
impl<'s> ParserState<'s> {
    pub fn new(tok: tokenizer::Context<'s>) -> Self {
        Self { tok }
    }

    pub const fn is_finished(&self) -> bool {
        self.tok.is_finished()
    }

    #[inline(always)]
    pub fn try_next(&mut self) -> Option<Token<'s>> {
        tokenizer::next_token(&mut self.tok)
    }

    #[inline(always)]
    pub fn next(&mut self) -> Result<Token<'s>, Error<'s>> {
        self.try_next().ok_or_else(|| Error::illformed_token(self))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<'s> {
    #[error("unexpected token at {}: expected {expected}, found {tok:?}", .tok.location())]
    UnexpectedToken {
        expected: &'static str,
        tok: Token<'s>,
    },
    #[error("illformed token found at {0}")]
    IllformedToken(Location),
    #[error("unknown type name at {}", .tok.location())]
    UnknownTypeName { tok: Identifier<'s> },
    #[error("missing {0} block ending, block starts at {1}")]
    MissingShaderBlockEnding(&'static str, Location),
}
impl<'s> Error<'s> {
    #[inline(always)]
    const fn unexpected_token(expected: &'static str, tok: Token<'s>) -> Self {
        Self::UnexpectedToken { expected, tok }
    }

    #[inline(always)]
    fn illformed_token(state: &ParserState<'s>) -> Self {
        Self::IllformedToken(state.tok.loc().clone())
    }

    #[inline(always)]
    const fn unknown_type_name(tok: Identifier<'s>) -> Self {
        Self::UnknownTypeName { tok }
    }

    #[inline(always)]
    const fn missing_block_ending(block_name: &'static str, starts_at: Location) -> Self {
        Self::MissingShaderBlockEnding(block_name, starts_at)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Attribute<'s> {
    pub open_bracket: OpenBracket,
    pub name: Identifier<'s>,
    pub close_bracket: CloseBracket,
}
impl Located for Attribute<'_> {
    #[inline(always)]
    fn location(&self) -> &Location {
        self.open_bracket.location()
    }
}
impl<'s> Attribute<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        let open_bracket = match state.next()? {
            Token::OpenBracket(x) => x,
            t => return Err(Error::unexpected_token("[", t)),
        };
        let name = match state.next()? {
            Token::Identifier(x) => x,
            t => return Err(Error::unexpected_token("identifier", t)),
        };
        let close_bracket = match state.next()? {
            Token::CloseBracket(x) => x,
            t => return Err(Error::unexpected_token("]", t)),
        };

        Ok(Self {
            open_bracket,
            name,
            close_bracket,
        })
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct Property<'s> {
    pub name: Identifier<'s>,
    pub colon: Colon,
    pub r#type: Type<'s>,
    pub equal: Equal,
    pub default: Expression<'s>,
}
impl Located for Property<'_> {
    #[inline(always)]
    fn location(&self) -> &Location {
        self.name.location()
    }
}
impl<'s> Property<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        let name = match state.next()? {
            Token::Identifier(x) => x,
            t => return Err(Error::unexpected_token("identifier", t)),
        };
        let colon = match state.next()? {
            Token::Colon(x) => x,
            t => return Err(Error::unexpected_token(":", t)),
        };
        let r#type = Type::parse(state)?;
        let equal = match state.next()? {
            Token::Equal(x) => x,
            t => return Err(Error::unexpected_token("=", t)),
        };
        let default = Expression::parse(state)?;

        Ok(Self {
            name,
            colon,
            r#type,
            equal,
            default,
        })
    }
}

#[derive(Debug)]
pub enum Type<'s> {
    Float4(Identifier<'s>),
    Float2(Identifier<'s>),
    Texture2D(Identifier<'s>),
    RGB(Identifier<'s>),
    UInt(Identifier<'s>),
    Int(Identifier<'s>),
}
impl Located for Type<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Float4(x) => x.location(),
            Self::Float2(x) => x.location(),
            Self::Texture2D(x) => x.location(),
            Self::RGB(x) => x.location(),
            Self::UInt(x) => x.location(),
            Self::Int(x) => x.location(),
        }
    }
}
impl<'s> Type<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        Self::parse_factor(state)
    }

    pub fn parse_factor(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        match state.next()? {
            Token::Identifier(ident) if ident.as_str() == "UInt" => Ok(Self::UInt(ident)),
            Token::Identifier(ident) if ident.as_str() == "Int" => Ok(Self::Int(ident)),
            Token::Identifier(ident) if ident.as_str() == "Float2" => Ok(Self::Float2(ident)),
            Token::Identifier(ident) if ident.as_str() == "Float4" => Ok(Self::Float4(ident)),
            Token::Identifier(ident) if ident.as_str() == "Texture2D" => Ok(Self::Texture2D(ident)),
            Token::Identifier(ident) if ident.as_str() == "RGB" => Ok(Self::RGB(ident)),
            Token::Identifier(ident) => Err(Error::unknown_type_name(ident)),
            t => Err(Error::unexpected_token("type token", t)),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Expression<'s> {
    Use(KwUse, StrLit<'s>),
    Tuple(OpenParen, Vec<(Expression<'s>, Option<Comma>)>, CloseParen),
    Wrapped(OpenParen, Box<Expression<'s>>, CloseParen),
    NumLit(NumLit<'s>),
    StrLit(StrLit<'s>),
}
impl Located for Expression<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Use(x, _) => x.location(),
            Self::Tuple(x, _, _) => x.location(),
            Self::Wrapped(x, _, _) => x.location(),
            Self::NumLit(x) => x.location(),
            Self::StrLit(x) => x.location(),
        }
    }
}
impl<'s> Expression<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        Self::parse_factor(state)
    }

    fn parse_factor(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        match state.next()? {
            Token::Keyword(Keyword::Use(x)) => match state.next()? {
                Token::StringLiteral(s) => Ok(Expression::Use(x, s)),
                t => Err(Error::unexpected_token("string literal", t)),
            },
            Token::OpenParenthese(open_paren) => {
                // tuple or wrapped
                let mut elements = Vec::new();
                loop {
                    let st = state.tok.save();
                    let expr = match state.try_next() {
                        Some(Token::CloseParenthese(_)) => {
                            state.tok.restore(st);
                            break;
                        }
                        _ => {
                            state.tok.restore(st);
                            Expression::parse(state)?
                        }
                    };

                    let sp = state.tok.save();
                    match state.try_next() {
                        Some(Token::Comma(c)) => {
                            elements.push((expr, Some(c)));
                        }
                        _ => {
                            state.tok.restore(sp);
                            elements.push((expr, None));
                            // no further elements allowed
                            break;
                        }
                    };
                }

                let close_paren = match state.next()? {
                    Token::CloseParenthese(x) => x,
                    t => return Err(Error::unexpected_token(")", t)),
                };

                match <[_; 1]>::try_from(elements) {
                    Ok([(expr, None)]) => {
                        // wrapped
                        Ok(Self::Wrapped(open_paren, Box::new(expr), close_paren))
                    }
                    Ok([x]) => {
                        // single element tuple
                        Ok(Self::Tuple(open_paren, vec![x], close_paren))
                    }
                    Err(xs) => {
                        // tuple
                        Ok(Self::Tuple(open_paren, xs, close_paren))
                    }
                }
            }
            Token::NumLiteral(x) => Ok(Expression::NumLit(x)),
            Token::StringLiteral(x) => Ok(Expression::StrLit(x)),
            t => Err(Error::unexpected_token("expr factor", t)),
        }
    }
}

#[derive(Debug)]
pub enum PropertiesBlockElement<'s> {
    Attribute(Attribute<'s>),
    Property(Property<'s>),
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct PropertiesBlock<'s> {
    pub properties: KwProperties,
    pub elements: Vec<PropertiesBlockElement<'s>>,
    pub end: KwEnd,
}
impl Located for PropertiesBlock<'_> {
    #[inline(always)]
    fn location(&self) -> &Location {
        self.properties.location()
    }
}
impl<'s> PropertiesBlock<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        let properties = match state.next()? {
            Token::Keyword(Keyword::Properties(x)) => x,
            t => return Err(Error::unexpected_token("Properties", t)),
        };

        let mut elements = Vec::new();
        loop {
            let sp = state.tok.save();
            match state.next()? {
                Token::Keyword(Keyword::End(end)) => {
                    break Ok(Self {
                        properties,
                        elements,
                        end,
                    });
                }
                Token::OpenBracket(_) => {
                    state.tok.restore(sp);
                    elements.push(PropertiesBlockElement::Attribute(Attribute::parse(state)?));
                }
                Token::Identifier(_) => {
                    state.tok.restore(sp);
                    elements.push(PropertiesBlockElement::Property(Property::parse(state)?));
                }
                t => return Err(Error::unexpected_token("[ or identifier", t)),
            }
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum PassBlockContent<'s> {
    RenderOptions {
        render_option: KwRenderOption,
        entries: Vec<(Identifier<'s>, Option<Comma>)>,
    },
    ShaderBlock {
        shader: KwShader,
        content: &'s str,
        end: KwEnd,
    },
}
impl Located for PassBlockContent<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::RenderOptions { render_option, .. } => render_option.location(),
            Self::ShaderBlock { shader, .. } => shader.location(),
        }
    }
}
impl<'s> PassBlockContent<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        match state.next()? {
            Token::Keyword(Keyword::RenderOption(render_option)) => {
                let mut entries = Vec::new();
                loop {
                    let st = state.tok.save();
                    let ident = match state.try_next() {
                        Some(Token::Identifier(ident)) => ident,
                        _ => {
                            state.tok.restore(st);
                            break;
                        }
                    };

                    let sp = state.tok.save();
                    match state.try_next() {
                        Some(Token::Comma(c)) => {
                            entries.push((ident, Some(c)));
                        }
                        _ => {
                            state.tok.restore(sp);
                            entries.push((ident, None));
                            // no further elements allowed
                            break;
                        }
                    };
                }

                Ok(PassBlockContent::RenderOptions {
                    render_option,
                    entries,
                })
            }
            Token::Keyword(Keyword::Shader(shader)) => {
                let Some((content, end)) = tokenizer::read_until_next_end(&mut state.tok) else {
                    return Err(Error::missing_block_ending(
                        "shader",
                        shader.location().clone(),
                    ));
                };

                Ok(PassBlockContent::ShaderBlock {
                    shader,
                    content,
                    end,
                })
            }
            t => Err(Error::unexpected_token("pass block content", t)),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum PassBlock<'s> {
    Standard {
        pass: KwPass,
        name: StrLit<'s>,
        contents: Vec<PassBlockContent<'s>>,
        end: KwEnd,
    },
    SimpleDerive {
        pass: KwPass,
        name: StrLit<'s>,
        equal: Equal,
        r#use: KwUse,
        org_name: StrLit<'s>,
    },
}
impl Located for PassBlock<'_> {
    #[inline]
    fn location(&self) -> &Location {
        match self {
            Self::Standard { pass, .. } => pass.location(),
            Self::SimpleDerive { pass, .. } => pass.location(),
        }
    }
}
impl<'s> PassBlock<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Self, Error<'s>> {
        let pass = match state.next()? {
            Token::Keyword(Keyword::Pass(x)) => x,
            t => return Err(Error::unexpected_token("Pass", t)),
        };
        let name = match state.next()? {
            Token::StringLiteral(x) => x,
            t => return Err(Error::unexpected_token("string literal", t)),
        };
        let st = state.tok.save();
        if let Some(Token::Equal(equal)) = state.try_next() {
            // simple deriving
            let r#use = match state.next()? {
                Token::Keyword(Keyword::Use(x)) => x,
                t => return Err(Error::unexpected_token("Use", t)),
            };
            let org_name = match state.next()? {
                Token::StringLiteral(x) => x,
                t => return Err(Error::unexpected_token("pass name", t)),
            };

            return Ok(Self::SimpleDerive {
                pass,
                name,
                equal,
                r#use,
                org_name,
            });
        }
        state.tok.restore(st);

        let mut contents = Vec::new();
        loop {
            let st = state.tok.save();
            match state.try_next() {
                Some(Token::Keyword(Keyword::End(end))) => {
                    break Ok(Self::Standard {
                        pass,
                        name,
                        contents,
                        end,
                    });
                }
                Some(_) => {
                    state.tok.restore(st);
                    contents.push(PassBlockContent::parse(state)?);
                }
                None => return Err(Error::missing_block_ending("pass", pass.location().clone())),
            }
        }
    }
}

#[derive(Debug)]
pub enum ToplevelElement<'s> {
    PropertiesBlock(PropertiesBlock<'s>),
    PassBlock(PassBlock<'s>),
}
impl<'s> ToplevelElement<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Result<Option<Self>, Error<'s>> {
        let st = state.tok.save();
        match state.next()? {
            Token::EndOfInput(_) => Ok(None),
            Token::Keyword(Keyword::Properties(_)) => {
                state.tok.restore(st);
                Ok(Some(ToplevelElement::PropertiesBlock(
                    PropertiesBlock::parse(state)?,
                )))
            }
            Token::Keyword(Keyword::Pass(_)) => {
                state.tok.restore(st);
                Ok(Some(ToplevelElement::PassBlock(PassBlock::parse(state)?)))
            }
            t => Err(Error::unexpected_token("toplevel token", t)),
        }
    }
}
