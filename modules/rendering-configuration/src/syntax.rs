use crate::tokenizer::{
    self, CloseBracket, CloseParen, Colon, Comma, Equal, Identifier, Keyword, KwEnd, KwPass,
    KwProperties, KwShader, KwUse, KwVertexBindings, NumLit, OpenBracket, OpenParen, StrLit, Token,
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
    pub fn next(&mut self) -> Option<Token<'s>> {
        tokenizer::next_token(&mut self.tok)
    }
}

#[derive(Debug)]
pub struct Attribute<'s> {
    pub open_bracket: OpenBracket,
    pub name: Identifier<'s>,
    pub close_bracket: CloseBracket,
}
impl<'s> Attribute<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Self {
        let open_bracket = match state.next() {
            Some(Token::OpenBracket(x)) => x,
            x => panic!("invalid attribute head: {x:?}"),
        };
        let name = match state.next() {
            Some(Token::Identifier(x)) => x,
            x => panic!("invalid attribute name: {x:?}"),
        };
        let close_bracket = match state.next() {
            Some(Token::CloseBracket(x)) => x,
            x => panic!("invalid attribute closing: {x:?}"),
        };

        Self {
            open_bracket,
            name,
            close_bracket,
        }
    }
}

#[derive(Debug)]
pub struct Property<'s> {
    pub name: Identifier<'s>,
    pub colon: Colon,
    pub r#type: Identifier<'s>,
    pub equal: Equal,
    pub default: Expression<'s>,
}
impl<'s> Property<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Self {
        let name = match state.next() {
            Some(Token::Identifier(x)) => x,
            x => panic!("invalid property name: {x:?}"),
        };
        let colon = match state.next() {
            Some(Token::Colon(x)) => x,
            x => panic!("unexpected token: {x:?}"),
        };
        let r#type = match state.next() {
            Some(Token::Identifier(x)) => x,
            x => panic!("invalid property type: {x:?}"),
        };
        let equal = match state.next() {
            Some(Token::Equal(x)) => x,
            x => panic!("unexpected token: {x:?}"),
        };
        let default = Expression::parse(state);

        Self {
            name,
            colon,
            r#type,
            equal,
            default,
        }
    }
}

#[derive(Debug)]
pub enum Expression<'s> {
    Use(KwUse, StrLit<'s>),
    Tuple(OpenParen, Vec<(Expression<'s>, Option<Comma>)>, CloseParen),
    Wrapped(OpenParen, Box<Expression<'s>>, CloseParen),
    NumLit(NumLit<'s>),
    StrLit(StrLit<'s>),
}
impl<'s> Expression<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Self {
        Self::parse_factor(state)
    }

    fn parse_factor(state: &mut ParserState<'s>) -> Self {
        match state.next() {
            Some(Token::Keyword(Keyword::Use(x))) => match state.next() {
                Some(Token::StringLiteral(s)) => Expression::Use(x, s),
                x => panic!("invalid use expression: {x:?}"),
            },
            Some(Token::OpenParenthese(open_paren)) => {
                // tuple or wrapped
                let mut elements = Vec::new();
                loop {
                    let st = state.tok.save();
                    let expr = match state.next() {
                        Some(Token::CloseParenthese(_)) => {
                            state.tok.restore(st);
                            break;
                        }
                        _ => {
                            state.tok.restore(st);
                            Expression::parse(state)
                        }
                    };

                    let sp = state.tok.save();
                    match state.next() {
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

                let close_paren = match state.next() {
                    Some(Token::CloseParenthese(x)) => x,
                    x => panic!("invalid tuple expression closing: {x:?}"),
                };

                match <[_; 1]>::try_from(elements) {
                    Ok([(expr, None)]) => {
                        // wrapped
                        Self::Wrapped(open_paren, Box::new(expr), close_paren)
                    }
                    Ok([x]) => {
                        // single element tuple
                        Self::Tuple(open_paren, vec![x], close_paren)
                    }
                    Err(xs) => {
                        // tuple
                        Self::Tuple(open_paren, xs, close_paren)
                    }
                }
            }
            Some(Token::NumLiteral(x)) => Expression::NumLit(x),
            Some(Token::StringLiteral(x)) => Expression::StrLit(x),
            x => panic!("invalid factor expression: {x:?}"),
        }
    }
}

#[derive(Debug)]
pub enum PropertiesBlockElement<'s> {
    Attribute(Attribute<'s>),
    Property(Property<'s>),
}

#[derive(Debug)]
pub struct PropertiesBlock<'s> {
    pub properties: KwProperties,
    pub elements: Vec<PropertiesBlockElement<'s>>,
    pub end: KwEnd,
}
impl<'s> PropertiesBlock<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Self {
        let properties = match state.next() {
            Some(Token::Keyword(Keyword::Properties(x))) => x,
            x => panic!("invalid properties block head: {x:?}"),
        };

        let mut elements = Vec::new();
        loop {
            let sp = state.tok.save();
            match state.next() {
                Some(Token::Keyword(Keyword::End(end))) => {
                    break Self {
                        properties,
                        elements,
                        end,
                    };
                }
                Some(Token::OpenBracket(_)) => {
                    state.tok.restore(sp);
                    elements.push(PropertiesBlockElement::Attribute(Attribute::parse(state)));
                }
                Some(Token::Identifier(_)) => {
                    state.tok.restore(sp);
                    elements.push(PropertiesBlockElement::Property(Property::parse(state)));
                }
                Some(x) => panic!("invalid properties block element: {x:?}"),
                None => panic!("unexpected end of tokens in properties block"),
            }
        }
    }
}

#[derive(Debug)]
pub enum PassBlockContent<'s> {
    VertexBindingsBlock {
        vertex_bindings: KwVertexBindings,
        entries: Vec<(Identifier<'s>, Colon, Identifier<'s>)>,
        end: KwEnd,
    },
    ShaderBlock {
        shader: KwShader,
        content: &'s str,
        end: KwEnd,
    },
}
impl<'s> PassBlockContent<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Self {
        match state.next() {
            Some(Token::Keyword(Keyword::VertexBindings(vertex_bindings))) => {
                let mut entries = Vec::new();
                loop {
                    match state.next() {
                        Some(Token::Keyword(Keyword::End(end))) => {
                            break PassBlockContent::VertexBindingsBlock {
                                vertex_bindings,
                                entries,
                                end,
                            };
                        }
                        Some(Token::Identifier(name)) => {
                            let colon = match state.next() {
                                Some(Token::Colon(x)) => x,
                                x => panic!("invalid vertex binding entry colon: {x:?}"),
                            };
                            let ty = match state.next() {
                                Some(Token::Identifier(ty)) => ty,
                                x => panic!("invalid vertex binding type: {x:?}"),
                            };
                            entries.push((name, colon, ty));
                        }
                        Some(x) => panic!("invalid vertex binding entry: {x:?}"),
                        None => panic!("unexpected end of tokens in vertex bindings block"),
                    }
                }
            }
            Some(Token::Keyword(Keyword::Shader(shader))) => {
                let Some((content, end)) = tokenizer::read_until_next_end(&mut state.tok) else {
                    panic!("invalid shader block ending: {:?}", state.tok.loc())
                };

                PassBlockContent::ShaderBlock {
                    shader,
                    content,
                    end,
                }
            }
            x => panic!("invalid pass block content: {x:?}"),
        }
    }
}

#[derive(Debug)]
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
impl<'s> PassBlock<'s> {
    pub fn parse(state: &mut ParserState<'s>) -> Self {
        let pass = match state.next() {
            Some(Token::Keyword(Keyword::Pass(x))) => x,
            x => panic!("invalid pass head: {x:?}"),
        };
        let name = match state.next() {
            Some(Token::StringLiteral(x)) => x,
            x => panic!("invalid pass name: {x:?}"),
        };
        let st = state.tok.save();
        if let Some(Token::Equal(equal)) = state.next() {
            // simple deriving
            let r#use = match state.next() {
                Some(Token::Keyword(Keyword::Use(x))) => x,
                x => panic!("invalid use in pass: {x:?}"),
            };
            let org_name = match state.next() {
                Some(Token::StringLiteral(x)) => x,
                x => panic!("invalid original pass name: {x:?}"),
            };

            return Self::SimpleDerive {
                pass,
                name,
                equal,
                r#use,
                org_name,
            };
        }
        state.tok.restore(st);

        let mut contents = Vec::new();
        loop {
            let st = state.tok.save();
            match state.next() {
                Some(Token::Keyword(Keyword::End(end))) => {
                    break Self::Standard {
                        pass,
                        name,
                        contents,
                        end,
                    };
                }
                Some(_) => {
                    state.tok.restore(st);
                    contents.push(PassBlockContent::parse(state));
                }
                None => panic!("unexpected end of tokens in pass block"),
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
    pub fn parse(state: &mut ParserState<'s>) -> Option<Self> {
        let st = state.tok.save();
        match state.next() {
            Some(Token::EndOfInput(_)) => None,
            Some(Token::Keyword(Keyword::Properties(_))) => {
                state.tok.restore(st);
                Some(ToplevelElement::PropertiesBlock(PropertiesBlock::parse(
                    state,
                )))
            }
            Some(Token::Keyword(Keyword::Pass(_))) => {
                state.tok.restore(st);
                Some(ToplevelElement::PassBlock(PassBlock::parse(state)))
            }
            x => panic!("invalid toplevel element: {x:?}"),
        }
    }
}
