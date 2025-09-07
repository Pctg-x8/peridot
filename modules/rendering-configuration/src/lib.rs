#[derive(Debug)]
pub enum Token<'s> {
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

fn strip_spaces<'s>(input: &'s str) -> &'s str {
    let mut read_bytes = 0;
    let mut chars = input.chars();
    while let Some(c) = chars.next() {
        if c == ' ' {
            read_bytes += c.len_utf8();
            continue;
        }
        if c == '\t' {
            read_bytes += c.len_utf8();
            continue;
        }
        if c == '\n' {
            read_bytes += c.len_utf8();
            continue;
        }

        break;
    }

    &input[read_bytes..]
}

pub fn next_token<'s>(input: &'s str) -> (Option<Token<'s>>, &'s str) {
    let input = strip_spaces(input);

    if input.starts_with('#') {
        // comment
        let (b, c) = input
            .chars()
            .take_while(|&c| c != '\n')
            .fold((0, 0), |(b, c), x| (b + x.len_utf8(), c + 1));
        return next_token(&input[b..]);
    }

    if input.starts_with('[') {
        return (Some(Token::OpenBracket), &input[1..]);
    }

    if input.starts_with(']') {
        return (Some(Token::CloseBracket), &input[1..]);
    }

    if input.starts_with('(') {
        return (Some(Token::OpenParenthese), &input[1..]);
    }

    if input.starts_with(')') {
        return (Some(Token::CloseParenthese), &input[1..]);
    }

    if input.starts_with('=') {
        return (Some(Token::Equal), &input[1..]);
    }

    if input.starts_with(':') {
        return (Some(Token::Colon), &input[1..]);
    }

    if input.starts_with(',') {
        return (Some(Token::Comma), &input[1..]);
    }

    if input.starts_with('"') {
        // string literal
        let mut read_bytes = 0;
        let mut chars = input.chars();
        let _ = chars.next();
        let mut escaping = false;
        loop {
            match chars.next() {
                None => return (None, input),
                Some(c @ '"') => {
                    read_bytes += c.len_utf8();

                    if !escaping {
                        break;
                    } else {
                        escaping = false;
                    }
                }
                Some(c @ '\\') => {
                    read_bytes += c.len_utf8();

                    if !escaping {
                        escaping = true;
                    } else {
                        escaping = false;
                    }
                }
                Some(c) => {
                    read_bytes += c.len_utf8();
                }
            }
        }

        return (
            Some(Token::StringLiteral(&input[1..read_bytes])),
            &input[read_bytes + 1..],
        );
    }

    if input.starts_with(|c: char| c.is_digit(10)) {
        let (b, c) = input
            .chars()
            .take_while(|&c| c.is_digit(10))
            .fold((0, 0), |(b, c), x| (b + x.len_utf8(), c + 1));

        return (Some(Token::NumLiteral(&input[..b])), &input[b..]);
    }

    let (b, c) = input
        .chars()
        .take_while(|&c| {
            !c.is_whitespace() && !matches!(c, '=' | ':' | ',' | '(' | ')' | '[' | ']' | '"')
        })
        .fold((0, 0), |(b, c), x| (b + x.len_utf8(), c + 1));
    if c == 0 {
        return (None, input);
    }
    let t = match &input[..b] {
        "Properties" => Token::Keyword(Keyword::Properties),
        "End" => Token::Keyword(Keyword::End),
        "Pass" => Token::Keyword(Keyword::Pass),
        "Use" => Token::Keyword(Keyword::Use),
        "Shader" => Token::Keyword(Keyword::Shader),
        "VertexBindings" => Token::Keyword(Keyword::VertexBindings),
        t => Token::Identifier(t),
    };

    (Some(t), &input[b..])
}
