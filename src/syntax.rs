use crate::IResult0;
use crate::tokenizer::{TokenStream, Token, TokenType};
use crate::identifier::VariableName;

pub fn anytoken(input: TokenStream) -> IResult0<Token> {
    input.next()
}

pub fn token(token_type: TokenType) -> impl Fn(TokenStream) -> IResult0<Token> {
    move |input: TokenStream| {
        let (input, token) = input.next()?;
        if token.matches(token_type) {
            Ok((input, token))
        } else {
            // TODO: Return a custom token error
            Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::IsNot }))
        }
    }
}

// Backtracks. This is basically lookahead.
pub fn peek_anytoken(input: TokenStream) -> IResult0<Token> {
    let (_, token) = input.next()?;
    Ok((input, token))
}

pub fn peek_token(token_type: TokenType) -> impl Fn(TokenStream) -> IResult0<Option<Token>> {
    move |input: TokenStream| {
        let (_, token) = input.next()?;
        Ok((
            input,
            if token.matches(token_type) {
                Some(token)
            } else {
                None
            }
        ))
    }
}

pub fn anyidentifier(input: TokenStream) -> IResult0<String> {
    let (input, token) = input.next()?;
    match token {
        Token::Identifier(id) => {
            Ok((input, id))
        },
        _ => {
            // TODO: Return a custom token error
            Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::IsNot }))
        }
    }
}

pub fn identifier<'a>(id_str: &'a str) -> impl Fn(TokenStream<'a>) -> IResult0<'a, String> {
    move |input: TokenStream| {
        let (input, token) = input.next()?;
        match token {
            Token::Identifier(id) => {
                if id == id_str {
                    Ok((input, id))
                } else {
                    // TODO: Return a custom token error
                    Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::IsNot }))
                }
            },
            _ => {
                // TODO: Return a custom token error
                Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::IsNot }))
            }
        }
    }
}

pub fn vector<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |mut input: TokenStream| {
        let mut elements = vec![];
        while let Ok((input0, element)) = pelement(input) {
            elements.push(element);
            input = input0;
        }
        Ok((input, elements))
    }
}

pub fn delimited_vector<A, D>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
    pdelimiter: impl Fn(TokenStream) -> IResult0<D>
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |mut input: TokenStream| {
        let mut elements;
        let input_backup = input;
        match pelement(input) {
            Ok((input0, element)) => {
                elements = vec![element];
                input = input0;
            },
            Err(_err) => return Ok((input_backup, vec![])),
        }
        loop {
            let input_backup = input;
            match pdelimiter(input) {
                Ok((input0, _)) => {
                    // commited
                    let (input0, element) = pelement(input0)?;
                    elements.push(element);
                    input = input0;
                },
                Err(_err) => return Ok((input_backup, elements)),
            }
        }
    }
}

pub fn take_while_succeeds<D>(
    p: impl Fn(TokenStream) -> IResult0<D>
) -> impl Fn(TokenStream) -> IResult0<()> {
    move |mut input: TokenStream| {
        while let Ok((input0, _)) = p(input) { input = input0; }
        Ok((input, ()))
    }
}

// Suppose our delimiter is `,`, then relaxed means we optionally allow arbitrary number of delimiters to appear at the start
// or at the end (or both). For example
//     a, b, c
//    ,a, b, c
//     a, b, c,
//    ,a, b, c,
//  ,,,a, b, c,
//  ,,,a, b, c,,,
// are all valid. All return ~> [a,b,c]
// Even the following is valid
//  ,,,  ~> []
// But
//    a,,b,c
// is not valid.
pub fn relaxed_delimited_vector<A, D>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
    pdelimiter: impl Fn(TokenStream) -> IResult0<D>
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |input: TokenStream| {
        let (input, _) = take_while_succeeds(&pdelimiter)(input)?;
        let (input, vector) = delimited_vector(&pelement, &pdelimiter)(input)?;
        let (input, _) = take_while_succeeds(&pdelimiter)(input)?;
        Ok((input, vector))
    }
}

pub fn delimited_nonempty_vector<A, D>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
    pdelimiter: impl Fn(TokenStream) -> IResult0<D>
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |mut input: TokenStream| {
        let mut elements;
        let (input0, element) = pelement(input)?;
        input = input0;
        elements = vec![element];
        loop {
            let input_backup = input;
            match pdelimiter(input) {
                Ok((input0, _)) => {
                    // commited
                    let (input0, element) = pelement(input0)?;
                    elements.push(element);
                    input = input0;
                },
                Err(_err) => return Ok((input_backup, elements)),
            }
        }
    }
}


// ===Specific Parsers====
// e0 , e1 , e2 , e3
pub fn comma_vector<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |input: TokenStream| {
        relaxed_delimited_vector(&pelement, token(TokenType::Comma))(input)
    }
}

// e0 | e1 | e2 | e3
pub fn or_vector<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |input: TokenStream| {
        relaxed_delimited_vector(&pelement, token(TokenType::OrSeparator))(input)
    }
}

// e0 . e1 . e2 . e3
pub fn binding_vector<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>,
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |input: TokenStream| {
        relaxed_delimited_vector(&pelement, token(TokenType::BindingSeparator))(input)
    }
}

pub fn variable_name(input: TokenStream) -> IResult0<VariableName> {
    let (input, str) = anyidentifier(input)?;
    Ok((input, VariableName::new(str)))
}

// No parens, just a possibly empty comma separated list of identifiers.
pub fn parameter_vector(input: TokenStream) -> IResult0<Vec<VariableName>> {
    // TODO: Check uniqueness.
    delimited_vector(variable_name, token(TokenType::Comma))(input)
}

pub fn enclose<D0, A, D1>(
    popen: impl Fn(TokenStream) -> IResult0<D0>,
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
    pclose: impl Fn(TokenStream) -> IResult0<D0>,
) -> impl Fn(TokenStream) -> IResult0<A> {
    move |input: TokenStream| {
        let (input, _) = popen(input)?;
        let (input, a) = pelement(input)?;
        let (input, _) = pclose(input)?;
        Ok((input, a))
    }
}

pub fn parens<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<A> {
    move |input: TokenStream| {
        enclose::<Token, A, Token>(token(TokenType::OpenParen), &pelement, token(TokenType::CloseParen))(input)
    }
}

pub fn optional_parens<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<Option<A>> {
    move |input: TokenStream| {
        let (_, maybe_paren) = peek_token(TokenType::OpenParen)(input)?;
        match maybe_paren {
            Some(_) => {
                let (input, a) = enclose::<Token, A, Token>(token(TokenType::OpenParen), &pelement, token(TokenType::CloseParen))(input)?;
                Ok((input, Some(a)))
            },
            None => Ok((input, None)),
        }
    }
}

pub fn brackets<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<A> {
    move |input: TokenStream| {
        enclose::<Token, A, Token>(token(TokenType::OpenBracket), &pelement, token(TokenType::CloseBracket))(input)
    }
}

pub fn optional_brackets<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<Option<A>> {
    move |input: TokenStream| {
        let (_, maybe_paren) = peek_token(TokenType::OpenBracket)(input)?;
        match maybe_paren {
            Some(_) => {
                let (input, a) = enclose::<Token, A, Token>(token(TokenType::OpenBracket), &pelement, token(TokenType::CloseBracket))(input)?;
                Ok((input, Some(a)))
            },
            None => Ok((input, None)),
        }
    }
}

pub fn curly_braces<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<A> {
    move |input: TokenStream| {
        enclose::<Token, A, Token>(token(TokenType::OpenCurly), &pelement, token(TokenType::CloseCurly))(input)
    }
}

pub fn possibly_empty_relaxed_paren_vector<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |input: TokenStream| {
        let (input, maybe_vec)= optional_parens(relaxed_delimited_vector(&pelement, token(TokenType::Comma)))(input)?;
        match maybe_vec {
            Some(xs) => Ok((input, xs)),
            None => Ok((input, vec![])),
        }
    }
}

pub fn possibly_empty_relaxed_bracket_vector<A>(
    pelement: impl Fn(TokenStream) -> IResult0<A>, 
) -> impl Fn(TokenStream) -> IResult0<Vec<A>> {
    move |input: TokenStream| {
        let (input, maybe_vec)= optional_brackets(relaxed_delimited_vector(&pelement, token(TokenType::Comma)))(input)?;
        match maybe_vec {
            Some(xs) => Ok((input, xs)),
            None => Ok((input, vec![])),
        }
    }
}
