use crate::IResult0;
use crate::tokenizer::{TokenStream, Token, TokenType};

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
