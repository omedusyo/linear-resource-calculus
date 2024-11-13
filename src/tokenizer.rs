use crate::{IResult0, IResult};
use nom::{
  bytes::complete::take_while,
  combinator::{peek, verify},
  character::complete::{char, anychar, i32, multispace0},
};

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Identifier(String),
    Eq,
    BindingSeparator,
    Comma,
    OrSeparator,
    VarLookupSymbol,
    VarMoveSymbol,
    VarCloneSymbol,
    VarDropSymbol,
    TagSymbol,
    Int(i32),
    End,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Identifier,
    Eq,
    BindingSeparator,
    Comma,
    OrSeparator,
    VarLookupSymbol,
    VarMoveSymbol,
    VarCloneSymbol,
    VarDropSymbol,
    TagSymbol,
    Int,
    End,
}

impl Token {
    pub fn type_(&self) -> TokenType {
        use Token::*;
        match self {
            OpenParen => TokenType::OpenParen,
            CloseParen => TokenType::CloseParen,
            OpenBracket => TokenType::OpenBracket,
            CloseBracket => TokenType::CloseBracket,
            OpenCurly => TokenType::OpenCurly,
            CloseCurly => TokenType::CloseCurly,
            Identifier(_) => TokenType::Identifier,
            Eq => TokenType::Eq,
            BindingSeparator => TokenType::BindingSeparator,
            Comma => TokenType::Comma,
            OrSeparator => TokenType::OrSeparator,
            VarLookupSymbol => TokenType::VarLookupSymbol,
            VarMoveSymbol => TokenType::VarMoveSymbol,
            VarCloneSymbol => TokenType::VarCloneSymbol,
            VarDropSymbol => TokenType::VarDropSymbol,
            TagSymbol => TokenType::TagSymbol,
            Int(_) => TokenType::Int,
            End => TokenType::End,
        }
    }

    pub fn matches(&self, token_type: TokenType) -> bool {
        self.type_() == token_type
    }
}

fn is_forbiden_char(c: char) -> bool {
    match c {
        '(' | ')' | '[' | ']'| '{' | '}' | '.' | ',' | '|' | '$' | '%' | '#' | ' ' | '\t' | '\r' | '\n' => true,
        _ => false,
    }
}

pub fn parse_identifier(input: &str) -> IResult<&str, String> {
    // Identifier can't start with a char in "(){},.|%$#-012345689=".
    // Afterwards we have a sequence of any chars except those contained in "()[]{},.|$#" or
    // whitespace.
    // We also allow the identifier to start with double equals e.g. "==" or "==foo"
    // VALID: "foo", "bar123", "_123", "_-_-_", "<=", "+", "*", "%", "foo!", "bar?",
    //        "==", "==foo==", "==?"
    //        "λ", "→", "↑", "↓", "⇒", "⇔"
    // INVALID: "123foo", "-foo", "f))x", "=foo"
    let (input, c0) = verify(anychar, |c: &char| !(is_forbiden_char(*c) || *c == '-' || c.is_digit(10)))(input)?;
    if c0 == '=' {
        // make sure the next char is also another '='.
        let (input, _) = char('=')(input)?;
        let (input, s) = take_while(|c: char| !is_forbiden_char(c))(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, format!("=={}", s)))
    } else {
        let (input, s) = take_while(|c: char| !is_forbiden_char(c))(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input, format!("{}{}", c0, s)))
    }
}

//   "let { inc = fn { x . add($x, 1) } . app($inc, 5) }"
// ~>
//   [Id("let"), '{', Id("inc"), '=', Id("fn"), '{', Id("x"), BindingSeparator, Id("add"), '(', '$', Id("x"), Int(1), ')', '}', BindingSeparator, Id("app"), '(', '$', Id("inc"), Int(5), ')', '}']
pub fn parse_token(input: &str) -> IResult<&str, Token> {
    // Assume the whitespace at the start of `input` is already consumed.
    if input.is_empty() {
        return Ok((input, Token::End))
    }

    let (_, c) = peek(anychar)(input)?;
    match c {
        '(' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::OpenParen))
        },
        ')' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::CloseParen))
        },
        '[' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::OpenBracket))
        },
        ']' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::CloseBracket))
        },
        '{' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::OpenCurly))
        },
        '}' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::CloseCurly))
        },
        '.' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::BindingSeparator))
        },
        ',' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::Comma))
        },
        '|' => {
            let (input, _) = anychar(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::OrSeparator))
        },
        '$' => {
            let (input, _) = anychar(input)?;
            // Note how there's no consumption of whitespace.
            Ok((input, Token::VarLookupSymbol))
        },
        '%' => {
            let (input, _) = anychar(input)?;
            // Note how there's no consumption of whitespace.
            Ok((input, Token::VarMoveSymbol))
        },
        '#' => {
            let (input, _) = anychar(input)?;
            // Note how there's no consumption of whitespace.
            Ok((input, Token::TagSymbol))
        },
        // TODO: We should allow '-' as a name on its own. We need to check the char following '-'
        // is not a digit, in which case we are looking at identifier.
        c if c == '-' || c.is_digit(10) => {
            // 123
            // -123
            let (input, x) = i32(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Token::Int(x)))
        },
        '=' => {
            if input.len() == 1 {
                let (input, _) = anychar(input)?;
                Ok((input, Token::Eq))
            } else {
                let input_backup = input;
                let (input, _) = anychar(input)?;
                let (_, c) = peek(anychar)(input)?;
                if c == '=' {
                    // identifier starting with "=="
                    let (input, str) = parse_identifier(input_backup)?;
                    Ok((input, Token::Identifier(str)))
                } else {
                    let (input, _) = multispace0(input)?;
                    Ok((input, Token::Eq))
                }
            }
        },
        _ => {
            let (input, str) = parse_identifier(input)?;
            match &str[..] {
                "read" => Ok((input, Token::VarLookupSymbol)),
                "move" => Ok((input, Token::VarMoveSymbol)),
                "clone" => Ok((input, Token::VarCloneSymbol)),
                "drop" => Ok((input, Token::VarDropSymbol)),
                // We assume we are looking at identifier.
                _ => Ok((input, Token::Identifier(str))),
            }
        },
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct TokenStream<'a> {
    pub input: &'a str,
}

impl <'a> TokenStream<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input }
    }

    // fn next(mut self) -> Result<(Self, Token), nom::Err<nom::error::Error<&'a str>>> {
    pub fn next(mut self) -> IResult0<'a, Token> {
        let result = parse_token(self.input);
        match result {
            Ok((input, token)) => {
                self.input = input;
                Ok((self, token))
            },
            Err(err) => Err(err),
        }
    }
}
