use crate::{IResult0, IResult};
use nom::{
  bytes::complete::{take_while, tag},
  branch::alt,
  combinator::{peek, verify},
  character::complete::{char, anychar, i32, multispace0, hex_digit1},
  number::complete::{
      float,
  },
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
    ConstructorSymbol,
    MessageSymbol,
    Int(i32),
    Escaped(EscapedContent),
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
    ConstructorSymbol,
    MessageSymbol,
    Int,
    Escaped(EscapedContentMode),
    End,
}

#[derive(Debug, PartialEq)]
pub enum EscapedContent {
    Float(f32),
    String(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum EscapedContentMode {
    Float,
    String,
}

impl EscapedContent {
    pub fn mode(&self) -> EscapedContentMode {
        use EscapedContent::*;
        match self {
            Float(_) => EscapedContentMode::Float,
            String(_) => EscapedContentMode::String,
        }
    }
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
            ConstructorSymbol => TokenType::ConstructorSymbol,
            MessageSymbol => TokenType::MessageSymbol,
            Int(_) => TokenType::Int,
            Escaped(x) => TokenType::Escaped(x.mode()),
            End => TokenType::End,
        }
    }

    pub fn is_start_of_expression(&self) -> bool {
        use Token::*;
        match self {
            OpenParen => true,
            CloseParen => false,
            OpenBracket => true,
            CloseBracket => false,
            OpenCurly => false,
            CloseCurly => false,
            Identifier(_) => true,
            Eq => false,
            BindingSeparator => false,
            Comma => false,
            OrSeparator => false,
            VarLookupSymbol => true,
            VarMoveSymbol => true,
            VarCloneSymbol => true,
            VarDropSymbol => true,
            ConstructorSymbol => true,
            MessageSymbol => false,
            Int(_) => true,
            Escaped(_) => true,
            End => false,
        }
    }

    pub fn matches(&self, token_type: TokenType) -> bool {
        self.type_() == token_type
    }
}

fn is_forbiden_char(c: char) -> bool {
    match c {
        '(' | ')' | '[' | ']'| '{' | '}' | '.' | ',' | '|' | '$' | '%' | '#' | '@' | ' ' | '◊' | '\t' | '\r' | '\n' => true,
        _ => false,
    }
}

fn comment(input: &str) -> IResult<&str, ()> {
    let (input, _) = char('/')(input)?;
    let (mut input, _) = char('/')(input)?;
    let (input_if_bracesopen_brace_commited, maybe_char) = peek_char_or_eof(input)?;
    let Some(c) = maybe_char else { return Ok(("", ())) };
    match c {
        '{' => { // The comment is `//{...}`
            input = input_if_bracesopen_brace_commited;
            // Consume everything until you see (unescaped) '}' symbol or EOF.
            // TODO: You need to handle the escaped '}', otherwise you can't have '}' in the comments.
            loop {
                match anychar(input)? {
                    (input0, '}') => {
                        input = input0;
                        break
                    },
                    (input0, _c) => {
                        input = input0;
                    },
                }
            }
            Ok((input, ()))
        },
        _ => {
            // Consume everything until you see '\n' or EOF
            loop {
                match anychar::<&str, ()>(input) {
                    Ok((_, '\n')) => {
                        break
                    },
                    Ok((input0, _c)) => {
                        input = input0;
                    },
                    Err(_err) => {
                        break
                    },
                }
            }
            Ok((input, ()))
        }
    }
}
#[cfg(test)]
#[test]
fn test_comment() {
    let input = "//foo";
    let result = comment(input);
    assert!(matches!(result, Ok(_)));

    let input = "/foo";
    let result = comment(input);
    assert!(matches!(result, Err(_)));

    let input = "//foo\nbar";
    let result = comment(input);
    assert!(matches!(result, Ok(("\nbar", _))));

    let input = "//    ";
    let result = comment(input);
    assert!(matches!(result, Ok(("", _))));

    let input = "//";
    let result = comment(input);
    assert!(matches!(result, Ok(("", _))));

    let input = "//{foo} bar";
    let result = comment(input);
    assert!(matches!(result, Ok((" bar", _))));

    let input = "//{foo \n quux} bar";
    let result = comment(input);
    assert!(matches!(result, Ok((" bar", _))));
}

fn peek_char_or_eof(input: &str) -> IResult<&str, Option<char>> {
    match anychar::<&str, ()>(input) {
        Ok((input, c)) => Ok((input, Some(c))),
        Err(_err) => {
            Ok((input, None))
        }
    }
}
#[cfg(test)]
#[test]
fn test_peek_char_or_eof() {
    let input = "";
    let result = peek_char_or_eof(input);
    assert!(matches!(result, Ok(_)));
    assert!(matches!(result, Ok((_, None))));

    let input = "foo";
    let result = peek_char_or_eof(input);
    assert!(matches!(result, Ok(_)));
    assert!(matches!(result, Ok((_, Some(_)))));
    assert!(matches!(result, Ok((_, Some('f')))));
}

// Includes comments too
fn whitespace(input: &str) -> IResult<&str, ()> {
    let (input, _) = multispace0(input)?;
    let (input0, c) = peek_char_or_eof(input)?;
    match c {
        Some('/') => {
            let (_, c) = peek_char_or_eof(input0)?;
            match c {
                Some('/') => {
                    // Comment detected.
                    let (input, ()) = comment(input)?;
                    return whitespace(input)
                },
                _ => {},
            }
        },
        _ => {}
    }
    Ok((input, ()))
}
#[cfg(test)]
#[test]
fn test_whitespace() {
    let input = "";
    let result = whitespace(input);
    assert!(matches!(result, Ok(_)));

    let input = "  foo ";
    let result = whitespace(input);
    assert!(matches!(result, Ok(("foo ", _))));

    let input = "  \n\n   \nfoo";
    let result = whitespace(input);
    assert!(matches!(result, Ok(("foo", _))));

    let input = "  \n\n   \n";
    let result = whitespace(input);
    assert!(matches!(result, Ok(("", _))));

    let input = "  \n\n //somecomment  \nfoo";
    let result = whitespace(input);
    assert!(matches!(result, Ok(("foo", _))));

    let input = "  \n\n //{somecomment}  \n  foo";
    let result = whitespace(input);
    assert!(matches!(result, Ok(("foo", _))));
}

pub fn parse_identifier(input: &str) -> IResult<&str, String> {
    // Identifier can't start with a char in "(){},.|%$#-012345689=".
    // Afterwards we have a sequence of any chars except those contained in "()[]{},.|$#@" or
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
        let (input, _) = whitespace(input)?;
        Ok((input, format!("=={}", s)))
    } else {
        let (input, s) = take_while(|c: char| !is_forbiden_char(c))(input)?;
        let (input, _) = whitespace(input)?;
        Ok((input, format!("{}{}", c0, s)))
    }
}

pub fn parse_string(input: &str) -> IResult<&str, String> {
    let (mut input, _) = char('{')(input)?;
    let mut chars: Vec<char> = vec![];
    let mut c;
    loop {
        (input, c) = anychar(input)?;
        match c {
            '\\' => {
                (input, c) = anychar(input)?;
                match c {
                    '\\' => chars.push('\\'),
                    '{' => chars.push('{'),
                    '}' => chars.push('}'),
                    'n' => chars.push('\n'),
                    't' => chars.push('\t'),
                    'u' => {
                        (input, _) = char('{')(input)?;

                        let (input0, digits) = hex_digit1(input)?;
                        input = input0;
                        let x = u32::from_str_radix(digits, 16).unwrap();

                        (input, _) = char('}')(input)?;

                        match char::from_u32(x) {
                            Some(c) => chars.push(c),
                            None => todo!("Expected Valid Unicode Sequence in String Literal"),
                        }
                    }
                    _c => todo!("Expected Valid Character After Escape Sequence In String Literal"),
                }
            },
            '}' => {
                break
            },
            c => chars.push(c),
        }
    }
    let str =  chars.into_iter().collect();
    Ok((input, str))
}

#[cfg(test)]
#[test]
fn test_string() {
    let input = "{foo} ";
    let result = parse_string(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, str) = result.unwrap();
    assert!(str == "foo");

    let input = "{} ";
    let result = parse_string(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, str) = result.unwrap();
    assert!(str == "");

    let input = "{foo ⊗ } ";
    let result = parse_string(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, str) = result.unwrap();
    assert!(str == "foo ⊗ ");

    let input = "{foo \u{2297}} ";
    let result = parse_string(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, str) = result.unwrap();
    assert!(str == "foo \u{2297}");
}

fn parse_float(input: &str) -> IResult<&str, f32> {
    let (input, _) = char('{')(input)?;
    let (input, x) = float(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, x))
}

#[cfg(test)]
#[test]
fn test_float() {
    let eps = 0.0000001;

    let input = "{123.0} ";
    let result = parse_float(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, x) = result.unwrap();
    assert!((x - 123.0).abs() < eps);
                         //
    let input = "{123} ";
    let result = parse_float(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, x) = result.unwrap();
    assert!((x - 123.0).abs() < eps);

    let input = "{123e0} ";
    let result = parse_float(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, x) = result.unwrap();
    assert!((x - 123.0).abs() < eps);

    let input = "{12.3e1} ";
    let result = parse_float(input);
    assert!(matches!(result, Ok((" ", _))));
    let (_, x) = result.unwrap();
    assert!((x - 123.0).abs() < eps);
}

fn parse_escaped_content_mode(input: &str) -> IResult<&str, EscapedContentMode> {
    // TODO: This is terrible
    let (input, x) = alt((tag("f32"), tag("str")))(input)?;
    match x {
        "f32" => Ok((input, EscapedContentMode::Float)),
        "str" => Ok((input, EscapedContentMode::String)),
        _ => unreachable!(),
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
            let (input, _) = whitespace(input)?;
            Ok((input, Token::OpenParen))
        },
        ')' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::CloseParen))
        },
        '[' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::OpenBracket))
        },
        ']' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::CloseBracket))
        },
        '{' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::OpenCurly))
        },
        '}' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::CloseCurly))
        },
        '.' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::BindingSeparator))
        },
        ',' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
            Ok((input, Token::Comma))
        },
        '|' => {
            let (input, _) = anychar(input)?;
            let (input, _) = whitespace(input)?;
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
            Ok((input, Token::ConstructorSymbol))
        },
        '@' => {
            let (input, _) = anychar(input)?;
            // Note how there's no consumption of whitespace.
            Ok((input, Token::MessageSymbol))
        },
        '◊' => {
            // Note how there's no consumption of whitespace.
            let (input, _) = anychar(input)?;

            let (input, mode) = parse_escaped_content_mode(input)?;
            // If this is `"`, then we have a string
            // If this is `f`, then we have a float
            use EscapedContentMode::*;
            match mode {
                String => {
                    let (input, str) = parse_string(input)?;
                    let (input, _) = whitespace(input)?;
                    Ok((input, Token::Escaped(EscapedContent::String(str))))
                },
                Float => {
                    let (input, x) = parse_float(input)?;
                    let (input, _) = whitespace(input)?;
                    Ok((input, Token::Escaped(EscapedContent::Float(x))))
                },
            }
        },
        // TODO: We should allow '-' as a name on its own. We need to check the char following '-'
        // is not a digit, in which case we are looking at identifier.
        c if c == '-' || c.is_digit(10) => {
            // 123
            // -123
            let (input, x) = i32(input)?;
            let (input, _) = whitespace(input)?;
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
                    let (input, _) = whitespace(input)?;
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
        match whitespace(input) {
            Ok((input, ())) => {
                Self { input } 
            },
            Err(_err) => {
                // eof
                Self { input }
            },
        }
    }

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
