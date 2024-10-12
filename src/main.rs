mod lambda;
mod lambda_linear;

use nom::{
  bytes::complete::{tag, take_while_m_n, take_while},
  combinator::{map_res, peek, verify},
  sequence::tuple,
  branch::alt,
  character::complete::{char, anychar, i32, multispace0, multispace1},
};
use rustyline::{
    error::ReadlineError,
    history::FileHistory,
    config::Builder as LineBuilder,
};
use std::rc::Rc;
use lambda::{VarName, Expression, OperationCode, eval_start};

type IResult<I, O> = Result<(I, O), nom::Err<nom::error::Error<I>>>;
type Editor = rustyline::Editor<(), FileHistory>;

type IResult0<'a, O> = Result<(TokenStream<'a>, O), nom::Err<nom::error::Error<&'a str>>>;

// ===tokenizer===
#[derive(Debug, PartialEq)]
pub enum Token {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Identifier(String),
    Eq,
    BindingSeparator,
    Comma,
    VarUseSymbol,
    Int(i32),
    End,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Identifier,
    Eq,
    BindingSeparator,
    Comma,
    VarUseSymbol,
    Int,
    End,
}

impl Token {
    pub fn type_(&self) -> TokenType {
        use Token::*;
        match self {
            OpenParen => TokenType::OpenParen,
            CloseParen => TokenType::CloseParen,
            OpenCurly => TokenType::OpenCurly,
            CloseCurly => TokenType::CloseCurly,
            Identifier(_) => TokenType::Identifier,
            Eq => TokenType::Eq,
            BindingSeparator => TokenType::BindingSeparator,
            Comma => TokenType::Comma,
            VarUseSymbol => TokenType::VarUseSymbol,
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
        '(' | ')' | '{' | '}' | '.' | ',' | '$' | ' ' | '\t' | '\r' | '\n' => true,
        _ => false,
    }
}

pub fn parse_identifier(input: &str) -> IResult<&str, String> {
    // Identifier can't start with a char in "(){},.$-012345689=".
    // Afterwards we have a sequence of any chars except those contained in "(){},.$" or
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
        '$' => {
            let (input, _) = anychar(input)?;
            // Note how there's no consumption of whitespace.
            Ok((input, Token::VarUseSymbol))
        },
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
            // We assume we are looking at identifier.
            let (input, str) = parse_identifier(input)?;
            Ok((input, Token::Identifier(str)))
        },
    }
}

#[derive(Debug, PartialEq)]
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

// ===parser===
fn identifier_to_operation_code(str: &str) -> Option<OperationCode> {
    match str {
        "add" => Some(OperationCode::Add),
        "mul" => Some(OperationCode::Mul),
        "eq" => Some(OperationCode::Eq),
        _ => None
    }
}

fn parse_operator_arguments(op_code: OperationCode, input: TokenStream) -> IResult0<Expression> {
    use OperationCode::*;
    match op_code {
        Add => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::OperationApplication(op_code, Rc::new(e0), Rc::new(e1))))
        },
        Mul => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::OperationApplication(op_code, Rc::new(e0), Rc::new(e1))))
        },
        Eq => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::OperationApplication(op_code, Rc::new(e0), Rc::new(e1))))
        },
    }
}

fn parse_arg_list2(input: TokenStream) -> IResult0<(Expression, Expression)> {
    // "(e0, e1)  "
    let (input, _) = token(TokenType::OpenParen)(input)?;
    let (input, e0) = parse_expression(input)?;
    let (input, _) = token(TokenType::Comma)(input)?;
    let (input, e1) = parse_expression(input)?;
    let (input, _) = token(TokenType::CloseParen)(input)?;
    Ok((input, (e0, e1)))
}

fn parse_expression(input: TokenStream) -> IResult0<Expression> {
    let (input, token0) = anytoken(input)?;
    use Token::*;
    match token0 {
        Int(x) => Ok((input, Expression::Int(x))),
        VarUseSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Expression::VarUse(VarName(var_name))))
        },
        Identifier(identifier) => {
            match &identifier[..] {
                "true" => Ok((input, Expression::Bool(true))),
                "false" => Ok((input, Expression::Bool(false))),
                "let" => {
                    // let { x = 5 . body }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, identifier) = anyidentifier(input)?;
                    let (input, _) = token(TokenType::Eq)(input)?;
                    let (input, arg) = parse_expression(input)?;

                    let (input, _) = token(TokenType::BindingSeparator)(input)?;

                    let (input, body) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, Expression::Let { arg: Rc::new(arg), var: VarName(identifier), body: Rc::new(body) }))
                },
                "if" => {
                    // if eq(3, 4) { 55 } { 67 }
                    let (input, arg) = parse_expression(input)?;

                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, then_branch) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, else_branch) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, Expression::If(Rc::new(arg), Rc::new(then_branch), Rc::new(else_branch))))
                },
                "fn" => {
                    // fn { x . add($x, 1) }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, identifier) = anyidentifier(input)?;
                    let (input, _) = token(TokenType::BindingSeparator)(input)?;
                    let (input, body) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, Expression::Lambda { var: VarName(identifier), body: Rc::new(body) }))
                },
                "app" => {
                    // app($f, $x)
                    let (input, _) = token(TokenType::OpenParen)(input)?;
                    let (input, e0) = parse_expression(input)?;
                    let (input, _) = token(TokenType::Comma)(input)?;
                    let (input, e1) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseParen)(input)?;

                    Ok((input, Expression::Apply(Rc::new(e0), Rc::new(e1))))
                },
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => parse_operator_arguments(op_code, input),
                    None => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
                },
            }
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}


#[derive(Debug)]
enum Line {
    Empty,
    Expression(Expression),
    Command(Command)
}

#[derive(Debug)]
enum Command {
    Help,
    PrintHello,
}

fn parse_command(input: &str) -> IResult<&str, Command> {
    fn parse_identifier(input: &str) -> IResult<&str, String> {
        // sequence of alphanumeric (or '_') characters that doesn't start with a digit.
        let (input, c0) = verify(anychar, |c: &char| char::is_alphabetic(*c) || *c == '_')(input)?;
        let (input, s) = take_while(|c: char| char::is_alphanumeric(c) || c == '_')(input)?;
        Ok((input, format!("{}{}", c0, s)))
    }

    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, identifier) = parse_identifier(input)?;
    match &identifier[..] {
        "help" => Ok((input, Command::Help)),
        "print_hello" => Ok((input, Command::PrintHello)),
        _ => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::NoneOf }))

    }
}

fn parse_line<'a>(rl: &mut Editor, input: &'a str) -> IResult<&'a str, Line> {
    let (input, _) = multispace0(input)?;
    if input.is_empty() {
        return Ok((input, Line::Empty))
    }
    let (_, c) = peek(anychar)(input)?;
    if c == ':' {
        let (input, cmd) = parse_command(input)?;
        return Ok((input, Line::Command(cmd)))
    }

    let input0 = input;
    let (input, expr) = parse_expression(TokenStream::new(input))?;
    if input.input.is_empty() {
        let _ = rl.add_history_entry(input0);
        Ok((input.input, Line::Expression(expr)))
    } else {
        Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty }))
    }
}

fn process_expression(expr: Expression) {
    match eval_start(expr) {
        Ok(val) => println!("{}", val),
        Err(err) => println!("Eval error: {:?}", err),
    }
}

fn process_command(cmd: Command) {
    match cmd {
        Command::Help => {
            println!(":help");
            println!(":print_hello");
        },
        Command::PrintHello => {
            println!("hello?")
        },
    }
}

fn main() -> rustyline::Result<()> {
    let config = LineBuilder::new()
        // .edit_mode(rustyline::config::EditMode::Vi)
        .build();

    let history_file_name = ".history.txt";

    let mut rl: Editor = Editor::with_config(config)?;

    if rl.load_history(history_file_name).is_err() {
        println!("No previous history.");
    }

    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                match parse_line(&mut rl, &line) {
                    Ok((_, line)) => {
                        match line {
                            Line::Expression(expr) => {
                                process_expression(expr)
                            },
                            Line::Command(cmd) => {
                                process_command(cmd)
                            },
                            Line::Empty => {},
                        }
                    },
                    Err(e) => {
                        println!("Parsing Error: {:?}", e);
                    },
                }
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

    rl.save_history(history_file_name)?;

    Ok(())
}
