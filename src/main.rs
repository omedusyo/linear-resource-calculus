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

// ===tokenizer===
enum Token {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Keyword(String),
    Eq,
    BindingSeparator,
    Comma,
    VarUseSymbol,
    Int(i32),
}

// ===parser===
fn parse_identifier(input: &str) -> IResult<&str, String> {
    // sequence of alphanumeric (or '_') characters that doesn't start with a digit.
    let (input, c0) = verify(anychar, |c: &char| char::is_alphabetic(*c) || *c == '_')(input)?;
    let (input, s) = take_while(|c: char| char::is_alphanumeric(c) || c == '_')(input)?;
    Ok((input, format!("{}{}", c0, s)))
}

fn parse_variable_use(input: &str) -> IResult<&str, VarName> {
    let (input, _) = tag("$")(input)?;
    let (input, str) = parse_identifier(input)?;
    Ok((input, VarName(str)))
}

fn identifier_to_operation_code(str: &str) -> Option<OperationCode> {
    match str {
        "add" => Some(OperationCode::Add),
        "mul" => Some(OperationCode::Mul),
        "eq" => Some(OperationCode::Eq),
        _ => None
    }
}


fn parse_operator_arguments(op_code: OperationCode, input: &str) -> IResult<&str, Expression> {
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

fn parse_arg_list2(input: &str) -> IResult<&str, (Expression, Expression)> {
    // "(e0, e1)  "
    let (input, _) = tag("(")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, e0) = parse_expression(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, e1) = parse_expression(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, _) = multispace0(input)?;
    Ok((input, (e0, e1)))
}

fn parse_integer(input: &str) -> IResult<&str, i32> {
    i32(input)
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    let (_, c) = peek(anychar)(input)?;
    match c {
        c if c == '-' || c.is_digit(10) => {
            // 123
            let (input, x) = parse_integer(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Expression::Int(x)))
        },
        '$' => {
            // $x
            let (input, var_name) = parse_variable_use(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, Expression::VarUse(var_name)))
        },
        _ => {
            // We assume it is an operator application (or constant := nullary operator)
            let (input, identifier) = parse_identifier(input)?;
            match &identifier[..] {
                "true" => {
                    // true
                    let (input, _) = multispace0(input)?;
                    Ok((input, Expression::Bool(true)))
                },
                "false" => {
                    // false
                    let (input, _) = multispace0(input)?;
                    Ok((input, Expression::Bool(false)))
                },
                "let" => {
                    // let { x = 5 . body }
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("{")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, identifier) = parse_identifier(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("=")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, arg) = parse_expression(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag(".")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, body) = parse_expression(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("}")(input)?;
                    let (input, _) = multispace0(input)?;
                    Ok((input, Expression::Let { arg: Rc::new(arg), var: VarName(identifier), body: Rc::new(body) }))
                },
                "if" => {
                    // if eq(3, 4) { 55 } { 67 }
                    let (input, _) = multispace0(input)?;
                    let (input, arg) = parse_expression(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("{")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, then_branch) = parse_expression(input)?;
                    let (input, _) = tag("}")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("{")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, else_branch) = parse_expression(input)?;
                    let (input, _) = tag("}")(input)?;
                    let (input, _) = multispace0(input)?;
                    Ok((input, Expression::If(Rc::new(arg), Rc::new(then_branch), Rc::new(else_branch))))
                },
                "fn" => {
                    // fn { x . add($x, 1) }
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("{")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, identifier) = parse_identifier(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag(".")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, body) = parse_expression(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag("}")(input)?;
                    let (input, _) = multispace0(input)?;
                    Ok((input, Expression::Lambda { var: VarName(identifier), body: Rc::new(body) }))
                },
                "app" => {
                    // app($f, $x)
                    let (input, _) = tag("(")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, e0) = parse_expression(input)?;
                    let (input, _) = tag(",")(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, e1) = parse_expression(input)?;
                    let (input, _) = multispace0(input)?;
                    let (input, _) = tag(")")(input)?;
                    let (input, _) = multispace0(input)?;
                    Ok((input, Expression::Apply(Rc::new(e0), Rc::new(e1))))
                },
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => parse_operator_arguments(op_code, input),
                    None => {
                       Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::Alt }))
                    },
                },
            }
        },
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
    let (input, expr) = parse_expression(input)?;
    if input.is_empty() {
        let _ = rl.add_history_entry(input0);
        Ok((input, Line::Expression(expr)))
    } else {
        Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::NonEmpty }))
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
