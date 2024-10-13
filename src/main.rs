mod tokenizer;
mod lambda;
mod lambda_linear;

use nom::{
  bytes::complete::{tag, take_while},
  combinator::{peek, verify},
  character::complete::{anychar, multispace0},
};
use rustyline::{
    error::ReadlineError,
    history::FileHistory,
    config::Builder as LineBuilder,
};
use lambda::{Expression, eval_start, parse_expression};
use tokenizer::TokenStream;

type IResult<I, O> = Result<(I, O), nom::Err<nom::error::Error<I>>>;
type Editor = rustyline::Editor<(), FileHistory>;

type IResult0<'a, O> = Result<(TokenStream<'a>, O), nom::Err<nom::error::Error<&'a str>>>;


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
