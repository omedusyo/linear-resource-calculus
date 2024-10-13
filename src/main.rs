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
use lambda::{FunctionDefinition, Expression, Program, eval_start, parse_function_declaration, parse_expression};
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
    Def(FunctionDefinition),
    ListFn,
    PrintHello,
}

fn parse_command<'a>(rl: &mut Editor, input: &'a str) -> IResult<&'a str, Command> {
    fn parse_identifier(input: &str) -> IResult<&str, String> {
        // sequence of alphanumeric (or '_') characters that doesn't start with a digit.
        let (input, c0) = verify(anychar, |c: &char| char::is_alphabetic(*c) || *c == '_')(input)?;
        let (input, s) = take_while(|c: char| char::is_alphanumeric(c) || c == '_')(input)?;
        Ok((input, format!("{}{}", c0, s)))
    }

    let input_backup = input;
    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, identifier) = parse_identifier(input)?;
    match &identifier[..] {
        "help" => Ok((input, Command::Help)),
        "def" => {
            let (input, _) = multispace0(input)?;
            let (input, fn_declaration) = parse_function_declaration(TokenStream::new(input))?;
            if input.input.is_empty() {
                let _ = rl.add_history_entry(input_backup);
                Ok((input.input, Command::Def(fn_declaration)))
            } else {
                Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty }))
            }
        },
        "list" => {
            let (input, _) = multispace0(input)?;
            let (input, identifier) = parse_identifier(input)?;
            match &identifier[..] {
                "fn" => Ok((input, Command::ListFn)),
                _ => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::NoneOf })),
            }
        },
        "print_hello" => Ok((input, Command::PrintHello)),
        _ => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::NoneOf })),

    }
}

struct State {
    program: Program,
    editor: Editor,
    history_file_name: &'static str,
}

impl State {
    fn new() -> rustyline::Result<Self> {
        let config = LineBuilder::new()
            // .edit_mode(rustyline::config::EditMode::Vi)
            .build();

        let mut editor: Editor = Editor::with_config(config)?;

        let history_file_name = ".history.txt";
        if editor.load_history(history_file_name).is_err() {
            println!("No previous history.");
        }

        Ok(Self {
            program: Program::new(),
            editor,
            history_file_name,
        })
    }

    fn save_history(&mut self) -> rustyline::Result<()> {
        self.editor.save_history(self.history_file_name)
    }
}

enum Msg {
    Line(Line),
}

fn update(state: &mut State, msg: Msg)  {
    match msg {
        Msg::Line(Line::Expression(expr)) => {
            // TODO: You need to eval the expression in the global env.
            match eval_start(&state.program, expr) {
                Ok(val) => println!("{}", val),
                Err(err) => println!("Eval error: {:?}", err),
            }
        },
        Msg::Line(Line::Command(cmd)) => match cmd {
            Command::Help => {
                println!(":help");
                println!(":def");
                println!(":list fn");
                println!(":print_hello");
            },
            Command::Def(fn_def) => {
                state.program.update_function_definition(fn_def);
                println!("Ok.");
            },
            Command::ListFn => {
                for fn_name in &state.program.function_definitions_ordering {
                    print!("{}, ", (*fn_name).0) // needs to be flushed...
                }
                println!(""); // flushes stdout
            },
            Command::PrintHello => {
                println!("hello?")
            },
        },
        Msg::Line(Line::Empty) => {},
    }
}

fn parse_line<'a>(rl: &mut Editor, input: &'a str) -> IResult<&'a str, Line> {
    let (input, _) = multispace0(input)?;
    if input.is_empty() {
        return Ok((input, Line::Empty))
    }
    let (_, c) = peek(anychar)(input)?;
    if c == ':' {
        let (input, cmd) = parse_command(rl, input)?;
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


fn main() -> rustyline::Result<()> {
    let mut state = State::new()?;

    loop {
        let readline = state.editor.readline("> ");
        match readline {
            Ok(line) => {
                match parse_line(&mut state.editor, &line) {
                    Ok((_, line)) => {
                        update(&mut state, Msg::Line(line));
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

    state.save_history()?;

    Ok(())
}
