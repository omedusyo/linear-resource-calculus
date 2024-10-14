mod tokenizer;
mod identifier;
mod lambda_cartesian;
mod lambda_linear;

use std::rc::Rc;
use std::fs;
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
use lambda_cartesian::{FunctionDefinition, Expression, Program, eval_start, parse_function_definition, parse_expression, parse_program};
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
    LoadProgram(String),
    Reload,
    PrintHello,
}

fn parse_command<'a>(rl: &mut Editor, input: &'a str) -> IResult<&'a str, Command> {
    fn parse_identifier(input: &str) -> IResult<&str, String> {
        // sequence of alphanumeric (or '_') characters that doesn't start with a digit.
        let (input, c0) = verify(anychar, |c: &char| char::is_alphabetic(*c) || *c == '_')(input)?;
        let (input, s) = take_while(|c: char| char::is_alphanumeric(c) || c == '_')(input)?;
        Ok((input, format!("{}{}", c0, s)))
    }

    fn parse_file_name(input: &str) -> IResult<&str, String> {
        fn is_forbidden(c: char) -> bool {
            match c {
                ' ' | '\r' | '\n' => true,
                _ => false,
            }
        }
        // sequence of consecutive non-whitespace chars.
        let (input, c0) = verify(anychar, |c: &char| !is_forbidden(*c))(input)?;
        let (input, s) = take_while(|c: char| !is_forbidden(c))(input)?;
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
            let (input, fn_declaration) = parse_function_definition(TokenStream::new(input))?;
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
        "load" | "l" => {
            let (input, _) = multispace0(input)?;
            let (input, file_name) = parse_file_name(input)?;
            Ok((input, Command::LoadProgram(file_name)))
        },
        "reload" | "r" => Ok((input, Command::Reload)),
        "print_hello" => Ok((input, Command::PrintHello)),
        _ => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::NoneOf })),

    }
}

struct State {
    program: Program,
    editor: Editor,
    current_file: Option<Rc<String>>,
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
            current_file: None,
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

fn load_file(state: &mut State, file_name: Rc<String>) {
    let file_name_copy = file_name.clone();
    match fs::read_to_string(&*file_name) {
        Ok(file_contents) => {
            state.current_file = Some(file_name_copy.clone());
            println!("Loading '{}'", file_name_copy);
            // println!("{}", file_contents);

            match parse_program(TokenStream::new(&file_contents)) {
                Ok((file_contents, program)) => {
                    if file_contents.input.is_empty() {
                        println!("Ok.");
                        state.program = program;
                    } else {
                        println!("Failed to parse the whole file. Remaining input: \n{}", file_contents.input);
                    }
                },
                Err(err) => {
                    dbg!(err);
                },
            }
        },
        Err(err) => {
            dbg!(err);
        },
    }
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
                println!(":load program.pmd (:l program.pmd)");
                println!(":reload (:r)");
                println!(":print_hello");
            },
            Command::Def(fn_def) => {
                state.program.update_function_definition(fn_def);
                println!("Ok.");
            },
            Command::ListFn => {
                for fn_name in &state.program.function_definitions_ordering {
                    print!("{}, ", fn_name.str()) // needs to be flushed...
                }
                println!(""); // flushes stdout
            },
            Command::LoadProgram(file_name) => {
                load_file(state, Rc::new(file_name))
            },
            Command::Reload => {
                match &state.current_file { 
                    Some(current_file) => {
                        load_file(state, Rc::clone(current_file))
                    },
                    None => {
                        println!("Nothing to reload.")
                    }
                }
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
