use std::rc::Rc;
use std::fs;
use nom::{
  bytes::complete::{tag, take_while},
  combinator::{peek, verify},
  character::complete::{anychar, multispace0},
};
use rustyline::{
    error::ReadlineError,
    config::Builder as LineBuilder,
};
use crate::{Editor, IResult};
use crate::interpreter::{Interpreter, CartesianInterpreter, LinearInterpreter, CombinedInterpreter};

#[derive(Debug)]
enum Command<'a> {
    Help,
    ParseDef(&'a str),
    ListFn,
    LoadProgram { file_name: String },
    Reload,
    PrintHello,
}

// ===REPL===
enum Msg<'a> {
    EmptyLineReceived,
    CommandReceived(Command<'a>),
    ExpressionStringReceived(&'a str),
}

pub trait Repl {
    type Interpreter: Interpreter;

    fn interpreter(&self) -> &Self::Interpreter;
    fn interpreter_mut(&mut self) -> &mut Self::Interpreter;

    fn editor(&self) -> &Editor;
    fn editor_mut(&mut self) -> &mut Editor;

    fn current_file(&self) -> Option<Rc<String>>;
    fn current_file_mut(&mut self) -> &mut Option<Rc<String>>;
    fn history_file_name(&self) -> &'static str;

    fn start_loop(&mut self, file_name: Option<&str>) {
        match file_name {
            Some(file_name) => {
                match self.load_file(Rc::new(file_name.to_string())) {
                    Some(()) => {},
                    None => return (),
                }
            },
            None => {}
        }
        loop {
            let readline = self.editor_mut().readline("> ");
            match readline {
                Ok(line) => {
                    match parse_line(&line) {
                        Ok((_, msg)) => {
                            // Note that we're not crashing on error.
                            // The error should be already handled
                            let _ = self.update(msg);
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
        self.save_history();

    }

    fn update(&mut self, msg: Msg<'_>) {
        let interpreter: &mut Self::Interpreter = self.interpreter_mut();
        match msg {
            Msg::ExpressionStringReceived(input) => {
                let input0 = input;
                let expr = interpreter.parse_expression(input);
                match expr {
                    Ok(expr) => {
                        let maybe_val = interpreter.eval_expression(expr);
                        println!("{}", interpreter.show(maybe_val));
                        let _ = self.editor_mut().add_history_entry(input0);
                    },
                    Err(err) => {
                        println!("{}", interpreter.show_parse_error(err));
                    }
                }
            },
            Msg::CommandReceived(cmd) => match cmd {
                Command::Help => {
                    println!(":help");
                    println!(":def");
                    println!(":list fn");
                    println!(":load program.pmd (:l program.pmd)");
                    println!(":reload (:r)");
                    println!(":print_hello");
                },
                Command::ParseDef(input) => {
                    //===Parsing Function Definition===
                    match interpreter.load_definition(input) {
                        Ok(()) => {
                            let _ = self.editor_mut().add_history_entry(input);
                            println!("Ok.");
                        },
                        Err(err) => {
                            println!("{}", interpreter.show_parse_error(err));
                        }
                    }

                },
                Command::ListFn => {
                    for fn_name in interpreter.list_functions() {
                        print!("{}, ", fn_name) // needs to be flushed...
                    }
                    println!(""); // flushes stdout
                },
                Command::LoadProgram { file_name } => {
                    let _ = self.load_file(Rc::new(file_name));
                },
                Command::Reload => {
                    match self.current_file() { 
                        Some(current_file) => {
                            let _ = self.load_file(current_file);
                        },
                        None => {
                            println!("Nothing to reload.");
                        }
                    }
                },
                Command::PrintHello => {
                    println!("hello?");
                },
            },
            Msg::EmptyLineReceived => {},
        }
    }

    fn load_file(&mut self, file_name: Rc<String>) -> Option<()> {
        let file_name_copy = file_name.clone();
        let interpreter: &mut Self::Interpreter = self.interpreter_mut();
        match fs::read_to_string(&*file_name) {
            Ok(file_contents) => {
                println!("Loading '{}'", file_name_copy);
                // println!("{}", file_contents);

                match interpreter.load_program(&file_contents) {
                    Ok(()) => {
                        *self.current_file_mut() = Some(file_name_copy.clone());
                        println!("Ok.");
                        Some(())
                    },
                    Err(err) => {
                        println!("{}", interpreter.show_parse_error(err));
                        None
                    }
                }
            },
            Err(err) => {
                dbg!(err);
                None
            },
        }
    }


    fn save_history(&mut self) {
        let history_file_name = self.history_file_name();
        // We're not printing the failure to save history.
        match self.editor_mut().save_history(history_file_name) {|
            Ok(_) => {},
            Err(err) => {
                println!("Failed to save history: {:?}", err)
            }
        }
    }
}

fn parse_line<'a>(input: &'a str) -> IResult<&'a str, Msg<'a>> {
    let (input, _) = multispace0(input)?;
    if input.is_empty() {
        return Ok((input, Msg::EmptyLineReceived))
    }
    let (_, c) = peek(anychar)(input)?;
    if c == ':' {
        let (input, cmd) = parse_command(input)?;
        return Ok((input, Msg::CommandReceived(cmd)))
    }

    // We have to assume the line contains an expression.
    Ok(("", Msg::ExpressionStringReceived(input)))
}


fn parse_command<'a>(input: &'a str) -> IResult<&'a str, Command<'a>> {
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

    let (input, _) = tag(":")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, identifier) = parse_identifier(input)?;
    match &identifier[..] {
        "help" => Ok((input, Command::Help)),
        "def" => {
            let (input, _) = multispace0(input)?;
            Ok(("", Command::ParseDef(input)))
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
            Ok((input, Command::LoadProgram { file_name }))
        },
        "reload" | "r" => Ok((input, Command::Reload)),
        "print_hello" => Ok((input, Command::PrintHello)),
        _ => Err(nom::Err::Error(nom::error::Error { input, code: nom::error::ErrorKind::NoneOf })),

    }
}

// ===Concrete repl implementations===
// =Cartesian=
pub struct CartesianRepl {
    interpreter: CartesianInterpreter,
    editor: Editor,
    current_file: Option<Rc<String>>,
    history_file_name: &'static str,
}

impl CartesianRepl {
    pub fn new() -> rustyline::Result<Self> {
        let config = LineBuilder::new()
            // .edit_mode(rustyline::config::EditMode::Vi)
            .build();

        let mut editor: Editor = Editor::with_config(config)?;

        let history_file_name = ".history_cartesian.txt";
        if editor.load_history(history_file_name).is_err() {
            println!("No previous history.");
        }

        Ok(Self {
            interpreter: CartesianInterpreter::new(),
            editor,
            current_file: None,
            history_file_name,
        })
    }
}

impl Repl for CartesianRepl {
    type Interpreter = CartesianInterpreter;

    fn interpreter(&self) -> &Self::Interpreter { &self.interpreter }
    fn interpreter_mut(&mut self) -> &mut Self::Interpreter { &mut self.interpreter }

    fn editor(&self) -> &Editor { &self.editor }
    fn editor_mut(&mut self) -> &mut Editor { &mut self.editor }

    fn current_file(&self) -> Option<Rc<String>> {
        match &self.current_file {
            Some(current_file) => Some(current_file.clone()),
            None => None
        }
    }
    fn current_file_mut(&mut self) -> &mut Option<Rc<String>> { &mut self.current_file }
    fn history_file_name(&self) -> &'static str { &self.history_file_name }
}

// =Linear=
pub struct LinearRepl {
    interpreter: LinearInterpreter,
    editor: Editor,
    current_file: Option<Rc<String>>,
    history_file_name: &'static str,
}

impl LinearRepl {
    pub fn new() -> rustyline::Result<Self> {
        let config = LineBuilder::new()
            // .edit_mode(rustyline::config::EditMode::Vi)
            .build();

        let mut editor: Editor = Editor::with_config(config)?;

        let history_file_name = ".history_linear.txt";
        if editor.load_history(history_file_name).is_err() {
            println!("No previous history.");
        }

        Ok(Self {
            interpreter: LinearInterpreter::new(),
            editor,
            current_file: None,
            history_file_name,
        })
    }
}

impl Repl for LinearRepl {
    type Interpreter = LinearInterpreter;

    fn interpreter(&self) -> &Self::Interpreter { &self.interpreter }
    fn interpreter_mut(&mut self) -> &mut Self::Interpreter { &mut self.interpreter }

    fn editor(&self) -> &Editor { &self.editor }
    fn editor_mut(&mut self) -> &mut Editor { &mut self.editor }

    fn current_file(&self) -> Option<Rc<String>> {
        match &self.current_file {
            Some(current_file) => Some(current_file.clone()),
            None => None
        }
    }
    fn current_file_mut(&mut self) -> &mut Option<Rc<String>> { &mut self.current_file }
    fn history_file_name(&self) -> &'static str { &self.history_file_name }
}

// =Combined=
pub struct CombinedRepl {
    interpreter: CombinedInterpreter,
    editor: Editor,
    current_file: Option<Rc<String>>,
    history_file_name: &'static str,
}

impl CombinedRepl {
    pub fn new() -> rustyline::Result<Self> {
        let config = LineBuilder::new()
            // .edit_mode(rustyline::config::EditMode::Vi)
            .build();

        let mut editor: Editor = Editor::with_config(config)?;

        let history_file_name = ".history_combined.txt";
        if editor.load_history(history_file_name).is_err() {
            println!("No previous history.");
        }

        Ok(Self {
            interpreter: CombinedInterpreter::new(),
            editor,
            current_file: None,
            history_file_name,
        })
    }
}

impl Repl for CombinedRepl {
    type Interpreter = CombinedInterpreter;

    fn interpreter(&self) -> &Self::Interpreter { &self.interpreter }
    fn interpreter_mut(&mut self) -> &mut Self::Interpreter { &mut self.interpreter }

    fn editor(&self) -> &Editor { &self.editor }
    fn editor_mut(&mut self) -> &mut Editor { &mut self.editor }

    fn current_file(&self) -> Option<Rc<String>> {
        match &self.current_file {
            Some(current_file) => Some(current_file.clone()),
            None => None
        }
    }
    fn current_file_mut(&mut self) -> &mut Option<Rc<String>> { &mut self.current_file }
    fn history_file_name(&self) -> &'static str { &self.history_file_name }
}
