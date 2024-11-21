use crate::calculi::cartesian;
use crate::calculi::linear;
use crate::calculi::combined;
use crate::tokenizer::TokenStream;

pub trait Interpreter {
    type Expression;
    type Value;
    type ParseError;
    type RuntimeError;
    fn parse_expression(&self, input: &str) -> Result<Self::Expression, Self::ParseError>;
    fn eval_expression(&mut self, expr: Self::Expression) -> Result<Self::Value, Self::RuntimeError>;
    // Even though we're gonna ignore self in all implementations,
    // apparently not having it there is problematic for Rust to build virtual table for "objects"
    // of type `dyn LanguageInterpreter`. Have no idea why.
    fn show_parse_error(&self, err: Self::ParseError) -> String;
    fn show(&self, eval_result: Result<Self::Value, Self::RuntimeError>) -> String;
    fn load_definition(&mut self, input: &str) -> Result<(), Self::ParseError>;
    fn load_program(&mut self, input: &str) -> Result<(), Self::ParseError>;
    fn list_functions(&mut self) -> Vec<String>;
}

pub struct CartesianInterpreter {
    program: cartesian::Program,
}

pub struct LinearInterpreter {
    program: linear::Program,
}

pub struct CombinedInterpreter {
    program: combined::Program,
}

impl CartesianInterpreter {
    pub fn new() -> Self {
        Self { program: cartesian::Program::new() }
    }
}

impl LinearInterpreter {
    pub fn new() -> Self {
        Self { program: linear::Program::new() }
    }
}

impl CombinedInterpreter {
    pub fn new() -> Self {
        Self { program: combined::Program::new() }
    }
}

// ===Cartesian===
impl Interpreter for CartesianInterpreter {
    type Expression = cartesian::Expression;
    type Value = cartesian::Value;
    type ParseError = String;
    type RuntimeError = cartesian::Error;

    fn parse_expression(&self, input: &str) -> Result<Self::Expression, Self::ParseError> {
        match cartesian::parse_expression(TokenStream::new(input)) {
            Ok((input, expr)) => if input.input.is_empty() {
                    Ok(expr)
                } else {
                    let err = nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty });
                    Err(format!("{:?}", err))
                }
            ,
            Err(err) => Err(format!("{:?}", err)),
        }
    }
    fn eval_expression(&mut self, expr: Self::Expression) -> Result<Self::Value, Self::RuntimeError> {
        cartesian::eval_start(&self.program, expr)
    }

    fn show_parse_error(&self, err: Self::ParseError) -> String { err }

    fn show(&self, eval_result: Result<Self::Value, Self::RuntimeError>) -> String {
        match eval_result {
            Ok(val) => format!("{}", val) ,
            Err(err) => format!("{:?}", err),
        }
    }

    fn load_definition(&mut self, input: &str) -> Result<(), Self::ParseError> {
        match cartesian::parse_function_definition(TokenStream::new(input)) {
            Ok((input, fn_def)) => if input.input.is_empty() {
                self.program.update_function_definition(fn_def);
                Ok(())
            } else {
                let err = nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty });
                Err(format!("{:?}", err))
            },
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn load_program(&mut self, input: &str) -> Result<(), Self::ParseError> {
        match cartesian::parse_program(TokenStream::new(input)) {
            Ok((input, program)) => {
                if input.input.is_empty() {
                    self.program = program;
                    Ok(())
                } else {
                    Err(format!("Failed to parse the whole file. Remaining input: \n{}", input.input))
                }
            },
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn list_functions(&mut self) -> Vec<String> {
        let mut fn_names = Vec::with_capacity(self.program.function_definitions_ordering.len());
        for fn_name in &self.program.function_definitions_ordering {
            fn_names.push(format!("{}", fn_name.str()));
        }
        fn_names
    }
}

// ===Linear===

impl Interpreter for LinearInterpreter {
    type Expression = linear::Expression;
    type Value = linear::Value;
    type ParseError = String;
    type RuntimeError = linear::Error;

    fn parse_expression(&self, input: &str) -> Result<Self::Expression, Self::ParseError> {
        match linear::parse_expression(TokenStream::new(input)) {
            Ok((input, expr)) => if input.input.is_empty() {
                    Ok(expr)
                } else {
                    let err = nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty });
                    Err(format!("{:?}", err))
                }
            ,
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn eval_expression(&mut self, expr: Self::Expression) -> Result<Self::Value, Self::RuntimeError> {
        let v =  &self.program;
        match linear::eval_start(&self.program, expr) {
            Ok(value) => {
                Ok(value)
            },
            Err(err) => Err(err)
        }
    }

    fn show_parse_error(&self, err: Self::ParseError) -> String { err }

    fn show(&self, eval_result: Result<Self::Value, Self::RuntimeError>) -> String {
        match eval_result {
            Ok(val) => format!("{}", val) ,
            Err(err) => format!("{:?}", err),
        }
    }

    fn load_definition(&mut self, input: &str) -> Result<(), Self::ParseError> {
        match linear::parse_function_definition(TokenStream::new(input)) {
            Ok((input, fn_def)) => if input.input.is_empty() {
                self.program.update_function_definition(fn_def);
                Ok(())
            } else {
                let err = nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty });
                Err(format!("{:?}", err))
            },
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn load_program(&mut self, input: &str) -> Result<(), Self::ParseError> {
        match linear::parse_program(TokenStream::new(input)) {
            Ok((input, program)) => {
                if input.input.is_empty() {
                    self.program = program;
                    Ok(())
                } else {
                    Err(format!("Failed to parse the whole file. Remaining input: \n{}", input.input))
                }
            },
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn list_functions(&mut self) -> Vec<String> {
        let mut fn_names = Vec::with_capacity(self.program.function_definitions_ordering.len());
        for fn_name in &self.program.function_definitions_ordering {
            fn_names.push(format!("{}", fn_name.str()));
        }
        fn_names
    }
}

// ===Combined===
impl Interpreter for CombinedInterpreter {
    type Expression = combined::Expression;
    type Value = combined::Value;
    type ParseError = String;
    type RuntimeError = combined::Error;

    fn parse_expression(&self, input: &str) -> Result<Self::Expression, Self::ParseError> {
        match combined::parse_expression(TokenStream::new(input)) {
            Ok((input, expr)) => if input.input.is_empty() {
                    Ok(expr)
                } else {
                    let err = nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty });
                    Err(format!("{:?}", err))
                }
            ,
            Err(err) => Err(format!("{:?}", err)),
        }
    }
    fn eval_expression(&mut self, expr: Self::Expression) -> Result<Self::Value, Self::RuntimeError> {
        combined::eval_start(&self.program, expr)
    }

    fn show_parse_error(&self, err: Self::ParseError) -> String { err }

    fn show(&self, eval_result: Result<Self::Value, Self::RuntimeError>) -> String {
        match eval_result {
            Ok(val) => format!("{}", val) ,
            Err(err) => format!("{:?}", err),
        }
    }

    fn load_definition(&mut self, input: &str) -> Result<(), Self::ParseError> {
        match combined::parse_function_definition(TokenStream::new(input)) {
            Ok((input, fn_def)) => if input.input.is_empty() {
                self.program.update_function_definition(fn_def);
                Ok(())
            } else {
                let err = nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::NonEmpty });
                Err(format!("{:?}", err))
            },
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn load_program(&mut self, input: &str) -> Result<(), Self::ParseError> {
        match combined::parse_program(TokenStream::new(input)) {
            Ok((input, program)) => {
                if input.input.is_empty() {
                    self.program = program;
                    Ok(())
                } else {
                    Err(format!("Failed to parse the whole file. Remaining input: \n{}", input.input))
                }
            },
            Err(err) => Err(format!("{:?}", err)),
        }
    }

    fn list_functions(&mut self) -> Vec<String> {
        // This definitely sucks. There are two kinds of functions, and this is ill equipped with
        // communicating such distinction.
        let mut fn_names = Vec::with_capacity(self.program.function_definitions_ordering.len());
        for (mode, fn_name) in &self.program.function_definitions_ordering {
            let mode_str = match mode {
                combined::Mode::Cartesian => "cart",
                combined::Mode::Linear => "lin",
            };
            fn_names.push(format!("{} {}", mode_str, fn_name.str()));
        }
        fn_names
    }
}
