use std::rc::Rc;
use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType, anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, delimited_nonempty_vector, delimited_vector};
use crate::IResult0;
use std::collections::HashMap;


// ===parser===
fn variable_name(input: TokenStream) -> IResult0<VariableName> {
    let (input, str) = anyidentifier(input)?;
    Ok((input, VariableName::new(str)))
}

fn identifier_to_operation_code(str: &str) -> Option<OperationCode> {
    match str {
        "+" => Some(OperationCode::Add),
        "*" => Some(OperationCode::Mul),
        "==" => Some(OperationCode::Eq),
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

// No parens, just a possibly empty comma separated list of identifiers.
fn parameter_vector(input: TokenStream) -> IResult0<Vec<VariableName>> {
    // TODO: Check uniqueness.
    delimited_vector(variable_name, token(TokenType::Comma))(input)
}

// No parens, just a possibly empty comma separated list of expressions.
fn expression_vector(input: TokenStream) -> IResult0<Vec<Expression>> {
    delimited_vector(parse_expression, token(TokenType::Comma))(input)
}


pub fn parse_function_declaration(input: TokenStream) -> IResult0<FunctionDefinition> {
    let (input, _) = identifier("fn")(input)?;
    let (input, function_name_str) = anyidentifier(input)?;
    let (input, _) = token(TokenType::OpenParen)(input)?;
    let (input, parameters) = parameter_vector(input)?;
    let (input, _) = token(TokenType::CloseParen)(input)?;

    let (input, _) = token(TokenType::OpenCurly)(input)?;
    let (input, body) = parse_expression(input)?;
    let (input, _) = token(TokenType::CloseCurly)(input)?;
    Ok((input, FunctionDefinition { name: FunctionName::new(function_name_str), parameters, body: Rc::new(body) }))
}

pub fn parse_expression(input: TokenStream) -> IResult0<Expression> {
    let (input, token0) = anytoken(input)?;
    use Token::*;
    match token0 {
        Int(x) => Ok((input, Expression::Int(x))),
        VarUseSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Expression::VarUse(VariableName::new(var_name))))
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

                    Ok((input, Expression::Let { arg: Rc::new(arg), var: VariableName::new(identifier), body: Rc::new(body) }))
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

                    Ok((input, Expression::Lambda { var: VariableName::new(identifier), body: Rc::new(body) }))
                },
                "app" => {
                    // app($f, $x)
                    let (input, (e0, e1)) = parse_arg_list2(input)?;
                    Ok((input, Expression::Apply(Rc::new(e0), Rc::new(e1))))
                },
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => parse_operator_arguments(op_code, input),
                    None => {
                        let (input, token_match) = peek_token(TokenType::OpenParen)(input)?;
                        match token_match {
                            Some(_) => {
                                // Here we have a function call
                                let (input, _) = token(TokenType::OpenParen)(input)?;
                                let (input, arguments) = expression_vector(input)?;
                                let (input, _) = token(TokenType::CloseParen)(input)?;
                                let arguments = arguments.into_iter().map(Rc::new).collect();
                                Ok((input, Expression::Call(FunctionName::new(identifier), arguments)))
                            },
                            None => {
                                // TODO: Check if this is a function application
                                // I need to peek if the next token is an open paren
                                Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt }))
                            }
                        }
                    },
                },
            }
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// ===Identifiers===
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableName(pub Rc<String>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionName(pub Rc<String>);

impl VariableName {
    fn new(str: String) -> Self {
        Self(Rc::new(str))
    }
}

impl FunctionName {
    fn new(str: String) -> Self {
        Self(Rc::new(str))
    }
}

// ===Program===
pub struct Program {
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            function_definitions: HashMap::new(),
            function_definitions_ordering: vec![],
        }
    }

    pub fn update_function_definition(&mut self, fn_def: FunctionDefinition) {
        let fn_name = fn_def.name.clone();
        self.function_definitions.insert(fn_name.clone(), fn_def);

        // This is a bit insane. But function redefinitions don't occur frequently.
        match self.function_definitions_ordering.iter().position(|name| name == &fn_name) {
            Some(fn_index) => {
                self.function_definitions_ordering.remove(fn_index);
            },
            None => {}
        }
        self.function_definitions_ordering.push(fn_name);
    }

    pub fn get_function_definition(&self, function_name: FunctionName) -> Option<&FunctionDefinition> {
        self.function_definitions.get(&function_name)
    }
}

// ===Declarations===
#[derive(Debug)]
pub struct FunctionDefinition {
    name: FunctionName,
    parameters: Vec<VariableName>,
    body: Rc<Expression>,
}

// ===Expressions===

// TODO: Seems like adding Lambda Expression messes everything up completely. Suddenly I have to
// put Rc everywhere instead of Box.
// Can I have `LinearExpression` that doesn't have this problem?
#[derive(Debug)]
pub enum Expression {
    Call(FunctionName, Vec<Rc<Expression>>),
    Int(i32),
    Bool(bool),
    OperationApplication(OperationCode, Rc<Expression>, Rc<Expression>),
    If(Rc<Expression>, Rc<Expression>, Rc<Expression>),
    // TODO: Add some sort of if-then-else. I guess then you also want to have boolean expressions.
    // IfIntEqThenElse {  }
    VarUse(VariableName),
    Let { arg: Rc<Expression>, var: VariableName, body: Rc<Expression> },
    // Note that the body is in Rc. This is because when evaluating a lambda expression,
    // a closure value is createad which references this body.
    // TODO: If I passed the expression not as a reference, but directly,
    //       couldn't I just take ownership of the body?
    Lambda { var: VariableName, body: Rc<Expression> },
    // Fat-chance
    LambdaRec { rec_var: VariableName, var: VariableName, body: Rc<Expression> },
    Apply(Rc<Expression>, Rc<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum OperationCode {
    Add,
    Mul,
    Eq,
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
    Bool(bool),
    // TODO: When adding a List as a value, remember to use Rc, since we're cloning values.
    // TODO: Why does the body have to be an Rc?
    //       Can it be a Box? It can't be...
    //       because Values are stored in environments.
    //       And environments have to be clonable.
    //
    //       Hmm, I feel like body has to be in Rc...
    //       since conceptually multiple closures with differing captured environments
    //       could point to the same body expression.
    Closure { env: Env, var: VariableName, body: Rc<Expression> },
    // But remember that to evaluate closure, we'll have to evaluate the underlying expression (in
    // the captured environment). But `eval_direct` takes ownership of the Expression.
    // So it seems that
}

impl Clone for Value {
    fn clone(&self) -> Self {
        use Value::*;
        match self {
            Int(x) => Int(*x),
            Bool(b) => Bool(*b),
            Closure { env, var, body } => Closure { env: env.clone(), var: var.clone(), body: body.clone() },
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Int(x) => write!(f, "{}", x),
            Bool(b) => write!(f, "{}", b),
            Closure { ..  } => write!(f, "#[closure]"),
        }
    }
}

// Apparently Rust is unable to derive Clone for Value, because Env doesn't implement Clone, even
// though it is wrapped in Rc<_>.

#[derive(Debug, PartialEq)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    FunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    VariableLookupFailure(VariableName),
}

// ===Environment===
#[derive(Debug, Clone)]
pub struct Env(Rc<Env0>);

#[derive(Debug)]
pub enum Env0 {
    Empty,
    Push { var: VariableName, value: Value, parent: Env },
}

// TODO: Should I make a new type alias for Rc<Env>?
//   CartesianEnv?  supports .clone()  and .extend and .get
impl Env0 {
    fn new() -> Self {
        Self::Empty
    }
}

impl Env {
    fn new() -> Self {
        Self(Rc::new(Env0::Empty))
    }

    fn get(&self, var_name: VariableName) -> Result<Value, Error> {
        use Env0::*;
        match &*self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if *var == var_name {
                    Ok(value.clone())
                } else {
                    parent.get(var_name)
                }
            },
        }
    }


    fn extend(self, var_name: VariableName, value: Value) -> Env {
        Self(Rc::new(Env0::Push { var: var_name, value, parent: self }))
    }

    fn extend_many(mut self, bindings: Vec<(VariableName, Value)>) -> Env {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ===Evaluation===

pub fn eval_start(program: &Program, e: Expression) -> Result<Value, Error> {
    let value = eval(program, Env::new(), Rc::new(e))?;
    Ok(value)
}

fn apply_function(program: &Program, fn_name: FunctionName, arg_values: Vec<Value>) -> Result<Value, Error> {
    let Some(fn_def) = program.get_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name)) };
    let num_of_arguments: usize = fn_def.parameters.len();
    if num_of_arguments != arg_values.len() {
        return Err(Error::FunctionCallArityMismatch { fn_name, expected: num_of_arguments, received: arg_values.len() })
    }
    
    let mut bindings: Vec<(VariableName, Value)> = Vec::with_capacity(num_of_arguments);
    for (i, val) in arg_values.into_iter().enumerate() {
        bindings.push((fn_def.parameters[i].clone(), val));
    }
    let env = Env::new().extend_many(bindings);
    eval(program, env, fn_def.body.clone())
}

// TODO: Why does e have to be passed as a reference? 
//       Seems the root cause was trying to evaluate a closure application
//       where the closure only had a reference to an expression,
//       but to call eval recursively on its body we required owning that body, which we could not.
fn eval(program: &Program, env: Env, e: Rc<Expression>) -> Result<Value, Error> {
    use Expression::*;
    match &*e {
        Call(fn_name, args) => {
            let mut values: Vec<Value> = Vec::with_capacity(args.len());
            for arg in args {
                values.push(eval(program, env.clone(), Rc::clone(arg))?);
            }
            apply_function(program, fn_name.clone(), values)
        },
        Int(x) => Ok(Value::Int(*x)),
        Bool(x) => Ok(Value::Bool(*x)),
        OperationApplication(code, e0, e1) => {
            let val0 = eval(program, env.clone(), Rc::clone(e0))?;
            let val1 = eval(program, env, Rc::clone(e1))?;
            match (val0, val1) {
                (Value::Int(x0), Value::Int(x1)) => {
                    use OperationCode::*;
                    match code {
                        Add => Ok(Value::Int(x0 + x1)),
                        Mul => Ok(Value::Int(x0 * x1)),
                        Eq => Ok(Value::Bool(x0 == x1)),
                    }
                },
                (Value::Bool(b0), Value::Bool(b1)) => {
                    use OperationCode::*;
                    match code {
                        Eq => Ok(Value::Bool(b0 == b1)),
                        _ => todo!(),
                    }
                }
                _ => todo!(),
            }
        },
        If(arg, then_branch, else_branch) => {
            let arg_value = eval(program, env.clone(), Rc::clone(arg))?;
            match arg_value {
                Value::Bool(b) => {
                    if b {
                        eval(program, env, Rc::clone(then_branch))
                    } else {
                        eval(program, env, Rc::clone(else_branch))
                    }
                }
                _ => todo!(),
            }
        },
        VarUse(var_name) => env.get(var_name.clone()),
        Let { arg, var, body } => {
            let arg_value = eval(program, env.clone(), Rc::clone(arg))?;
            let env = env.extend(var.clone(), arg_value);
            eval(program, env, Rc::clone(body))
        },
        Lambda { var, body } => {
            Ok(Value::Closure { env, var: var.clone(), body: Rc::clone(body) })
        },
        LambdaRec { rec_var, var, body } => {
            let closure = Value::Closure { env: env.clone(), var: var.clone(), body: Rc::clone(body) };
            let env = Rc::new(env.extend(rec_var.clone(), closure));
            // closure.env = env;
            // TODO: How the hell can I do this? I probably need interior mutability for this,
            // that's insane.
            todo!()
        },
        Apply(e0, e1) => {
            let closure = eval(program, env.clone(), Rc::clone(e0))?;
            match closure {
                Value::Closure { env: closure_env, var, body } => {
                    let arg_value = eval(program, env.clone(), Rc::clone(e1))?;
                    eval(program, closure_env.extend(var, arg_value), body)
                },
                _ => todo!(),
            }
        },
    }
}

// TODO: Is it wise for eval to take ownership of the expression?
//       Seems like when we're doing some sort of a loop,
//       we're gonna be going through the same expression multiple times.
//       And that doesn't look like ownership to me.
//
//       I bet if this was some sort of a linear eval, then we could take ownership...
// fn eval_direct(env: Rc<Env>, e: Expression) -> Result<Value, Error> {
//     use Expression::*;
//     match e {
//         Int(x) => Ok(Value::Int(x)),
//         OperationApplication(code, e0, e1) => {
//             let val0 = eval_direct(Rc::clone(&env), *e0)?;
//             let val1 = eval_direct(env, *e1)?;
//             match (val0, val1) {
//                 (Value::Int(x0), Value::Int(x1)) => {
//                     use OperationCode::*;
//                     match code {
//                         Add => Ok(Value::Int(x0 + x1)),
//                         Mul => Ok(Value::Int(x0 * x1)),
//                     }
//                 },
//                 _ => todo!(),
//             }
//         },
//         VarUse(var_name) => env_get(env, var_name),
//         Let { arg, var, body } => {
//             let arg_value = eval_direct(Rc::clone(&env), *arg)?;
//             let env = Rc::new(env_extend(env, var.clone(), arg_value));
//             eval_direct(env, *body)
//         },
//         Lambda { var, body } => {
//             Ok(Value::Closure { env, var: var.clone(), body: Rc::new(*body) })
//         },
//         Apply(e0, e1) => {
//             let closure = eval_direct(Rc::clone(&env), *e0)?;
//             match closure {
//                 Value::Closure { env: closure_env, var, body } => {
//                     let arg_value = eval_direct(Rc::clone(&env), *e1)?;
//                     // let x = eval_direct(Rc::new(env_extend(closure_env, var, arg_value)), body);
//                     todo!()
//                 },
//                 _ => todo!(),
//             }
//         },
//         _ => todo!(),
//     }
// }
