use std::rc::Rc;
use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType, anyidentifier, anytoken, token};
use crate::IResult0;

// ===parser===
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

pub fn parse_expression(input: TokenStream) -> IResult0<Expression> {
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
                    let (input, (e0, e1)) = parse_arg_list2(input)?;
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

// ===Expressions===
#[derive(Debug, PartialEq, Clone)]
pub struct VarName(pub String);


// TODO: Seems like adding Lambda Expression messes everything up completely. Suddenly I have to
// put Rc everywhere instead of Box.
// Can I have `LinearExpression` that doesn't have this problem?
#[derive(Debug)]
pub enum Expression {
    Int(i32),
    Bool(bool),
    OperationApplication(OperationCode, Rc<Expression>, Rc<Expression>),
    If(Rc<Expression>, Rc<Expression>, Rc<Expression>),
    // TODO: Add some sort of if-then-else. I guess then you also want to have boolean expressions.
    // IfIntEqThenElse {  }
    VarUse(VarName),
    Let { arg: Rc<Expression>, var: VarName, body: Rc<Expression> },
    // Note that the body is in Rc. This is because when evaluating a lambda expression,
    // a closure value is createad which references this body.
    // TODO: If I passed the expression not as a reference, but directly,
    //       couldn't I just take ownership of the body?
    Lambda { var: VarName, body: Rc<Expression> },
    // Fat-chance
    LambdaRec { rec_var: VarName, var: VarName, body: Rc<Expression> },
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
    Closure { env: Rc<Env>, var: VarName, body: Rc<Expression> },
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
            Closure { env, var, body } => Closure { env: Rc::clone(env), var: var.clone(), body: body.clone() },
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
    VariableLookupFailure(VarName),
}

#[derive(Debug)]
pub enum Env {
    Empty,
    Push { var: VarName, value: Value, parent: Rc<Env> },
}

// TODO: Should I make a new type alias for Rc<Env>?
//   CartesianEnv?  supports .clone()  and .extend and .get
impl Env {
    fn new() -> Self {
        Self::Empty
    }
}

fn env_get(env: Rc<Env>, var_name: VarName) -> Result<Value, Error> {
    use Env::*;
    match &*env {
        Empty => Err(Error::VariableLookupFailure(var_name)),
        Push { var, value, parent } => {
            if *var == var_name {
                Ok(value.clone())
            } else {
                env_get(Rc::clone(parent), var_name)
            }
        },
    }
}

// TODO: Should I return an Rc<Env>?
fn env_extend(env: Rc<Env>, var_name: VarName, value: Value) -> Env {
    Env::Push { var: var_name, value, parent: env }
}

// TODO: Is this one better?
fn env_extend_rc(env: Rc<Env>, var_name: VarName, value: Value) -> Rc<Env> {
    Rc::new(Env::Push { var: var_name, value, parent: env })
}

pub fn eval_start(e: Expression) -> Result<Value, Error> {
    let env: Rc<Env> = Rc::new(Env::new());
    let value = eval(env, Rc::new(e))?;

    Ok(value)
}

// TODO: Why does e have to be passed as a reference? 
//       Seems the root cause was trying to evaluate a closure application
//       where the closure only had a reference to an expression,
//       but to call eval recursively on its body we required owning that body, which we could not.
fn eval(env: Rc<Env>, e: Rc<Expression>) -> Result<Value, Error> {
    use Expression::*;
    match &*e {
        Int(x) => Ok(Value::Int(*x)),
        Bool(x) => Ok(Value::Bool(*x)),
        OperationApplication(code, e0, e1) => {
            let val0 = eval(Rc::clone(&env), Rc::clone(e0))?;
            let val1 = eval(env, Rc::clone(e1))?;
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
            let arg_value = eval(Rc::clone(&env), Rc::clone(arg))?;
            match arg_value {
                Value::Bool(b) => {
                    if b {
                        eval(env, Rc::clone(then_branch))
                    } else {
                        eval(env, Rc::clone(else_branch))
                    }
                }
                _ => todo!(),
            }
        },
        VarUse(var_name) => env_get(env, var_name.clone()),
        Let { arg, var, body } => {
            let arg_value = eval(Rc::clone(&env), Rc::clone(arg))?;
            let env = Rc::new(env_extend(env, var.clone(), arg_value));
            eval(env, Rc::clone(body))
        },
        Lambda { var, body } => {
            Ok(Value::Closure { env, var: var.clone(), body: Rc::clone(body) })
        },
        LambdaRec { rec_var, var, body } => {
            let closure = Value::Closure { env: Rc::clone(&env), var: var.clone(), body: Rc::clone(body) };
            let env = Rc::new(env_extend(env, rec_var.clone(), closure));
            // closure.env = env;
            // TODO: How the hell can I do this? I probably need interior mutability for this,
            // that's insane.
            todo!()
        },
        Apply(e0, e1) => {
            let closure = eval(Rc::clone(&env), Rc::clone(e0))?;
            match closure {
                Value::Closure { env: closure_env, var, body } => {
                    let arg_value = eval(Rc::clone(&env), Rc::clone(e1))?;
                    eval(Rc::new(env_extend(closure_env, var, arg_value)), body)
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
