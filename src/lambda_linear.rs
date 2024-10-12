use std::fmt;

// ===Expressions===
#[derive(Debug, PartialEq, Clone)]
pub struct VarName(pub String);

#[derive(Debug)]
pub enum Expression {
    BigBadResource,
    OperationApplication(OperationCode, Box<Expression>, Box<Expression>),
    LinearVarUse(VarName),
}

#[derive(Debug, PartialEq)]
pub enum OperationCode {
    Add,
    Mul,
}

#[derive(Debug)]
pub enum Value {
    Int(i32),
    // Closure { env: Rc<Env>, var: VarName, body: Rc<Expression> },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Int(x) => write!(f, "{}", x),
            // Closure { ..  } => write!(f, "#[closure]"),
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
    Push { var: VarName, value: Value, parent: Box<Env> },
}

// TODO: Should I make a new type alias for Rc<Env>?
//   CartesianEnv?  supports .clone()  and .extend and .get
impl Env {
    fn new() -> Self {
        Self::Empty
    }
}

// fn env_get(env: Rc<Env>, var_name: VarName) -> Result<Value, Error> {
//     use Env::*;
//     match &*env {
//         Empty => Err(Error::VariableLookupFailure(var_name)),
//         Push { var, value, parent } => {
//             if *var == var_name {
//                 Ok(value.clone())
//             } else {
//                 env_get(Rc::clone(parent), var_name)
//             }
//         },
//     }
// }

// // TODO: Should I return an Rc<Env>?
// fn env_extend(env: Rc<Env>, var_name: VarName, value: Value) -> Env {
//     Env::Push { var: var_name, value, parent: env }
// }

pub fn eval_start(e: Expression) -> Result<Value, Error> {
    // let env: Rc<Env> = Rc::new(Env::new());
    // let value = eval(env, Rc::new(e))?;

    // Ok(value)
    todo!()
}

// TODO: Why does e have to be passed as a reference? 
//       Seems the root cause was trying to evaluate a closure application
//       where the closure only had a reference to an expression,
//       but to call eval recursively on its body we required owning that body, which we could not.
fn eval(env: Env, e: Expression) -> Result<Value, Error> {
    use Expression::*;
    match e {
        BigBadResource => todo!(),
        OperationApplication(code, e0, e1) => {
            // let val0 = eval(Rc::clone(&env), Rc::clone(e0))?;
            // let val1 = eval(env, Rc::clone(e1))?;
            // match (val0, val1) {
            //     (Value::Int(x0), Value::Int(x1)) => {
            //         use OperationCode::*;
            //         match code {
            //             Add => Ok(Value::Int(x0 + x1)),
            //             Mul => Ok(Value::Int(x0 * x1)),
            //         }
            //     },
            //     _ => todo!(),
            // }
            todo!()
        },
        LinearVarUse(var_name) => todo!(),
        // Let { arg, var, body } => {
        //     let arg_value = eval(Rc::clone(&env), Rc::clone(arg))?;
        //     let env = Rc::new(env_extend(env, var.clone(), arg_value));
        //     eval(env, Rc::clone(body))
        // },
        // Lambda { var, body } => {
        //     Ok(Value::Closure { env, var: var.clone(), body: Rc::clone(body) })
        // },
        // Apply(e0, e1) => {
        //     let closure = eval(Rc::clone(&env), Rc::clone(e0))?;
        //     match closure {
        //         Value::Closure { env: closure_env, var, body } => {
        //             let arg_value = eval(Rc::clone(&env), Rc::clone(e1))?;
        //             eval(Rc::new(env_extend(closure_env, var, arg_value)), body)
        //         },
        //         _ => todo!(),
        //     }
        // },
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
