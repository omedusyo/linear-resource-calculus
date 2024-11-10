use std::rc::Rc;
use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType, anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, vector, delimited_nonempty_vector, delimited_vector};
use crate::identifier::{VariableName, FunctionName, Tag, variable_name};
use crate::IResult0;
use std::collections::HashMap;

// ===parser===
// ===Program===
#[derive(Debug, Clone)]
pub struct Program {
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
}

#[derive(Debug, Clone)]
pub enum FunctionDefinition {
    Cartesian(CartesianFunctionDefinition),
    Linear(LinearFunctionDefinition),
}

#[derive(Debug, Clone)]
pub struct CartesianFunctionDefinition {
    name: FunctionName,
    cartesian_parameters: Vec<VariableName>,
    // Note the absence of linear parameters./
    body: CartesianExpression,
}

#[derive(Debug, Clone)]
pub struct LinearFunctionDefinition {
    name: FunctionName,
    cartesian_parameters: Vec<VariableName>,
    linear_parameters: Vec<VariableName>,
    body: LinearExpression,
}

impl FunctionDefinition {
    fn name(&self) -> &FunctionName {
        use FunctionDefinition::*;
        match self {
            Cartesian(fn_def) => &fn_def.name,
            Linear(fn_def) => &fn_def.name,
        }
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            function_definitions: HashMap::new(),
            function_definitions_ordering: vec![],
        }
    }

    pub fn update_function_definition(&mut self, fn_def: FunctionDefinition) {
        let fn_name = fn_def.name().clone();
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

    pub fn get_linear_function_definition(&self, function_name: FunctionName) -> Option<&LinearFunctionDefinition> {
        use FunctionDefinition::*;
        match self.function_definitions.get(&function_name) {
            Some(Linear(fn_def)) => {
                Some(fn_def)
            },
            _ => None,
        }
    }

    pub fn get_cartesian_function_definition(&self, function_name: FunctionName) -> Option<&CartesianFunctionDefinition> {
        use FunctionDefinition::*;
        match self.function_definitions.get(&function_name) {
            Some(Cartesian(fn_def)) => {
                Some(fn_def)
            },
            _ => None,
        }
    }

    // TODO: Why do I need this?
    pub fn get_clone_of_linear_function_definition(&self, function_name: FunctionName) -> Option<LinearFunctionDefinition> {
        match self.get_linear_function_definition(function_name) {
            Some(fn_def) => Some(fn_def.clone()),
            None => None,
        }
    }
}

// ===Expressions===
// TODO
// #[derive(Debug, Clone)]
// pub enum Expression {
//     Cartesian(CartesianExpression),
//     Linear(LinearExpression),
// }

#[derive(Debug, PartialEq, Clone)]
pub enum OperationCode {
    Add,
    Sub,
    Mul,
    Eq,
    Duplicate,
    Discard,
}

// ==Cartesian==
#[derive(Debug, Clone)]
pub struct CartesianExpression(pub Rc<CartesianExpression0>);

#[derive(Debug, Clone)]
pub enum CartesianExpression0 {
    Int(i32),
    OperationApplication(OperationCode, CartesianExpression, CartesianExpression),
    Call(FunctionName, Vec<CartesianExpression>),
    Tagged(Tag, CartesianExpression),
    Tuple(Vec<CartesianExpression>),
    Match { arg: CartesianExpression, branches: Vec<CartesianPatternBranch> },
    VarLookup(VariableName),
    Let { arg: CartesianExpression, var: VariableName, body: CartesianExpression },
    Object { branches: Rc<Vec<CartesianPatternBranch>> },
    Send(CartesianExpression, CartesianExpression),
}

// ==Linear==
#[derive(Debug, Clone)]
pub struct LinearExpression(pub Box<LinearExpression0>);

#[derive(Debug, Clone)]
pub enum LinearExpression0 {
    Int(i32),
    OperationApplication1(OperationCode, LinearExpression),
    OperationApplication2(OperationCode, LinearExpression, LinearExpression),
    Call(FunctionName, Vec<CartesianExpression>, Vec<LinearExpression>),
    Tagged(Tag, LinearExpression),
    Tuple(Vec<LinearExpression>),
    Match { arg: LinearExpression, branches: Vec<LinearPatternBranch> },
    VarUse(VariableName),
    Let { arg: LinearExpression, var: VariableName, body: LinearExpression },
    Object { captured_bindings: LinearBindings, branches: Vec<LinearPatternBranch> },
    Send(LinearExpression, LinearExpression),
}

#[derive(Debug, Clone)]
pub struct LinearBindings(Box<LinearBindings0>);

#[derive(Debug, Clone)]
pub enum LinearBindings0 {
    Empty,
    Push { var: VariableName, expr: LinearExpression, parent: LinearBindings },
}

// ===Patterns===
#[derive(Debug, Clone)]
pub struct CartesianPatternBranch {
    pub pattern: Pattern,
    pub body: CartesianExpression,
}

#[derive(Debug, Clone)]
pub struct LinearPatternBranch {
    pub pattern: Pattern,
    pub body: LinearExpression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(VariableName),
    Tagged(Tag, Box<Pattern>),
    Tuple(Vec<Pattern>),
}

// ===Values===

#[derive(Debug)]
pub enum Value {
    Linear(LinearValue),
    Cartesian(CartesianValue),
}

#[derive(Debug, Clone)]
pub enum CartesianValue {
    Int(i32),
    Tagged(Tag, Box<CartesianValue>),
    Tuple(Vec<CartesianValue>),
    ClosureObject { captured_env: CartesianEnv, branches: Rc<Vec<CartesianPatternBranch>> },
}

#[derive(Debug)]
pub enum LinearValue {
    Int(i32),
    Tagged(Tag, Box<LinearValue>),
    Tuple(Vec<LinearValue>), // Would be cool if we could use Box<[Value]>, since we don't need to resize
    ClosureObject { captured_env: LinearEnv, branches: Vec<LinearPatternBranch> },
}

impl LinearValue {
    fn discard(self) -> () {
        use LinearValue::*;
        match self {
            Int(_x) => (),
            Tagged(tag, val) => (*val).discard(),
            Tuple(values) => {
                for val in values {
                    let _ = val.discard();
                }
                ()
            },
            ClosureObject { .. } => todo!(), // this should crash
        }
    }

    fn duplicate(self) -> (Self, Self) {
        use LinearValue::*;
        match self {
            Int(x) => (Int(x), Int(x)),
            Tagged(tag, val) => {
                let (val0, val1) = (*val).duplicate();
                (Tagged(tag.clone(), Box::new(val0)), Tagged(tag, Box::new(val1)))
            },
            Tuple(values) => {
                let mut values0 = Vec::with_capacity(values.len());
                let mut values1 = Vec::with_capacity(values.len());
                for val in values {
                    let (val0, val1) = val.duplicate();
                    values0.push(val0);
                    values1.push(val1);
                }
                (Tuple(values0), Tuple(values1))
            },
            ClosureObject { .. } => todo!(), // this should crash
        }
    }
}

// ===Env===
// ==Cartesian==
#[derive(Debug, Clone)]
pub struct CartesianEnv(Rc<CartesianEnv0>);

#[derive(Debug)]
pub enum CartesianEnv0 {
    Empty,
    Push { var: VariableName, value: CartesianValue, parent: CartesianEnv },
}

impl CartesianEnv {
    fn new() -> Self { Self(Rc::new(CartesianEnv0::Empty)) }

    fn get(&self, var_name: VariableName) -> Result<CartesianValue, Error> {
        use CartesianEnv0::*;
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

    fn extend(self, var_name: VariableName, value: CartesianValue) -> Self {
        Self(Rc::new(CartesianEnv0::Push { var: var_name, value, parent: self }))
    }

    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, CartesianValue)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ==Linear==
#[derive(Debug)]
pub struct LinearEnv(Box<LinearEnv0>);

#[derive(Debug)]
pub enum LinearEnv0 {
    Empty,
    Push { var: VariableName, value: LinearValue, parent: LinearEnv },
}

impl LinearEnv {
    fn new() -> Self { Self(Box::new(LinearEnv0::Empty)) }

    fn is_empty(&self) -> bool { matches!(*self.0, LinearEnv0::Empty) }

    fn use_(self, var_name: VariableName) -> Result<(LinearValue, Self), Error> {
        use LinearEnv0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    Ok((value, parent))
                } else {
                    let (value0, parent) = parent.use_(var_name)?;
                    Ok((value0, LinearEnv(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }

    fn extend(self, var: VariableName, value: LinearValue) -> Self {
        Self(Box::new(LinearEnv0::Push { var, value, parent: self }))
    }

    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, LinearValue)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ===Error===
#[derive(Debug)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    CartesianFunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    LinearFunctionCallArityMismatch { fn_name: FunctionName, expected_cartesian: usize, received_cartesian: usize, expected_linear: usize, received_linear: usize },
    VariableLookupFailure(VariableName),
    UnableToFindMatchingPattern,
    InvalidPatternMatch(PatternMatchErrror),
    UnconsumedResources(LinearEnv),
    AttemptToSendMessageToNonObject(Value),
}

#[derive(Debug)]
pub enum PatternMatchErrror {
    TaggedValueFedToNonTaggedPattern,
    TupleFedToNonTuplePattern,
    TupleSizesDidNotMatch,
    TupleMatchedAgainstMatchExpressionWithMultipleBranches,
    AttemptToMatchNonInductiveValue,
}

// ===Evaluation===
fn cartesian_eval(program: &Program, env: &CartesianEnv, e: &CartesianExpression) -> Result<CartesianValue, Error> {
    use CartesianExpression0::*;
    match &*(e.0) {
        Int(x) => Ok(CartesianValue::Int(*x)),
        OperationApplication(code, e0, e1) => {
            let val0 = cartesian_eval(program, env, e0)?;
            let val1 = cartesian_eval(program, env, e1)?;
            match (val0, val1) {
                (CartesianValue::Int(x0), CartesianValue::Int(x1)) => {
                    use OperationCode::*;
                    match code {
                        Add => Ok(CartesianValue::Int(x0 + x1)),
                        Sub => Ok(CartesianValue::Int(x0 - x1)),
                        Mul => Ok(CartesianValue::Int(x0 * x1)),
                        Eq => if x0 == x1 {
                            Ok(CartesianValue::Tagged(Tag::new("T".to_string()), Box::new(CartesianValue::Tuple(vec![]))))
                        } else {
                            Ok(CartesianValue::Tagged(Tag::new("F".to_string()), Box::new(CartesianValue::Tuple(vec![]))))
                        },
                        Duplicate | Discard => todo!(),
                    }
                },
                _ => todo!(),
            }
        },
        Call(fn_name, args) => {
            let mut values: Vec<CartesianValue> = Vec::with_capacity(args.len());
            for arg in args {
                values.push(cartesian_eval(program, env, &arg)?);
            }
            let Some(fn_def) = program.get_cartesian_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name.clone())) };
            apply_cartesian_function(program, fn_def, values)
        },
        Tagged(tag, e) => {
            let val = cartesian_eval(program, env, e)?;
            Ok(CartesianValue::Tagged(tag.clone(), Box::new(val)))
        },
        Tuple(args) => {
            let mut values: Vec<CartesianValue> = Vec::with_capacity((&*args).len());
            for arg in &*args {
                values.push(cartesian_eval(program, env, arg)?)
            }
            Ok(CartesianValue::Tuple(values))
        },
        Match { arg, branches } => {
            let arg_value = cartesian_eval(program, env, arg)?;
            cartesian_apply_msg_to_branches(program, env, branches, &arg_value)
        },
        VarLookup(var_name) => env.get(var_name.clone()),
        Let { arg, var, body } => {
            let arg_value = cartesian_eval(program, env, arg)?;
            let env = env.clone().extend(var.clone(), arg_value);
            cartesian_eval(program, &env, body)
        },
        Object { branches } => {
            Ok(CartesianValue::ClosureObject { captured_env: env.clone(), branches: Rc::clone(branches) })
        },
        Send(e0, e1) => {
            let obj = cartesian_eval(program, env, e0)?;
            match obj {
                CartesianValue::ClosureObject { captured_env, branches } => {
                    let msg = cartesian_eval(program, env, e1)?;
                    cartesian_apply_msg_to_branches(program, &captured_env, &branches, &msg)
                },
                obj => Err(Error::AttemptToSendMessageToNonObject(Value::Cartesian(obj))),
            }
        }
    }
}

fn linear_eval(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, e: LinearExpression) -> Result<(LinearEnv, LinearValue), Error> {
    use LinearExpression0::*;
    match *e.0 {
        Int(x) => Ok((env, LinearValue::Int(x))),
        OperationApplication1(code, e0) => {
            let (env, val0) = linear_eval(program, cartesian_env, env, e0)?;
            use OperationCode::*;
            Ok((
                env,
                match code {
                    Duplicate => {
                        let (v0, v1) = val0.duplicate();
                        LinearValue::Tuple(vec![v0, v1])
                    },
                    Discard => {
                        let () = val0.discard();
                        LinearValue::Tuple(vec![])
                    },
                    _ => todo!(), // This should crash
                }
            ))
        },
        OperationApplication2(code, e0, e1) => {
            let (env, val0) = linear_eval(program, cartesian_env, env, e0)?;
            let (env, val1) = linear_eval(program, cartesian_env, env, e1)?;
            match (val0, val1) {
                (LinearValue::Int(x0), LinearValue::Int(x1)) => {
                    use OperationCode::*;
                    Ok((
                        env,
                        match code {
                            Add => LinearValue::Int(x0 + x1),
                            Sub => LinearValue::Int(x0 - x1),
                            Mul => LinearValue::Int(x0 * x1),
                            Eq => if x0 == x1 {
                                LinearValue::Tagged(Tag::new("T".to_string()), Box::new(LinearValue::Tuple(vec![])))
                            } else {
                                LinearValue::Tagged(Tag::new("F".to_string()), Box::new(LinearValue::Tuple(vec![])))
                            },
                            Discard | Duplicate => todo!()
                        }
                    ))
                },
                _ => todo!(),
            }
        },
        Call(fn_name, cartesian_args, args) => {
            let mut cartesian_values: Vec<CartesianValue> = Vec::with_capacity(cartesian_args.len());
            for arg in cartesian_args {
                let val = cartesian_eval(program, cartesian_env, &arg)?;
                cartesian_values.push(val);
            }
            let mut values: Vec<LinearValue> = Vec::with_capacity(args.len());
            let mut env = env;
            for arg in args {
                let (env0, val) = linear_eval(program, cartesian_env, env, arg)?;
                env = env0;
                values.push(val);
            }
            let Some(fn_def) = program.get_clone_of_linear_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name)) };
            let val = linear_apply_function(program, fn_def, cartesian_values, values)?;
            Ok((env, val))
        },
        Tagged(tag, e) => {
            let (env, val) = linear_eval(program, cartesian_env, env, e)?;
            Ok((env, LinearValue::Tagged(tag, Box::new(val))))
        },
        Tuple(args) => {
            let mut env = env;
            let mut values: Vec<LinearValue> = Vec::with_capacity(args.len());
            for arg in args {
                let (env0, val) = linear_eval(program, cartesian_env, env, arg)?;
                env = env0;
                values.push(val);
            }
            Ok((env, LinearValue::Tuple(values)))
        },
        VarUse(var_name) => {
            let (value, env) = env.use_(var_name)?;
            Ok((env, value))
        },
        Match { arg, branches } => {
            let (env, arg_value) = linear_eval(program, cartesian_env, env, arg)?;
            linear_apply_msg_to_branches(program, cartesian_env, env, branches, arg_value)
        },
        Let { arg, var, body } => {
            let (env, arg_value) = linear_eval(program, cartesian_env, env, arg)?;
            let env = env.extend(var, arg_value);
            linear_eval(program, cartesian_env, env, body)
        },
        Object { captured_bindings, branches } => {
            let (env, captured_env) = linear_eval_bindings(program, cartesian_env, env, captured_bindings)?;
            Ok((env, LinearValue::ClosureObject { captured_env, branches }))
        },
        Send(e0, e1) => {
            let (env, obj) = linear_eval(program, cartesian_env, env, e0)?;
            match obj {
                LinearValue::ClosureObject { captured_env, branches } => {
                    let (env, msg) = linear_eval(program, cartesian_env, env, e1)?;
                    let (captured_env, val) = linear_apply_msg_to_branches(program, cartesian_env, captured_env, branches, msg)?;
                    if captured_env.is_empty() {
                        Ok((env, val))
                    } else {
                        Err(Error::UnconsumedResources(captured_env))
                    }
                },
                obj => Err(Error::AttemptToSendMessageToNonObject(Value::Linear(obj))),
            }
        }
    }
}

fn linear_eval_bindings(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, bindings: LinearBindings) -> Result<(LinearEnv, LinearEnv), Error> {
    use LinearBindings0::*;
    match *bindings.0 {
        Empty => Ok((env, LinearEnv::new())),
        Push { var, expr, parent } => {
            let (env, bindings_env_parent) = linear_eval_bindings(program, cartesian_env, env, parent)?;
            let (env, val) = linear_eval(program, cartesian_env, env, expr)?;
            let bindings_env_parent = bindings_env_parent.extend(var, val);
            Ok((env, bindings_env_parent))
        },
    }
}

// ===Apply Function===
// ==Cartesian==
fn apply_cartesian_function(program: &Program, fn_def: &CartesianFunctionDefinition, arg_values: Vec<CartesianValue>) -> Result<CartesianValue, Error> {
    let num_of_arguments: usize = fn_def.cartesian_parameters.len();
    if num_of_arguments != arg_values.len() {
        return Err(Error::CartesianFunctionCallArityMismatch { fn_name: fn_def.name.clone(), expected: num_of_arguments, received: arg_values.len() })
    }
    
    let mut bindings: Vec<(VariableName, CartesianValue)> = Vec::with_capacity(num_of_arguments);
    for (i, val) in arg_values.into_iter().enumerate() {
        bindings.push((fn_def.cartesian_parameters[i].clone(), val));
    }
    cartesian_eval(program, &CartesianEnv::new().extend_many(bindings.into_iter()), &fn_def.body)
}

// ==Linear==
fn linear_apply_function(program: &Program, fn_def: LinearFunctionDefinition, cartesian_arg_values: Vec<CartesianValue>, arg_values: Vec<LinearValue>) -> Result<LinearValue, Error> {
    let num_of_cartesian_arguments: usize = fn_def.cartesian_parameters.len();
    let num_of_arguments: usize = fn_def.linear_parameters.len();
    if num_of_cartesian_arguments != cartesian_arg_values.len() || num_of_arguments != arg_values.len() {
        return Err(Error::LinearFunctionCallArityMismatch { fn_name: fn_def.name, expected_cartesian: num_of_cartesian_arguments, received_cartesian: cartesian_arg_values.len(), expected_linear: num_of_arguments, received_linear: arg_values.len() })
    }
    let cartesian_env = CartesianEnv::new().extend_many(fn_def.cartesian_parameters.into_iter().zip(cartesian_arg_values));
    let env = LinearEnv::new().extend_many(fn_def.linear_parameters.into_iter().zip(arg_values));
    let (env, val) = linear_eval(program, &cartesian_env, env, fn_def.body)?;
    if env.is_empty() {
        Ok(val)
    } else {
        Err(Error::UnconsumedResources(env))
    }
}

// ===Send Message===
// ==Cartesian==
type CartesianPatternMatchResult = Option<Vec<(VariableName, CartesianValue)>>;

// TODO: Move this into the message  sending function.
impl Pattern {
    fn match_(&self, val: &CartesianValue) -> CartesianPatternMatchResult {
        fn loop_(pattern: &Pattern, val: &CartesianValue, mut bindings: Vec<(VariableName, CartesianValue)>) -> CartesianPatternMatchResult {
            use Pattern::*;
            match (pattern, val) {
                (Variable(var), val) => {
                    bindings.push((var.clone(), val.clone()));
                    Some(bindings)
                },
                (Tagged(tag0, pattern), CartesianValue::Tagged(tag1, val)) => {
                    if tag0 == tag1 {
                        loop_(pattern, val, bindings)
                    } else {
                        None
                    }
                },
                (Tuple(patterns), CartesianValue::Tuple(values)) => {
                    if patterns.len() == values.len() {
                        for (i, val) in values.into_iter().enumerate() {
                            bindings = loop_(&patterns[i], val, bindings)?
                        }
                        Some(bindings)
                    } else {
                        None
                    }
                },
                _ => None
            }
        }

        loop_(self, val, vec![])
    }
}

fn cartesian_apply_msg_to_branches(program: &Program, env: &CartesianEnv, branches: &[CartesianPatternBranch], val: &CartesianValue) -> Result<CartesianValue, Error> {
    for branch in branches {
        match branch.pattern.match_(val) {
            Some(bindings) => {
                let env = env.clone().extend_many(bindings.into_iter());
                return cartesian_eval(program, &env, &branch.body)
            },
            None => {},
        }
    }
    Err(Error::UnableToFindMatchingPattern)
}

// ==Linear==
fn linear_apply_msg_to_branches(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, branches: Vec<LinearPatternBranch>, val: LinearValue) -> Result<(LinearEnv, LinearValue), Error> {
    if branches.len() == 0 { return Err(Error::UnableToFindMatchingPattern) }
    if branches.len() == 1 && matches!(branches[0].pattern, Pattern::Variable(_)) { // non-destructive read
        // Can't just do branches[0]... that has ownership problems for some reason.
        let LinearPatternBranch { pattern, body } =  branches.into_iter().next().unwrap();
        match pattern {
            Pattern::Variable(var) => {
                let env = env.extend(var, val);
                return linear_eval(program, cartesian_env, env, body)
            },
            _ => unreachable!(),
        }
    }
    match val {
        LinearValue::Tagged(tag0, val) => {
            let mut filtered_branches: Vec<LinearPatternBranch> = vec![];
            for LinearPatternBranch { pattern, body } in branches {
                match pattern {
                    Pattern::Tagged(tag, pattern) => {
                        if tag == tag0 {
                            filtered_branches.push(LinearPatternBranch { pattern: *pattern, body });
                        }
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TaggedValueFedToNonTaggedPattern))
                }
            }
            linear_apply_msg_to_branches(program, cartesian_env, env, filtered_branches, *val)
        },
        LinearValue::Tuple(values) => {
            // We require in this case that we have exactly one branch that destructures the tuple,
            // and that the patterns are match-all variables
            if branches.len() == 1 {
                let LinearPatternBranch { pattern, body } =  branches.into_iter().next().unwrap();
                match pattern {
                    Pattern::Tuple(patterns) => {
                        if patterns.len() != values.len() {
                            return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleSizesDidNotMatch))
                        }
                        let mut env = env;
                        for (pattern, val) in patterns.into_iter().zip(values) {
                            match pattern {
                                Pattern::Variable(var) => {
                                    env = env.extend(var, val);
                                },
                                _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleFedToNonTuplePattern))
                            }
                        }
                        return linear_eval(program, cartesian_env, env, body)
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleFedToNonTuplePattern))
                }
            } else {
                return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleMatchedAgainstMatchExpressionWithMultipleBranches))
            }
        },
        _ => {
            // The only pattern that could match this is MatchAll (i.e. a variable).
            // In which case we would have already matched this. This means that we have an error
            // here, e.g. someone fed an object into a match case that's not a match-all.
            return Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonInductiveValue))
        },
    }
}
