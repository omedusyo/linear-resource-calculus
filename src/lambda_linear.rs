use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType, anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, vector, delimited_nonempty_vector, delimited_vector};
use crate::identifier::{VariableName, FunctionName, Tag, variable_name};
use crate::IResult0;
use std::collections::HashMap;

// ===Program===
#[derive(Debug)]
pub struct Program {
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
}

#[derive(Debug, Clone)]
// This is awkward, but I think I need to put body: Rc<Expression> atleaest...
// These functions don't capture any linear resources,
// so we should be able to call them repeatedly.
pub struct FunctionDefinition {
    name: FunctionName,
    parameters: Vec<VariableName>,
    body: Expression,
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

    // It makes no sense to use up a function definition.
    // But then when applying a function to resources, we have to clone its body, which sucks.
    // Perhaps Rc<_> would help? But that seems like the wrong solution.
    //
    // When evaluating an expression, we completely disintegrate it.
    pub fn get_function_definition(&self, function_name: FunctionName) -> Option<&FunctionDefinition> {
        self.function_definitions.get(&function_name)
    }

    pub fn get_clone_of_function_definition(&self, function_name: FunctionName) -> Option<FunctionDefinition> {
        match self.function_definitions.get(&function_name) {
            Some(fn_def) => Some(fn_def.clone()),
            None => None,
        }
    }
}

// ===Expressions===
#[derive(Debug, Clone)]
pub struct Expression(pub Box<Expression0>);

// Clone is here only because we have function definitions.
#[derive(Debug, Clone)]
pub enum Expression0 {
    Int(i32),
    Call(FunctionName, Vec<Expression>),
    Tagged(Tag, Expression),
    Tuple(Vec<Expression>),
    Match { arg: Expression, branches: Vec<PatternBranch> },
    VarUse(VariableName),
    Let { arg: Expression, var: VariableName, body: Expression },
    // Here I was forced to include an explicit list of moved bindings.
    // This will also require an extension in syntax.
    Object { captured_bindings: Bindings, branches: Vec<PatternBranch> },
    Send(Expression, Expression),
}

// TODO: Can you refine this into a dependent version of bindings?
// Note that these bindings contain expressions, not values.
// This is purely for the move into object syntax.
#[derive(Debug, Clone)]
pub struct Bindings(Box<Bindings0>);

#[derive(Debug, Clone)]
pub enum Bindings0 {
    Empty,
    Push { var: VariableName, expr: Expression, parent: Bindings },
}

#[derive(Debug, Clone)]
pub struct PatternBranch {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug)]
pub struct TuplePatternBranch {
    pub patterns: Vec<Pattern>,
    pub body: Expression,
}

// match v { x => ... }  // Should be valid right? This is basically a let-expression equivalent.
// match v { #foo x => ... | #bar y => ... } // Should also be valid. But we should assume that the
#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(VariableName),
    Tagged(Tag, Box<Pattern>),
    Tuple(Vec<Pattern>),
}

impl Expression {
    fn int(x: i32) -> Self { Self(Box::new(Expression0::Int(x))) }
    fn call(fn_name: FunctionName, args: Vec<Self>) -> Self { Self(Box::new(Expression0::Call(fn_name, args))) }
    fn tagged(tag: Tag, e: Expression) -> Self { Self(Box::new(Expression0::Tagged(tag, e))) }
    fn tuple(args: Vec<Expression>) -> Self { Self(Box::new(Expression0::Tuple(args))) }
    fn match_(arg: Self, branches: Vec<PatternBranch>) -> Self { Self(Box::new(Expression0::Match { arg, branches })) }
    fn var_use(var: VariableName) -> Self { Self(Box::new(Expression0::VarUse(var))) }
    fn let_(arg: Self, var: VariableName, body: Self) -> Self { Self(Box::new(Expression0::Let { arg, var, body })) }
    fn object(moved_env: Bindings, branches: Vec<PatternBranch>) -> Self { Self(Box::new(Expression0::Object { captured_bindings: moved_env, branches })) }
    fn send(obj: Self, msg: Self) -> Self { Self(Box::new(Expression0::Send(obj, msg))) }
}

// ===Values===
#[derive(Debug)]
pub enum Value {
    Int(i32),
    Tagged(Tag, Box<Value>),
    Tuple(Vec<Value>), // Would be cool if we could use Box<[Value]>, since we don't need to resize
    ClosureObject { captured_env: Env, branches: Vec<PatternBranch> },
}

// ===Environment===
#[derive(Debug)]
pub struct Env(Box<Env0>);

#[derive(Debug)]
pub enum Env0 {
    Empty,
    Push { var: VariableName, value: Value, parent: Env },
}

impl Env {
    fn new() -> Self {
        Self(Box::new(Env0::Empty))
    }

    fn is_empty(&self) -> bool {
        use Env0::*;
        match *self.0 {
            Empty => true,
            _ => false,
        }
    }

    // Very different from the cartesian case.
    fn use_(self, var_name: VariableName) -> Result<(Value, Self), Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    Ok((value, parent))
                } else {
                    let (value0, parent) = parent.use_(var_name)?;
                    // TODO: Would be nice to reuse the old env
                    Ok((value0, Env(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }

    // No difference from cartesian case.
    fn extend(self, var: VariableName, value: Value) -> Env {
        Self(Box::new(Env0::Push { var, value, parent: self }))
    }

    // No difference from cartesian case.
    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, Value)>) -> Env {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ===Error===
#[derive(Debug, PartialEq)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    FunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    VariableLookupFailure(VariableName),
    UnableToFindMatchingPattern,
    InvalidPatternMatch(PatternMatchErrror),
}

#[derive(Debug, PartialEq)]
pub enum PatternMatchErrror {
    TaggedValueFedToNonTaggedPattern,
    TupleFedToNonTuplePattern,
    TupleSizesDidNotMatch,
    TupleMatchedAgainstMatchExpressionWithMultipleBranches,
    AttemptToMatchNonInductiveValue,
}

// ===Evaluation===
pub fn eval_start(program: Program, e: Expression) -> Result<(Program, Value), Error> {
    let (program, env, value) = eval(program, Env::new(), e)?;
    if env.is_empty() {
        Ok((program, value))
    } else {
        todo!()
    }
}

// Both the environment and the expression should be completely consumed to construct the final value.
fn eval(program: Program, env: Env, e: Expression) -> Result<(Program, Env, Value), Error> {
    use Expression0::*;
    match *e.0 {
        Int(x) => Ok((program, env, Value::Int(x))),
        Call(fn_name, args) => {
            let mut values: Vec<Value> = Vec::with_capacity(args.len());
            let mut env = env;
            let mut program = program;
            for arg in args {
                let (program0, env0, val) = eval(program, env, arg)?;
                program = program0;
                env = env0;
                values.push(val);
            }
            let (program, val) = apply_function(program, fn_name, values)?;
            Ok((program, env, val))
        }
        Tagged(tag, e) => {
            let (program, env, val) = eval(program, env, e)?;
            Ok((program, env, Value::Tagged(tag, Box::new(val))))
        },
        Tuple(args) => {
            let mut program = program;
            let mut env = env;
            let mut values: Vec<Value> = Vec::with_capacity(args.len());
            for arg in args {
                let (program0, env0, val) = eval(program, env, arg)?;
                program = program0;
                env = env0;
                values.push(val);
            }
            Ok((program, env, Value::Tuple(values)))
        },
        VarUse(var_name) => {
            // I need to take out the binding completely out of the environment
            let (value, env) = env.use_(var_name)?;
            Ok((program, env, value))
        },
        Match { arg, branches } => {
            let (program, env, arg_value) = eval(program, env, arg)?;
            apply_msg_to_branches(program, env, branches, arg_value)
        },
        Let { arg, var, body } => {
            let (program, env, arg_value) = eval(program, env, arg)?;
            let env = env.extend(var, arg_value);
            eval(program, env, body)
        },
        // Now here is a big problem. The object should not capture the whole environement.
        // We should only capture those resources that are needed by the object
        // i.e. we should move them out of the environment, and return the rest of the environment.
        // I think the correct solution for a dynamic language is to have the move explicitely
        Object { captured_bindings, branches } => {
            let (program, env, captured_env) = eval_bindings(program, env, captured_bindings)?;
            Ok((program, env, Value::ClosureObject { captured_env, branches }))
        },
        Send(e0, e1) => {
            let (program, env, obj) = eval(program, env, e0)?;
            match obj {
                Value::ClosureObject { captured_env, branches } => {
                    let (program, env, msg) = eval(program, env, e1)?;
                    let (program, captured_env, val) = apply_msg_to_branches(program, captured_env, branches, msg)?;
                    if captured_env.is_empty() {
                        Ok((program, env, val))
                    } else {
                        todo!()
                    }
                },
                _ => todo!(),
            }
        }
        _ => {
            todo!()
        },
    }
}

// We return `(Env, Env)`.
// The first component is what remains of `env`, while the second is the result of evaluating `bindings`
fn eval_bindings(program: Program, env: Env, bindings: Bindings) -> Result<(Program, Env, Env), Error> {
    use Bindings0::*;
    match *bindings.0 {
        Empty => Ok((program, env, Env::new())),
        Push { var, expr, parent } => {
            let (program, env, bindings_env_parent) = eval_bindings(program, env, parent)?;
            // TODO: Would be nice to have dependent bindings, but it's not trivial to implement.
            let (program, env, val) = eval(program, env, expr)?;
            let bindings_env_parent = bindings_env_parent.extend(var, val);
            Ok((program, env, bindings_env_parent))
        },
    }
}

fn match_with_universal_pattern(program: Program, env: Env, branches: Vec<PatternBranch>, val: Value) -> Result<(Program, Env, Value), Error> {
    todo!()
}

//   [v1, v2, v3]
//
//   [p1, p2, p3] => e0
//   [p1, p2, p3] => e1
// ~>
//  [[p1] , [p2] , [p3]]  => [e0,]
//  [[p1]   [p2]   [p3]]  => [e1 ]

// WARNING: This is a transposed version of patterns.
#[derive(Debug)]
pub struct TuplePatternBranches {
    pattern_columns: Vec<Vec<Pattern>>,
    bodies: Vec<Expression>,
}

impl TuplePatternBranches {
    fn new(number_of_branches: usize, tuple_count: usize) -> Self {
        let mut pattern_columns = Vec::with_capacity(tuple_count);
        for _ in 0..tuple_count {
            pattern_columns.push(Vec::with_capacity(number_of_branches));
        }
        let bodies = Vec::with_capacity(number_of_branches);
        Self { pattern_columns, bodies }
    }
}

// TODO: There should be some syntactic check
//       that makes sure that `match` expressions are either
//          a single match-all
//       or a bunch of pattertns that query the tag
//       or a single tuple pattern.
//
//
// Should we allow the following?
// match (v1, v2) {
// | (#A, #A) . ...
// | (#A, #B) . ...
// | (#B, #A) . ...
// | (#B, #B) . ...
// }
// Right now we won't.
// Instead do the following
// match (v1, v2) {
// | (x, y) . match x {
//   | #A . match y {
//     | #A . ...
//     | #B . ...
//     }
//   | #B . match x {
//     | #A . ...
//     | #B . ...
//     }
//   }
// }
fn apply_msg_to_branches(program: Program, env: Env, branches: Vec<PatternBranch>, val: Value) -> Result<(Program, Env, Value), Error> {
    if branches.len() == 0 { return Err(Error::UnableToFindMatchingPattern) }
    if branches.len() == 1 && matches!(branches[0].pattern, Pattern::Variable(_)) { // non-destructive read
        // Can't just do branches[0]... that has ownership problems for some reason.
        let PatternBranch { pattern, body } =  branches.into_iter().next().unwrap();
        match pattern {
            Pattern::Variable(var) => {
                let env = env.extend(var, val);
                return eval(program, env, body)
            },
            _ => unreachable!(),
        }
    }
    match val {
        Value::Tagged(tag0, val) => {
            let mut filtered_branches: Vec<PatternBranch> = vec![];
            for PatternBranch { pattern, body } in branches {
                match pattern {
                    Pattern::Tagged(tag, pattern) => {
                        if tag == tag0 {
                            filtered_branches.push(PatternBranch { pattern: *pattern, body });
                        }
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TaggedValueFedToNonTaggedPattern))
                }
            }
            apply_msg_to_branches(program, env, filtered_branches, *val)
        },
        Value::Tuple(values) => {
            // We require in this case that we have exactly one branch that destructures the tuple,
            // and that the patterns are match-all variables
            if branches.len() == 1 {
                let PatternBranch { pattern, body } =  branches.into_iter().next().unwrap();
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
                        return eval(program, env, body)
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

fn apply_function(program: Program, fn_name: FunctionName, arg_values: Vec<Value>) -> Result<(Program, Value), Error> {
    let Some(fn_def) = program.get_clone_of_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name)) };
    let num_of_arguments: usize = fn_def.parameters.len();
    if num_of_arguments != arg_values.len() {
        return Err(Error::FunctionCallArityMismatch { fn_name, expected: num_of_arguments, received: arg_values.len() })
    }
    
    let env = Env::new().extend_many(fn_def.parameters.into_iter().zip(arg_values));
    let (program, env, val) = eval(program, env, fn_def.body)?;
    if env.is_empty() {
        Ok((program, val))
    } else {
        todo!()
    }
}
