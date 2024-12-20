//============This is the old linear calculus where the evaluator takes ownership of the expresion and completely consumes it==============
use crate::identifier::{VariableName, FunctionName, Tag};
use std::collections::HashMap;

// ===Program===
#[derive(Debug, Clone)]
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
    OperationApplication1(OperationCode, Expression),
    OperationApplication2(OperationCode, Expression, Expression),
    Call(FunctionName, Vec<Expression>),
    Tagged(Tag, Expression),
    Tuple(Vec<Expression>),
    Match { arg: Expression, branches: Vec<PatternBranch> },
    VarMove(VariableName),
    VarClone(VariableName),
    VarDrop(VariableName, Expression),
    LetMove { arg: Expression, var: VariableName, body: Expression },
    // Here I was forced to include an explicit list of moved bindings.
    // This will also require an extension in syntax.
    Object { captured_bindings: Bindings, branches: Vec<PatternBranch> },
    Send(Expression, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum OperationCode {
    Add,
    Sub,
    Mul,
    Eq,
    Clone,
    Discard,
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

// match v { x => ... }  // Should be valid right? This is basically a let-expression equivalent.
// match v { #foo x => ... | #bar y => ... } // Should also be valid. But we should assume that the
#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(VariableName),
    Tagged(Tag, Box<Pattern>),
    Tuple(Vec<TuplePattern>),
}

#[derive(Debug, Clone)]
pub enum TuplePattern {
    Variable(VariableName),
    Tuple(Vec<TuplePattern>),
}

impl Expression {
    fn int(x: i32) -> Self { Self(Box::new(Expression0::Int(x))) }
    fn operation_application1(op_code: OperationCode, e0: Self) -> Self { Self(Box::new(Expression0::OperationApplication1(op_code, e0))) }
    fn operation_application2(op_code: OperationCode, e0: Self, e1: Self) -> Self { Self(Box::new(Expression0::OperationApplication2(op_code, e0, e1))) }
    fn call(fn_name: FunctionName, args: Vec<Self>) -> Self { Self(Box::new(Expression0::Call(fn_name, args))) }
    fn tagged(tag: Tag, e: Expression) -> Self { Self(Box::new(Expression0::Tagged(tag, e))) }
    fn tuple(args: Vec<Expression>) -> Self { Self(Box::new(Expression0::Tuple(args))) }
    fn match_(arg: Self, branches: Vec<PatternBranch>) -> Self { Self(Box::new(Expression0::Match { arg, branches })) }
    fn var_move(var: VariableName) -> Self { Self(Box::new(Expression0::VarMove(var))) }
    fn var_clone(var: VariableName) -> Self { Self(Box::new(Expression0::VarClone(var))) }
    fn var_drop(var: VariableName, expr: Expression) -> Self { Self(Box::new(Expression0::VarDrop(var, expr))) }
    fn let_move(arg: Self, var: VariableName, body: Self) -> Self { Self(Box::new(Expression0::LetMove { arg, var, body })) }
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

impl Value {
    fn discard(self) -> () {
        use Value::*;
        match self {
            Int(_x) => (),
            Tagged(_tag, val) => (*val).discard(),
            Tuple(values) => {
                for val in values {
                    let _ = val.discard();
                }
            },
            ClosureObject { .. } => todo!(), // this should crash
        }
    }

    fn duplicate(self) -> (Self, Self) {
        use Value::*;
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
    fn move_(self, var_name: VariableName) -> Result<(Value, Self), Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    Ok((value, parent))
                } else {
                    let (value0, parent) = parent.move_(var_name)?;
                    Ok((value0, Env(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }
        
    fn clone_(self, var_name: VariableName) -> Result<(Value, Self), Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    let (value0, value1) = value.duplicate();
                    Ok((value0, Env(Box::new(Push { var, value: value1, parent }))))
                } else {
                    let (value0, parent) = parent.clone_(var_name)?;
                    Ok((value0, Env(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }

    fn drop_(self, var_name: VariableName) -> Result<Self, Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    Ok(parent)
                } else {
                    let parent = parent.drop_(var_name)?;
                    Ok(Env(Box::new(Push { var, value, parent })))
                }
            },
        }
    }

    // No difference from cartesian case.
    fn extend(self, var: VariableName, value: Value) -> Self {
        Self(Box::new(Env0::Push { var, value, parent: self }))
    }

    // No difference from cartesian case.
    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, Value)>) -> Self {
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
    FunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    VariableLookupFailure(VariableName),
    UnableToFindMatchingPattern,
    InvalidPatternMatch(PatternMatchErrror),
    UnconsumedResources(Env),
    AttemptToSendMessageToNonObject(Value),
}

#[derive(Debug)]
pub enum PatternMatchErrror {
    TaggedValueFedToNonTaggedPattern,
    TupleFedToNonTuplePattern,
    TupleSizesDidNotMatch,
    TupleMatchedAgainstMatchExpressionWithMultipleBranches,
    AttemptToMatchNonInductiveValue,
    AttemptToMatchNonTupleToTuplePattern,
}

// ===Evaluation===
pub fn eval_start(program: Program, e: Expression) -> Result<(Program, Value), Error> {
    let (program, env, value) = eval(program, Env::new(), e)?;
    if env.is_empty() {
        Ok((program, value))
    } else {
        Err(Error::UnconsumedResources(env))
    }
}

// Both the environment and the expression should be completely consumed to construct the final value.
fn eval(program: Program, env: Env, e: Expression) -> Result<(Program, Env, Value), Error> {
    use Expression0::*;
    match *e.0 {
        Int(x) => Ok((program, env, Value::Int(x))),
        OperationApplication1(code, e0) => {
            let (program, env, val0) = eval(program, env, e0)?;
            use OperationCode::*;
            Ok((
                program,
                env,
                match code {
                    Clone => {
                        let (v0, v1) = val0.duplicate();
                        Value::Tuple(vec![v0, v1])
                    },
                    Discard => {
                        let () = val0.discard();
                        Value::Tuple(vec![])
                    },
                    _ => todo!(), // This should crash
                }
            ))
        },
        OperationApplication2(code, e0, e1) => {
            let (program, env, val0) = eval(program, env, e0)?;
            let (program, env, val1) = eval(program, env, e1)?;
            match (val0, val1) {
                (Value::Int(x0), Value::Int(x1)) => {
                    use OperationCode::*;
                    Ok((
                        program,
                        env,
                        match code {
                            Add => Value::Int(x0 + x1),
                            Sub => Value::Int(x0 - x1),
                            Mul => Value::Int(x0 * x1),
                            Eq => if x0 == x1 {
                                Value::Tagged(Tag::new("T".to_string()), Box::new(Value::Tuple(vec![])))
                            } else {
                                Value::Tagged(Tag::new("F".to_string()), Box::new(Value::Tuple(vec![])))
                            },
                            Discard | Clone => todo!()
                        }
                    ))
                },
                _ => todo!(),
            }
        },
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
        VarMove(var_name) => {
            // I need to take out the binding completely out of the environment
            let (value, env) = env.move_(var_name)?;
            Ok((program, env, value))
        },
        VarClone(var_name) => {
            // This looks up the var in the environment, then attempts a clone.
            let (value, env) = env.clone_(var_name)?;
            Ok((program, env, value))
        },
        VarDrop(var_name, expr) => {
            // This looks up the var in the environmeent, then attempts to drop the var.
            let env = env.drop_(var_name)?;
            eval(program, env, expr)
        },
        Match { arg, branches } => {
            let (program, env, arg_value) = eval(program, env, arg)?;
            apply_msg_to_branches(program, env, branches, arg_value)
        },
        LetMove { arg, var, body } => {
            let (program, env, arg_value) = eval(program, env, arg)?;
            let env = env.extend(var, arg_value);
            eval(program, env, body)
        },
        // Now here is a big problem. The object should not capture the whole environement.
        // We should only capture those resources that are needed by the object
        // i.e. we should move them out of the environment, and return the rest of the environment.
        // I think the correct solution for a dynamic language is to have the move explicitely
        //
        // This is only a problem in a dynamic language, right? When we have types, the parts of
        // the environment that are captured can be known statically.
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
                        Err(Error::UnconsumedResources(captured_env))
                    }
                },
                obj => Err(Error::AttemptToSendMessageToNonObject(obj)),
            }
        }
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
            // and that the patterns are match-all variables or tuple-patterns
            if branches.len() == 1 {
                let branch = branches.into_iter().next().unwrap();
                match branch.pattern {
                    Pattern::Tuple(patterns) => apply_tuple_to_branch(program, env, patterns, branch.body, values),
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleFedToNonTuplePattern)),
                }
            } else {
                Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleMatchedAgainstMatchExpressionWithMultipleBranches))
            }
        },
        _ => {
            // The only pattern that could match this is MatchAll (i.e. a variable).
            // In which case we would have already matched this. This means that we have an error
            // here, e.g. someone fed an object into a match case that's not a match-all.
            Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonInductiveValue))
        },
    }
}
fn apply_tuple_to_branch(program: Program, env: Env, patterns: Vec<TuplePattern>, body: Expression, values: Vec<Value>) -> Result<(Program, Env, Value), Error> {
    let (program, env) = bind_tuple_to_tuple_pattern(program, env, patterns, values)?;
    eval(program, env, body)
}
fn bind_tuple_to_tuple_pattern(mut program: Program, mut env: Env, patterns: Vec<TuplePattern>, values: Vec<Value>) -> Result<(Program, Env), Error> {
    if patterns.len() != values.len() {
        return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleSizesDidNotMatch))
    }
    for (pattern, val) in patterns.into_iter().zip(values) {
        match pattern {
            TuplePattern::Variable(var) => {
                env = env.extend(var, val);
            },
            TuplePattern::Tuple(patterns) => {
                match val {
                    Value::Tuple(values) => {
                        let (program0, env0) = bind_tuple_to_tuple_pattern(program, env, patterns, values)?;
                        program = program0;
                        env = env0;
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonTupleToTuplePattern)),
                }
            },
        }
    }
    Ok((program, env))
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
        Err(Error::UnconsumedResources(env))
    }
}
