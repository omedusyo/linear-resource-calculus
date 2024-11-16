use std::rc::Rc;
use crate::identifier::{VariableName, Tag};

// note that
//   {
//   | #a { #a0 . body-a0
//        | #a1 . body-a1
//        }
//   | #b [x, y] . body-b
//   | #c . body-c
//   | #d x . body-d
//   }
// desugars into
//   or{
//   | #a . or{
//        | #a0 . <body-a0>
//        | #a1 . <body-a1>
//        }
//   | #b . and{ [x, y] . <body-b> }
//   | #c . <body-c>
//   | #d . match-all{ x . <body-d>}
//   }

// Branch =
// | Or
// | And
// | MatchAll
// BranchOrBody =
// | Branch
// | Body

// Or(Vec<(Tag, BranchOrBody)>)
// And(Vec<TuplePattern>, Body)

// TuplePattern =
// | MatchAllPattern
// | Tuple(Vec<TuplePattern>)
//
// MatchAll(Var, Body)


// ===Branches===
#[derive(Debug, Clone)]
struct Branch<Body>(Rc<Branch0<Body>>);
#[derive(Debug)]
enum Branch0<Body> {
    MatchAll(MatchAllBranch<Body>),
    Or(OrBranch<Body>),
    And(AndBranch<Body>),
}

#[derive(Debug, Clone)]
struct MatchAllBranch<Body>(Rc<MatchAllBranch0<Body>>);
#[derive(Debug)]
struct MatchAllBranch0<Body> {
    var: VariableName,
    body: Body,
}

#[derive(Debug, Clone)]
struct OrBranch<Body>(Rc<OrBranch0<Body>>);
#[derive(Debug)]
struct OrBranch0<Body>(Vec<(Tag, BranchOrBody<Body>)>);

#[derive(Debug, Clone)]
struct AndBranch<Body>(Rc<AndBranch0<Body>>);
#[derive(Debug)]
struct AndBranch0<Body> {
    tuple_patterns: Vec<TuplePattern>,
    body: Body,
}

#[derive(Debug, Clone)]
struct BranchOrBody<Body>(Rc<BranchOrBody0<Body>>);
#[derive(Debug)]
enum BranchOrBody0<Body> {
    Branch(Branch<Body>),
    Body(Body)
}

#[derive(Debug, Clone)]
struct TuplePattern(Rc<TuplePattern0>);
#[derive(Debug)]
enum TuplePattern0 {
    MatchAllPattern(VariableName),
    Tuple(Vec<TuplePattern>)
}

// ===Value Shape===
#[derive(Debug)]
struct ValueShape<Value>(Box<ValueShape0<Value>>);
#[derive(Debug)]
enum ValueShape0<Value> {
    Value(Value),
    Tuple(Vec<ValueShape<Value>>),
    Tagged(Tag, ValueShape<Value>),
}


trait PatternMatchableValue {
    fn to_shape(self) -> ValueShape<Self> where Self: Sized;
    fn from_shape(value_shape: ValueShape<Self>) -> Self where Self: Sized;
}

// ===Pattern Matching===
#[derive(Debug)]
enum Error<Value> {
    AttemptToFeedNonTupleValueIntoAndPatternBranch(Value),
    AttemptToFeedNonTaggedValueIntoOrPatternBranch(Value),
    ExpectedMatchAllPattern,
    AttemptToMatchTuplesToTuplePatternsOfDifferingLengths,
    AttemptToFeedTaggedValue
}

struct Env<Value> {
    bindings: Vec<(VariableName, Value)>
}

impl <Value> Env<Value> {
    fn new() -> Self {
        Self { bindings: vec![] }
    }

    fn extend(mut self, var: VariableName, value: Value) -> Self {
        self.bindings.push((var, value));
        self
    }

    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, Value)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

fn match_<'branch, Body, Value: PatternMatchableValue>(
    value_shape: ValueShape<Value>,
    branch: &'branch Branch<Body>,
    env: Env<Value>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    use ValueShape0::*;
    use Branch0::*;
    match *(value_shape.0) {
        Value(value) => {
            match &(*branch.0) {
                MatchAll(match_all_branch) => {
                    let match_all_branch = &(*match_all_branch.0);
                    let env = env.extend(match_all_branch.var.clone(), value);
                    Ok((env, &match_all_branch.body))
                },
                Or(_or_branch) => Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternBranch(value)),
                And(_and_branch) => Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value)),
            }
        },
        Tuple(tuple_value_shapes) => {
            match &(*branch.0) {
                MatchAll(match_all_branch) => {
                    let match_all_branch = &(*match_all_branch.0);
                    let value_shape = ValueShape(Box::new(Tuple(tuple_value_shapes)));
                    let value = PatternMatchableValue::from_shape(value_shape);
                    let env = env.extend(match_all_branch.var.clone(), value);
                    Ok((env, &match_all_branch.body))
                },
                Or(_or_branch) => {
                    let value_shape = ValueShape(Box::new(Tuple(tuple_value_shapes)));
                    Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                },
                And(and_branch) => {
                    let and_branch = &(*and_branch.0);
                    let tuple_patterns = &and_branch.tuple_patterns;
                    let body = &and_branch.body;
                    let env = match_tuple_values_to_tuple_patterns(tuple_value_shapes, tuple_patterns, env)?;
                    Ok((env, body))
                },
            }
        },
        Tagged(tag, value_shape) => {
            match &(*branch.0) {
                MatchAll(match_all_branch) => {
                    let match_all_branch = &(*match_all_branch.0);
                    let value_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                    let value = PatternMatchableValue::from_shape(value_shape);
                    let env = env.extend(match_all_branch.var.clone(), value);
                    Ok((env, &match_all_branch.body))
                },
                Or(_or_branch) => {
                    // TODO
                    todo!()
                },
                And(_and_branch) => {
                    let value_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                    Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                },
            }
        },
    }
}


fn match_tuple_values_to_tuple_patterns<Value: PatternMatchableValue>(
    tuple_value_shapes: Vec<ValueShape<Value>>,
    tuple_patterns: &[TuplePattern],
    mut env: Env<Value>,
) -> Result<Env<Value>, Error<Value>> {
    if tuple_value_shapes.len() != tuple_patterns.len() {
        return Err(Error::AttemptToMatchTuplesToTuplePatternsOfDifferingLengths)
    }
    for (value_shape, tuple_pattern) in tuple_value_shapes.into_iter().zip(tuple_patterns) {
        env = match_tuple_value_to_tuple_pattern(value_shape, tuple_pattern, env)?;
    }
    Ok(env)
}

fn match_tuple_value_to_tuple_pattern<Value: PatternMatchableValue>(
    tuple_value_shape: ValueShape<Value>,
    tuple_pattern: &TuplePattern,
    env: Env<Value>,
) -> Result<Env<Value>, Error<Value>> {
    use ValueShape0::*;
    match *tuple_value_shape.0 {
        Tagged(tag, value_shape) => match &(*tuple_pattern.0) {
            TuplePattern0::MatchAllPattern(var) => {
                // TODO: Should this be even supported?
                let tuple_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                let env = env.extend(var.clone(), value);
                Ok(env)
            },
            TuplePattern0::Tuple(_tuple_pattern) => {
                let tuple_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Value(value) => match &(*tuple_pattern.0) {
            TuplePattern0::MatchAllPattern(var) => {
                let env = env.extend(var.clone(), value);
                Ok(env)
            },
            TuplePattern0::Tuple(_tuple_pattern) => {
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Tuple(tuple_shape) => match &(*tuple_pattern.0) {
            TuplePattern0::MatchAllPattern(var) => {
                let tuple_shape = ValueShape(Box::new(Tuple(tuple_shape)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                let env = env.extend(var.clone(), value);
                Ok(env)
            },
            TuplePattern0::Tuple(tuple_pattern) => {
                match_tuple_values_to_tuple_patterns(tuple_shape, tuple_pattern, env)
            },
        },
    }
}
