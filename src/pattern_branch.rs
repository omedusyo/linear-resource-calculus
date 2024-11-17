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
pub struct Branch<Body>(Rc<Branch0<Body>>);
#[derive(Debug)]
enum Branch0<Body> {
    MatchAll(MatchAllBranch<Body>),
    Or(OrBranch<Body>),
    And(AndBranch<Body>),
}

impl <Body> Branch<Body> {
    pub fn match_all(match_all_branch: MatchAllBranch<Body>) -> Self {
        Self(Rc::new(Branch0::MatchAll(match_all_branch)))
    }
    pub fn or(or_branch: OrBranch<Body>) -> Self {
        Self(Rc::new(Branch0::Or(or_branch)))
    }
    pub fn and(and_branch: AndBranch<Body>) -> Self {
        Self(Rc::new(Branch0::And(and_branch)))
    }
    pub fn pattern(pattern: Pattern, body: Body) -> Self {
        use Pattern0::*;
        match &(*(pattern.0)) {
            MatchAllPattern(var_name) => Branch::match_all(MatchAllBranch::new(var_name.clone(), body)),
            Tuple(tuple_patterns) => Branch::and(AndBranch::new(tuple_patterns.to_vec(), body)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchAllBranch<Body>(Rc<MatchAllBranch0<Body>>);
#[derive(Debug)]
struct MatchAllBranch0<Body> {
    var: VariableName,
    body: Body,
}

impl <Body> MatchAllBranch<Body> {
    pub fn new(var: VariableName, body: Body) -> Self {
        Self(Rc::new(MatchAllBranch0 { var, body }))
    }
}

#[derive(Debug, Clone)]
pub struct OrBranch<Body>(Rc<OrBranch0<Body>>);
#[derive(Debug)]
struct OrBranch0<Body>(Vec<(Tag, BranchOrBody<Body>)>);

impl <Body> OrBranch<Body> {
    pub fn new(branches: Vec<(Tag, BranchOrBody<Body>)>) -> Self {
        Self(Rc::new(OrBranch0(branches)))
    }
}

#[derive(Debug, Clone)]
pub struct AndBranch<Body>(Rc<AndBranch0<Body>>);
#[derive(Debug)]
struct AndBranch0<Body> {
    tuple_patterns: Vec<Pattern>,
    body: Body,
}

impl <Body> AndBranch<Body> {
    pub fn new(tuple_patterns: Vec<Pattern>, body: Body) -> Self {
        Self(Rc::new(AndBranch0 { tuple_patterns, body }))
    }
}

#[derive(Debug)]
pub enum BranchOrBody<Body> {
    Branch(Branch<Body>),
    Body(Body)
}

#[derive(Debug, Clone)]
pub struct Pattern(Rc<Pattern0>);
#[derive(Debug)]
enum Pattern0 {
    MatchAllPattern(VariableName),
    Tuple(Vec<Pattern>)
}

impl Pattern {
    pub fn match_all(var: VariableName) -> Self {
        Self(Rc::new(Pattern0::MatchAllPattern(var)))
    }
    pub fn tuple(patterns: Vec<Self>) -> Self {
        Self(Rc::new(Pattern0::Tuple(patterns)))
    }
}

// ===Value Shape===
#[derive(Debug)]
pub struct ValueShape<Value>(pub Box<ValueShape0<Value>>);
#[derive(Debug)]
pub enum ValueShape0<Value> {
    Value(Value),
    Tuple(Vec<ValueShape<Value>>),
    Tag(Tag),
    Tagged(Tag, ValueShape<Value>),
}

impl <Value> ValueShape<Value> {
    pub fn value(value: Value) -> Self {
        Self(Box::new(ValueShape0::Value(value)))
    }
    pub fn tuple(values: Vec<Self>) -> Self {
        Self(Box::new(ValueShape0::Tuple(values)))
    }
    pub fn tag(tag: Tag) -> Self {
        Self(Box::new(ValueShape0::Tag(tag)))
    }
    pub fn tagged(tag: Tag, value: Self) -> Self {
        Self(Box::new(ValueShape0::Tagged(tag, value)))
    }
}

pub trait PatternMatchableValue {
    fn to_shape(self) -> ValueShape<Self> where Self: Sized;
    fn from_shape(value_shape: ValueShape<Self>) -> Self where Self: Sized;
}

// ===Pattern Matching===
#[derive(Debug)]
pub enum Error<Value> {
    AttemptToFeedNonTupleValueIntoAndPatternBranch(Value),
    AttemptToFeedNonTaggedValueIntoOrPatternBranch(Value),
    ExpectedMatchAllPattern,
    AttemptToMatchTuplesToTuplePatternsOfDifferingLengths,
    AttemptToFeedTaggedValue,
    AttemptToFeedNonTagValueIntoTagPattern(Value),
    AttemptToFeedTagIntoComplexTaggedBranch(Value),
    UnableToMatchTag(Tag),
}

pub struct Env<Value> {
    pub bindings: Vec<(VariableName, Value)>
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

pub fn match_<'branch, Body, Value: PatternMatchableValue>(
    value_shape: ValueShape<Value>,
    branch: &'branch Branch<Body>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    match_loop(Env::new(), value_shape, branch)
}


#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug)]
    pub enum Value {
        Int(i32),
        Tag(Tag),
        Tagged(Tag, Box<Value>),
        Tuple(Vec<Value>),
    }
    impl PatternMatchableValue for Value {
        fn to_shape(self) -> ValueShape<Self>  {
            use Value::*;
            match self {
                Tagged(tag, value) => ValueShape::tagged(tag, value.to_shape()),
                Tuple(values) => ValueShape::tuple(values.into_iter().map(|val| val.to_shape()).collect()),
                val => ValueShape::value(val),
            }
        }
        fn from_shape(value_shape: ValueShape<Self>) -> Self  {
            use Value::*;
            match *value_shape.0 {
                ValueShape0::Value(value) => value,
                ValueShape0::Tuple(value_shapes) => Tuple(value_shapes.into_iter().map(|value_shape| PatternMatchableValue::from_shape(value_shape)).collect()),
                ValueShape0::Tagged(tag, value_shape) => Tagged(tag, Box::new(PatternMatchableValue::from_shape(value_shape))),
                ValueShape0::Tag(tag) => Tag(tag),
            }
        }
    }


    impl PatternMatchableValue for Value {
        fn to_shape(self) -> ValueShape<Self> where Self: Sized {
            todo!()
        }
        fn from_shape(value_shape: ValueShape<Self>) -> Self where Self: Sized {
            todo!()
        }
    }

    #[test]
    fn example0() {
    }
}
fn match_loop<'branch, Body, Value: PatternMatchableValue>(
    env: Env<Value>,
    value_shape: ValueShape<Value>,
    branch: &'branch Branch<Body>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    use ValueShape0::*;
    use Branch0::*;
    match &(*branch.0) {
        MatchAll(match_all_branch) => {
            let match_all_branch = &(*match_all_branch.0);
            let value = PatternMatchableValue::from_shape(value_shape);
            let env = env.extend(match_all_branch.var.clone(), value);
            return Ok((env, &match_all_branch.body))
        },
        _ => {}
    }

    match *(value_shape.0) {
        Value(value) => {
            match &(*branch.0) {
                MatchAll(_) => { unreachable!() },
                Or(_or_branch) => {
                    Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternBranch(value))
                },
                And(_and_branch) => Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value)),
            }
        },
        Tuple(tuple_value_shapes) => {
            match &(*branch.0) {
                And(and_branch) => {
                    let and_branch = &(*and_branch.0);
                    let tuple_patterns = &and_branch.tuple_patterns;
                    let body = &and_branch.body;
                    let env = match_values_to_patterns(tuple_value_shapes, tuple_patterns, env)?;
                    Ok((env, body))
                },
                Or(_or_branch) => {
                    let value_shape = ValueShape(Box::new(Tuple(tuple_value_shapes)));
                    Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                }
                MatchAll(_) => { unreachable!() },
            }
        },
        Tagged(tag, value_shape) => {
            match &(*branch.0) {
                Or(or_branch) => {
                    let tagged_branches = &(*or_branch.0).0;
                    for (tag0, branch_or_body) in tagged_branches.iter() {
                        if tag == *tag0 {
                            match branch_or_body {
                                BranchOrBody::Branch(branch) => {
                                    return match_loop(env, value_shape, branch);
                                },
                                BranchOrBody::Body(_body) => {
                                    let value_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                                    let value = PatternMatchableValue::from_shape(value_shape);
                                    return Err(Error::AttemptToFeedNonTagValueIntoTagPattern(value))
                                },
                            }
                        }
                    }
                    Err(Error::UnableToMatchTag(tag))
                },
                And(_and_branch) => {
                    let value_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                    Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                },
                MatchAll(_) => { unreachable!() },
            }
        },
        Tag(tag) => {
            match &(*branch.0) {
                Or(or_branch) => {
                    let tagged_branches = &(*or_branch.0).0;
                    for (tag0, branch_or_body) in tagged_branches.iter() {
                        if tag == *tag0 {
                            match branch_or_body {
                                BranchOrBody::Branch(_branch) => {
                                    let value_shape = ValueShape(Box::new(Tag(tag)));
                                    let value = PatternMatchableValue::from_shape(value_shape);
                                    return Err(Error::AttemptToFeedTagIntoComplexTaggedBranch(value))
                                },
                                BranchOrBody::Body(body) => {
                                    return Ok((env, body))
                                },
                            }
                        }
                    }
                    Err(Error::UnableToMatchTag(tag))
                },
                And(_and_branch) => {
                    let value_shape = ValueShape(Box::new(Tag(tag)));
                    Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                },
                MatchAll(_) => { unreachable!() },
            }
        },
    }
}


fn match_values_to_patterns<Value: PatternMatchableValue>(
    tuple_value_shapes: Vec<ValueShape<Value>>,
    tuple_patterns: &[Pattern],
    mut env: Env<Value>,
) -> Result<Env<Value>, Error<Value>> {
    if tuple_value_shapes.len() != tuple_patterns.len() {
        return Err(Error::AttemptToMatchTuplesToTuplePatternsOfDifferingLengths)
    }
    for (value_shape, tuple_pattern) in tuple_value_shapes.into_iter().zip(tuple_patterns) {
        env = match_value_to_pattern(value_shape, tuple_pattern, env)?;
    }
    Ok(env)
}

fn match_value_to_pattern<Value: PatternMatchableValue>(
    value_shape: ValueShape<Value>,
    pattern: &Pattern,
    env: Env<Value>,
) -> Result<Env<Value>, Error<Value>> {
    use ValueShape0::*;

    match &(*pattern.0) {
        Pattern0::MatchAllPattern(var) => {
            let value = PatternMatchableValue::from_shape(value_shape);
            let env = env.extend(var.clone(), value);
            return Ok(env)
        },
        _ => {},
    }

    match *value_shape.0 {
        Tag(tag) => match &(*pattern.0) {
            Pattern0::MatchAllPattern(_) => { unreachable!() },
            Pattern0::Tuple(_tuple_pattern) => {
                let tuple_shape = ValueShape(Box::new(Tag(tag)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Tagged(tag, value_shape) => match &(*pattern.0) {
            Pattern0::MatchAllPattern(_) => { unreachable!() },
            Pattern0::Tuple(_tuple_pattern) => {
                let tuple_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Value(value) => match &(*pattern.0) {
            Pattern0::MatchAllPattern(_) => { unreachable!() },
            Pattern0::Tuple(_tuple_pattern) => {
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Tuple(tuple_shape) => match &(*pattern.0) {
            Pattern0::MatchAllPattern(_) => { unreachable!() },
            Pattern0::Tuple(tuple_pattern) => {
                match_values_to_patterns(tuple_shape, tuple_pattern, env)
            },
        },
    }
}
