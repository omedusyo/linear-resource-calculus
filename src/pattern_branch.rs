use std::rc::Rc;
use crate::identifier::{VariableName, Tag};

// example.
//   or{
//   | a . or{
//         | a0 . seq{ . body-a0 }
//         | a1 . seq{ . body-a1 }
//         }
//   | b . seq{ x, y . body-b}
//   | c . seq{ .  body-c }
//   | d . seq{ x . body-d }
//   | e . seq{ x, [y, [z0, z1]], w . body-e }
//   }

// This can be used for let-bindings, match-expressions, or object-expressions.
// (let)
//   The branch that can be extracted from
//     let { x = e0, [y0, [y10, y11], []] = e1, [z0, z1] = e2 . body }
//   is
//     seq{ x, [y0, [y10, y11], []], [z0, z1] . body }
// (match)
//   The branch that can be extracted from
//     match e {
//     | #a . body0
//     | #b x . body1
//     | #c [x, [y0, y1], z] . body2
//     }
//   is
//     or{
//     | a . seq{ . body0 }
//     | b . seq{ x . body1 }
//     | c . seq{ [x, [y0, y1], z] . body2 }
//     }
// (object)
//   The branch that can be extracted from
//     object {
//     | @msg0 . body0
//     | @msg1 x . body1
//     | @msg2 @msg22 [x, y] . body2
//     | @msg3 { @msg30 . body30 | @msg31 . body31 }
//     }
//   is
//     or{
//     | msg0 . seq{ . body0 }
//     | msg1 . seq{ x . body1 }
//     | msg2 . or{ msg22 . seq{ [x, y] . body2 }}
//     | msg3 . or{ msg30 . seq{ . body30 } | msg31 . seq{ . body31 } }
//     }

// Branch =
// | Or
// | Seq

// Or(Vec<(Tag, Option<Branch>)>)
// Seq(Vec<Pattern>, Body)

// Pattern =
// | MatchAll(Var)
// | Tuple(Vec<Pattern>)


// ===Branches===
#[derive(Debug, Clone)]
pub enum Branch<Body> {
    Or(OrBranch<Body>),
    Seq(SeqBranch<Body>),
}

impl <Body> Branch<Body> {
    pub fn or(or_branch: OrBranch<Body>) -> Self {
        Self::Or(or_branch)
    }
    pub fn seq(and_branch: SeqBranch<Body>) -> Self {
        Self::Seq(and_branch)
    }
    pub fn body(body: Body) -> Self {
        Branch::seq(SeqBranch::new(vec![], body))
    }
    pub fn pattern(pattern: Pattern, body: Body) -> Self {
        Branch::seq(SeqBranch::new(vec![pattern], body))
    }
    pub fn pattern_seq(patterns: Vec<Pattern>, body: Body) -> Self {
        Branch::seq(SeqBranch::new(patterns, body))
    }
}

#[derive(Debug, Clone)]
pub struct OrBranch<Body>(Rc<OrBranch0<Body>>);
#[derive(Debug)]
struct OrBranch0<Body>(Vec<(Tag, Branch<Body>)>);

impl <Body> OrBranch<Body> {
    pub fn new(branches: Vec<(Tag, Branch<Body>)>) -> Self {
        Self(Rc::new(OrBranch0(branches)))
    }
}

#[derive(Debug, Clone)]
pub struct SeqBranch<Body>(Rc<SeqBranch0<Body>>);
#[derive(Debug)]
struct SeqBranch0<Body> {
    patterns: Vec<Pattern>,
    body: Body,
}

impl <Body> SeqBranch<Body> {
    pub fn new(patterns: Vec<Pattern>, body: Body) -> Self {
        Self(Rc::new(SeqBranch0 { patterns, body }))
    }
}

#[derive(Debug, Clone)]
pub struct Pattern(Rc<Pattern0>);
#[derive(Debug)]
enum Pattern0 {
    MatchAll(VariableName),
    Tuple(Vec<Pattern>)
}

impl Pattern {
    pub fn match_all(var: VariableName) -> Self {
        Self(Rc::new(Pattern0::MatchAll(var)))
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
    AttemptToFeedTagIntoComplexSeqBranch(Value),
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

// TODO:
// #[cfg(test)]
// mod test {
//     use super::*;

//     #[derive(Debug)]
//     pub enum Value {
//         Int(i32),
//         Tag(Tag),
//         Tagged(Tag, Box<Value>),
//         Tuple(Vec<Value>),
//     }
//     impl PatternMatchableValue for Value {
//         fn to_shape(self) -> ValueShape<Self>  {
//             use Value::*;
//             match self {
//                 Tagged(tag, value) => ValueShape::tagged(tag, value.to_shape()),
//                 Tuple(values) => ValueShape::tuple(values.into_iter().map(|val| val.to_shape()).collect()),
//                 val => ValueShape::value(val),
//             }
//         }
//         fn from_shape(value_shape: ValueShape<Self>) -> Self  {
//             use Value::*;
//             match *value_shape.0 {
//                 ValueShape0::Value(value) => value,
//                 ValueShape0::Tuple(value_shapes) => Tuple(value_shapes.into_iter().map(|value_shape| PatternMatchableValue::from_shape(value_shape)).collect()),
//                 ValueShape0::Tagged(tag, value_shape) => Tagged(tag, Box::new(PatternMatchableValue::from_shape(value_shape))),
//                 ValueShape0::Tag(tag) => Tag(tag),
//             }
//         }
//     }


//     impl PatternMatchableValue for Value {
//         fn to_shape(self) -> ValueShape<Self> where Self: Sized {
//             todo!()
//         }
//         fn from_shape(value_shape: ValueShape<Self>) -> Self where Self: Sized {
//             todo!()
//         }
//     }

//     #[test]
//     fn example0() {
//     }
// }
fn match_loop<'branch, Body, Value: PatternMatchableValue>(
    env: Env<Value>,
    value_shape: ValueShape<Value>,
    branch: &'branch Branch<Body>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    use ValueShape0::*;
    use Branch::*;

    match *(value_shape.0) {
        Value(value) => {
            match branch {
                Or(_or_branch) => {
                    Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternBranch(value))
                },
                Seq(_and_branch) => Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value)),
            }
        },
        Tuple(value_shapes) => {
            match branch {
                Seq(and_branch) => { match_and_loop_against_values(env, value_shapes, and_branch) },
                Or(_or_branch) => {
                    let value_shape = ValueShape(Box::new(Tuple(value_shapes)));
                    Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                }
            }
        },
        Tagged(tag, value_shape) => {
            match branch {
                Or(or_branch) => { match_or_loop_against_tagged_value(env, tag, value_shape, or_branch) },
                Seq(_and_branch) => {
                    let value_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                    Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                },
            }
        },
        Tag(tag) => {
            match branch {
                Or(or_branch) => { match_or_loop_against_tag(env, tag, or_branch) },
                Seq(_and_branch) => {
                    let value_shape = ValueShape(Box::new(Tag(tag)));
                    Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(PatternMatchableValue::from_shape(value_shape)))
                },
            }
        },
    }
}

pub fn match_and_loop_against_values<'branch, Body, Value: PatternMatchableValue>(
    env: Env<Value>,
    value_shapes: Vec<ValueShape<Value>>,
    and_branch: &'branch SeqBranch<Body>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    let and_branch = &(*and_branch.0);
    let tuple_patterns = &and_branch.patterns;
    let body = &and_branch.body;
    let env = match_values_to_patterns(value_shapes, tuple_patterns, env)?;
    Ok((env, body))
}

pub fn match_or_loop_against_tagged_value<'branch, Body, Value: PatternMatchableValue>(
    env: Env<Value>,
    tag: Tag,
    value_shape: ValueShape<Value>,
    or_branch: &'branch OrBranch<Body>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    let tagged_branches = &(*or_branch.0).0;
    for (tag0, branch) in tagged_branches.iter() {
        if tag == *tag0 {
            return match_loop(env, value_shape, branch);
        }
    }
    Err(Error::UnableToMatchTag(tag))
}

pub fn match_or_loop_against_tag<'branch, Body, Value: PatternMatchableValue>(
    env: Env<Value>,
    tag: Tag,
    or_branch: &'branch OrBranch<Body>,
) -> Result<(Env<Value>, &'branch Body), Error<Value>> {
    use Branch::*;
    let tagged_branches = &(*or_branch.0).0;
    for (tag0, branch) in tagged_branches.iter() {
        if tag == *tag0 {
            // we commit
            match branch {
                Or(_) => {
                    let value_shape = ValueShape::tag(tag);
                    let value = PatternMatchableValue::from_shape(value_shape);
                    return Err(Error::AttemptToFeedTagIntoComplexTaggedBranch(value))
                },
                Seq(seq_branch) => {
                    let patterns = &(*seq_branch.0).patterns;
                    if patterns.is_empty() {
                        let body = &(*seq_branch.0).body;
                        return Ok((env, body))
                    } else {
                        let value_shape = ValueShape::tag(tag);
                        let value = PatternMatchableValue::from_shape(value_shape);
                        return Err(Error::AttemptToFeedTagIntoComplexTaggedBranch(value))
                    }
                },
            }
        }
    }
    Err(Error::UnableToMatchTag(tag))
}

fn match_values_to_patterns<Value: PatternMatchableValue>(
    value_shapes: Vec<ValueShape<Value>>,
    patterns: &[Pattern],
    mut env: Env<Value>,
) -> Result<Env<Value>, Error<Value>> {
    if value_shapes.len() != patterns.len() {
        return Err(Error::AttemptToMatchTuplesToTuplePatternsOfDifferingLengths)
    }
    for (value_shape, pattern) in value_shapes.into_iter().zip(patterns) {
        env = match_value_to_pattern(value_shape, pattern, env)?;
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
        Pattern0::MatchAll(var) => {
            let value = PatternMatchableValue::from_shape(value_shape);
            let env = env.extend(var.clone(), value);
            return Ok(env)
        },
        _ => {},
    }

    match *value_shape.0 {
        Tag(tag) => match &(*pattern.0) {
            Pattern0::MatchAll(_) => { unreachable!() },
            Pattern0::Tuple(_patterns) => {
                let tuple_shape = ValueShape(Box::new(Tag(tag)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Tagged(tag, value_shape) => match &(*pattern.0) {
            Pattern0::MatchAll(_) => { unreachable!() },
            Pattern0::Tuple(_patterns) => {
                let tuple_shape = ValueShape(Box::new(Tagged(tag, value_shape)));
                let value = PatternMatchableValue::from_shape(tuple_shape);
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Value(value) => match &(*pattern.0) {
            Pattern0::MatchAll(_) => { unreachable!() },
            Pattern0::Tuple(_patterns) => {
                Err(Error::AttemptToFeedNonTupleValueIntoAndPatternBranch(value))
            },
        },
        Tuple(tuple_shape) => match &(*pattern.0) {
            Pattern0::MatchAll(_) => { unreachable!() },
            Pattern0::Tuple(patterns) => {
                match_values_to_patterns(tuple_shape, patterns, env)
            },
        },
    }
}
