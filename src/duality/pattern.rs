use std::rc::Rc;
use std::fmt;
use crate::identifier::VariableName;
use crate::identifier::Tag;
use crate::helper::fmt_sequence;
use crate::duality::value::{PatternMatchableValue, ValueShape, ValueShape0, Env};

// ===Errors===
#[derive(Debug)]
pub enum Error<Value> {
    AttemptToFeedNonTaggedValueIntoOrPatternCode(Value),
    AttemptToMatchTuplesToTupleCodeOfDifferingLengths,
    AttemptToFeedComplexValueIntoPatternUnguarded(Value),
    AttemptToFeedTagIntoComplexCode,
    AttemptToMatchNonTupleValueAgainstTuplePatternCode(Value),
    UnableToMatchTag(Tag),
}

// Pattern =
// | MatchAll(Var)
// | Tuple(Vec<Pattern>)

#[derive(Debug, Clone)]
pub struct Pattern(pub Rc<Pattern0>);
#[derive(Debug)]
pub enum Pattern0 {
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
    pub fn is_match_all(&self) -> Option<&VariableName> {
        use Pattern0::*;
        match &(*self.0) {
            MatchAll(var) => Some(var),
            Tuple(_) => None
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Pattern0::*;
        match &(*self.0) {
            MatchAll(var) => write!(f, "{}", var.0),
            Tuple(patterns) => {
                write!(f, "[",)?;
                fmt_sequence(patterns, ",", f)?;
                write!(f, "]",)?;
                Ok(())
            },
        }
    }
}


impl Pattern {
    pub fn match_<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        value_shape: ValueShape<Value>
    ) -> Result<Env<Value>, Error<Value>> {
        self.match_loop(Env::new(), value_shape)
    }

    pub fn match_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        value_shape: ValueShape<Value>
    ) -> Result<Env<Value>, Error<Value>> {
        use ValueShape0::*;
        match &(*self.0) {
            Pattern0::MatchAll(var) => {
                let value = PatternMatchableValue::from_shape(value_shape);
                let env = env.extend(var.clone(), value);
                return Ok(env)
            },
            Pattern0::Tuple(patterns) => {
                match *value_shape.0 {
                    Tuple(tuple_shape) => {
                        match_seq_loop(env, patterns, tuple_shape)
                    },
                    value_shape => {
                        let value_shape= ValueShape(Box::new(value_shape));
                        let value = PatternMatchableValue::from_shape(value_shape);
                        Err(Error::AttemptToMatchNonTupleValueAgainstTuplePatternCode(value))
                    },
                }
            },
        }
    }

    pub fn match_value_loop<Value>(
        &self,
        env: Env<Value>,
        value: Value,
    ) -> Result<Env<Value>, Error<Value>> {
        match &(*self.0) {
            Pattern0::MatchAll(var) => {
                let env = env.extend(var.clone(), value);
                Ok(env)
            },
            Pattern0::Tuple(_patterns) => Err(Error::AttemptToMatchNonTupleValueAgainstTuplePatternCode(value)),
        }
    }
}

pub fn match_seq<Value: PatternMatchableValue + std::fmt::Debug>(
    patterns: &[Pattern],
    value_shapes: Vec<ValueShape<Value>>,
) -> Result<Env<Value>, Error<Value>>{
    match_seq_loop(Env::new(), patterns, value_shapes)
}

pub fn match_seq_loop<Value: PatternMatchableValue + std::fmt::Debug>(
    mut env: Env<Value>,
    patterns: &[Pattern],
    value_shapes: Vec<ValueShape<Value>>,
) -> Result<Env<Value>, Error<Value>>{
    if value_shapes.len() != patterns.len() {
        return Err(Error::AttemptToMatchTuplesToTupleCodeOfDifferingLengths)
    }
    for (value_shape, pattern) in value_shapes.into_iter().zip(patterns) {
        env = pattern.match_loop(env, value_shape)?;
    }
    Ok(env)
}
