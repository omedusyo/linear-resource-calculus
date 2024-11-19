use std::rc::Rc;
use crate::identifier::{VariableName, Tag};
use std::fmt;

// example.
//   or{
//   | a . or{
//         | a0 . body-a0
//         | a1 . pb{ [] . body-a1 }
//         }
//   | b . pb{ [x, y] . body-b}
//   | c . body-c
//   | d . pb{ x . body-d }
//   | e . pb{ [x, [y, [z0, z1]], w] . body-e }
//   }

// This can be used for let-bindings, match-expressions, or object-expressions.
// (let)
//   The code that can be extracted from
//     let { x = e0, [y0, [y10, y11], []] = e1, [z0, z1] = e2 . body }
//   is
//     object {
//     | @msg0 . body0
//     | @msg1 x . body1
//     | @msg2 @msg22 [x, y] . body2
//     | @msg3 { @msg30 . body30 | @msg31 . body31 }
//     }
//   is
//     or{
//     | msg0 . body0
//     | msg1 . pb{ x . body1 }
//     | msg2 . or{ msg22 . pb{ [x, y] . body2 }}
//     | msg3 . or{ msg30 . body30 | msg31 . body31 }
//     }

// Pattern =
// | MatchAll(Var)
// | Tuple(Vec<Pattern>)
// OrCode := Vec<(Tag, [ Code | PatternCode | OrCode ])>
// PatternCode := (Pattern, Code)


// ===Code Shapes===
#[derive(Debug, Clone)]
pub enum CodeShape<Code> {
    Code(Code),
    Or(OrCode<Code>),
    Pattern(PatternCode<Code>),
}

impl <Code> CodeShape<Code> {
    pub fn or(or_code: OrCode<Code>) -> Self {
        Self::Or(or_code)
    }
    pub fn seq(pattern_code: PatternCode<Code>) -> Self {
        Self::Pattern(pattern_code)
    }
    pub fn code(code: Code) -> Self {
        Self::Code(code)
    }
    pub fn pattern(pattern: Pattern, code: Code) -> Self {
        Self::Pattern(PatternCode::new(pattern, code))
    }
    fn is_match_all(&self) -> Option<(&VariableName, &Code)> {
        use CodeShape::*;
        match self {
            Code(_) => None,
            Or(_) => None,
            Pattern(pattern_code) => pattern_code.is_match_all(),
        }
    }
}

impl <Code: fmt::Display> fmt::Display for CodeShape<Code> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CodeShape::*;
        match self {
            Code(code) => code.fmt(f),
            Or(or_code) => or_code.fmt(f),
            Pattern(seq_code) => seq_code.fmt(f),
        }
    }
}

impl <Code: fmt::Display> fmt::Display for OrCode<Code> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "or{{")?;
        let pairs = &(*self.0.0);
        match pairs.len() {
            0 => write!(f, "")?,
            1 => write!(f, "{} . {}", pairs[0].0, pairs[0].1)?,
            _ => {
                write!(f, "{} . {} ", pairs[0].0.0, pairs[0].1)?;
                for (tag, code) in &pairs[1..] {
                    write!(f, "| {} . {} ", tag.0, code)?;
                }
            },
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl <Code: fmt::Display> fmt::Display for PatternCode<Code> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "pattern{{")?;
        let pattern = &(*self.0).pattern;
        let code = &self.0.code;
        pattern.fmt(f)?;
        write!(f, ". {}", code)?;
        write!(f, "}}")?;
        Ok(())
    }
}

fn fmt_sequence<A: fmt::Display>(patterns: &[A], delimiter: &str, f: &mut fmt::Formatter) -> fmt::Result {
    match patterns.len() {
        0 => write!(f, ""),
        1 => write!(f, "{}", patterns[0]),
        _ => {
            write!(f, "{}", patterns[0])?;
            for pattern in patterns {
                write!(f, "{} {}", delimiter, pattern)?;
            }
            Ok(())
        },
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


#[derive(Debug, Clone)]
pub struct OrCode<Code>(Rc<OrCode0<Code>>);
#[derive(Debug)]
struct OrCode0<Code>(Vec<(Tag, CodeShape<Code>)>);

impl <Code> OrCode<Code> {
    pub fn new(tagged_codes: Vec<(Tag, CodeShape<Code>)>) -> Self {
        Self(Rc::new(OrCode0(tagged_codes)))
    }
}

#[derive(Debug, Clone)]
pub struct PatternCode<Code>(Rc<PatternCode0<Code>>);
#[derive(Debug)]
struct PatternCode0<Code> {
    pattern: Pattern,
    code: Code,
}

impl <Code> PatternCode<Code> {
    pub fn new(pattern: Pattern, code: Code) -> Self {
        Self(Rc::new(PatternCode0 { pattern, code }))
    }
    fn is_match_all(&self) -> Option<(&VariableName, &Code)> {
        let pattern =  &(*self.0).pattern;
        let code =  &(*self.0).code;
        let var_name = pattern.is_match_all()?;
        Some((var_name, code))
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
    fn is_match_all(&self) -> Option<&VariableName> {
        use Pattern0::*;
        match &(*self.0) {
            MatchAll(var) => Some(var),
            Tuple(_) => None
        }
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

// ===Env===
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

// ===Codes===
impl <Code> OrCode<Code> {
    pub fn match_<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        value_shape: ValueShape<Value>,
    ) -> Result<(Env<Value>, &Code), Error<Value>> {
        self.match_loop(Env::new(), value_shape)
    }

    fn match_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        value_shape: ValueShape<Value>,
    ) -> Result<(Env<Value>, &Code), Error<Value>> {
        use ValueShape0::*;
        let (tag, maybe_value_shape) = match *value_shape.0 {
            Tag(tag) => (tag, None),
            Tagged(tag, value_shape) => (tag, Some(value_shape)),
            value_shape => {
                let value_shape= ValueShape(Box::new(value_shape));
                let value = PatternMatchableValue::from_shape(value_shape);
                return Err(Error::AttemptToFeedNonTaggedValueIntoOrPatternCode(value))
            },
        };

        let tagged_codes = &(*self.0).0;
        for (tag0, code) in tagged_codes.iter() {
            if tag == *tag0 {
                return code.match_loop(env, maybe_value_shape)
            }
        }
        Err(Error::UnableToMatchTag(tag))
    }
}

impl <Code> PatternCode<Code> {
    pub fn match_<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        value_shape: ValueShape<Value>,
    ) -> Result<(Env<Value>, &Code), Error<Value>> {
        self.match_loop(Env::new(), value_shape)
    }

    fn match_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        value_shape: ValueShape<Value>,
    ) -> Result<(Env<Value>, &Code), Error<Value>> {
        let pattern_code = &*self.0;
        let env = pattern_code.pattern.match_loop(env, value_shape)?;
        Ok((env, &pattern_code.code))
    }
}

impl <Code> CodeShape<Code> {
    pub fn match_<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        maybe_value_shape: Option<ValueShape<Value>>,
    ) -> Result<(Env<Value>, &Code), Error<Value>> {
        self.match_loop(Env::new(), maybe_value_shape)
    }

    fn match_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        maybe_value_shape: Option<ValueShape<Value>>,
    ) -> Result<(Env<Value>, &Code), Error<Value>> {
        use CodeShape::*;
        match maybe_value_shape {
            Some(value_shape) => {
                match self {
                    Or(or_code) => {
                        or_code.match_loop(env, value_shape)
                    },
                    Pattern(pattern_code) => {
                        pattern_code.match_loop(env, value_shape)
                    },
                    Code(_) => {
                        let value = PatternMatchableValue::from_shape(value_shape);
                        Err(Error::AttemptToFeedComplexValueIntoPatternUnguarded(value))
                    },
                }
            },
            None => {
                match self {
                    Code(code) => Ok((env, code)),
                    _ => Err(Error::AttemptToFeedTagIntoComplexCode),
                }
            },
        }
    }
}



// ===Pattern Matching===
impl Pattern {
    pub fn match_<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        value_shape: ValueShape<Value>
    ) -> Result<Env<Value>, Error<Value>> {
        self.match_loop(Env::new(), value_shape)
    }

    fn match_loop<Value: PatternMatchableValue + std::fmt::Debug>(
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
}

pub fn match_seq<Value: PatternMatchableValue + std::fmt::Debug>(
    patterns: &[Pattern],
    value_shapes: Vec<ValueShape<Value>>,
) -> Result<Env<Value>, Error<Value>>{
    match_seq_loop(Env::new(), patterns, value_shapes)
}

fn match_seq_loop<Value: PatternMatchableValue + std::fmt::Debug>(
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
