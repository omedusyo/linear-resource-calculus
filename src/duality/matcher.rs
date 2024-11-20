use std::rc::Rc;
use std::fmt;
use crate::identifier::{VariableName, Tag};
use crate::duality::{
    pattern::Pattern,
    error::Error,
    value::{ValueShape, ValueShape0, Env, PatternMatchableValue}
};

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

// StrictCode :=
// | Code
// | PatternCode
// | OrCode
// OrCode := Vec<(Tag, )>
// PatternCode := (Pattern, Code)

// TODO: Consider introducing new terminology: PassiveConsumerCodeShape<Code>.
//       vs AggressiveConsumerCodeShape

// ===Code===
#[derive(Debug, Clone)]
pub enum StrictCode<Code> {
    Code(Code),
    Or(StrictOrCode<Code>),
    Pattern(StrictPatternCode<Code>),
}

impl <Code> StrictCode<Code> {
    pub fn or(or_code: StrictOrCode<Code>) -> Self {
        Self::Or(or_code)
    }
    pub fn seq(pattern_code: StrictPatternCode<Code>) -> Self {
        Self::Pattern(pattern_code)
    }
    pub fn code(code: Code) -> Self {
        Self::Code(code)
    }
    pub fn pattern(pattern: Pattern, code: Code) -> Self {
        Self::Pattern(StrictPatternCode::new(pattern, code))
    }
    fn is_match_all(&self) -> Option<(&VariableName, &Code)> {
        use StrictCode::*;
        match self {
            Code(_) => None,
            Or(_) => None,
            Pattern(pattern_code) => pattern_code.is_match_all(),
        }
    }
}

impl <Code: fmt::Display> fmt::Display for StrictCode<Code> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use StrictCode::*;
        match self {
            Code(code) => code.fmt(f),
            Or(or_code) => or_code.fmt(f),
            Pattern(seq_code) => seq_code.fmt(f),
        }
    }
}

impl <Code: fmt::Display> fmt::Display for StrictOrCode<Code> {
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

impl <Code: fmt::Display> fmt::Display for StrictPatternCode<Code> {
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


#[derive(Debug, Clone)]
pub struct StrictOrCode<Code>(Rc<OrCode0<Code>>);
#[derive(Debug)]
struct OrCode0<Code>(Vec<(Tag, StrictCode<Code>)>);

impl <Code> StrictOrCode<Code> {
    pub fn new(tagged_codes: Vec<(Tag, StrictCode<Code>)>) -> Self {
        Self(Rc::new(OrCode0(tagged_codes)))
    }
}

#[derive(Debug, Clone)]
pub struct StrictPatternCode<Code>(Rc<PatternCode0<Code>>);
#[derive(Debug)]
struct PatternCode0<Code> {
    pattern: Pattern,
    code: Code,
}

impl <Code> StrictPatternCode<Code> {
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

// ===Fully Consumming Code===
impl <Code> StrictOrCode<Code> {
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

impl <Code> StrictPatternCode<Code> {
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

impl <Code> StrictCode<Code> {
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
        use StrictCode::*;
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
