use std::rc::Rc;
use std::fmt;
use crate::identifier::{VariableName, Tag};
use crate::duality::{
    pattern::Pattern,
    error::Error,
    value::{ValueShape, Env, PatternMatchableValue},
};

// OrCode := Vec<(Tag, [ Code | PatternCode | OrCode ])>
// PatternCode := (Pattern, [ Code | PatternCode | OrCode])


// ===Code===
#[derive(Debug, Clone)]
pub enum LazyCode<Code> {
    Code(Code),
    // TODO: Would actually be cool to allow also inductive tags. OrStrict vs OrLazy
    Or(LazyOrCode<Code>),
    Pattern(LazyPatternCode<Code>),
}

impl <Code> LazyCode<Code> {
    pub fn or(or_code: LazyOrCode<Code>) -> Self {
        Self::Or(or_code)
    }
    pub fn seq(pattern_code: LazyPatternCode<Code>) -> Self {
        Self::Pattern(pattern_code)
    }
    pub fn code(code: Code) -> Self {
        Self::Code(code)
    }
    pub fn tagged(tag: Tag, code: LazyCode<Code>) -> Self {
        Self::Or(LazyOrCode::singleton(tag, code))
    }
    pub fn pattern(pattern: Pattern, code: LazyCode<Code>) -> Self {
        Self::Pattern(LazyPatternCode::new(pattern, code))
    }
    fn is_match_all(&self) -> Option<(&VariableName, &LazyCode<Code>)> {
        use LazyCode::*;
        match self {
            Code(_) => None,
            Or(_) => None,
            Pattern(pattern_code) => pattern_code.is_match_all(),
        }
    }
}


#[derive(Debug, Clone)]
pub struct LazyOrCode<Code>(pub Rc<OrCode0<Code>>);
#[derive(Debug)]
pub struct OrCode0<Code>(pub Vec<(Tag, LazyCode<Code>)>);

impl <Code> LazyOrCode<Code> {
    pub fn new(tagged_codes: Vec<(Tag, LazyCode<Code>)>) -> Self {
        Self(Rc::new(OrCode0(tagged_codes)))
    }
    fn singleton(tag: Tag, code: LazyCode<Code>) -> Self {
        Self::new(vec![(tag, code)])
    }
}

#[derive(Debug, Clone)]
pub struct LazyPatternCode<Code>(Rc<PatternCode0<Code>>);
#[derive(Debug)]
struct PatternCode0<Code> {
    pattern: Pattern,
    code: LazyCode<Code>,
}

impl <Code> LazyPatternCode<Code> {
    pub fn new(pattern: Pattern, code: LazyCode<Code>) -> Self {
        Self(Rc::new(PatternCode0 { pattern, code }))
    }
    fn is_match_all(&self) -> Option<(&VariableName, &LazyCode<Code>)> {
        let pattern =  &(*self.0).pattern;
        let code =  &(*self.0).code;
        let var_name = pattern.is_match_all()?;
        Some((var_name, code))
    }
}

// ===Display===
impl <Code: fmt::Display> fmt::Display for LazyCode<Code> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LazyCode::*;
        match self {
            Code(code) => code.fmt(f),
            Or(or_code) => or_code.fmt(f),
            Pattern(seq_code) => seq_code.fmt(f),
        }
    }
}

impl <Code: fmt::Display> fmt::Display for LazyOrCode<Code> {
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

impl <Code: fmt::Display> fmt::Display for LazyPatternCode<Code> {
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

// ===Sender===
pub enum SenderResponse<'c, Value, Code> {
    Success(Env<Value>, &'c LazyCode<Code>),
    StuckSendingTag(Env<Value>, &'c Code, Tag),
    StuckSendingValue(Env<Value>, &'c Code, Value),
}

impl <Code: std::fmt::Debug> LazyCode<Code> {
    pub fn send_tag<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        tag: Tag
    ) -> Result<SenderResponse<'_, Value, Code>, Error<Value>> {
        self.send_tag_loop(Env::new(), tag)
    }

    pub fn send_tag_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        tag: Tag
    ) -> Result<SenderResponse<'_, Value, Code>, Error<Value>> {
        use LazyCode::*;
        match self {
            Code(code) => {
                Ok(SenderResponse::StuckSendingTag(env, code, tag))
            },
            Or(or_code) => or_code.send_loop(env, tag),
            Pattern(_pattern_code) => {
                Err(Error::AttemptToFeedTagIntoComplexCode)
            },
        }
    }

    pub fn send_value_shape<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        value_shape: ValueShape<Value>,
    ) -> Result<SenderResponse<'_, Value, Code>, Error<Value>> {
        self.send_value_shape_loop(Env::new(), value_shape)
    }

    fn send_value_shape_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        value_shape: ValueShape<Value>,
    ) -> Result<SenderResponse<'_, Value, Code>, Error<Value>> {
        use LazyCode::*;
        match self {
            Code(code) => {
                let value = PatternMatchableValue::from_shape(value_shape);
                Ok(SenderResponse::StuckSendingValue(env, code, value))
            },
            // TODO
            Or(_or_code) => todo!("error-or"),
            Pattern(pattern_code) => pattern_code.send_value_shape_loop(env, value_shape),
        }
    }
}

impl <Code> LazyOrCode<Code> {
    fn send_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        tag: Tag
    ) -> Result<SenderResponse<'_, Value, Code>, Error<Value>> {
        let tagged_codes = &*self.0.0;
        for (tag0, code) in tagged_codes {
            if tag == *tag0 {
                return Ok(SenderResponse::Success(env, code))
            }
        }
        Err(Error::UnableToMatchTag(tag))
    }
}

impl <Code> LazyPatternCode<Code> {
    fn send_value_loop<Value>(
        &self,
        env: Env<Value>,
        value: Value,
    ) -> Result<(Env<Value>, &LazyCode<Code>), Error<Value>> {
        let pattern = &(*self.0).pattern;
        let env = pattern.match_value_loop(env, value)?;
        let code = &(*self.0).code;
        Ok((env, code))
    }

    fn send_value_shape_loop<Value: PatternMatchableValue + std::fmt::Debug>(
        &self,
        env: Env<Value>,
        value_shape: ValueShape<Value>,
    ) -> Result<SenderResponse<'_, Value, Code>, Error<Value>> {
        let pattern = &(*self.0).pattern;
        let env = pattern.match_loop(env, value_shape)?;
        let code = &(*self.0).code;
        Ok(SenderResponse::Success(env, code))
    }
}
