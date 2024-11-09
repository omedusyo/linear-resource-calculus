use std::rc::Rc;
use std::fmt;
use crate::{TokenStream, IResult0};
use crate::tokenizer::anyidentifier;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableName(Rc<String>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionName(Rc<String>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tag(pub Rc<String>); // This is basically constructor name.

impl VariableName {
    pub fn new(str: String) -> Self {
        Self(Rc::new(str))
    }

    pub fn str(&self) -> &str {
        &self.0
    }
}

impl FunctionName {
    pub fn new(str: String) -> Self {
        Self(Rc::new(str))
    }
    
    pub fn str(&self) -> &str {
        &self.0
    }
}

impl Tag {
    pub fn new(str: String) -> Self {
        Self(Rc::new(str))
    }

    pub fn str(&self) -> &str {
        &self.0
    }
}

pub fn variable_name(input: TokenStream) -> IResult0<VariableName> {
    let (input, str) = anyidentifier(input)?;
    Ok((input, VariableName::new(str)))
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}
