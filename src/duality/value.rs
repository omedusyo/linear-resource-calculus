use crate::identifier::{VariableName, Tag};

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

// ===Env===
pub struct Env<Value> {
    pub bindings: Vec<(VariableName, Value)>
}

impl <Value> Env<Value> {
    pub fn new() -> Self {
        Self { bindings: vec![] }
    }

    pub fn extend(mut self, var: VariableName, value: Value) -> Self {
        self.bindings.push((var, value));
        self
    }

    pub fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, Value)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}
