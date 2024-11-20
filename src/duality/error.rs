use crate::identifier::Tag;

#[derive(Debug)]
pub enum Error<Value> {
    AttemptToFeedNonTaggedValueIntoOrPatternCode(Value),
    AttemptToMatchTuplesToTupleCodeOfDifferingLengths,
    AttemptToFeedComplexValueIntoPatternUnguarded(Value),
    AttemptToFeedTagIntoComplexCode,
    AttemptToMatchNonTupleValueAgainstTuplePatternCode(Value),
    UnableToMatchTag(Tag),
}
