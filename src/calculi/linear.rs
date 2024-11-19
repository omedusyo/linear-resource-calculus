use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType};
use crate::syntax::{
    anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, vector, delimited_nonempty_vector, delimited_vector,
    parameter_vector, or_vector, comma_vector, binding_vector, parens, brackets, curly_braces, 
};
use crate::identifier::{VariableName, FunctionName, Tag};
use crate::IResult0;
use std::collections::HashMap;
use crate::duality::{
    pattern,
    pattern::Pattern,
    matcher::{StrictCode, StrictOrCode, StrictPatternCode},
    value,
    value::{PatternMatchableValue, ValueShape, ValueShape0},
};

// ===parser===
fn identifier_to_operation_code(str: &str) -> Option<OperationCode> {
    match str {
        "+" => Some(OperationCode::Add),
        "*" => Some(OperationCode::Mul),
        "sub" => Some(OperationCode::Sub),
        "==" => Some(OperationCode::Eq),
        "dup" => Some(OperationCode::Clone),
        "discard" => Some(OperationCode::Discard),
        _ => None
    }
}

fn parse_operator_arguments(op_code: OperationCode, input: TokenStream) -> IResult0<Expression> {
    use OperationCode::*;
    match op_code {
        Add => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application2(op_code, e0, e1)))
        },
        Sub => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application2(op_code, e0, e1)))
        },
        Mul => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application2(op_code, e0, e1)))
        },
        Eq => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application2(op_code, e0, e1)))
        },
        Clone => {
            let (input, e0) = parse_arg_list1(input)?;
            Ok((input, Expression::operation_application1(op_code, e0)))
        },
        Discard => {
            let (input, e0) = parse_arg_list1(input)?;
            Ok((input, Expression::operation_application1(op_code, e0)))
        },
    }
}

fn parse_arg_list2(input: TokenStream) -> IResult0<(Expression, Expression)> {
    // "[e0, e1]  "
    let (input, _) = token(TokenType::OpenBracket)(input)?;
    let (input, e0) = parse_expression(input)?;
    let (input, _) = token(TokenType::Comma)(input)?;
    let (input, e1) = parse_expression(input)?;
    let (input, _) = token(TokenType::CloseBracket)(input)?;
    Ok((input, (e0, e1)))
}

fn parse_arg_list1(input: TokenStream) -> IResult0<Expression> {
    // "[e0]  "
    let (input, _) = token(TokenType::OpenBracket)(input)?;
    let (input, e0) = parse_expression(input)?;
    let (input, _) = token(TokenType::CloseBracket)(input)?;
    Ok((input, e0))
}

// No parens, just a possibly empty comma separated list of expressions.
fn expression_vector(input: TokenStream) -> IResult0<Vec<Expression>> {
    comma_vector(parse_expression)(input)
}

// #tag
fn parse_tag_as_constructor(input: TokenStream) -> IResult0<Tag> {
    use Token::*;
    let (input, token0) = anytoken(input)?;
    match token0 {
        TagSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Tag::new(var_name)))
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// @msg
fn parse_tag_as_message(input: TokenStream) -> IResult0<Tag> {
    use Token::*;
    let (input, token0) = anytoken(input)?;
    match token0 {
        TagSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Tag::new(var_name)))
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

pub fn parse_program(input: TokenStream) -> IResult0<Program> {
    let (input, definitions) = vector(parse_function_definition)(input)?;
    let mut program = Program::new();
    for definition in definitions {
        let name = definition.name.clone();
        program.function_definitions.insert(name.clone(), definition);
        program.function_definitions_ordering.push(name);
    }
    Ok((input, program))
}

pub fn parse_function_definition(input: TokenStream) -> IResult0<FunctionDefinition> {
    let (input, _) = identifier("fn")(input)?;
    let (input, function_name_str) = anyidentifier(input)?;
    let (input, parameters) = brackets(parameter_vector)(input)?;

    let (input, body) = curly_braces(parse_expression)(input)?;
    Ok((input, FunctionDefinition { name: FunctionName::new(function_name_str), parameters, body }))
}

// =====Parsing Patterns========

// x, y, z
// x, [y0, y1], [[]]
// x, [[y0, y1], z], w
fn parse_pattern_sequence(input: TokenStream) -> IResult0<Vec<Pattern>> {
    comma_vector(parse_pattern)(input)
}

// [x, y, z]
// [x, [y0, y1], [[]]]
// [x, [[y0, y1], z], w]
fn parse_tuple_pattern(input: TokenStream) -> IResult0<Vec<Pattern>> {
    brackets(parse_pattern_sequence)(input)
}

// var
// [x,y,z]
// [x,[y0, y1],z]
fn parse_pattern(input: TokenStream) -> IResult0<Pattern> {
    use Token::*;
    let (input, token0) = anytoken(input)?;
    match token0 {
        Identifier(var_name) => Ok((input, Pattern::match_all(VariableName::new(var_name)))),
        OpenBracket => {
            let (input, patterns) = parse_pattern_sequence(input)?;
            let (input, _) = token(TokenType::CloseBracket)(input)?;
            Ok((input, Pattern::tuple(patterns)))
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// =====Parsing Code========

// { inner-code }
fn parse_code(input: TokenStream) -> IResult0<StrictCode<Expression>> {
    let (input, code) = curly_braces(parse_inner_code)(input)?;
    Ok((input, code))
}

// =primitive code=
// . body
//
// =or-code=
// #tag0 . body
// #tag0 #tag1 . body
// #tag0 #tag1 #tag2 . body
// #tag0 { #tag00 . body 
//       | #tag01 . body
//       }
// #tag [x, y, z] . body
// #tag0 . bod0 | #tag1 x . body1 | #tag2 . body2
//
// =pattern-code=
// [x, y, z] . body
// x . body
fn parse_inner_code(input: TokenStream) -> IResult0<StrictCode<Expression>> {
    use Token::*;
    let (input_if_commited, token0) = anytoken(input)?;
    match token0 {
        TagSymbol | OrSeparator  => { // do not commit
            let (input, or_code) = parse_or_code(input)?;
            Ok((input, StrictCode::or(or_code)))
        },
        OpenBracket | Identifier(_) => { // do not commit
            let (input, and_branch) = parse_pattern_code(input)?;
            Ok((input, StrictCode::seq(and_branch)))
        },
        BindingSeparator => { // commit
            let (input, body) = parse_expression(input_if_commited)?;
            Ok((input, StrictCode::code(body)))
        },
        _ => return Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// tagged_code | tagged_code | tagged_code
fn parse_or_code(input: TokenStream) -> IResult0<StrictOrCode<Expression>> {
    let (input, tagged_codes) = or_vector(parse_tag_code)(input)?;
    let or_branch = StrictOrCode::new(tagged_codes);
    Ok((input, or_branch))
}

// #tag0 . body
// #tag0 x . body
// #tag0 #tag1 . body
// #tag0 { #tag00 . body 
//       | #tag01 . body
//       }
// #tag [x, y, z] . body
fn parse_tag_code(input: TokenStream) -> IResult0<(Tag, StrictCode<Expression>)> {
    use Token::*;

    let (input, tag) = parse_tag_as_constructor(input)?;
    let (input_if_commited, token0) = anytoken(input)?;
    match token0 {
        BindingSeparator => { // commit
            // #tag . body
            let (input, body) = parse_expression(input_if_commited)?;
            Ok((input, (tag, StrictCode::code(body))))
        },
        Identifier(_) => { // do not commit
            // #tag x . body
            let (input, pattern_code) = parse_pattern_code(input)?;
            Ok((input, (tag, StrictCode::seq(pattern_code))))
        }
        TagSymbol => { // do not commit
            // #tag #tag1 . body
            let (input, tagged_branch) = parse_tag_code(input)?;
            let or_code = StrictOrCode::new(vec![tagged_branch]);
            Ok((input, (tag, StrictCode::or(or_code))))
        },
        OpenBracket => { // do not commit
            // #tag [x, y, z] . body
            let (input, pattern_code) = parse_pattern_code(input)?;
            Ok((input, (tag, StrictCode::seq(pattern_code))))
        },
        OpenCurly => { // commit
            // #tag { #tag00 . body 
            //      | #tag01 . body
            //      }
            let (input, or_code) = parse_or_code(input_if_commited)?;
            let branch = StrictCode::or(or_code);

            let (input, _) = token(TokenType::CloseCurly)(input)?;
            Ok((input, (tag, branch)))
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// x . body
// [p, q, r] . body
// [[p0, p1], q, r] . body
fn parse_pattern_code(input: TokenStream) -> IResult0<StrictPatternCode<Expression>> {
    let (input, pattern) = parse_pattern(input)?;
    let (input, _) = token(TokenType::BindingSeparator)(input)?;
    let (input, body) = parse_expression(input)?;
    Ok((input, StrictPatternCode::new(pattern, body)))
}

// ===Parsing bindings===
// x = e0
// [x0, x1] = e0
// [x, [y0, y1], []] = e0
// [] = e0
pub fn parse_binding(input: TokenStream) -> IResult0<(Pattern, Expression)> {
    let (input, pattern) = parse_pattern(input)?;
    let (input, _) = token(TokenType::Eq)(input)?;
    let (input, arg) = parse_expression(input)?;

    Ok((input, (pattern, arg)))
}

// x = e0, y = e1, z = e2
// [x0, x1] = e0, [] = e1, y = e2, [z0, [z1, z2], []] = e3
pub fn parse_bindings(input: TokenStream) -> IResult0<Bindings> {
    let (input, binding) = comma_vector(parse_binding)(input)?;
    let mut bindings = Bindings0::Empty;
    for (pattern, expr) in binding {
        bindings = Bindings0::Push { pattern, expr, parent: Bindings(Box::new(bindings)) };
    }
    Ok((input, Bindings(Box::new(bindings))))
}

// ===Parsing expressions===
pub fn parse_expression(input: TokenStream) -> IResult0<Expression> {
    use Token::*;

    let (input, token0) = anytoken(input)?;
    match token0 {
        Int(x) => Ok((input, Expression::int(x))),
        VarMoveSymbol => {
            // move x
            // %x
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Expression::var_move(VariableName::new(var_name))))
        },
        VarCloneSymbol => {
            // clone x
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Expression::var_clone(VariableName::new(var_name))))
        },
        VarDropSymbol => {
            // drop x . expr
            // Note that `drop x` by itself doesn't make any sense. You have to atlesat do `drop x . []`
            let (input, var_name) = anyidentifier(input)?;
            let (input, _) = token(TokenType::BindingSeparator)(input)?;
            let (input, expr) = parse_expression(input)?;
            Ok((input, Expression::var_drop(VariableName::new(var_name), expr)))
        },
        TagSymbol => {
            let (input, tag) = anyidentifier(input)?;
            let tag = Tag::new(tag);
            let (_, token) = peek_anytoken(input)?;
            if token.is_start_of_expression() {
                let (input, arg) = parse_expression(input)?;
                Ok((input, Expression::tagged(tag, arg)))
            } else {
                Ok((input, Expression::tag(tag)))
            }
        },
        OpenBracket => {
            // tuple
            let (input, args) = expression_vector(input)?;
            let (input, _) = token(TokenType::CloseBracket)(input)?;
            Ok((input, Expression::tuple(args)))
        },
        OpenParen => {
            todo!("Unexpected `(` in linear calculus.")
        },
        Identifier(identifier) => {
            match &identifier[..] {
                "let" => {
                    // Basic let binding
                    //   let { a = 5 . body }
                    //   let { a = 5, b = 6 . body }
                    // The following is just syntactic sugar for `match [1, 2] { [a, b] . body }
                    //   let { [a, b] = [1, 2] . body } 
                    // The following is just syntactic sugar for `match [   [1, 2],   [512, [60, 61], []]  ] { [a, b] . body }`
                    //   let { [a, b] = [1, 2], [c, [d0, d1], []] = [512, [60, 61], []] . body } 
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, bindings) = parse_bindings(input)?;
                    let (input, _) = token(TokenType::BindingSeparator)(input)?;
                    let (input, body) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;
                    Ok((input, Expression::let_move(bindings, body)))
                },
                "cut" => {
                    //   cut { a = 5 . body }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, bindings) = parse_bindings(input)?;
                    let (input, _) = token(TokenType::BindingSeparator)(input)?;
                    let (input, body) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, Expression::cut(bindings, body)))
                },
                "match" => {
                    // match[e, code]
                    // match[code]
                    //
                    // match[e, { #T . body | #F . body }]
                    // match[e, { #nil . body | #cons [x, xs] . body }]
                    // TODO: The next token could reasonable be with empty (unit) argument i.e.
                    //         match { . body }
                    //       But this would require a conversion of Expression<Code> into Expression.
                    //       And it seems pretty useless... we would have it only for consistency
                    //       sake...
                    let (input, arg) = parse_expression(input)?;
                    let (input, code) = parse_code(input)?;
                    Ok((input, Expression::match_(arg, code)))
                },
                "obj" => {
                    // obj { #hd . body0 | #tl . body1 }
                    let (input, code) = parse_code(input)?;
                    Ok((input, Expression::object(code)))
                },
                "send" => {
                    // Same discussion applies as in the "match" case.
                    // send[%obj, %msg]
                    let (input, obj) = parse_expression(input)?;
                    let (input, msg) = parse_expression(input)?;
                    Ok((input, Expression::send(obj, msg)))
                },
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => 
                        parse_operator_arguments(op_code, input),
                    None => {
                        let (input, token_match) = peek_token(TokenType::OpenBracket)(input)?;
                        match token_match {
                            Some(_) => {
                                // Here we have a function call
                                let (input, arguments) = brackets(expression_vector)(input)?;
                                Ok((input, Expression::call(FunctionName::new(identifier), arguments)))
                            },
                            None => {
                                // TODO: Check if this is a function application
                                // I need to peek if the next token is an open paren
                                Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt }))
                            }
                        }
                    },
                },
            }
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

pub fn parse_match_expression(input: TokenStream) -> IResult0<Expression> {
                    // TODO: take a lookahead if this happens to be {
                    // let (input, arg) = parse_expression(input)?;
                    // let (input, branches) = parse_match_expression(input)?;
                    // Ok((input, Expression::match_(arg, branches)))
    todo!()

}
pub fn parse_obj_expression(input: TokenStream) -> IResult0<Expression> {
    // obj { #hd . body0 | #tl . body1 }
                    // TODO: take a lookahead if this happens to be {
                    // let (input, arg) = parse_expression(input)?;
                    // let (input, branches) = parse_match_expression(input)?;
                    // Ok((input, Expression::object(branches)))
    todo!()

}



// ===Program===
#[derive(Debug)]
pub struct Program {
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
}

#[derive(Debug)]
// This is awkward, but I think I need to put body: Rc<Expression> atleaest...
// These functions don't capture any linear resources,
// so we should be able to call them repeatedly.
pub struct FunctionDefinition {
    name: FunctionName,
    parameters: Vec<VariableName>,
    body: Expression,
}

impl Program {
    pub fn new() -> Self {
        Self {
            function_definitions: HashMap::new(),
            function_definitions_ordering: vec![],
        }
    }

    pub fn update_function_definition(&mut self, fn_def: FunctionDefinition) {
        let fn_name = fn_def.name.clone();
        self.function_definitions.insert(fn_name.clone(), fn_def);

        // This is a bit insane. But function redefinitions don't occur frequently.
        match self.function_definitions_ordering.iter().position(|name| name == &fn_name) {
            Some(fn_index) => {
                self.function_definitions_ordering.remove(fn_index);
            },
            None => {}
        }
        self.function_definitions_ordering.push(fn_name);
    }

    pub fn get_function_definition(&self, function_name: FunctionName) -> Option<&FunctionDefinition> {
        self.function_definitions.get(&function_name)
    }
}

// ===Expressions===
#[derive(Debug, Clone)]
// TODO: Replace Box with Rc
pub struct Expression(pub Box<Expression0>);

// TODO: Get rid of the Clone
// Clone is here only because we have function definitions.
#[derive(Debug, Clone)]
pub enum Expression0 {
    Int(i32),
    OperationApplication1(OperationCode, Expression),
    OperationApplication2(OperationCode, Expression, Expression),
    Call(FunctionName, Vec<Expression>),
    // TODO: Separate constructors into its own type
    Tag_(Tag),
    Tagged(Tag, Expression),
    Tuple(Vec<Expression>),
    Match { arg: Expression, code: StrictCode<Expression> },
    VarMove(VariableName),
    VarClone(VariableName),
    VarDrop(VariableName, Expression),
    Cut { bindings: Bindings, body: Expression },
    // You can always replace LetMove with Cut, but it can be incredebly tedious.
    LetMove { bindings: Bindings, body: Expression },
    Object { code: StrictCode<Expression> },
    Send(Expression, Expression),
}

#[derive(Debug, Clone)]
pub struct Cons(Box<Cons0>);
#[derive(Debug, Clone)]
enum Cons0 {
    Tag(Tag),
    Tagged(Tag, Expression),
    Tuple(Vec<Expression>),
}


// TODO: Replace Box with Rc
#[derive(Debug, Clone)]
pub struct Msg(Box<Msg0>);
// TODO: Get rid of the Clone
#[derive(Debug, Clone)]
pub enum Msg0 {
    Tag_(Tag),
    Tagged(Tag, Msg),
    Tuple(Vec<Expression>),
    Expression(Expression),
}

impl Msg {
    fn tag_(tag: Tag) -> Self { Self(Box::new(Msg0::Tag_(tag))) }
    fn tagged(tag: Tag, msg: Msg) -> Self { Self(Box::new(Msg0::Tagged(tag, msg))) }
    fn tuple(tuple: Vec<Expression>) -> Self { Self(Box::new(Msg0::Tuple(tuple))) }
    fn expression(expr: Expression) -> Self { Self(Box::new(Msg0::Expression(expr))) }
}

// TODO:
// impl PatternMatchableValue for Msg {
//     fn to_shape(self) -> ValueShape<Self>  {
//         use Msg0::*;
//         match *self.0 {
//             Tagged(tag, value) => ValueShape::tagged(tag, value.to_shape()),
//             Tuple(expressions) => ValueShape::tuple(expressions.into_iter().map(|expr| ValueShape::value(Msg::expression(expr))).collect()),
//             Tag_(tag) => ValueShape::tag(tag),
//             Expression(expression) => ValueShape::value(Msg::expression(expression)),
//         }
//     }
//     fn from_shape(value_shape: ValueShape<Self>) -> Self  {
//         match *value_shape.0 {
//             ValueShape0::Value(msg) => msg,
//             // TODO: This should be very interesting... think it through... for tuples... there's
//             // something very strange... we don't have messages in tuples...
//             ValueShape0::Tuple(value_shapes) => Msg::tuple(value_shapes.into_iter().map(|value_shape| PatternMatchableValue::from_shape(value_shape)).collect()),
//             ValueShape0::Tagged(tag, value_shape) => Msg::tagged(tag, PatternMatchableValue::from_shape(value_shape)),
//             ValueShape0::Tag(tag) => Msg::tag_(tag),
//         }
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub enum OperationCode {
    Add,
    Sub,
    Mul,
    Eq,
    Clone,
    Discard,
}

#[derive(Debug, Clone)]
pub struct Bindings(Box<Bindings0>);

#[derive(Debug, Clone)]
pub enum Bindings0 {
    Empty,
    Push { pattern: Pattern, expr: Expression, parent: Bindings },
}

impl Bindings {
    fn unzip(&self) -> (Vec<Expression>, Vec<Pattern>) {
        let mut bindings = self;
        use Bindings0::*;
        let mut expressions = vec![];
        let mut patterns = vec![];
        while let Push { pattern, expr, parent } = &(*bindings.0) {
            expressions.push(expr.clone());
            patterns.push(pattern.clone());
            bindings = parent;
        }
        (expressions, patterns)
    }
}

impl Expression {
    fn int(x: i32) -> Self { Self(Box::new(Expression0::Int(x))) }
    fn operation_application1(op_code: OperationCode, e0: Self) -> Self { Self(Box::new(Expression0::OperationApplication1(op_code, e0))) }
    fn operation_application2(op_code: OperationCode, e0: Self, e1: Self) -> Self { Self(Box::new(Expression0::OperationApplication2(op_code, e0, e1))) }
    fn call(fn_name: FunctionName, args: Vec<Self>) -> Self { Self(Box::new(Expression0::Call(fn_name, args))) }
    fn tag(tag: Tag) -> Self { Self(Box::new(Expression0::Tag_(tag))) }
    fn tagged(tag: Tag, e: Expression) -> Self { Self(Box::new(Expression0::Tagged(tag, e))) }
    fn tuple(args: Vec<Expression>) -> Self { Self(Box::new(Expression0::Tuple(args))) }
    fn match_(arg: Self, code: StrictCode<Expression>) -> Self { Self(Box::new(Expression0::Match { arg, code })) }
    fn var_move(var: VariableName) -> Self { Self(Box::new(Expression0::VarMove(var))) }
    fn var_clone(var: VariableName) -> Self { Self(Box::new(Expression0::VarClone(var))) }
    fn var_drop(var: VariableName, expr: Expression) -> Self { Self(Box::new(Expression0::VarDrop(var, expr))) }
    fn let_move(bindings: Bindings, body: Self) -> Self { Self(Box::new(Expression0::LetMove { bindings, body })) }
    fn cut(bindings: Bindings, body: Self) -> Self { Self(Box::new(Expression0::Cut { bindings, body })) }
    fn object(code: StrictCode<Expression>) -> Self { Self(Box::new(Expression0::Object { code })) }
    fn send(obj: Self, e: Expression) -> Self { Self(Box::new(Expression0::Send(obj, e))) }
}

// ===Values===
#[derive(Debug)]
pub enum Value {
    Int(i32),
    Tag(Tag),
    Tagged(Tag, Box<Value>),
    Tuple(Vec<Value>), // Would be cool if we could use Box<[Value]>, since we don't need to resize
    Closure { captured_env: Env, code: StrictCode<Expression> },
}

impl Value {
    fn discard(self) -> () {
        use Value::*;
        match self {
            Int(_x) => (),
            Tag(_tag) => (),
            Tagged(_tag, val) => (*val).discard(),
            Tuple(values) => {
                for val in values {
                    let _ = val.discard();
                }
            },
            Closure { .. } => todo!(), // this should crash
        }
    }

    fn duplicate(self) -> (Self, Self) {
        use Value::*;
        match self {
            Int(x) => (Int(x), Int(x)),
            Tag(tag) => (Tag(tag.clone()), Tag(tag)),
            Tagged(tag, val) => {
                let (val0, val1) = (*val).duplicate();
                (Tagged(tag.clone(), Box::new(val0)), Tagged(tag, Box::new(val1)))
            },
            Tuple(values) => {
                let mut values0 = Vec::with_capacity(values.len());
                let mut values1 = Vec::with_capacity(values.len());
                for val in values {
                    let (val0, val1) = val.duplicate();
                    values0.push(val0);
                    values1.push(val1);
                }
                (Tuple(values0), Tuple(values1))
            },
            Closure { .. } => todo!(), // this should crash
        }
    }
}

impl PatternMatchableValue for Value {
    fn to_shape(self) -> ValueShape<Self>  {
        use Value::*;
        match self {
            Tagged(tag, value) => ValueShape::tagged(tag, value.to_shape()),
            Tuple(values) => ValueShape::tuple(values.into_iter().map(|val| val.to_shape()).collect()),
            Int(x) => ValueShape::value(Int(x)),
            Tag(tag) => ValueShape::tag(tag),
            Closure { captured_env, code: branches } => ValueShape::value(Closure { captured_env, code: branches }),
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Int(x) => write!(f, "{}", x),
            Tag(tag) => write!(f, "{}", tag),
            Tagged(tag, val) => write!(f, "{} {}", tag, val),
            Tuple(values) => {
                write!(f, "[")?;
                let mut values = (&**values).iter().peekable();
                while let Some(val) = values.next() {
                    match values.peek() {
                        Some(_) => write!(f, "{}, ", val)?,
                        None => write!(f, "{}", val)?,
                    }
                }
                write!(f, "]")
            },
            Closure { captured_env, ..  } => write!(f, "obj {{ {}@code }}", captured_env),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Exp")
    }
}

// ===Environment===
#[derive(Debug)]
pub struct Env(Box<Env0>);

#[derive(Debug)]
pub enum Env0 {
    Empty,
    Push { var: VariableName, value: Value, parent: Env },
}

impl Env {
    fn new() -> Self {
        Self(Box::new(Env0::Empty))
    }

    fn is_empty(&self) -> bool {
        use Env0::*;
        match *self.0 {
            Empty => true,
            _ => false,
        }
    }

    // Very different from the cartesian case.
    fn move_(self, var_name: &VariableName) -> Result<(Value, Self), Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name.clone())),
            Push { var, value, parent } => {
                if var == *var_name {
                    Ok((value, parent))
                } else {
                    let (value0, parent) = parent.move_(var_name)?;
                    Ok((value0, Env(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }
        
    fn clone_(self, var_name: &VariableName) -> Result<(Value, Self), Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name.clone())),
            Push { var, value, parent } => {
                if var == *var_name {
                    let (value0, value1) = value.duplicate();
                    Ok((value0, Env(Box::new(Push { var, value: value1, parent }))))
                } else {
                    let (value0, parent) = parent.clone_(var_name)?;
                    Ok((value0, Env(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }

    fn drop_(self, var_name: &VariableName) -> Result<Self, Error> {
        use Env0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name.clone())),
            Push { var, value, parent } => {
                if var == *var_name {
                    Ok(parent)
                } else {
                    let parent = parent.drop_(var_name)?;
                    Ok(Env(Box::new(Push { var, value, parent })))
                }
            },
        }
    }

    // No difference from cartesian case.
    fn extend(self, var: VariableName, value: Value) -> Self {
        Self(Box::new(Env0::Push { var, value, parent: self }))
    }

    // No difference from cartesian case.
    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, Value)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }

    fn extend_from_pattern_branch_env(self, env_pb: value::Env<Value>) -> Self {
        self.extend_many(env_pb.bindings.into_iter())
    }

    fn join(mut env0: Self, mut env1: Self) -> Self {
        while let Env0::Push { var, value, parent } = *env1.0 {
            env1 = parent;
            env0 = env0.extend(var, value)
        }
        env0
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Env0::*;
        match &(*self.0) {
            Empty => write!(f, ""),
            Push {  var, value, parent } => write!(f, "{} = {} . {}", var.0, value, parent),
        }
    }
}


// ===Error===
#[derive(Debug)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    FunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    VariableLookupFailure(VariableName),
    PatternMatch(pattern::Error<Value>),
    UnconsumedResources(Env),
    AttemptToSendMessageToNonObject(Value),
}

// ===Evaluation===
pub fn eval_start(program: &Program, e: Expression) -> Result<Value, Error> {
    eval_consumming(program, Env::new(), &e)
}

fn eval_consumming(program: &Program, env: Env, e: &Expression) -> Result<Value, Error> {
    let (env, value) = eval(program, env, e)?;
    if env.is_empty() {
        Ok(value)
    } else {
        Err(Error::UnconsumedResources(env))
    }
}

// Both the environment and the expression should be completely consumed to construct the final value.
fn eval(program: &Program, env: Env, e: &Expression) -> Result<(Env, Value), Error> {
    use Expression0::*;
    match &(*e.0) {
        Int(x) => Ok((env, Value::Int(*x))),
        OperationApplication1(code, e0) => {
            let (env, val0) = eval(program, env, e0)?;
            use OperationCode::*;
            Ok((
                env,
                match code {
                    Clone => {
                        let (v0, v1) = val0.duplicate();
                        Value::Tuple(vec![v0, v1])
                    },
                    Discard => {
                        let () = val0.discard();
                        Value::Tuple(vec![])
                    },
                    _ => todo!(), // This should crash
                }
            ))
        },
        OperationApplication2(code, e0, e1) => {
            let (env, val0) = eval(program, env, e0)?;
            let (env, val1) = eval(program, env, e1)?;
            match (val0, val1) {
                (Value::Int(x0), Value::Int(x1)) => {
                    use OperationCode::*;
                    Ok((
                        env,
                        match code {
                            Add => Value::Int(x0 + x1),
                            Sub => Value::Int(x0 - x1),
                            Mul => Value::Int(x0 * x1),
                            Eq => if x0 == x1 {
                                Value::Tag(Tag::new("T".to_string()))
                            } else {
                                Value::Tag(Tag::new("F".to_string()))
                            },
                            Discard | Clone => todo!()
                        }
                    ))
                },
                _ => todo!(),
            }
        },
        Call(fn_name, args) => {
            let mut values: Vec<Value> = Vec::with_capacity(args.len());
            let mut env = env;
            for arg in args {
                let (env0, val) = eval(program, env, arg)?;
                env = env0;
                values.push(val);
            }

            let Some(fn_def) = program.get_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name.clone())) };
            let val = apply_function(program, fn_def, values)?;
            Ok((env, val))
        }
        Tag_(tag) => {
            Ok((env, Value::Tag(tag.clone())))
        },
        Tagged(tag, e) => {
            let (env, val) = eval(program, env, e)?;
            Ok((env, Value::Tagged(tag.clone(), Box::new(val))))
        },
        Tuple(args) => {
            let mut env = env;
            let mut values: Vec<Value> = Vec::with_capacity(args.len());
            for arg in args {
                let (env0, val) = eval(program, env, arg)?;
                env = env0;
                values.push(val);
            }
            Ok((env, Value::Tuple(values)))
        },
        VarMove(var_name) => {
            // I need to take out the binding completely out of the environment
            let (value, env) = env.move_(var_name)?;
            Ok((env, value))
        },
        VarClone(var_name) => {
            // This looks up the var in the environment, then attempts a clone.
            let (value, env) = env.clone_(var_name)?;
            Ok((env, value))
        },
        VarDrop(var_name, expr) => {
            // This looks up the var in the environmeent, then attempts to drop the var.
            let env = env.drop_(var_name)?;
            eval(program, env, expr)
        },
        Match { arg, code } => {
            let (env, arg_value) = eval(program, env, arg)?;
            match code.match_(Some(arg_value.to_shape())) {
                Ok((env_pb, body)) => {
                    let env = env.extend_from_pattern_branch_env(env_pb);
                    eval(program, env, body)
                },
                Err(err) => Err(Error::PatternMatch(err))
            }
        },
        Cut { bindings, body } => {
            let (env, captured_env) = eval_bindings(program, env, bindings)?;
            let val = eval_consumming(program, captured_env, body)?;
            Ok((env, val))
        },
        LetMove { bindings, body } => {
            let (env, captured_env) = eval_bindings(program, env, bindings)?;
            let env = Env::join(env, captured_env);
            eval(program, env, body)
        },
        // Now here is a problem. The object should not capture the whole environement.
        // We should only capture those resources that are needed by the object
        // i.e. we should move them out of the environment, and return the rest of the environment.
        // To do this, use `let` to separate stuff that you need in the current environment.
        //
        // I wonder if this is only a problem in dynamic language. When we have types, the parts of
        // the environment that are captured can be known statically, right? So we wouldn't have to
        // do the explicit `let`? They'd be infered?
        Object { code: branch } => {
            // Note how this takes ownership of the whole environment and returns nothing!
            Ok((Env::new(), Value::Closure { captured_env: env, code: branch.clone() }))
        },
        Send(e0, e1) => {
            let (env, obj) = eval(program, env, e0)?;
            match obj {
                Value::Closure { captured_env, code } => {
                    let (env, msg) = eval(program, env, e1)?;
                    match code.match_(Some(msg.to_shape())) {
                        Ok((env_pb, body)) => {
                            let captured_env = captured_env.extend_from_pattern_branch_env(env_pb);
                            Ok((env, eval_consumming(program, captured_env, body)?))
                        },
                        Err(err) => Err(Error::PatternMatch(err))
                    }
                },
                obj => Err(Error::AttemptToSendMessageToNonObject(obj)),
            }
        }
    }
}

// We return `(Env, Env)`.
// The first component is what remains of `env`, while the second is the result of evaluating `bindings`
//
// Note how in a presence of bindings, this actually has type
//    Env -> (Env, Env)
// which looks like a tyape for duplication, but actually this is the type of split too...
fn eval_bindings(program: &Program, mut env: Env, bindings: &Bindings) -> Result<(Env, Env), Error> {
    let (expressions, patterns) = bindings.unzip();

    let mut value_shapes = Vec::with_capacity(expressions.len());
    for expression in expressions {
        let (env0, value) = eval(program, env, &expression)?;
        env = env0;
        value_shapes.push(value.to_shape());
    }

    match pattern::match_seq(&patterns[..], value_shapes) {
        Ok(env_pb) => {
            let captured_env = Env::new().extend_from_pattern_branch_env(env_pb);
            Ok((env, captured_env))
        },
        Err(err) => Err(Error::PatternMatch(err))
    }
}

fn apply_function(program: &Program, fn_def: &FunctionDefinition, arg_values: Vec<Value>) -> Result<Value, Error> {
    let num_of_arguments: usize = fn_def.parameters.len();
    if num_of_arguments != arg_values.len() {
        return Err(Error::FunctionCallArityMismatch { fn_name: fn_def.name.clone(), expected: num_of_arguments, received: arg_values.len() })
    }
    
    let env = Env::new().extend_many(fn_def.parameters.iter().zip(arg_values).map(|(var, val)| (var.clone(), val)));
    let (env, val) = eval(program, env, &fn_def.body)?;
    if env.is_empty() {
        Ok(val)
    } else {
        Err(Error::UnconsumedResources(env))
    }
}
