use std::fmt;
use std::rc::Rc;
use rpds::Queue; // immutable queue
use crate::tokenizer::{TokenStream, Token, TokenType};
use crate::syntax::{
    anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, vector, delimited_nonempty_vector, delimited_vector,
    parameter_vector, or_vector, comma_vector, binding_vector, parens, brackets, curly_braces, 
};
use crate::identifier::{VariableName, FunctionName, Tag};
use crate::IResult0;
use std::collections::HashMap;
use crate::duality;
use crate::duality::{
    pattern,
    pattern::Pattern,
    matcher::{StrictCode, StrictOrCode, StrictPatternCode},
    sender::{LazyCode, LazyOrCode, LazyPatternCode, SenderResponse},
    value,
    value::{PatternMatchableValue, ValueShape, ValueShape0},
};

// ===parser===
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
        ConstructorSymbol => {
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
        MessageSymbol => {
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
        let name = definition.name().clone();
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
    Ok((input, FunctionDefinition::User( UserFunctionDefinition { name: FunctionName::new(function_name_str), parameters, body })))
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
// ===Parsing Strict Code===

// { inner-strict-code }
fn parse_strict_code(input: TokenStream) -> IResult0<StrictCode<Expression>> {
    let (input, code) = curly_braces(parse_strict_inner_code)(input)?;
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
fn parse_strict_inner_code(input: TokenStream) -> IResult0<StrictCode<Expression>> {
    use Token::*;
    let (input_if_commited, token0) = anytoken(input)?;
    match token0 {
        ConstructorSymbol | OrSeparator  => { // do not commit
            let (input, or_code) = parse_strict_or_code(input)?;
            Ok((input, StrictCode::or(or_code)))
        },
        OpenBracket | Identifier(_) => { // do not commit
            let (input, pattern_branch) = parse_strict_pattern_code(input)?;
            Ok((input, StrictCode::seq(pattern_branch)))
        },
        BindingSeparator => { // commit
            let (input, body) = parse_expression(input_if_commited)?;
            Ok((input, StrictCode::code(body)))
        },
        _ => return Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// tagged_code | tagged_code | tagged_code
fn parse_strict_or_code(input: TokenStream) -> IResult0<StrictOrCode<Expression>> {
    let (input, tagged_codes) = or_vector(parse_strict_tag_code)(input)?;
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
fn parse_strict_tag_code(input: TokenStream) -> IResult0<(Tag, StrictCode<Expression>)> {
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
            let (input, pattern_code) = parse_strict_pattern_code(input)?;
            Ok((input, (tag, StrictCode::seq(pattern_code))))
        }
        ConstructorSymbol => { // do not commit
            // #tag #tag1 . body
            let (input, tagged_branch) = parse_strict_tag_code(input)?;
            let or_code = StrictOrCode::new(vec![tagged_branch]);
            Ok((input, (tag, StrictCode::or(or_code))))
        },
        OpenBracket => { // do not commit
            // #tag [x, y, z] . body
            let (input, pattern_code) = parse_strict_pattern_code(input)?;
            Ok((input, (tag, StrictCode::seq(pattern_code))))
        },
        OpenCurly => { // commit
            // #tag { #tag00 . body 
            //      | #tag01 . body
            //      }
            let (input, or_code) = parse_strict_or_code(input_if_commited)?;
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
fn parse_strict_pattern_code(input: TokenStream) -> IResult0<StrictPatternCode<Expression>> {
    let (input, pattern) = parse_pattern(input)?;
    let (input, _) = token(TokenType::BindingSeparator)(input)?;
    let (input, body) = parse_expression(input)?;
    Ok((input, StrictPatternCode::new(pattern, body)))
}

// ===Parsing Lazy Code===
// { inner-lazy-code }
fn parse_lazy_code(input: TokenStream) -> IResult0<LazyCode<Expression>> {
    let (input, code) = curly_braces(parse_lazy_inner_code)(input)?;
    Ok((input, code))
}

// =primitive code=
// . body
//
// =or-code=
// @tag0 . body
// @tag0 @tag1 . body
// @tag0 @tag1 @tag2 . body
// @tag0 { @tag00 . body 
//       | @tag01 . body
//       }
// @tag [x, y, z] . body
// @tag0 . bod0 | @tag1 x . body1 | @tag2 . body2
//
// =pattern-code=
// [x, y, z] . body
// x . body
fn parse_lazy_inner_code(input: TokenStream) -> IResult0<LazyCode<Expression>> {

    use Token::*;
    let (input_if_commited, token0) = anytoken(input)?;
    match token0 {
        // TODO: There is an ambiguity. You have to require each constructor start symbol be prefixed with `{`
        ConstructorSymbol => {
            todo!("crash for now! We don't yet allow constructor #tags in lazy code.")
        },
        BindingSeparator => { // commit
            // @tag . body
            let (input, body) = parse_expression(input_if_commited)?;
            Ok((input, LazyCode::code(body)))
        },
        OpenBracket | Identifier(_) => { // do not commit
            // TODO: x . 123 }
            // @tag x . body
            // @tag [x, y, z] . body
            let (input, pattern_branch) = parse_lazy_pattern_code(input)?;
            Ok((input, LazyCode::seq(pattern_branch)))
        },
        MessageSymbol | OrSeparator => { // do not commit
            let (input, or_code) = parse_lazy_or_code(input)?;
            Ok((input, LazyCode::or(or_code)))
        },
        CloseCurly => { // do not commit
            // this is empty object
            Ok((input, LazyCode::empty_or()))
        },
        _ => return Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// Can't have or's such as `@tag0 . bod0 | @tag1 x . body1 | @tag2 . body2`
//
// =primitive code=
// . body
//
// =or-code=
// @tag0 . body
// @tag0 @tag1 . body
// @tag0 @tag1 @tag2 . body
// @tag0 { @tag00 . body 
//       | @tag01 . body
//       }
// @tag [x, y, z] . body
// { @tag0 . body | @tag1 . body }
//
// =pattern-code=
// [x, y, z] . body
// x . body
// TODO: Better name...
fn parse_lazy_postfix_code(input: TokenStream) -> IResult0<LazyCode<Expression>> {
    use Token::*;
    let (input_if_commited, token0) = anytoken(input)?;
    match token0 {
        // TODO: There is an ambiguity. You have to require each constructor start symbol be prefixed with `{`
        ConstructorSymbol => {
            todo!("crash for now! We don't yet allow constructor #tags in lazy code.")
        },
        BindingSeparator => { // commit
            // @tag . body
            let (input, body) = parse_expression(input_if_commited)?;
            Ok((input, LazyCode::code(body)))
        },
        MessageSymbol => { // do not commit
            // @tag @tag1 . body
            let (input, (tag, code)) = parse_lazy_tagged_code(input)?;
            Ok((input, LazyCode::tagged(tag, code)))
        },
        OpenBracket | Identifier(_) => { // do not commit
            // @tag x . body
            // @tag [x, y, z] . body
            let (input, pattern_branch) = parse_lazy_pattern_code(input)?;
            Ok((input, LazyCode::seq(pattern_branch)))
        },
        OpenCurly => { // commit
            let (input, code) = parse_lazy_inner_code(input_if_commited)?;
            let (input, _) = token(TokenType::CloseCurly)(input)?;
            Ok((input, code))
        },
        _ => return Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// tagged_code | tagged_code | tagged_code
fn parse_lazy_or_code(input: TokenStream) -> IResult0<LazyOrCode<Expression>> {
    let (input, tagged_codes) = or_vector(parse_lazy_tagged_code)(input)?;
    let or_branch = LazyOrCode::new(tagged_codes);
    Ok((input, or_branch))
}

// x lazy-code
// [p, q, r] lazy-code
// [[p0, p1], q, r] lazy-code
fn parse_lazy_pattern_code(input: TokenStream) -> IResult0<LazyPatternCode<Expression>> {
    let (input, pattern) = parse_pattern(input)?;
    let (input, code) = parse_lazy_inner_code(input)?;
    Ok((input, LazyPatternCode::new(pattern, code)))
}

// @tag0 . body
// @tag0 x . body
// @tag0 @tag1 . body
// @tag0 { @tag00 . body 
//       | @tag01 . body
//       }
// @tag [x, y, z] . body
fn parse_lazy_tagged_code(input: TokenStream) -> IResult0<(Tag, LazyCode<Expression>)> {
    let (input, tag) = parse_tag_as_message(input)?;
    let (input, code) = parse_lazy_postfix_code(input)?;
    Ok((input, (tag, code)))
}

// ===Parsing Messages===
// @msg
// expression
fn parse_primitive_msg(input: TokenStream) -> IResult0<PrimitiveMsg> {
    use Token::*;
    use PrimitiveMsg::*;
    let (_, token0) = anytoken(input)?;
    match token0 {
        MessageSymbol => { // do not commit
            let (input, tag) = parse_tag_as_message(input)?;
            Ok((input, SendTag(tag)))
        },
        _ => {
            let (input, e) = parse_expression(input)?;
            Ok((input, Apply(e)))
        },
    }
}

// sequence of messages
fn parse_msg(mut input: TokenStream) -> IResult0<Msg> {
    let mut msg = Msg::identity();
    while let Ok((input0, m)) = parse_primitive_msg(input) {
        msg = msg.enqueue(m);
        input = input0;
    }
    Ok((input, msg))
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
        ConstructorSymbol => {
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
                    let (input, code) = parse_strict_code(input)?;
                    Ok((input, Expression::match_(arg, code)))
                },
                "obj" => {
                    // obj { #hd . body0 | #tl . body1 }
                    let (input, code) = parse_lazy_code(input)?;
                    Ok((input, Expression::object(code)))
                },
                "send" => {
                    // send[%obj]
                    // send[%obj, %msg]
                    let (input, _) = token(TokenType::OpenBracket)(input)?;
                    let (input, obj) = parse_expression(input)?;

                    let (input, token0) = anytoken(input)?;
                    match token0 {
                        Comma => {
                            let (input, msg) = parse_msg(input)?;
                            let (input, _) = token(TokenType::CloseBracket)(input)?;
                            Ok((input, Expression::send(obj, msg)))
                        },
                        CloseBracket => {
                            todo!()
                            //  The following is wrong! It should be a completely new thing like Expression::force(obj) which we don't have yet.
                            //  We probably don't want this anyway.
                            // Ok((input, Expression::send(obj, Msg::identity())))l
                        },
                        _ => todo!()
                    }
                },
                _ => {
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
            }
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// ===Program===
#[derive(Debug)]
pub struct Program {
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
}

#[derive(Debug)]
pub enum FunctionDefinition {
    User(UserFunctionDefinition),
    Primitive(PrimitiveFunctionDefinition),
}

impl FunctionDefinition {
    fn name(&self) -> &FunctionName {
        use FunctionDefinition::*;
        match self {
            User(fn_) => &fn_.name,
            Primitive(fn_) => &fn_.name,
        }
    }
}

#[derive(Debug)]
pub struct UserFunctionDefinition {
    name: FunctionName,
    parameters: Vec<VariableName>,
    body: Expression,
}

pub struct PrimitiveFunctionDefinition {
    name: FunctionName,
    // TODO: Allow multi arity
    number_of_parameters: usize,
    fn_: Rc<dyn Fn(Vec<Value>) -> Value>,
}

impl fmt::Debug for PrimitiveFunctionDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PrimitiveFunctionDefinition")
         .field("name", &self.name)
         .field("number_of_parameters", &self.number_of_parameters)
         .field("fn_", &"...")
         .finish()
    }
}

impl Program {
    pub fn new() -> Self {
        use FunctionDefinition::*;
        let mut function_definitions = HashMap::new();

        let name = FunctionName::new("+".to_string());
        function_definitions.insert(name.clone(),
            Primitive(PrimitiveFunctionDefinition {
                name,
                number_of_parameters: 2,
                fn_: Rc::new(|args: Vec<Value>| -> Value {
                    let mut iter = args.into_iter();
                    let x = iter.next().unwrap();
                    let y = iter.next().unwrap();
                    match (x, y) {
                        (Value::Int(x), Value::Int(y)) => {
                            Value::Int(x + y)
                        }
                        _ => todo!()
                    }
                }),
            }),
        );

        let name = FunctionName::new("-".to_string());
        function_definitions.insert(name.clone(),
            Primitive(PrimitiveFunctionDefinition {
                name,
                number_of_parameters: 2,
                fn_: Rc::new(|args: Vec<Value>| -> Value {
                    let mut iter = args.into_iter();
                    let x = iter.next().unwrap();
                    let y = iter.next().unwrap();
                    match (x, y) {
                        (Value::Int(x), Value::Int(y)) => {
                            Value::Int(x - y)
                        }
                        _ => todo!()
                    }
                }),
            }),
        );

        let name = FunctionName::new("*".to_string());
        function_definitions.insert(name.clone(),
            Primitive(PrimitiveFunctionDefinition {
                name,
                number_of_parameters: 2,
                fn_: Rc::new(|args: Vec<Value>| -> Value {
                    let mut iter = args.into_iter();
                    let x = iter.next().unwrap();
                    let y = iter.next().unwrap();
                    match (x, y) {
                        (Value::Int(x), Value::Int(y)) => {
                            Value::Int(x * y)
                        }
                        _ => todo!()
                    }
                }),
            }),
        );

        let name = FunctionName::new("++".to_string());
        function_definitions.insert(name.clone(),
            Primitive(PrimitiveFunctionDefinition {
                name,
                number_of_parameters: 2,
                fn_: Rc::new(|args: Vec<Value>| -> Value {
                    let mut iter = args.into_iter();
                    let x = iter.next().unwrap();
                    let y = iter.next().unwrap();
                    match (x, y) {
                        (Value::String(mut x), Value::String(y)) => {
                            x.push_str(&y);
                            Value::String(x)
                        }
                        _ => todo!()
                    }
                }),
            }),
        );

        let name = FunctionName::new("==".to_string());
        function_definitions.insert(name.clone(),
            Primitive(PrimitiveFunctionDefinition {
                name,
                number_of_parameters: 2,
                fn_: Rc::new(|args: Vec<Value>| -> Value {
                    let mut iter = args.into_iter();
                    let x = iter.next().unwrap();
                    let y = iter.next().unwrap();
                    x.eq_consuming(y)
                }),
            }),
        );

        Self {
            function_definitions,
            function_definitions_ordering: vec![],
        }
    }

    pub fn update_function_definition(&mut self, fn_def: FunctionDefinition) {
        let fn_name = fn_def.name().clone();
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
pub struct Expression(pub Rc<Expression0>);

#[derive(Debug)]
pub enum Expression0 {
    Lit(Literal),

    Call(FunctionName, Vec<Expression>),

    VarMove(VariableName),
    VarClone(VariableName),
    VarDrop(VariableName, Expression),
    Cut { bindings: Bindings, body: Expression },
    // You can always replace LetMove with Cut, but it can be incredebly tedious.
    LetMove { bindings: Bindings, body: Expression },

    // TODO: Separate constructors into its own type
    Tag(Tag),
    Tagged(Tag, Expression),
    Tuple(Vec<Expression>),
    Match { arg: Expression, code: StrictCode<Expression> },

    Object { code: LazyCode<Expression> },
    Send(Expression, Msg),
}

#[derive(Debug)]
enum Literal {
    Int(i32),
    String(String),
    // TODO: How is it possible to absorb a file into program?
    //       What happens during crash? The captured resources should not
    //       be able to go away somehow... they should persist on disk.
    // TODO: floats, strings, files
}

// TODO: Separate constructors.
// #[derive(Debug, Clone)]
// pub struct Cons(Box<Cons0>);
// #[derive(Debug, Clone)]
// enum Cons0 {
//     Tag(Tag),
//     Tagged(Tag, Expression),
//     Tuple(Vec<Expression>),
// }

#[derive(Debug, Clone)]
enum PrimitiveMsg {
    SendTag(Tag),
    Apply(Expression),
}

// TODO: Is it really necessary to have a queue? It makes sense from the p.o.v of composition of
//       messages, but messages right now are not first-class.
//       Once messages become first-class, then yes, the queue will definitely make sense.
#[derive(Debug, Clone)]
pub struct Msg(Rc<Queue<PrimitiveMsg>>);

impl Msg {
    fn identity() -> Self {
        Self(Rc::new(Queue::new()))
    }
    fn tag(self, tag: Tag) -> Self {
        self.enqueue(PrimitiveMsg::SendTag(tag))
    }
    fn expression(self, e: Expression) -> Self {
        self.enqueue(PrimitiveMsg::Apply(e))
    }
    fn enqueue(self, m: PrimitiveMsg) -> Self {
        let queue = self.0;
        Self(Rc::new(queue.enqueue(m)))
    }
    fn split(&self) -> Option<(&PrimitiveMsg, Msg)> {
        let m = self.0.peek()?;
        let msg = Msg(Rc::new(self.0.dequeue().unwrap()));
        Some((m, msg))
    }
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
    fn int(x: i32) -> Self { Self(Rc::new(Expression0::Lit(Literal::Int(x)))) }
    fn call(fn_name: FunctionName, args: Vec<Self>) -> Self { Self(Rc::new(Expression0::Call(fn_name, args))) }
    fn tag(tag: Tag) -> Self { Self(Rc::new(Expression0::Tag(tag))) }
    fn tagged(tag: Tag, e: Expression) -> Self { Self(Rc::new(Expression0::Tagged(tag, e))) }
    fn tuple(args: Vec<Expression>) -> Self { Self(Rc::new(Expression0::Tuple(args))) }
    fn match_(arg: Self, code: StrictCode<Expression>) -> Self { Self(Rc::new(Expression0::Match { arg, code })) }
    fn var_move(var: VariableName) -> Self { Self(Rc::new(Expression0::VarMove(var))) }
    fn var_clone(var: VariableName) -> Self { Self(Rc::new(Expression0::VarClone(var))) }
    fn var_drop(var: VariableName, expr: Expression) -> Self { Self(Rc::new(Expression0::VarDrop(var, expr))) }
    fn let_move(bindings: Bindings, body: Self) -> Self { Self(Rc::new(Expression0::LetMove { bindings, body })) }
    fn cut(bindings: Bindings, body: Self) -> Self { Self(Rc::new(Expression0::Cut { bindings, body })) }
    fn object(code: LazyCode<Expression>) -> Self { Self(Rc::new(Expression0::Object { code })) }
    fn send(obj: Self, msg: Msg) -> Self { Self(Rc::new(Expression0::Send(obj, msg))) }
}

// ===Values===
#[derive(Debug)]
pub enum Value {
    Int(i32),
    String(String),

    Tag(Tag),
    Tagged(Tag, Box<Value>),
    Tuple(Vec<Value>), // Would be cool if we could use Box<[Value]>, since we don't need to resize

    Closure(Closure),
}

// TODO:
#[derive(Debug)]
pub enum PrimitiveValue {
    Int(i32),
}

#[derive(Debug)]
pub struct Closure {
    env: Env,
    code: LazyCode<Expression>,
}

impl Value {
    fn discard(self) -> () {
        use Value::*;
        match self {
            Int(_x) => (),
            String(_s) => panic!(), // TODO: Is it reasonable to discard a whole String? Is that a
                               // discardable thing? It kinda is... but I'm not sure it should be
                               // implemented here... It would be more appropriate to have the
                               // discard as a primitive operation special to the string...
            Tag(_tag) => (),
            Tagged(_tag, val) => (*val).discard(),
            Tuple(values) => {
                for val in values {
                    let _ = val.discard();
                }
            },
            Closure { .. } => panic!(), // this should crash
        }
    }

    fn duplicate(self) -> (Self, Self) {
        use Value::*;
        match self {
            Int(x) => (Int(x), Int(x)),
            Tag(tag) => (Tag(tag.clone()), Tag(tag)),
            String(_s) => panic!(), // TODO: I think this should be exposed as a primitive
                                    // operation.
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

    fn eq_consuming(self, y: Self) -> Self {
        fn eq(x0: Value, x1: Value) -> bool {
            match (x0, x1) {
                (Value::Int(x0), Value::Int(x1)) => x0 == x1, 
                (Value::Tag(tag0), Value::Tag(tag1)) => tag0 == tag1,
                (Value::Tagged(tag0, v0), Value::Tagged(tag1, v1)) => tag0 == tag1 && eq(*v0, *v1),
                (Value::Tuple(vs0), Value::Tuple(vs1)) => {
                    for (v0, v1) in vs0.into_iter().zip(vs1) {
                        if !eq(v0, v1) { return false }
                    }
                    return true
                },
                _ => panic!(),
            }
        }
        if eq(self, y) {
            Value::Tag(Tag::new("T".to_string()))
        } else {
            Value::Tag(Tag::new("F".to_string()))
        }
    }

    fn force_closure(self) -> Result<Closure, Error> {
        match self {
            Value::Closure(closure) => {
                Ok(closure)
            },
            value => Err(Error::AttemptToSendMessageToNonObject(value)),
        }
    }
}

impl PatternMatchableValue for Value {
    fn to_shape(self) -> ValueShape<Self>  {
        use Value::*;
        match self {
            Tag(tag) => ValueShape::tag(tag),
            Tagged(tag, value) => ValueShape::tagged(tag, value.to_shape()),
            Tuple(values) => ValueShape::tuple(values.into_iter().map(|val| val.to_shape()).collect()),
            value => ValueShape::value(value),
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
        String(s) => write!(f, "\"{}\"", s),
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
        Closure(closure) => {
            write!(f, "obj {{ {} . ", closure.env)?;
            display_closure_code(f, &closure.code)?;
            write!(f, " }}",)?;
            Ok(())
        },
    }
}
}

fn display_closure_code(f: &mut fmt::Formatter, code: &LazyCode<Expression>) -> fmt::Result {
use LazyCode::*;
match code {
    Code(_code) => {
        write!(f, "...")
    },
    Or(or_code) => {
        let xs = &(*or_code.0).0;
        match xs.len() {
            0 => write!(f, ""),
            1 => write!(f, "@{} . ..", xs[0].0.0),
            _ => {
                write!(f, "@{}", xs[0].0.0)?;
                for (tag, _) in &xs[1..] {
                    write!(f, " | @{}", tag.0)?
                }
                write!(f, " . ...")
            }
        }
    },
    Pattern(pattern_code) => {
        let pattern = &(*pattern_code.0).pattern;
        write!(f, "{}", pattern)?;
        write!(f, " . ...")?;
        Ok(())
    },
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

    fn extend_from_pattern_branch_env(self, env_pm: value::Env<Value>) -> Self {
        self.extend_many(env_pm.bindings.into_iter())
    }

    fn join(mut env0: Self, mut env1: Self) -> Self {
        while let Env0::Push { var, value, parent } = *env1.0 {
            env1 = parent;
            env0 = env0.extend(var, value)
        }
        env0
    }

    fn consume(self) -> Result<(), Error> {
        if self.is_empty() {
            Ok(())
        } else {
            Err(Error::UnconsumedResources(self))
        }
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn fmt_loop(env: &Env, f: &mut fmt::Formatter) -> fmt::Result {
            use Env0::*;
            match &(*env.0) {
                Empty => write!(f, ""),
                Push {  var, value, parent } => {
                    if matches!(&(*parent.0), Empty) {
                        write!(f, "{} = {}", var.0, value)
                    } else {
                        write!(f, "{}, {} = {}", parent, var.0, value)
                    }
                },
            }
        }
        write!(f, "[")?;
        fmt_loop(self, f)?;
        write!(f, "]")?;
        Ok(())
    }
}


// ===Error===
#[derive(Debug)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    FunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    VariableLookupFailure(VariableName),
    PatternMatch(duality::error::Error<Value>),
    UnconsumedResources(Env),
    AttemptToSendMessageToNonObject(Value),
}

// ===Evaluation===
pub fn eval_start(program: &Program, e: Expression) -> Result<Value, Error> {
    eval_consumming(program, Env::new(), &e)
}

fn eval_consumming(program: &Program, env: Env, e: &Expression) -> Result<Value, Error> {
    let (env, value) = eval(program, env, e)?;
    env.consume()?;
    Ok(value)
}

// Both the environment and the expression should be completely consumed to construct the final value.
fn eval(program: &Program, env: Env, e: &Expression) -> Result<(Env, Value), Error> {
    use Expression0::*;
    match &(*e.0) {
        Lit(lit) => {
            use Literal::*;
            match lit {
                Int(x) => Ok((env, Value::Int(*x))),
                String(s) => Ok((env, Value::String(s.clone()))), // TODO: The clone is weird, right?
                                                          // Maybe not.
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
        Tag(tag) => {
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
                Ok((env_pm, body)) => {
                    let env = env.extend_from_pattern_branch_env(env_pm);
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
        Object { code } => {
            // Note how this takes ownership of the whole environment and returns nothing!
            Ok((Env::new(), Value::Closure(Closure { env, code: code.clone() })))
        },
        Send(e, msg) => {
            let (env, obj) = eval(program, env, e)?;
            send_msg(program, env, obj, msg)
        },
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
        Ok(env_pm) => {
            let captured_env = Env::new().extend_from_pattern_branch_env(env_pm);
            Ok((env, captured_env))
        },
        Err(err) => Err(Error::PatternMatch(err))
    }
}

fn apply_function(program: &Program, fn_def: &FunctionDefinition, arg_values: Vec<Value>) -> Result<Value, Error> {
    use FunctionDefinition::*;
    match fn_def {
        User(fn_def) => {
            let num_of_arguments: usize = fn_def.parameters.len();
            if num_of_arguments != arg_values.len() {
                return Err(Error::FunctionCallArityMismatch { fn_name: fn_def.name.clone(), expected: num_of_arguments, received: arg_values.len() })
            }
            
            let env = Env::new().extend_many(fn_def.parameters.iter().zip(arg_values).map(|(var, val)| (var.clone(), val)));
            let (env, val) = eval(program, env, &fn_def.body)?;
            env.consume()?;
            Ok(val)
        },
        Primitive(fn_def) => {
            let num_of_arguments: usize = fn_def.number_of_parameters;
            if num_of_arguments != arg_values.len() {
                return Err(Error::FunctionCallArityMismatch { fn_name: fn_def.name.clone(), expected: num_of_arguments, received: arg_values.len() })
            }

            let val = (fn_def.fn_)(arg_values);
            Ok(val)
        },
    }
}

// ===Sending messages===
fn make_value_from_code_and_env(program: &Program, env: Env, code: &LazyCode<Expression>) -> Result<Value, Error> {
    use LazyCode::*;
    match code {
        Code(e) => {
            eval_consumming(program, env, &e)
        },
        code => Ok(Value::Closure(Closure { env, code: code.clone() }))
    }
}

fn send_value_to_closure(program: &Program, closure: Closure, arg_value: Value) -> Result<Value, Error> {
    let value_shape = arg_value.to_shape();
    match closure.code.send_value_shape(value_shape) {
        Ok(response) => {
            use SenderResponse::*;
            match response {
                Success(env_pm, code) => {
                    let val = make_value_from_code_and_env(program, closure.env.extend_from_pattern_branch_env(env_pm), code)?;
                    Ok(val)
                },
                StuckSendingTag(env_pm, e, tag) => {
                    let closure_env = closure.env.extend_from_pattern_branch_env(env_pm);
                    let obj = eval_consumming(program, closure_env, e)?;
                    let closure = obj.force_closure()?;
                    let (env, val) = send_tag_to_closure(program, Env::new(), closure, &tag)?;
                    env.consume()?;
                    Ok(val)
                },
                StuckSendingValue(env_pm, e, arg_value) => {
                    let closure_env = closure.env.extend_from_pattern_branch_env(env_pm);
                    let obj = eval_consumming(program, closure_env, e)?;

                    let closure = obj.force_closure()?;
                    let value = send_value_to_closure(program, closure, arg_value)?;
                    Ok(value)
                },
            }
        },
        Err(err) => return Err(Error::PatternMatch(err)),
    }
}

fn send_tag_to_closure(program: &Program, env: Env, closure: Closure, tag: &Tag) -> Result<(Env, Value), Error> {
    match closure.code.send_tag::<Value>(tag.clone()) {
        Ok(response) => {
            use SenderResponse::*;
            match response {
                Success(env_pm, code) => {
                    let val = make_value_from_code_and_env(program, closure.env.extend_from_pattern_branch_env(env_pm), code)?;
                    Ok((env, val))
                },
                StuckSendingTag(env_pm, e, tag) => {
                    let closure_env = closure.env.extend_from_pattern_branch_env(env_pm);
                    let obj = eval_consumming(program, closure_env, e)?;
                    send_primitive_msg(program, env, obj, &PrimitiveMsg::SendTag(tag))
                },
                StuckSendingValue(env_pm, e, arg_value) => {
                    let closure_env = closure.env.extend_from_pattern_branch_env(env_pm);
                    let obj = eval_consumming(program, closure_env, e)?;
                    let closure = obj.force_closure()?;
                    let value = send_value_to_closure(program, closure, arg_value)?;
                    Ok((env, value))
                },
            }
        },
        Err(err) => return Err(Error::PatternMatch(err)),
    }
}

fn send_primitive_msg_to_closure(program: &Program, env: Env, closure: Closure, m: &PrimitiveMsg) -> Result<(Env, Value), Error> {
    use PrimitiveMsg::*;
    match m {
        SendTag(tag) => {
            send_tag_to_closure(program, env, closure, tag)
        },
        Apply(expr) => {
            let (env, arg_value) = eval(program, env, expr)?;
            let value = send_value_to_closure(program, closure, arg_value)?;
            Ok((env, value))
        },
    }
}

fn send_primitive_msg(program: &Program, env: Env, obj: Value, m: &PrimitiveMsg) -> Result<(Env, Value), Error> {
    let closure = obj.force_closure()?;
    send_primitive_msg_to_closure(program, env, closure, m)
}

fn send_msg(program: &Program, mut env: Env, mut value: Value, msg: &Msg) -> Result<(Env, Value), Error> {
    for m in msg.0.into_iter() {
        (env, value) = send_primitive_msg(program, env, value, m)?;
    }
    Ok((env, value))
}
