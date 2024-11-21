use std::rc::Rc;
use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType};
use crate::syntax::{
    anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, vector, delimited_nonempty_vector, delimited_vector,
    variable_name, parameter_vector, or_vector, comma_vector, binding_vector, parens, brackets, curly_braces, possibly_empty_relaxed_paren_vector, possibly_empty_relaxed_bracket_vector,
};
use crate::identifier::{VariableName, FunctionName, Tag};
use crate::IResult0;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    Cartesian,
    Linear,
}

// ===parser===
fn identifier_to_operation_code(str: &str) -> Option<OperationCode> {
    match str {
        "+" => Some(OperationCode::Add),
        "*" => Some(OperationCode::Mul),
        "sub" => Some(OperationCode::Sub),
        "==" => Some(OperationCode::Eq),
        "dup" => Some(OperationCode::Duplicate),
        "discard" => Some(OperationCode::Discard),
        _ => None
    }
}

fn parse_mode_keyword(input: TokenStream) -> IResult0<Mode> {
    let (input, mode_str) = anyidentifier(input)?;
    match &mode_str[..] {
        "cart" => Ok((input, Mode::Cartesian)),
        "lin" => Ok((input, Mode::Linear)),
        _ => return Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt }))
    }
}

fn parse_cartesian_operator_arguments(op_code: OperationCode, input: TokenStream) -> IResult0<CartesianExpression> {
    use OperationCode::*;
    match op_code {
        Add => {
            let (input, (e0, e1)) = parse_cartesian_arg_list2(input)?;
            Ok((input, CartesianExpression::operation_application(op_code, e0, e1)))
        },
        Sub => {
            let (input, (e0, e1)) = parse_cartesian_arg_list2(input)?;
            Ok((input, CartesianExpression::operation_application(op_code, e0, e1)))
        },
        Mul => {
            let (input, (e0, e1)) = parse_cartesian_arg_list2(input)?;
            Ok((input, CartesianExpression::operation_application(op_code, e0, e1)))
        },
        Eq => {
            let (input, (e0, e1)) = parse_cartesian_arg_list2(input)?;
            Ok((input, CartesianExpression::operation_application(op_code, e0, e1)))
        },
        Discard | Duplicate => todo!(),
    }
}
fn parse_linear_operator_arguments(op_code: OperationCode, input: TokenStream) -> IResult0<LinearExpression> {
    use OperationCode::*;
    match op_code {
        Add => {
            let (input, (e0, e1)) = parse_linear_arg_list2(input)?;
            Ok((input, LinearExpression::operation_application2(op_code, e0, e1)))
        },
        Sub => {
            let (input, (e0, e1)) = parse_linear_arg_list2(input)?;
            Ok((input, LinearExpression::operation_application2(op_code, e0, e1)))
        },
        Mul => {
            let (input, (e0, e1)) = parse_linear_arg_list2(input)?;
            Ok((input, LinearExpression::operation_application2(op_code, e0, e1)))
        },
        Eq => {
            let (input, (e0, e1)) = parse_linear_arg_list2(input)?;
            Ok((input, LinearExpression::operation_application2(op_code, e0, e1)))
        },
        Duplicate => {
            let (input, e0) = parse_linear_arg_list1(input)?;
            Ok((input, LinearExpression::operation_application1(op_code, e0)))
        },
        Discard => {
            let (input, e0) = parse_linear_arg_list1(input)?;
            Ok((input, LinearExpression::operation_application1(op_code, e0)))
        },
    }
}

fn parse_linear_arg_list1(input: TokenStream) -> IResult0<LinearExpression> {
    // "[e0]  "
    let (input, e0) = brackets(parse_linear_expression)(input)?;
    Ok((input, e0))
}
fn parse_cartesian_arg_list2(input: TokenStream) -> IResult0<(CartesianExpression, CartesianExpression)> {
    // "(e0, e1)  "
    let (input, _) = token(TokenType::OpenParen)(input)?;
    let (input, e0) = parse_cartesian_expression(input)?;
    let (input, _) = token(TokenType::Comma)(input)?;
    let (input, e1) = parse_cartesian_expression(input)?;
    let (input, _) = token(TokenType::CloseParen)(input)?;
    Ok((input, (e0, e1)))
}
fn parse_linear_arg_list2(input: TokenStream) -> IResult0<(LinearExpression, LinearExpression)> {
    // "[e0, e1]  "
    let (input, _) = token(TokenType::OpenBracket)(input)?;
    let (input, e0) = parse_linear_expression(input)?;
    let (input, _) = token(TokenType::Comma)(input)?;
    let (input, e1) = parse_linear_expression(input)?;
    let (input, _) = token(TokenType::CloseBracket)(input)?;
    Ok((input, (e0, e1)))
}

pub fn parse_program(input: TokenStream) -> IResult0<Program> {
    let (input, definitions) = vector(parse_function_definition)(input)?;
    let mut program = Program::new();
    for definition in definitions {
        let name = definition.name().clone();
        use FunctionDefinition::*;
        match definition {
            Cartesian(definition) => {
                program.cartesian_function_definitions.insert(name.clone(), definition);
                program.function_definitions_ordering.push((Mode::Cartesian, name));
            },
            Linear(definition) => {
                program.linear_function_definitions.insert(name.clone(), definition);
                program.function_definitions_ordering.push((Mode::Linear, name));
            },
        }
    }
    Ok((input, program))
}

pub fn parse_function_definition(input: TokenStream) -> IResult0<FunctionDefinition> {
    let (input, _) = identifier("fn")(input)?;
    let (input, mode) = parse_mode_keyword(input)?;
    let (input, function_name_str) = anyidentifier(input)?;
    match mode {
        Mode::Cartesian => {
            let (input, cartesian_parameters) = possibly_empty_relaxed_paren_vector(variable_name)(input)?;
            let (input, body) = curly_braces(parse_cartesian_expression)(input)?;
            Ok((input, FunctionDefinition::Cartesian(CartesianFunctionDefinition { name: FunctionName::new(function_name_str), cartesian_parameters, body })))
        },
        Mode::Linear => {
            let (input, cartesian_parameters) = possibly_empty_relaxed_paren_vector(variable_name)(input)?;
            let (input, linear_parameters) = possibly_empty_relaxed_bracket_vector(variable_name)(input)?;
            let (input, body) = curly_braces(parse_linear_expression)(input)?;
            Ok((input, FunctionDefinition::Linear(LinearFunctionDefinition { name: FunctionName::new(function_name_str), cartesian_parameters, linear_parameters, body })))
        },
    }
}

// x = e0
pub fn parse_cartesian_var_binding(input: TokenStream) -> IResult0<(VariableName, CartesianExpression)> {
    let (input, identifier) = anyidentifier(input)?;
    let (input, _) = token(TokenType::Eq)(input)?;
    let (input, arg) = parse_cartesian_expression(input)?;

    Ok((input, (VariableName::new(identifier), arg)))
}
// a = e0
pub fn parse_linear_var_binding(input: TokenStream) -> IResult0<(VariableName, LinearExpression)> {
    let (input, identifier) = anyidentifier(input)?;
    let (input, _) = token(TokenType::Eq)(input)?;
    let (input, arg) = parse_linear_expression(input)?;

    Ok((input, (VariableName::new(identifier), arg)))
}

// a = e0, b = e1, c = e2
pub fn parse_linear_var_bindings(input: TokenStream) -> IResult0<LinearBindings> {
    let (input, var_expr_pair) = delimited_vector(parse_linear_var_binding, token(TokenType::Comma))(input)?;
    let mut bindings = LinearBindings0::Empty;
    for (var, expr) in var_expr_pair {
        bindings = LinearBindings0::Push { var, expr, parent: LinearBindings(Box::new(bindings)) };
    }
    Ok((input, LinearBindings(Box::new(bindings))))
}

pub fn parse_cartesian_branch(input: TokenStream) -> IResult0<CartesianPatternBranch> {
    let (input, pattern) = parse_pattern(Mode::Cartesian)(input)?;
    let (input, _) = token(TokenType::BindingSeparator)(input)?;
    let (input, body) = parse_cartesian_expression(input)?;

    Ok((input, CartesianPatternBranch { pattern, body }))
}
pub fn parse_linear_branch(input: TokenStream) -> IResult0<LinearPatternBranch> {
    let (input, pattern) = parse_pattern(Mode::Linear)(input)?;
    let (input, _) = token(TokenType::BindingSeparator)(input)?;
    let (input, body) = parse_linear_expression(input)?;

    Ok((input, LinearPatternBranch { pattern, body }))
}

pub fn parse_cartesian_branches(input: TokenStream) -> IResult0<Vec<CartesianPatternBranch>> {
    or_vector(parse_cartesian_branch)(input)
}
pub fn parse_linear_branches(input: TokenStream) -> IResult0<Vec<LinearPatternBranch>> {
    or_vector(parse_linear_branch)(input)
}

fn parse_pattern(mode: Mode) -> impl FnOnce(TokenStream) -> IResult0<Pattern> {
    move |input: TokenStream| {
        let (input, token0) = anytoken(input)?;
        use Token::*;
        match token0 {
            ConstructorSymbol => {
                // Tag pattern
                let (input, var_name) = anyidentifier(input)?;
                let (input, pattern) = parse_pattern(mode)(input)?;
                Ok((input, Pattern::Tagged(Tag::new(var_name), Box::new(pattern))))
            },
            OpenParen if matches!(mode, Mode::Cartesian) => {
                // Tuple pattern
                let (input, patterns) = parse_tuple_pattern_sequence(mode)(input)?;
                let (input, _) = token(TokenType::CloseParen)(input)?;
                Ok((input, Pattern::Tuple(patterns)))
            },
            OpenBracket if matches!(mode, Mode::Linear) => {
                // Tuple pattern
                let (input, patterns) = parse_tuple_pattern_sequence(mode)(input)?;
                let (input, _) = token(TokenType::CloseBracket)(input)?;
                Ok((input, Pattern::Tuple(patterns)))
            },
            Identifier(var_name) => Ok((input, Pattern::Variable(VariableName::new(var_name)))),
            _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
        }
    }
}
fn parse_tuple_pattern_sequence(mode: Mode) -> impl FnOnce(TokenStream) -> IResult0<Vec<TuplePattern>> {
    delimited_vector(move |input: TokenStream| parse_tuple_pattern(mode)(input), token(TokenType::Comma))
}
fn parse_tuple_pattern(mode: Mode) -> impl FnOnce(TokenStream) -> IResult0<TuplePattern> {
    move |input: TokenStream| {
        let (input, token0) = anytoken(input)?;
        use Token::*;
        match token0 {
            OpenParen if matches!(mode, Mode::Cartesian) => {
                let (input, patterns) = parse_tuple_pattern_sequence(mode)(input)?;
                let (input, _) = token(TokenType::CloseParen)(input)?;
                Ok((input, TuplePattern::Tuple(patterns)))
            },
            OpenBracket if matches!(mode, Mode::Linear) => {
                let (input, patterns) = parse_tuple_pattern_sequence(mode)(input)?;
                let (input, _) = token(TokenType::CloseBracket)(input)?;
                Ok((input, TuplePattern::Tuple(patterns)))
            },
            Identifier(var_name) => Ok((input, TuplePattern::Variable(VariableName::new(var_name)))),
            _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
        }
    }
}

pub fn parse_expression(input: TokenStream) -> IResult0<Expression> {
    let (input, mode) = parse_mode_keyword(input)?;
    match mode {
        Mode::Cartesian => {
            let (input, expr) = parse_cartesian_expression(input)?;
            Ok((input, Expression::Cartesian(expr)))
        },
        Mode::Linear => {
            let (input, expr) = parse_linear_expression(input)?;
            Ok((input, Expression::Linear(expr)))
        }
    }
}

pub fn parse_cartesian_expression(input: TokenStream) -> IResult0<CartesianExpression> {
    let (input, token0) = anytoken(input)?;
    use Token::*;
    match token0 {
        Int(x) => Ok((input, CartesianExpression::int(x))),
        VarLookupSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, CartesianExpression::var_lookup(VariableName::new(var_name))))
        },
        ConstructorSymbol => {
            let (input, tag) = anyidentifier(input)?;
            let (input, arg) = parse_cartesian_expression(input)?;
            Ok((input, CartesianExpression::tagged(Tag::new(tag), arg)))
        },
        OpenParen => {
            // tuple
            let (input, args) = comma_vector(parse_cartesian_expression)(input)?;
            let (input, _) = token(TokenType::CloseParen)(input)?;
            Ok((input, CartesianExpression::tuple(args)))
        },
        OpenBracket => {
            todo!("Unexpected `[` in cartesian mode.")
        },
        Identifier(identifier) => {
            match &identifier[..] {
                "let" => {
                    // let { x = 5 . body }
                    // let { (a, b) = (1, 2) . body } 
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    // Check if this is a tuple pattern or identifier.
                    let (input_if_commited, token0) = anytoken(input)?;
                    match token0 {
                        Token::Identifier(identifier) => { // we commit the identifier
                            let var = VariableName::new(identifier);
                            let (input, _) = token(TokenType::Eq)(input_if_commited)?;
                            let (input, arg) = parse_cartesian_expression(input)?;

                            let (input, _) = token(TokenType::BindingSeparator)(input)?;

                            let (input, body) = parse_cartesian_expression(input)?;
                            let (input, _) = token(TokenType::CloseCurly)(input)?;

                            Ok((input, CartesianExpression::let_(arg, var, body)))
                        },
                        Token::OpenParen => { // we don't commit the paren `(`
                            let (input, tuple_pattern) = parse_pattern(Mode::Cartesian)(input)?;
                            let (input, _) = token(TokenType::Eq)(input)?;
                            let (input, arg) = parse_cartesian_expression(input)?;
                            let (input, _) = token(TokenType::BindingSeparator)(input)?;

                            let (input, body) = parse_cartesian_expression(input)?;
                            let (input, _) = token(TokenType::CloseCurly)(input)?;
                            // match arg { tuple_pattern . body }
                            Ok((input, CartesianExpression::match_(arg, vec![CartesianPatternBranch { pattern: tuple_pattern, body }])))
                        },
                        _ => {
                            // TODO: This is an error
                            todo!()
                        }
                    }
                },
                "match" => {
                    let (input, arg) = parse_cartesian_expression(input)?;
                    let (input, branches) = curly_braces(parse_cartesian_branches)(input)?;

                    Ok((input, CartesianExpression::match_(arg, branches)))
                },
                "obj" => {
                    // obj { #hd () . e0 | #tl () . e1 }
                    let (input, branches) = curly_braces(parse_cartesian_branches)(input)?;
                    Ok((input, CartesianExpression::object(branches)))
                },
                "send" => {
                    // send($obj, $msg)
                    let (input, (e0, e1)) = parse_cartesian_arg_list2(input)?;
                    Ok((input, CartesianExpression::send(e0, e1)))
                },
                "thunk" => {
                    // thunk[e]
                    let (input, e) = brackets(parse_linear_expression)(input)?;
                    Ok((input, CartesianExpression::thunk(e)))
                },
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => parse_cartesian_operator_arguments(op_code, input),
                    None => {
                        // Here we have a function call
                        let (input, arguments) = possibly_empty_relaxed_paren_vector(parse_cartesian_expression)(input)?;
                        Ok((input, CartesianExpression::call(FunctionName::new(identifier), arguments)))
                    },
                },
            }
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

pub fn parse_linear_expression(input: TokenStream) -> IResult0<LinearExpression> {
    let (input, token0) = anytoken(input)?;
    use Token::*;
    match token0 {
        Int(x) => Ok((input, LinearExpression::int(x))),
        VarMoveSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, LinearExpression::var_move(VariableName::new(var_name))))
        },
        VarCloneSymbol => {
            // clone x
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, LinearExpression::var_clone(VariableName::new(var_name))))
        },
        VarDropSymbol => {
            // drop x . expr
            // Note that `drop x` by itself doesn't make any sense. You have to atlesat do `drop x . []`
            let (input, var_name) = anyidentifier(input)?;
            let (input, _) = token(TokenType::BindingSeparator)(input)?;
            let (input, expr) = parse_linear_expression(input)?;
            Ok((input, LinearExpression::var_drop(VariableName::new(var_name), expr)))
        },
        VarLookupSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, LinearExpression::var_lookup(VariableName::new(var_name))))
        },
        ConstructorSymbol => {
            let (input, tag) = anyidentifier(input)?;
            let (input, arg) = parse_linear_expression(input)?;
            Ok((input, LinearExpression::tagged(Tag::new(tag), arg)))
        },
        OpenBracket => {
            // tuple
            let (input, args) = comma_vector(parse_linear_expression)(input)?;
            let (input, _) = token(TokenType::CloseBracket)(input)?;
            Ok((input, LinearExpression::tuple(args)))
        },
        OpenParen => {
            todo!("Unexpected `(` in linear mode.")
        },
        Identifier(identifier) => {
            match &identifier[..] {
                "let" => {
                    // let { a = 5 . body }
                    // let { [a, b] = [1, 2] . body } 
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    // Check if this is a tuple pattern or identifier.
                    let (input_if_commited, token0) = anytoken(input)?;
                    match token0 {
                        Token::Identifier(identifier) => { // we commit the identifier
                            let var = VariableName::new(identifier);
                            let (input, _) = token(TokenType::Eq)(input_if_commited)?;
                            let (input, arg) = parse_linear_expression(input)?;

                            let (input, _) = token(TokenType::BindingSeparator)(input)?;

                            let (input, body) = parse_linear_expression(input)?;
                            let (input, _) = token(TokenType::CloseCurly)(input)?;

                            Ok((input, LinearExpression::let_move(arg, var, body)))
                        },
                        Token::OpenBracket => { // we don't commit the bracket `[`
                            let (input, tuple_pattern) = parse_pattern(Mode::Linear)(input)?;
                            let (input, _) = token(TokenType::Eq)(input)?;
                            let (input, arg) = parse_linear_expression(input)?;
                            let (input, _) = token(TokenType::BindingSeparator)(input)?;

                            let (input, body) = parse_linear_expression(input)?;
                            let (input, _) = token(TokenType::CloseCurly)(input)?;
                            // match arg { tuple_pattern . body }
                            Ok((input, LinearExpression::match_(arg, vec![LinearPatternBranch { pattern: tuple_pattern, body }])))
                        },
                        _ => {
                            // TODO: This is an error
                            todo!()
                        }
                    }
                },
                "let-cart" => { // TODO: This syntax is a bit inconsistent... it should be atleast `let cart`
                    // let-cart { x = 5 . body }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, (var_name, arg)) = parse_cartesian_var_binding(input)?;

                    let (input, _) = token(TokenType::BindingSeparator)(input)?;

                    let (input, body) = parse_linear_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, LinearExpression::let_cartesean(arg, var_name, body)))
                },
                "match" => {
                    let (input, arg) = parse_linear_expression(input)?;
                    let (input, branches) = curly_braces(parse_linear_branches)(input)?;

                    Ok((input, LinearExpression::match_(arg, branches)))
                },
                "obj" => {
                    // obj { a = e0, b = e1 . #hd () . body0 | #tl () . body1 }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, captured_bindings) = parse_linear_var_bindings(input)?;
                    let (input, _) = token(TokenType::BindingSeparator)(input)?;
                    let (input, branches) = parse_linear_branches(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;
                    Ok((input, LinearExpression::object(captured_bindings, branches)))
                },
                "send" => {
                    // send[%obj, %msg]
                    let (input, (e0, e1)) = parse_linear_arg_list2(input)?;
                    Ok((input, LinearExpression::send(e0, e1)))
                },
                "force" => {
                    // force($some_thunk_in_cartesian_mode)
                    let (input, e) = parens(parse_cartesian_expression)(input)?;
                    Ok((input, LinearExpression::force(e)))
                }
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => 
                        parse_linear_operator_arguments(op_code, input),
                    None => {
                        let (input, cartesian_arguments) = possibly_empty_relaxed_paren_vector(parse_cartesian_expression)(input)?;
                        let (input, arguments) = possibly_empty_relaxed_bracket_vector(parse_linear_expression)(input)?;
                        Ok((input, LinearExpression::call(FunctionName::new(identifier), cartesian_arguments, arguments)))
                    },
                },
            }
        },
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

// ===Program===
#[derive(Debug, Clone)]
pub struct Program {
    pub cartesian_function_definitions: HashMap<FunctionName, CartesianFunctionDefinition>,
    pub linear_function_definitions: HashMap<FunctionName, LinearFunctionDefinition>,
    pub function_definitions_ordering: Vec<(Mode, FunctionName)>,
}

#[derive(Debug, Clone)]
pub enum FunctionDefinition {
    Cartesian(CartesianFunctionDefinition),
    Linear(LinearFunctionDefinition),
}

#[derive(Debug, Clone)]
pub struct CartesianFunctionDefinition {
    name: FunctionName,
    cartesian_parameters: Vec<VariableName>,
    // Note the absence of linear parameters./
    body: CartesianExpression,
}

#[derive(Debug, Clone)]
pub struct LinearFunctionDefinition {
    name: FunctionName,
    cartesian_parameters: Vec<VariableName>,
    linear_parameters: Vec<VariableName>,
    body: LinearExpression,
}

impl FunctionDefinition {
    fn name(&self) -> &FunctionName {
        use FunctionDefinition::*;
        match self {
            Cartesian(fn_def) => &fn_def.name,
            Linear(fn_def) => &fn_def.name,
        }
    }
}

impl Program {
    pub fn new() -> Self {
        Self {
            cartesian_function_definitions: HashMap::new(),
            linear_function_definitions: HashMap::new(),
            function_definitions_ordering: vec![],
        }
    }

    pub fn update_function_definition(&mut self, fn_def: FunctionDefinition) {
        let fn_name = fn_def.name().clone();
        match fn_def {
            FunctionDefinition::Cartesian(fn_def) => {
                self.cartesian_function_definitions.insert(fn_name.clone(), fn_def);

                // This is a bit insane. But function redefinitions don't occur frequently.
                match self.function_definitions_ordering.iter().position(|(mode, name)| matches!(mode, Mode::Cartesian) && name == &fn_name) {
                    Some(fn_index) => {
                        self.function_definitions_ordering.remove(fn_index);
                    },
                    None => {}
                }
                self.function_definitions_ordering.push((Mode::Cartesian, fn_name));
            },
            FunctionDefinition::Linear(fn_def) => {
                self.linear_function_definitions.insert(fn_name.clone(), fn_def);

                // This is a bit insane. But function redefinitions don't occur frequently.
                match self.function_definitions_ordering.iter().position(|(mode, name)| matches!(mode, Mode::Linear) && name == &fn_name) {
                    Some(fn_index) => {
                        self.function_definitions_ordering.remove(fn_index);
                    },
                    None => {}
                }
                self.function_definitions_ordering.push((Mode::Linear, fn_name));
            },
        }
    }

    pub fn get_linear_function_definition(&self, function_name: FunctionName) -> Option<&LinearFunctionDefinition> {
        self.linear_function_definitions.get(&function_name)
    }

    pub fn get_cartesian_function_definition(&self, function_name: FunctionName) -> Option<&CartesianFunctionDefinition> {
        self.cartesian_function_definitions.get(&function_name)
    }

    // TODO: Why do I need this?
    pub fn get_clone_of_linear_function_definition(&self, function_name: FunctionName) -> Option<LinearFunctionDefinition> {
        match self.get_linear_function_definition(function_name) {
            Some(fn_def) => Some(fn_def.clone()),
            None => None,
        }
    }
}

// ===Expressions===
// TODO
// #[derive(Debug, Clone)]
// pub enum Expression {
//     Cartesian(CartesianExpression),
//     Linear(LinearExpression),
// }

#[derive(Debug, PartialEq, Clone)]
pub enum OperationCode {
    Add,
    Sub,
    Mul,
    Eq,
    Duplicate,
    Discard,
}

// ==Cartesian==
#[derive(Debug, Clone)]
pub enum Expression {
    Cartesian(CartesianExpression),
    Linear(LinearExpression), 
}

#[derive(Debug, Clone)]
pub struct CartesianExpression(pub Rc<CartesianExpression0>);

#[derive(Debug, Clone)]
pub enum CartesianExpression0 {
    Int(i32),
    OperationApplication(OperationCode, CartesianExpression, CartesianExpression),
    Call(FunctionName, Vec<CartesianExpression>),
    Tagged(Tag, CartesianExpression),
    Tuple(Vec<CartesianExpression>),
    Match { arg: CartesianExpression, branches: Vec<CartesianPatternBranch> },
    VarLookup(VariableName),
    Let { arg: CartesianExpression, var: VariableName, body: CartesianExpression },
    // TODO: Can you make this into  Rc<[Rc<CartesianPatternBranch>]>?
    //       Or even Rc<[CartesianPatternBranch]>, but then you'd have to modify the
    //       CartesianPatternBranch slightly, by introducing Rc for Pattern so it is cheap to
    //       clone.
    Object { branches: Rc<Vec<CartesianPatternBranch>> },
    Send(CartesianExpression, CartesianExpression),

    // TODO: Is this a good name for this? Maybe `Freeze` would be better?
    // TODO: The Rc is problematic. Why would it be there?
    //       I guess the only reason is that it is eventually gona be passed in to the linear_eval.
    //  Hmm, but it is consistent with Object definition above.
    Thunk(Rc<LinearExpression>), // This is the coinductive going up thing to which you can send a `force` message.
}

impl CartesianExpression {
    fn int(x: i32) -> Self { Self(Rc::new(CartesianExpression0::Int(x))) }
    fn operation_application(op_code: OperationCode, e0: Self, e1: Self) -> Self { Self(Rc::new(CartesianExpression0::OperationApplication(op_code, e0, e1))) }
    fn call(fn_name: FunctionName, args: Vec<Self>) -> Self { Self(Rc::new(CartesianExpression0::Call(fn_name, args))) }
    fn tagged(tag: Tag, e: CartesianExpression) -> Self { Self(Rc::new(CartesianExpression0::Tagged(tag, e))) }
    fn tuple(args: Vec<CartesianExpression>) -> Self { Self(Rc::new(CartesianExpression0::Tuple(args))) }
    fn match_(arg: Self, branches: Vec<CartesianPatternBranch>) -> Self { Self(Rc::new(CartesianExpression0::Match { arg, branches })) }
    fn var_lookup(var: VariableName) -> Self { Self(Rc::new(CartesianExpression0::VarLookup(var))) }
    fn let_(arg: Self, var: VariableName, body: Self) -> Self { Self(Rc::new(CartesianExpression0::Let { arg, var, body })) }
    // TODO: The double Rc<_> here is very suspicious.
    fn object(branches: Vec<CartesianPatternBranch>) -> Self { Self(Rc::new(CartesianExpression0::Object { branches: Rc::new(branches) })) }
    fn send(obj: Self, msg: Self) -> Self { Self(Rc::new(CartesianExpression0::Send(obj, msg))) }
    fn thunk(e: LinearExpression) -> Self { Self(Rc::new(CartesianExpression0::Thunk(Rc::new(e)))) }
}

// ==Linear==
#[derive(Debug, Clone)]
pub struct LinearExpression(pub Box<LinearExpression0>);

#[derive(Debug, Clone)]
pub enum LinearExpression0 {
    Int(i32),
    OperationApplication1(OperationCode, LinearExpression),
    OperationApplication2(OperationCode, LinearExpression, LinearExpression),
    Call(FunctionName, Vec<CartesianExpression>, Vec<LinearExpression>),
    Tagged(Tag, LinearExpression),
    Tuple(Vec<LinearExpression>),
    Match { arg: LinearExpression, branches: Vec<LinearPatternBranch> },
    VarMove(VariableName),
    VarClone(VariableName),
    VarDrop(VariableName, LinearExpression),
    VarLookup(VariableName), // TODO: This is very suspicious. It makes sense dynamically, but what
                             // does it mean on type level? Each cartesian value must also be a linear value.
    LetMove { arg: LinearExpression, var: VariableName, body: LinearExpression },
    // TODO: you have 
    //   let-cart { x = cartesian-expr . lin-body }
    // but you also need to unwrap the cartesian expression hiding as linear:
    //   let-cart { x = linear-expr . lin-body } where at runtime `linear-expr` has to eval to `cart(cart-val)`
    LetCartesian { cartesian_arg: CartesianExpression, var: VariableName, body: LinearExpression },
    Object { captured_bindings: LinearBindings, branches: Vec<LinearPatternBranch> },
    Send(LinearExpression, LinearExpression),

    Force(CartesianExpression),
}

#[derive(Debug, Clone)]
pub struct LinearBindings(Box<LinearBindings0>);

#[derive(Debug, Clone)]
pub enum LinearBindings0 {
    Empty,
    Push { var: VariableName, expr: LinearExpression, parent: LinearBindings },
}

impl LinearExpression {
    fn int(x: i32) -> Self { Self(Box::new(LinearExpression0::Int(x))) }
    fn operation_application1(op_code: OperationCode, e0: Self) -> Self { Self(Box::new(LinearExpression0::OperationApplication1(op_code, e0))) }
    fn operation_application2(op_code: OperationCode, e0: Self, e1: Self) -> Self { Self(Box::new(LinearExpression0::OperationApplication2(op_code, e0, e1))) }
    fn call(fn_name: FunctionName, cartesian_args: Vec<CartesianExpression>, args: Vec<Self>) -> Self { Self(Box::new(LinearExpression0::Call(fn_name, cartesian_args, args))) }
    fn tagged(tag: Tag, e: LinearExpression) -> Self { Self(Box::new(LinearExpression0::Tagged(tag, e))) }
    fn tuple(args: Vec<LinearExpression>) -> Self { Self(Box::new(LinearExpression0::Tuple(args))) }
    fn match_(arg: Self, branches: Vec<LinearPatternBranch>) -> Self { Self(Box::new(LinearExpression0::Match { arg, branches })) }
    fn var_move(var: VariableName) -> Self { Self(Box::new(LinearExpression0::VarMove(var))) }
    fn var_clone(var: VariableName) -> Self { Self(Box::new(LinearExpression0::VarClone(var))) }
    fn var_drop(var: VariableName, expr: LinearExpression) -> Self { Self(Box::new(LinearExpression0::VarDrop(var, expr))) }
    fn var_lookup(var: VariableName) -> Self { Self(Box::new(LinearExpression0::VarLookup(var))) }
    fn let_move(arg: Self, var: VariableName, body: Self) -> Self { Self(Box::new(LinearExpression0::LetMove { arg, var, body })) }
    fn let_cartesean(arg: CartesianExpression, var: VariableName, body: Self) -> Self { Self(Box::new(LinearExpression0::LetCartesian { cartesian_arg: arg, var, body })) }
    fn object(moved_env: LinearBindings, branches: Vec<LinearPatternBranch>) -> Self { Self(Box::new(LinearExpression0::Object { captured_bindings: moved_env, branches })) }
    fn send(obj: Self, msg: Self) -> Self { Self(Box::new(LinearExpression0::Send(obj, msg))) }
    fn force(obj: CartesianExpression) -> Self { Self(Box::new(LinearExpression0::Force(obj))) }
}

// ===Patterns===
#[derive(Debug, Clone)]
pub struct CartesianPatternBranch {
    pub pattern: Pattern,
    pub body: CartesianExpression,
}

#[derive(Debug, Clone)]
pub struct LinearPatternBranch {
    pub pattern: Pattern,
    pub body: LinearExpression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(VariableName),
    Tagged(Tag, Box<Pattern>),
    Tuple(Vec<TuplePattern>),
}

#[derive(Debug, Clone)]
pub enum TuplePattern {
    Variable(VariableName),
    Tuple(Vec<TuplePattern>),
}

// ===Values===

#[derive(Debug)]
pub enum Value {
    Linear(LinearValue),
    Cartesian(CartesianValue),
}

#[derive(Debug)]
pub enum CartesianValue {
    Int(i32),
    Tagged(Tag, Rc<CartesianValue>),
    Tuple(Rc<[Rc<CartesianValue>]>),
    ClosureObject { captured_cartesian_env: CartesianEnv, branches: Rc<Vec<CartesianPatternBranch>> },
    Thunk { captured_cartesian_env: CartesianEnv, body: Rc<LinearExpression> }
}

// ==Cartesian==
#[derive(Debug, Clone)]
pub struct CartesianEnv(Rc<CartesianEnv0>);

#[derive(Debug)]
pub enum CartesianEnv0 {
    Empty,
    Push { var: VariableName, value: CartesianValue, parent: CartesianEnv },
}

impl CartesianValue {
    // This is also called a shallow copy. In implementation remember: this should no do recursion.
    // It should be very fast!
    fn duplicate_with_sharing(&self) -> Self {
        use CartesianValue::*;
        match self {
            Int(x) => Int(*x),
            Tagged(tag, val) => Tagged(tag.clone(), val.clone()),
            Tuple(boxed_tuple) => Tuple(boxed_tuple.clone()),
            ClosureObject { captured_cartesian_env, branches } => ClosureObject { captured_cartesian_env: captured_cartesian_env.clone(), branches: branches.clone() },
            Thunk { captured_cartesian_env, body } => Thunk { captured_cartesian_env: captured_cartesian_env.clone(), body: body.clone() },
        }
    }
}


#[derive(Debug)]
pub enum LinearValue {
    Int(i32),
    Tagged(Tag, Box<LinearValue>),
    Tuple(Vec<LinearValue>), // Would be cool if we could use Box<[Value]>, since we don't need to resize
    ClosureObject { captured_env: LinearEnv, branches: Vec<LinearPatternBranch> },
    Cartesian(CartesianValue),
}

impl LinearValue {
    fn discard(self) -> () {
        use LinearValue::*;
        match self {
            Int(_x) => (),
            Tagged(_tag, val) => (*val).discard(),
            Tuple(values) => {
                for val in values {
                    let _ = val.discard();
                }
            },
            ClosureObject { .. } => todo!(), // this should crash
            Cartesian(_cart_val) => (), // Interesting. We don't have to do anything.
        }
    }

    fn duplicate(self) -> (Self, Self) {
        use LinearValue::*;
        match self {
            Int(x) => (Int(x), Int(x)),
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
            ClosureObject { .. } => todo!(), // this should crash
            // Note that the clone here is very cheap.
            Cartesian(cart_val) => {
                // TODO: clone
                (Cartesian(cart_val.duplicate_with_sharing()), Cartesian(cart_val)) 
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
        match self {
            Cartesian(value) => value.fmt(f),
            Linear(value) => value.fmt(f),
        }
    }
}

impl fmt::Display for CartesianValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use CartesianValue::*;
        match self {
            Int(x) => write!(f, "{}", x),
            Tagged(tag, val) => write!(f, "{} {}", tag, val),
            Tuple(values) => {
                write!(f, "(")?;
                let mut values = (&**values).iter().peekable();
                while let Some(val) = values.next() {
                    match values.peek() {
                        Some(_) => write!(f, "{}, ", val)?,
                        None => write!(f, "{}", val)?,
                    }
                }
                write!(f, ")")
            },
            ClosureObject { ..  } => write!(f, "obj {{...}}"),
            Thunk { .. } => write!(f, "thunk {{...}}"),
        }
    }
}

impl fmt::Display for LinearValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LinearValue::*;
        match self {
            Int(x) => write!(f, "{}", x),
            Tagged(tag, val) => write!(f, "{}{}", tag, val),
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
            // TODO: Perhaps show captured resources?
            ClosureObject { ..  } => write!(f, "obj {{...}}"),
            Cartesian(cart_val) => write!(f, "cart({})", cart_val)
        }
    }
}

// ===Env===

impl CartesianEnv {
    fn new() -> Self { Self(Rc::new(CartesianEnv0::Empty)) }

    fn get(&self, var_name: VariableName) -> Result<CartesianValue, Error> {
        use CartesianEnv0::*;
        match &*self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if *var == var_name {
                    Ok(value.duplicate_with_sharing())
                } else {
                    parent.get(var_name)
                }
            },
        }
    }

    fn extend(self, var_name: VariableName, value: CartesianValue) -> Self {
        Self(Rc::new(CartesianEnv0::Push { var: var_name, value, parent: self }))
    }

    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, CartesianValue)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ==Linear==
#[derive(Debug)]
pub struct LinearEnv(Box<LinearEnv0>);

#[derive(Debug)]
pub enum LinearEnv0 {
    Empty,
    Push { var: VariableName, value: LinearValue, parent: LinearEnv },
}

impl LinearEnv {
    fn new() -> Self { Self(Box::new(LinearEnv0::Empty)) }

    fn is_empty(&self) -> bool { matches!(*self.0, LinearEnv0::Empty) }

    fn move_(self, var_name: VariableName) -> Result<(LinearValue, Self), Error> {
        use LinearEnv0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    Ok((value, parent))
                } else {
                    let (value0, parent) = parent.move_(var_name)?;
                    Ok((value0, LinearEnv(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }

    fn clone_(self, var_name: VariableName) -> Result<(LinearValue, Self), Error> {
        use LinearEnv0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    let (value0, value1) = value.duplicate();
                    Ok((value0, LinearEnv(Box::new(Push { var, value: value1, parent }))))
                } else {
                    let (value0, parent) = parent.clone_(var_name)?;
                    Ok((value0, LinearEnv(Box::new(Push { var, value, parent }))))
                }
            },
        }
    }

    fn drop_(self, var_name: VariableName) -> Result<Self, Error> {
        use LinearEnv0::*;
        match *self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if var == var_name {
                    Ok(parent)
                } else {
                    let parent = parent.drop_(var_name)?;
                    Ok(LinearEnv(Box::new(Push { var, value, parent })))
                }
            },
        }
    }

    fn extend(self, var: VariableName, value: LinearValue) -> Self {
        Self(Box::new(LinearEnv0::Push { var, value, parent: self }))
    }

    fn extend_many(mut self, bindings: impl Iterator<Item=(VariableName, LinearValue)>) -> Self {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ===Error===
#[derive(Debug)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    CartesianFunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    LinearFunctionCallArityMismatch { fn_name: FunctionName, expected_cartesian: usize, received_cartesian: usize, expected_linear: usize, received_linear: usize },
    VariableLookupFailure(VariableName),
    UnableToFindMatchingPattern,
    InvalidPatternMatch(PatternMatchErrror),
    UnconsumedResources(LinearEnv),
    AttemptToForceThunkWhileMultiplyOwned(Rc<LinearExpression>),
    AttemptToSendMessageToNonObject(Value),
}

#[derive(Debug)]
pub enum PatternMatchErrror {
    TaggedValueFedToNonTaggedPattern,
    TupleFedToNonTuplePattern,
    TupleSizesDidNotMatch,
    TupleMatchedAgainstMatchExpressionWithMultipleBranches,
    AttemptToMatchNonInductiveValue,
    AttemptToMatchNonTupleToTuplePattern,
}

// ===Evaluation===
pub fn eval_start(program: &Program, e: Expression) -> Result<Value, Error> {
    use Expression::*;
    match e {
        Cartesian(e) => {
            let value = cartesian_eval(program, &CartesianEnv::new(), &e)?;
            Ok(Value::Cartesian(value))
        },
        Linear(e) => {
            let (env, value) = linear_eval(program, &CartesianEnv::new(), LinearEnv::new(), e)?;
            if env.is_empty() {
                Ok(Value::Linear(value))
            } else {
                Err(Error::UnconsumedResources(env))
            }
        }
    }
}

fn cartesian_eval(program: &Program, env: &CartesianEnv, e: &CartesianExpression) -> Result<CartesianValue, Error> {
    use CartesianExpression0::*;
    match &*(e.0) {
        Int(x) => Ok(CartesianValue::Int(*x)),
        OperationApplication(code, e0, e1) => {
            let val0 = cartesian_eval(program, env, e0)?;
            let val1 = cartesian_eval(program, env, e1)?;
            match (val0, val1) {
                (CartesianValue::Int(x0), CartesianValue::Int(x1)) => {
                    use OperationCode::*;
                    match code {
                        Add => Ok(CartesianValue::Int(x0 + x1)),
                        Sub => Ok(CartesianValue::Int(x0 - x1)),
                        Mul => Ok(CartesianValue::Int(x0 * x1)),
                        Eq => if x0 == x1 {
                            Ok(CartesianValue::Tagged(Tag::new("T".to_string()), Rc::new(CartesianValue::Tuple(Rc::new([])))))
                        } else {
                            Ok(CartesianValue::Tagged(Tag::new("F".to_string()), Rc::new(CartesianValue::Tuple(Rc::new([])))))
                        },
                        Duplicate | Discard => todo!(),
                    }
                },
                _ => todo!(),
            }
        },
        Call(fn_name, args) => {
            let mut values: Vec<CartesianValue> = Vec::with_capacity(args.len());
            for arg in args {
                values.push(cartesian_eval(program, env, &arg)?);
            }
            let Some(fn_def) = program.get_cartesian_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name.clone())) };
            apply_cartesian_function(program, fn_def, values)
        },
        Tagged(tag, e) => {
            let val = cartesian_eval(program, env, e)?;
            Ok(CartesianValue::Tagged(tag.clone(), Rc::new(val)))
        },
        Tuple(args) => {
            // TODO: Can you construct the tuple directly? Without going through the vector first?
            let mut values: Vec<Rc<CartesianValue>> = Vec::with_capacity((&*args).len());
            for arg in &*args {
                values.push(Rc::new(cartesian_eval(program, env, arg)?))
            }
            Ok(CartesianValue::Tuple(values.into()))
        },
        Match { arg, branches } => {
            let arg_value = cartesian_eval(program, env, arg)?;
            cartesian_apply_msg_to_branches(program, env, branches, &arg_value)
        },
        VarLookup(var_name) => {
            env.get(var_name.clone())
        },
        Let { arg, var, body } => {
            let arg_value = cartesian_eval(program, env, arg)?;
            let env = env.clone().extend(var.clone(), arg_value);
            cartesian_eval(program, &env, body)
        },
        Object { branches } => {
            Ok(CartesianValue::ClosureObject { captured_cartesian_env: env.clone(), branches: Rc::clone(branches) })
        },
        Send(e0, e1) => {
            let obj = cartesian_eval(program, env, e0)?;
            match obj {
                CartesianValue::ClosureObject { captured_cartesian_env: captured_env, branches } => {
                    let msg = cartesian_eval(program, env, e1)?;
                    cartesian_apply_msg_to_branches(program, &captured_env, &branches, &msg)
                },
                obj => Err(Error::AttemptToSendMessageToNonObject(Value::Cartesian(obj))),
            }
        },
        Thunk(linear_expr) => {
            // TODO: I really don't like that we have an Rc here.
            Ok(CartesianValue::Thunk { captured_cartesian_env: env.clone(), body: linear_expr.clone() })
        }
    }
}

fn linear_eval(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, e: LinearExpression) -> Result<(LinearEnv, LinearValue), Error> {
    use LinearExpression0::*;
    match *e.0 {
        Int(x) => Ok((env, LinearValue::Int(x))),
        OperationApplication1(code, e0) => {
            let (env, val0) = linear_eval(program, cartesian_env, env, e0)?;
            use OperationCode::*;
            Ok((
                env,
                match code {
                    Duplicate => {
                        let (v0, v1) = val0.duplicate();
                        LinearValue::Tuple(vec![v0, v1])
                    },
                    Discard => {
                        let () = val0.discard();
                        LinearValue::Tuple(vec![])
                    },
                    _ => todo!(), // This should crash
                }
            ))
        },
        OperationApplication2(code, e0, e1) => {
            let (env, val0) = linear_eval(program, cartesian_env, env, e0)?;
            let (env, val1) = linear_eval(program, cartesian_env, env, e1)?;
            match (val0, val1) {
                (LinearValue::Int(x0), LinearValue::Int(x1)) => {
                    use OperationCode::*;
                    Ok((
                        env,
                        match code {
                            Add => LinearValue::Int(x0 + x1),
                            Sub => LinearValue::Int(x0 - x1),
                            Mul => LinearValue::Int(x0 * x1),
                            Eq => if x0 == x1 {
                                LinearValue::Tagged(Tag::new("T".to_string()), Box::new(LinearValue::Tuple(vec![])))
                            } else {
                                LinearValue::Tagged(Tag::new("F".to_string()), Box::new(LinearValue::Tuple(vec![])))
                            },
                            Discard | Duplicate => todo!()
                        }
                    ))
                },
                _ => todo!(),
            }
        },
        Call(fn_name, cartesian_args, args) => {
            let mut cartesian_values: Vec<CartesianValue> = Vec::with_capacity(cartesian_args.len());
            for arg in cartesian_args {
                let val = cartesian_eval(program, cartesian_env, &arg)?;
                cartesian_values.push(val);
            }
            let mut values: Vec<LinearValue> = Vec::with_capacity(args.len());
            let mut env = env;
            for arg in args {
                let (env0, val) = linear_eval(program, cartesian_env, env, arg)?;
                env = env0;
                values.push(val);
            }
            let Some(fn_def) = program.get_clone_of_linear_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name)) };
            let val = linear_apply_function(program, fn_def, cartesian_values, values)?;
            Ok((env, val))
        },
        Tagged(tag, e) => {
            let (env, val) = linear_eval(program, cartesian_env, env, e)?;
            Ok((env, LinearValue::Tagged(tag, Box::new(val))))
        },
        Tuple(args) => {
            let mut env = env;
            let mut values: Vec<LinearValue> = Vec::with_capacity(args.len());
            for arg in args {
                let (env0, val) = linear_eval(program, cartesian_env, env, arg)?;
                env = env0;
                values.push(val);
            }
            Ok((env, LinearValue::Tuple(values)))
        },
        VarMove(var_name) => {
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
            linear_eval(program, cartesian_env, env, expr)
        },
        VarLookup(var_name) => {
            let cartesian_value = cartesian_env.get(var_name)?;
            Ok((env, LinearValue::Cartesian(cartesian_value)))
        },
        Match { arg, branches } => {
            let (env, arg_value) = linear_eval(program, cartesian_env, env, arg)?;
            linear_apply_msg_to_branches(program, cartesian_env, env, branches, arg_value)
        },
        LetMove { arg, var, body } => {
            let (env, arg_value) = linear_eval(program, cartesian_env, env, arg)?;
            let env = env.extend(var, arg_value);
            linear_eval(program, cartesian_env, env, body)
        },
        LetCartesian { cartesian_arg, var, body } => {
            let arg_cartesian_value = cartesian_eval(program, cartesian_env, &cartesian_arg)?;
            let cartesian_env = cartesian_env.clone().extend(var.clone(), arg_cartesian_value);
            linear_eval(program, &cartesian_env, env, body)
        },
        Object { captured_bindings, branches } => {
            let (env, captured_env) = linear_eval_bindings(program, cartesian_env, env, captured_bindings)?;
            Ok((env, LinearValue::ClosureObject { captured_env, branches }))
        },
        Send(e0, e1) => {
            let (env, obj) = linear_eval(program, cartesian_env, env, e0)?;
            match obj {
                LinearValue::ClosureObject { captured_env, branches } => {
                    let (env, msg) = linear_eval(program, cartesian_env, env, e1)?;
                    let (captured_env, val) = linear_apply_msg_to_branches(program, cartesian_env, captured_env, branches, msg)?;
                    if captured_env.is_empty() {
                        Ok((env, val))
                    } else {
                        Err(Error::UnconsumedResources(captured_env))
                    }
                },
                obj => Err(Error::AttemptToSendMessageToNonObject(Value::Linear(obj))),
            }
        },

        Force(cartesian_expr) => {
            // We need to evaluate this is empty linear environment, right?
            // This better be a frozen linear object (that doesn't use resources, but it is still
            // weird ... it could be something in [Bool⊸ Bool], which is a process that need not
            // capture a resource, but how the hell can it be copied?)
            let cartesian_val = cartesian_eval(program, cartesian_env, &cartesian_expr)?;
            match cartesian_val {
                CartesianValue::Thunk { captured_cartesian_env, body } => {
                    // example
                    //   lin force[let { x = thunk { 10 } . $x }]
                    // In the above example the thunk  is in the cartesian environment.
                    // This is highly problematic.
                    //
                    // TODO: body is an Rc'd linear expression.
                    //       To evaluate it, we must take ownership of it...
                    //       Is this really the right thing?
                    // Actually think thunk still could be in the cartesian environment (and it
                    // probably is!). I think the correct thing must be to rip out of the cartesian
                    // environment
                    
                    match Rc::try_unwrap(body) {
                        Ok(body) => {
                            let (new_env, val) = linear_eval(program, &captured_cartesian_env, LinearEnv::new(), body)?;
                            if new_env.is_empty() {
                                Ok((env, val))
                            } else {
                                Err(Error::UnconsumedResources(new_env))
                            }
                        },
                        Err(body) => {
                            Err(Error::AttemptToForceThunkWhileMultiplyOwned(body))
                        },
                    }
                },
                _ => todo!() // crash
            }
        }
    }
}

fn linear_eval_bindings(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, bindings: LinearBindings) -> Result<(LinearEnv, LinearEnv), Error> {
    use LinearBindings0::*;
    match *bindings.0 {
        Empty => Ok((env, LinearEnv::new())),
        Push { var, expr, parent } => {
            let (env, bindings_env_parent) = linear_eval_bindings(program, cartesian_env, env, parent)?;
            let (env, val) = linear_eval(program, cartesian_env, env, expr)?;
            let bindings_env_parent = bindings_env_parent.extend(var, val);
            Ok((env, bindings_env_parent))
        },
    }
}

// ===Apply Function===
// ==Cartesian==
fn apply_cartesian_function(program: &Program, fn_def: &CartesianFunctionDefinition, arg_values: Vec<CartesianValue>) -> Result<CartesianValue, Error> {
    let num_of_arguments: usize = fn_def.cartesian_parameters.len();
    if num_of_arguments != arg_values.len() {
        return Err(Error::CartesianFunctionCallArityMismatch { fn_name: fn_def.name.clone(), expected: num_of_arguments, received: arg_values.len() })
    }
    
    let mut bindings: Vec<(VariableName, CartesianValue)> = Vec::with_capacity(num_of_arguments);
    for (i, val) in arg_values.into_iter().enumerate() {
        bindings.push((fn_def.cartesian_parameters[i].clone(), val));
    }
    cartesian_eval(program, &CartesianEnv::new().extend_many(bindings.into_iter()), &fn_def.body)
}

// ==Linear==
fn linear_apply_function(program: &Program, fn_def: LinearFunctionDefinition, cartesian_arg_values: Vec<CartesianValue>, arg_values: Vec<LinearValue>) -> Result<LinearValue, Error> {
    let num_of_cartesian_arguments: usize = fn_def.cartesian_parameters.len();
    let num_of_arguments: usize = fn_def.linear_parameters.len();
    if num_of_cartesian_arguments != cartesian_arg_values.len() || num_of_arguments != arg_values.len() {
        return Err(Error::LinearFunctionCallArityMismatch { fn_name: fn_def.name, expected_cartesian: num_of_cartesian_arguments, received_cartesian: cartesian_arg_values.len(), expected_linear: num_of_arguments, received_linear: arg_values.len() })
    }
    let cartesian_env = CartesianEnv::new().extend_many(fn_def.cartesian_parameters.into_iter().zip(cartesian_arg_values));
    let env = LinearEnv::new().extend_many(fn_def.linear_parameters.into_iter().zip(arg_values));
    let (env, val) = linear_eval(program, &cartesian_env, env, fn_def.body)?;
    if env.is_empty() {
        Ok(val)
    } else {
        Err(Error::UnconsumedResources(env))
    }
}

// ===Send Message===
// ==Cartesian==
fn cartesian_apply_msg_to_branches(program: &Program, env: &CartesianEnv, branches: &[CartesianPatternBranch], val: &CartesianValue) -> Result<CartesianValue, Error> {
    if branches.len() == 0 { return Err(Error::UnableToFindMatchingPattern) }
    if branches.len() == 1 && matches!(branches[0].pattern, Pattern::Variable(_)) { // non-destructive read
        let CartesianPatternBranch { pattern, body } =  branches.into_iter().next().unwrap();
        match pattern {
            Pattern::Variable(var) => {
                let env = env.clone().extend(var.clone(), val.duplicate_with_sharing());
                return cartesian_eval(program, &env, body)
            },
            _ => unreachable!(),
        }
    }
    match val {
        CartesianValue::Tagged(tag0, val) => {
            let mut filtered_branches: Vec<CartesianPatternBranch> = vec![];
            for CartesianPatternBranch { pattern, body } in branches {
                match pattern {
                    Pattern::Tagged(tag, pattern) => {
                        if tag == tag0 {
                            filtered_branches.push(CartesianPatternBranch { pattern: *pattern.clone(), body: body.clone() });
                        }
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TaggedValueFedToNonTaggedPattern))
                }
            }
            cartesian_apply_msg_to_branches(program, env, &filtered_branches, &*val)
        },
        CartesianValue::Tuple(values) => {
            if branches.len() == 1 {
                let branch = branches.into_iter().next().unwrap();
                match &branch.pattern {
                    Pattern::Tuple(patterns) =>  {
                        apply_cartesian_tuple_to_branch(program, env, &patterns[..], &branch.body, values)
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleFedToNonTuplePattern)),
                }
            } else {
                Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleMatchedAgainstMatchExpressionWithMultipleBranches))
            }
        },
        _ => {
            // The only pattern that could match this is MatchAll (i.e. a variable).
            // In which case we would have already matched this. This means that we have an error
            // here, e.g. someone fed an object into a match case that's not a match-all.
            return Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonInductiveValue))
        },
    }
}
fn apply_cartesian_tuple_to_branch(program: &Program, env: &CartesianEnv, patterns: &[TuplePattern], body: &CartesianExpression, values: &[Rc<CartesianValue>]) -> Result<CartesianValue, Error> {
    let env = bind_cartesian_tuple_to_tuple_pattern(program, env, patterns, values)?;
    cartesian_eval(program, &env, body)
}
fn bind_cartesian_tuple_to_tuple_pattern(program: &Program, env: &CartesianEnv, patterns: &[TuplePattern], values: &[Rc<CartesianValue>]) -> Result<CartesianEnv, Error> {
    if patterns.len() != values.len() {
        return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleSizesDidNotMatch))
    }
    let mut env = env.clone();
    for (pattern, val) in patterns.into_iter().zip(values) {
        match pattern {
            TuplePattern::Variable(var) => {
                env = env.extend(var.clone(), val.duplicate_with_sharing());
            },
            TuplePattern::Tuple(patterns) => {
                match &**val {
                    CartesianValue::Tuple(values) => {
                        env = bind_cartesian_tuple_to_tuple_pattern(program, &env, patterns, &values)?;
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonTupleToTuplePattern)),
                }
            },
        }
    }
    Ok(env)
}

// ==Linear==
fn linear_apply_msg_to_branches(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, branches: Vec<LinearPatternBranch>, val: LinearValue) -> Result<(LinearEnv, LinearValue), Error> {
    if branches.len() == 0 { return Err(Error::UnableToFindMatchingPattern) }
    if branches.len() == 1 && matches!(branches[0].pattern, Pattern::Variable(_)) { // non-destructive read
        // Can't just do branches[0]... that has ownership problems for some reason.
        let LinearPatternBranch { pattern, body } =  branches.into_iter().next().unwrap();
        match pattern {
            Pattern::Variable(var) => {
                let env = env.extend(var, val);
                return linear_eval(program, cartesian_env, env, body)
            },
            _ => unreachable!(),
        }
    }
    match val {
        LinearValue::Tagged(tag0, val) => {
            let mut filtered_branches: Vec<LinearPatternBranch> = vec![];
            for LinearPatternBranch { pattern, body } in branches {
                match pattern {
                    Pattern::Tagged(tag, pattern) => {
                        if tag == tag0 {
                            filtered_branches.push(LinearPatternBranch { pattern: *pattern, body });
                        }
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TaggedValueFedToNonTaggedPattern))
                }
            }
            linear_apply_msg_to_branches(program, cartesian_env, env, filtered_branches, *val)
        },
        LinearValue::Tuple(values) => {
            if branches.len() == 1 {
                let branch = branches.into_iter().next().unwrap();
                match branch.pattern {
                    Pattern::Tuple(patterns) => apply_linear_tuple_to_branch(program, cartesian_env, env, patterns, branch.body, values),
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleFedToNonTuplePattern)),
                }
            } else {
                Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleMatchedAgainstMatchExpressionWithMultipleBranches))
            }
        },
        _ => {
            // The only pattern that could match this is MatchAll (i.e. a variable).
            // In which case we would have already matched this. This means that we have an error
            // here, e.g. someone fed an object into a match case that's not a match-all.
            return Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonInductiveValue))
        },
    }
}
fn apply_linear_tuple_to_branch(program: &Program, cartesian_env: &CartesianEnv, env: LinearEnv, patterns: Vec<TuplePattern>, body: LinearExpression, values: Vec<LinearValue>) -> Result<(LinearEnv, LinearValue), Error> {
    let env = bind_linear_tuple_to_tuple_pattern(program, cartesian_env, env, patterns, values)?;
    linear_eval(program, cartesian_env, env, body)
}
fn bind_linear_tuple_to_tuple_pattern(program: &Program, cartesian_env: &CartesianEnv, mut env: LinearEnv, patterns: Vec<TuplePattern>, values: Vec<LinearValue>) -> Result<LinearEnv, Error> {
    if patterns.len() != values.len() {
        return Err(Error::InvalidPatternMatch(PatternMatchErrror::TupleSizesDidNotMatch))
    }
    for (pattern, val) in patterns.into_iter().zip(values) {
        match pattern {
            TuplePattern::Variable(var) => {
                env = env.extend(var, val);
            },
            TuplePattern::Tuple(patterns) => {
                match val {
                    LinearValue::Tuple(values) => {
                        env = bind_linear_tuple_to_tuple_pattern(program, cartesian_env, env, patterns, values)?;
                    },
                    _ => return Err(Error::InvalidPatternMatch(PatternMatchErrror::AttemptToMatchNonTupleToTuplePattern)),
                }
            },
        }
    }
    Ok(env)
}
