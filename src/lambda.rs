use std::rc::Rc;
use std::fmt;
use crate::tokenizer::{TokenStream, Token, TokenType, anyidentifier, anytoken, identifier, token, peek_anytoken, peek_token, delimited_nonempty_vector, delimited_vector};
use crate::identifier::{VariableName, FunctionName, Tag};
use crate::IResult0;
use std::collections::HashMap;


// ===parser===
fn variable_name(input: TokenStream) -> IResult0<VariableName> {
    let (input, str) = anyidentifier(input)?;
    Ok((input, VariableName::new(str)))
}

fn identifier_to_operation_code(str: &str) -> Option<OperationCode> {
    match str {
        "+" => Some(OperationCode::Add),
        "*" => Some(OperationCode::Mul),
        "sub" => Some(OperationCode::Sub),
        "==" => Some(OperationCode::Eq),
        _ => None
    }
}

fn parse_operator_arguments(op_code: OperationCode, input: TokenStream) -> IResult0<Expression> {
    use OperationCode::*;
    match op_code {
        Add => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application(op_code, e0, e1)))
        },
        Sub => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application(op_code, e0, e1)))
        },
        Mul => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application(op_code, e0, e1)))
        },
        Eq => {
            let (input, (e0, e1)) = parse_arg_list2(input)?;
            Ok((input, Expression::operation_application(op_code, e0, e1)))
        },
    }
}

fn parse_arg_list2(input: TokenStream) -> IResult0<(Expression, Expression)> {
    // "(e0, e1)  "
    let (input, _) = token(TokenType::OpenParen)(input)?;
    let (input, e0) = parse_expression(input)?;
    let (input, _) = token(TokenType::Comma)(input)?;
    let (input, e1) = parse_expression(input)?;
    let (input, _) = token(TokenType::CloseParen)(input)?;
    Ok((input, (e0, e1)))
}

// No parens, just a possibly empty comma separated list of identifiers.
fn parameter_vector(input: TokenStream) -> IResult0<Vec<VariableName>> {
    // TODO: Check uniqueness.
    delimited_vector(variable_name, token(TokenType::Comma))(input)
}

// No parens, just a possibly empty comma separated list of expressions.
fn expression_vector(input: TokenStream) -> IResult0<Vec<Expression>> {
    delimited_vector(parse_expression, token(TokenType::Comma))(input)
}

pub fn parse_function_declaration(input: TokenStream) -> IResult0<FunctionDefinition> {
    let (input, _) = identifier("fn")(input)?;
    let (input, function_name_str) = anyidentifier(input)?;
    let (input, _) = token(TokenType::OpenParen)(input)?;
    let (input, parameters) = parameter_vector(input)?;
    let (input, _) = token(TokenType::CloseParen)(input)?;

    let (input, _) = token(TokenType::OpenCurly)(input)?;
    let (input, body) = parse_expression(input)?;
    let (input, _) = token(TokenType::CloseCurly)(input)?;
    Ok((input, FunctionDefinition { name: FunctionName::new(function_name_str), parameters, body }))
}

fn parse_pattern_sequence(input: TokenStream) -> IResult0<Vec<Pattern>> {
    delimited_vector(parse_pattern, token(TokenType::Comma))(input)
}

fn parse_pattern(input: TokenStream) -> IResult0<Pattern> {
    let (input, token0) = anytoken(input)?;
    use Token::*;
    match token0 {
        TagSymbol => {
            // Tag pattern
            let (input, var_name) = anyidentifier(input)?;
            let (input, pattern) = parse_pattern(input)?;
            Ok((input, Pattern::Tagged(Tag::new(var_name), Box::new(pattern))))
        },
        OpenParen => {
            // Tuple pattern
            let (input, patterns) = parse_pattern_sequence(input)?;
            let (input, _) = token(TokenType::CloseParen)(input)?;
            Ok((input, Pattern::Tuple(patterns)))
        },
        Identifier(var_name) => Ok((input, Pattern::Variable(VariableName::new(var_name)))),
        _ => Err(nom::Err::Error(nom::error::Error { input: input.input, code: nom::error::ErrorKind::Alt })),
    }
}

pub fn parse_branch(input: TokenStream) -> IResult0<PatternBranch> {
    let (input, pattern) = parse_pattern(input)?;
    let (input, _) = token(TokenType::BindingSeparator)(input)?;
    let (input, body) = parse_expression(input)?;

    Ok((input, PatternBranch { pattern, body }))
}

pub fn parse_branches(input: TokenStream) -> IResult0<Vec<PatternBranch>> {
    delimited_vector(parse_branch, token(TokenType::OrSeparator))(input)
}

pub fn parse_expression(input: TokenStream) -> IResult0<Expression> {
    let (input, token0) = anytoken(input)?;
    use Token::*;
    match token0 {
        Int(x) => Ok((input, Expression::int(x))),
        VarUseSymbol => {
            let (input, var_name) = anyidentifier(input)?;
            Ok((input, Expression::var_use(VariableName::new(var_name))))
        },
        TagSymbol => {
            let (input, tag) = anyidentifier(input)?;
            let (input, arg) = parse_expression(input)?;
            Ok((input, Expression::tagged(Tag::new(tag), arg)))
        },
        OpenParen => {
            // tuple
            let (input, args) = expression_vector(input)?;
            let (input, _) = token(TokenType::CloseParen)(input)?;
            Ok((input, Expression::tuple(args)))
        },
        Identifier(identifier) => {
            match &identifier[..] {
                "let" => {
                    // let { x = 5 . body }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, identifier) = anyidentifier(input)?;
                    let (input, _) = token(TokenType::Eq)(input)?;
                    let (input, arg) = parse_expression(input)?;

                    let (input, _) = token(TokenType::BindingSeparator)(input)?;

                    let (input, body) = parse_expression(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, Expression::let_(arg, VariableName::new(identifier), body)))
                },
                "match" => {
                    let (input, arg) = parse_expression(input)?;
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, branches) = parse_branches(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;

                    Ok((input, Expression::match_(arg, branches)))
                },
                "obj" => {
                    // obj { #hd () . e0 | #tl () . e1 }
                    let (input, _) = token(TokenType::OpenCurly)(input)?;
                    let (input, branches) = parse_branches(input)?;
                    let (input, _) = token(TokenType::CloseCurly)(input)?;
                    Ok((input, Expression::object(branches)))
                },
                "send" => {
                    // send($obj, $msg)
                    let (input, (e0, e1)) = parse_arg_list2(input)?;
                    Ok((input, Expression::send(e0, e1)))
                },
                s => match identifier_to_operation_code(s) {
                    Some(op_code) => parse_operator_arguments(op_code, input),
                    None => {
                        let (input, token_match) = peek_token(TokenType::OpenParen)(input)?;
                        match token_match {
                            Some(_) => {
                                // Here we have a function call
                                let (input, _) = token(TokenType::OpenParen)(input)?;
                                let (input, arguments) = expression_vector(input)?;
                                let (input, _) = token(TokenType::CloseParen)(input)?;
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

// ===Program===
pub struct Program {
    pub function_definitions: HashMap<FunctionName, FunctionDefinition>,
    pub function_definitions_ordering: Vec<FunctionName>,
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

#[derive(Debug)]
pub struct FunctionDefinition {
    name: FunctionName,
    parameters: Vec<VariableName>,
    body: Expression,
}

// ===Expressions===

// TODO: Seems like adding Lambda Expression messes everything up completely. Suddenly I have to
// put Rc everywhere instead of Box.
// Can I have `LinearExpression` that doesn't have this problem?

#[derive(Debug, Clone)]
pub struct Expression(pub Rc<Expression0>);

#[derive(Debug)]
pub enum Expression0 {
    Int(i32),
    OperationApplication(OperationCode, Expression, Expression),
    Call(FunctionName, Vec<Expression>),
    Tagged(Tag, Expression),
    Tuple(Vec<Expression>),
    Match { arg: Expression, branches: Vec<PatternBranch> },
    VarUse(VariableName),
    Let { arg: Expression, var: VariableName, body: Expression },
    // Note that the body is in Rc. This is because when evaluating a lambda expression,
    // a closure value is createad which references this body.
    // TODO: If I passed the expression not as a reference, but directly,
    //       couldn't I just take ownership of the body?
    // Lambda { var: VariableName, body: Expression },
    Object { branches: Rc<Vec<PatternBranch>> },
    Send(Expression, Expression),
}

#[derive(Debug, Clone)]
pub struct PatternBranch {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(VariableName),
    Tagged(Tag, Box<Pattern>),
    Tuple(Vec<Pattern>),
}

type PatternMatchResult = Option<Vec<(VariableName, Value)>>;

impl Pattern {
    // The only interesting thing is the type of `val`.
    // This could be implemented with `val: Value`, but it would have to be way more complicated,
    // where if we fail a deep pattern, we have to reconstruct the value as it was before,
    // so we can reuse is for the next match. This reconstruction is expensive.
    //
    // Now we're doing it with `val: &Value`, which means we'll have to clone value a bunch of
    // times, which is also not great.
    //
    // Ideal situation would analyze the pattern beforehand and factor it out in such a way,
    // that we never have to backtrack.
    fn match_(&self, val: &Value) -> PatternMatchResult {
        fn loop_(pattern: &Pattern, val: &Value, mut bindings: Vec<(VariableName, Value)>) -> PatternMatchResult {
            use Pattern::*;
            match (pattern, val) {
                (Variable(var), val) => {
                    bindings.push((var.clone(), val.clone()));
                    Some(bindings)
                },
                (Tagged(tag0, pattern), Value::Tagged(tag1, val)) => {
                    if tag0 == tag1 {
                        loop_(pattern, val, bindings)
                    } else {
                        None
                    }
                },
                (Tuple(patterns), Value::Tuple(values)) => {
                    if patterns.len() == values.len() {
                        for (i, val) in values.into_iter().enumerate() {
                            bindings = loop_(&patterns[i], val, bindings)?
                        }
                        Some(bindings)
                    } else {
                        None
                    }
                },
                _ => None
            }
        }

        loop_(self, val, vec![])
    }
}

impl Expression {
    fn call(fn_name: FunctionName, args: Vec<Self>) -> Self { Self(Rc::new(Expression0::Call(fn_name, args))) }
    fn tagged(tag: Tag, e: Expression) -> Self { Self(Rc::new(Expression0::Tagged(tag, e))) }
    fn tuple(args: Vec<Expression>) -> Self { Self(Rc::new(Expression0::Tuple(args))) }
    fn int(x: i32) -> Self { Self(Rc::new(Expression0::Int(x))) }
    fn operation_application(op_code: OperationCode, e0: Self, e1: Self) -> Self { Self(Rc::new(Expression0::OperationApplication(op_code, e0, e1))) }
    fn match_(arg: Self, branches: Vec<PatternBranch>) -> Self { Self(Rc::new(Expression0::Match { arg, branches })) }
    fn var_use(var: VariableName) -> Self { Self(Rc::new(Expression0::VarUse(var))) }
    fn let_(arg: Self, var: VariableName, body: Self) -> Self { Self(Rc::new(Expression0::Let { arg, var, body })) }
    // TODO: The double Rc<_> here is very suspicious.
    fn object(branches: Vec<PatternBranch>) -> Self { Self(Rc::new(Expression0::Object { branches: Rc::new(branches) })) }
    fn send(obj: Self, msg: Self) -> Self { Self(Rc::new(Expression0::Send(obj, msg))) }
}

#[derive(Debug, PartialEq)]
pub enum OperationCode {
    Add,
    Sub,
    Mul,
    Eq,
}

// ===Values===
#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Tagged(Tag, Box<Value>), // It's interesting that I don't have to do Rc here.
    Tuple(Vec<Value>), // Would be cool if we could use Rc<[Value]>, since we don't need to resize
                           // tuples at runtime.
                           // But it seems you can create an array of only statically known size.
                           // 
                           // Note really sure about the Rc above.
    // TODO: When adding a List as a value, remember to use Rc, since we're cloning values.
    // TODO: Why does the body have to be an Rc?
    //       Can it be a Box? It can't be...
    //       because Values are stored in environments.
    //       And environments have to be clonable.
    //
    //       Hmm, I feel like body has to be in Rc...
    //       since conceptually multiple closures with differing captured environments
    //       could point to the same body expression.
    // Closure { captured_env: Env, var: VariableName, body: Expression },
    // TODO: Are you sure about the Rc<_> in ClosureObject?
    ClosureObject { captured_env: Env, branches: Rc<Vec<PatternBranch>> },
    // But remember that to evaluate closure, we'll have to evaluate the underlying expression (in
    // the captured environment). But `eval_direct` takes ownership of the Expression.
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;
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
            ClosureObject { ..  } => write!(f, "@{{...}}"),
        }
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}


// ===Error===
#[derive(Debug, PartialEq)]
pub enum Error {
    FunctionLookupFailure(FunctionName),
    FunctionCallArityMismatch { fn_name: FunctionName, expected: usize, received: usize },
    VariableLookupFailure(VariableName),
    UnableToFindMatchingPattern,
}

// ===Environment===
#[derive(Debug, Clone)]
pub struct Env(Rc<Env0>);

#[derive(Debug)]
pub enum Env0 {
    Empty,
    Push { var: VariableName, value: Value, parent: Env },
}

impl Env {
    fn new() -> Self {
        Self(Rc::new(Env0::Empty))
    }

    fn get(&self, var_name: VariableName) -> Result<Value, Error> {
        use Env0::*;
        match &*self.0 {
            Empty => Err(Error::VariableLookupFailure(var_name)),
            Push { var, value, parent } => {
                if *var == var_name {
                    Ok(value.clone())
                } else {
                    parent.get(var_name)
                }
            },
        }
    }


    fn extend(self, var_name: VariableName, value: Value) -> Env {
        Self(Rc::new(Env0::Push { var: var_name, value, parent: self }))
    }

    fn extend_many(mut self, bindings: Vec<(VariableName, Value)>) -> Env {
        for (var, val) in bindings {
            self = self.extend(var, val);
        }
        self
    }
}

// ===Evaluation===
pub fn eval_start(program: &Program, e: Expression) -> Result<Value, Error> {
    let value = eval(program, &Env::new(), &e)?;
    Ok(value)
}

fn apply_function(program: &Program, fn_name: FunctionName, arg_values: Vec<Value>) -> Result<Value, Error> {
    let Some(fn_def) = program.get_function_definition(fn_name.clone()) else { return Err(Error::FunctionLookupFailure(fn_name)) };
    let num_of_arguments: usize = fn_def.parameters.len();
    if num_of_arguments != arg_values.len() {
        return Err(Error::FunctionCallArityMismatch { fn_name, expected: num_of_arguments, received: arg_values.len() })
    }
    
    let mut bindings: Vec<(VariableName, Value)> = Vec::with_capacity(num_of_arguments);
    for (i, val) in arg_values.into_iter().enumerate() {
        bindings.push((fn_def.parameters[i].clone(), val));
    }
    eval(program, &Env::new().extend_many(bindings), &fn_def.body)
}

fn apply_msg_to_branches(program: &Program, env: &Env, branches: &[PatternBranch], val: &Value) -> Result<Value, Error> {
    for branch in branches {
        match branch.pattern.match_(val) {
            Some(bindings) => {
                let env = env.clone().extend_many(bindings);
                return eval(program, &env, &branch.body)
            },
            None => {},
        }
    }
    Err(Error::UnableToFindMatchingPattern)
}

// TODO: Why does e have to be passed as a reference? 
//       Seems the root cause was trying to evaluate a closure application
//       where the closure only had a reference to an expression,
//       but to call eval recursively on its body we required owning that body, which we could not.
fn eval(program: &Program, env: &Env, e: &Expression) -> Result<Value, Error> {
    use Expression0::*;
    match &*(e.0) {
        Call(fn_name, args) => {
            let mut values: Vec<Value> = Vec::with_capacity(args.len());
            for arg in args {
                values.push(eval(program, env, &arg)?);
            }
            apply_function(program, fn_name.clone(), values)
        },
        Tagged(tag, e) => {
            let val = eval(program, env, e)?;
            Ok(Value::Tagged(tag.clone(), Box::new(val)))
        },
        Tuple(args) => {
            let mut values: Vec<Value> = Vec::with_capacity((&*args).len());
            for arg in &*args {
                values.push(eval(program, env, arg)?)
            }
            Ok(Value::Tuple(values))
        },
        Int(x) => Ok(Value::Int(*x)),
        OperationApplication(code, e0, e1) => {
            let val0 = eval(program, env, e0)?;
            let val1 = eval(program, env, e1)?;
            match (val0, val1) {
                (Value::Int(x0), Value::Int(x1)) => {
                    use OperationCode::*;
                    match code {
                        Add => Ok(Value::Int(x0 + x1)),
                        Sub => Ok(Value::Int(x0 - x1)),
                        Mul => Ok(Value::Int(x0 * x1)),
                        Eq => if x0 == x1 {
                            Ok(Value::Tagged(Tag::new("T".to_string()), Box::new(Value::Tuple(vec![]))))
                        } else {
                            Ok(Value::Tagged(Tag::new("F".to_string()), Box::new(Value::Tuple(vec![]))))
                        }
                    }
                },
                _ => todo!(),
            }
        },
        Match { arg, branches } => {
            let arg_value = eval(program, env, arg)?;
            apply_msg_to_branches(program, env, branches, &arg_value)
        },
        VarUse(var_name) => env.get(var_name.clone()),
        Let { arg, var, body } => {
            let arg_value = eval(program, env, arg)?;
            let env = env.clone().extend(var.clone(), arg_value);
            eval(program, &env, body)
        },
        Object { branches } => {
            Ok(Value::ClosureObject { captured_env: env.clone(), branches: Rc::clone(branches) })
        },
        Send(e0, e1) => {
            let obj = eval(program, env, e0)?;
            match obj {
                Value::ClosureObject { captured_env, branches } => {
                    let msg = eval(program, env, e1)?;
                    apply_msg_to_branches(program, &captured_env, &branches, &msg)
                },
                _ => todo!(),
            }
        }
    }
}
