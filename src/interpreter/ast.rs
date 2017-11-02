use interpreter::tokenizer::*;
use interpreter::{Memory, Ident, DebugInfo};

use std::collections::HashMap;
use std::cmp::Ordering;
use std::fmt;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Node {
    Noop,
    Ident(Ident),
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Array(Vec<Expr>),
    Return(Box<Expr>),
    Op(Box<Operator>),
    If(Box<If>),
    For(Box<For>),
    While(Box<While>),
    Function(Box<Function>),
    Builtin(Builtin),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Operator {
    Call(Ident, Vec<Expr>),
    Command(String, Vec<Expr>),
    ArrayAccess(Expr, Expr),
    Assign(Ident, Option<Expr>, Expr),
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Mod(Expr, Expr),
    Negate(Expr),
    Not(Expr),
    And(Expr, Expr),
    Or(Expr, Expr),
    Equals(Expr, Expr),
    NotEquals(Expr, Expr),
    Less(Expr, Expr),
    LessOrEquals(Expr, Expr),
    Greater(Expr, Expr),
    GreaterOrEquals(Expr, Expr),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Function {
    pub name: Ident,
    pub args: Vec<Ident>,
    pub body: Vec<Expr>,
}

impl Function {
    pub fn new(name: Ident, args: Vec<Ident>, body: Vec<Expr>) -> Function {
        Function { name, args, body }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct If {
    pub condition: Expr,
    pub true_body: Vec<Expr>,
    pub else_body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct For {
    pub loopvar: Ident,
    pub iterator: Expr,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Expr {
    pub node: Node,
    pub debug: DebugInfo,
}

#[derive(Clone)]
pub struct Builtin(pub fn(memory: &mut Memory) -> Result<Expr, String>);

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<builtin function>")
    }
}

impl fmt::Debug for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<builtin function>")
    }
}

impl PartialEq for Builtin {
    fn eq(&self, other: &Builtin) -> bool {
        self as *const Builtin == other as *const Builtin
    }
}


impl PartialOrd for Builtin {
    fn partial_cmp(&self, _: &Builtin) -> Option<Ordering> {
        None
    }
}

type Tokenizer<'a> = RushTokenizer<'a>;

#[allow(unused)]
impl Expr {
    pub fn new(node: Node, debug: DebugInfo) -> Expr {
        Expr { node, debug }
    }

    // this should really return Result<AST, String>
    pub fn parse(buffer: &str) -> Result<AST, String> {
        let mut ast = AST::new();
        let tokenizer = &mut RushTokenizer::new(buffer);
        let body = parse_exprs(tokenizer, true)?;
        ast.memory.insert(
            Ident("<anonymous>".to_string()),
            Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    body,
                ))),
                debug: DebugInfo::new(buffer, 0),
            },
        );
        Ok(ast)
    }

    pub fn parse_main(buffer: &str) -> Result<Expr, String> {
        Ok(Expr::parse(buffer)?.get_main())
    }

    pub fn parse_one(buffer: &str) -> Result<Expr, String> {
        let tokenizer = &mut RushTokenizer::new(buffer);
        parse_expr(tokenizer)
    }
}

pub struct AST {
    pub memory: HashMap<Ident, Expr>,
}

impl AST {
    fn new() -> AST {
        AST { memory: HashMap::new() }
    }

    /// calls the anonymous outer function that surrounds the result of Expr::parse
    pub fn run(&mut self) -> Result<Expr, String> {
        Expr::new(
            Node::Op(Box::new(
                Operator::Call(Ident("<anonymous>".to_string()), vec![]),
            )),
            DebugInfo::none(),
        ).run(&mut self.memory)
    }

    pub fn get_main(&self) -> Expr {
        self.memory
            .get(&Ident("<anonymous>".to_string()))
            .unwrap()
            .clone()
    }
}

fn parse_exprs(tokenizer: &mut Tokenizer, outermost: bool) -> Result<Vec<Expr>, String> {
    if !outermost {
        tokenizer
            .next()
            .ok_or("Expected '{', reached end of input instead!")?
            .expect_operator_specific("{")?;
    }

    let mut exprs = Vec::new();

    loop {
        let consume;
        if let Some(token) = tokenizer.peek() {
            #[cfg(test)]
            println!("parse_exprs: found token {:?}", token);
            if !outermost && token.expect_operator_specific("}").is_ok() {
                consume = true;
            } else {
                consume = false;
            }
        } else {
            break;
        }
        if consume {
            tokenizer.next();
            return Ok(exprs);
        }
        exprs.push(parse_expr(tokenizer)?)
    }

    if outermost {
        Ok(exprs)
    } else {
        Err("Expected '}', reached end of input instead!")?
    }
}

fn parse_while(tokenizer: &mut Tokenizer, debug: DebugInfo) -> Result<Expr, String> {
    let condition = parse_expr(tokenizer)?;

    let body = parse_exprs(tokenizer, false)?;

    Ok(Expr::new(
        Node::While(Box::new(While { condition, body })),
        debug,
    ))
}

fn parse_for(tokenizer: &mut Tokenizer, debug: DebugInfo) -> Result<Expr, String> {
    let loopvar = tokenizer
        .next()
        .ok_or(
            debug.to_string() + ", reached end of input while trying to parse for loop.",
        )?
        .expect_ident("The loop iterator must be a simple identifier.")?;

    tokenizer
        .next()
        .ok_or(
            debug.to_string() + ", reached end of input while trying to parse for loop",
        )?
        .expect_unknown_specific("in")?;

    let iterator = parse_expr(tokenizer)?;

    let body = parse_exprs(tokenizer, false)?;

    Ok(Expr::new(
        Node::For(Box::new(For {
            loopvar,
            iterator,
            body,
        })),
        debug,
    ))

}

fn parse_if(tokenizer: &mut Tokenizer, debug: DebugInfo) -> Result<Expr, String> {
    #[cfg(test)]
    println!("parse_if");
    let condition = parse_expr(tokenizer)?;

    let true_body = parse_exprs(tokenizer, false)?;

    match tokenizer.peek().map(|token| {
        token.expect_unknown_specific("else")
    }) {
        Some(Ok(_)) => {}
        None | Some(Err(_)) => {
            return Ok(Expr::new(
                Node::If(Box::new(If {
                    condition,
                    true_body,
                    else_body: Vec::new(),
                })),
                debug,
            ))
        }
    }

    tokenizer.next();

    {
        let peek = match tokenizer.peek() {
            Some(token) => token,
            None => {
                return Ok(Expr::new(
                    Node::If(Box::new(If {
                        condition,
                        true_body,
                        else_body: Vec::new(),
                    })),
                    debug,
                ))
            }
        };
        match *peek {
            Token::Operator(ref op, _) => {
                if op != "{" {
                    return Ok(Expr::new(
                        Node::If(Box::new(If {
                            condition,
                            true_body,
                            else_body: Vec::new(),
                        })),
                        debug,
                    ));
                }
            }
            _ => {
                return Ok(Expr::new(
                    Node::If(Box::new(If {
                        condition,
                        true_body,
                        else_body: Vec::new(),
                    })),
                    debug,
                ));
            }
        }
    }

    let else_body = parse_exprs(tokenizer, false)?;

    Ok(Expr::new(
        Node::If(Box::new(If {
            condition,
            true_body,
            else_body,
        })),
        debug,
    ))
}

fn parse_fn(tokenizer: &mut Tokenizer, debug: DebugInfo) -> Result<Expr, String> {
    let name = tokenizer
        .next()
        .ok_or(
            debug.to_string() + ", reached end of input while trying to parse function declaration.",
        )?
        .expect_ident("The function name must be a simple identifier.")?;

    tokenizer
        .next()
        .ok_or(
            debug.to_string() + ", reached end of input while trying to parse function declaration",
        )?
        .expect_operator_specific("(")?;

    let mut args = Vec::new();
    loop {
        let arg = match tokenizer.next() {
            Some(Token::Ident(id, _)) => id,
            Some(Token::Operator(ref op, _)) if op == ")" => break,
            None => {
                Err(
                    debug.to_string() +
                        ", reached end of input while trying to parse function declaration",
                )?
            }
            _ => {
                Err(
                    debug.to_string() + ", each function arg must be a simple identifier.",
                )?
            }
        };

        args.push(arg);

        match tokenizer.next() {
            Some(Token::Operator(ref op, _)) if op == ")" => break,
            Some(Token::Operator(ref op, _)) if op == "," => {}
            _ => {
                Err(
                    debug.to_string() + ", expected ')' or ',' while parsing function arguments.",
                )?
            }
        }
    }

    let body = parse_exprs(tokenizer, false)?;

    Ok(Expr::new(
        Node::Function(Box::new(Function { name, args, body })),
        debug,
    ))
}

fn parse_fn_call(primary_expr: Expr, tokenizer: &mut Tokenizer) -> Result<Expr, String> {
    #[cfg(test)]
    println!("parse_fn_call");
    let fn_name = match primary_expr.node {
        Node::Ident(id) => id,
        _ => Err("The function name must be a simple identifier.")?,
    };

    let debug = primary_expr.debug;

    tokenizer
        .next()
        .ok_or(
            debug.to_string() + ", reached end of input while trying to parse function call",
        )?
        .expect_operator_specific("(")?;

    let mut args = Vec::new();

    let has_args = match tokenizer.peek().ok_or(
        debug.to_string() +
            ", reached end of input while trying to parse function call",
    )? {
        &Token::Operator(ref op, _) if op == ")" => false,
        _ => true,
    };

    if has_args {
        loop {
            let expr = parse_expr(tokenizer)?;
            args.push(expr);

            match tokenizer.next().ok_or(
                debug.to_string() +
                    ", reached end of input while trying to parse function call",
            )? {
                Token::Operator(ref op, _) if op == "," => {}
                Token::Operator(ref op, _) if op == ")" => break,
                token => {
                    Err(
                        debug.to_string() + ", while parsing function call, " +
                            &token.get_debug_info().to_string(),
                    )?
                }
            }
        }
    } else {
        tokenizer.next();
    }

    Ok(Expr::new(
        Node::Op(Box::new(Operator::Call(fn_name, args))),
        debug,
    ))
}

fn parse_cmd(cmd: String, tokenizer: &mut Tokenizer, debug: DebugInfo) -> Result<Expr, String> {
    #[cfg(test)]
    println!("parse_cmd: {}", cmd);
    let mut args = Vec::new();

    loop {
        let next = tokenizer.next_basic().ok_or(
            debug.to_string() +
                ", reached end of input while trying to parse command expression",
        )?;
        let expr = match next {
            Token::Str(ref str_val, _) if str_val == "#" => break,
            Token::Str(ref str_val, ref dbg)
                if str_val.starts_with("$") && str_val.contains("[") => {
                Err(
                    dbg.to_string() + ", array indexing is not supported in command calls yet.",
                )?
            }
            Token::Str(str_val, dbg) => Expr::new(Node::Str(str_val), dbg),
            Token::Ident(id, dbg) => Expr::new(Node::Ident(id), dbg),
            val => {
                Err(
                    val.get_debug_info().to_string() +
                        ", found impossible token type while parsing command expression",
                )?
            }
        };
        args.push(expr);
    }

    Ok(Expr::new(
        Node::Op(Box::new(Operator::Command(cmd, args))),
        debug,
    ))
}

fn parse_primary(tokenizer: &mut Tokenizer) -> Result<Expr, String> {
    let token = tokenizer.next().ok_or(
        "Reached end of input while trying to parse_primary",
    )?;

    #[cfg(test)]
    println!("parse_primary: {:?}", token);

    Ok(match token {
        Token::Ident(id, debug) => Expr::new(Node::Ident(id), debug),
        Token::Int(int, debug) => Expr::new(Node::Int(int), debug),
        Token::Float(float, debug) => Expr::new(Node::Float(float), debug),
        Token::Str(str_val, debug) => Expr::new(Node::Str(str_val), debug),
        Token::Bool(bool_val, debug) => Expr::new(Node::Bool(bool_val), debug),
        Token::Operator(op, debug) => {
            if op == ";" {
                Expr::new(Node::Noop, debug)
            } else if op == "(" {
                parse_parenthetic_expr(&op, &debug, tokenizer)?
            } else if op == "[" {
                parse_arr(&op, debug, tokenizer)?
            } else {
                parse_unary_operator(&op, debug, tokenizer)?
            }
        }
        Token::Unknown(unknown, debug) => {
            match unknown.as_str() {
                "while" => parse_while(tokenizer, debug)?,
                "for" => parse_for(tokenizer, debug)?,
                "if" => parse_if(tokenizer, debug)?,
                "fn" => parse_fn(tokenizer, debug)?,
                "return" => Expr::new(Node::Return(Box::new(parse_expr(tokenizer)?)), debug),
                _ => parse_cmd(unknown, tokenizer, debug)?,
            }
        }
    })
}

fn parse_expr(tokenizer: &mut Tokenizer) -> Result<Expr, String> {
    let primary_expr = parse_primary(tokenizer)?;

    #[cfg(test)]
    println!("parse_expr");

    {
        let peek = match tokenizer.peek() {
            Some(token) => token,
            None => return Ok(primary_expr),
        };
        match *peek {
            Token::Operator(ref op, _) => {
                if op == ")" || op == "!" || op == ";" || op == "{" || op == "}" || op == "," ||
                    op == "]"
                {
                    return Ok(primary_expr);
                }
            }
            Token::Unknown(ref val, _) if val == "#" => return Ok(primary_expr),
            _ => {
                return Ok(primary_expr);
            }
        }
    }

    let expr = parse_binary_operator(0, primary_expr, tokenizer)?;
    check_for_end_of_expr(tokenizer)?;
    Ok(expr)
}

fn parse_unary_operator(
    op: &str,
    debug: DebugInfo,
    tokenizer: &mut Tokenizer,
) -> Result<Expr, String> {

    #[cfg(test)]
    println!("parse_unary: {:?}, {:?}", op, debug);

    if !(op == "!" || op == "-") {
        return Err(
            debug.to_string() + ", but there is no expression preceding it and '" + op +
                "' is not a unary operator!",
        )?;
    }

    tokenizer.peek().ok_or(
        debug.to_string() + ", then encountered end of input! '" + op +
            "' must be followed by an expression.",
    )?;

    let next_expr = parse_expr(tokenizer)?;

    let operator = match op.as_ref() {
        "!" => Operator::Not(next_expr),
        "-" => Operator::Negate(next_expr),
        _ => {
            return Err(
                debug.to_string() +
                    ", which caused Rush to experience an internal error! Please report this!",
            )?
        }
    };

    let expr = Expr::new(Node::Op(Box::new(operator)), debug);

    Ok(expr)
}

fn op_precendence(op: &str) -> i32 {
    match op {
        "=" => 0,
        "+" => 20,
        "-" => 30,
        "*" => 40,
        "/" | "%" => 50,
        "(" => 60,
        "[" => 1,
        "||" => 5,
        "==" | "!=" | "&&" => 10,
        "<" | ">" | "<=" | ">=" => 15,
        _ => -1,
    }
}

fn operator_expr_from(
    op: &str,
    debug: &DebugInfo,
    first_expr: Expr,
    next_expr: Expr,
    tokenizer: &mut Tokenizer,
) -> Result<Expr, String> {
    Ok(Expr::new(
        Node::Op(Box::new(match op {
            "+" => Operator::Add(first_expr, next_expr),
            "-" => Operator::Sub(first_expr, next_expr),
            "*" => Operator::Mul(first_expr, next_expr),
            "/" => Operator::Div(first_expr, next_expr),
            "%" => Operator::Mod(first_expr, next_expr),
            "&&" => Operator::And(first_expr, next_expr),
            "||" => Operator::Or(first_expr, next_expr),
            "==" => Operator::Equals(first_expr, next_expr),
            "!=" => Operator::NotEquals(first_expr, next_expr),
            "<" => Operator::Less(first_expr, next_expr),
            "<=" => Operator::LessOrEquals(first_expr, next_expr),
            ">" => Operator::Greater(first_expr, next_expr),
            ">=" => Operator::GreaterOrEquals(first_expr, next_expr),
            "=" => {
                match first_expr.node {
                    Node::Ident(id) => Operator::Assign(id, None, next_expr),
                    Node::Op(ref operator) => {
                        match operator.as_ref() {
                            &Operator::ArrayAccess(ref arr, ref idx) => {
                                match arr.node {
                                    Node::Ident(ref arr_id) => {
                                        Operator::Assign(
                                            arr_id.clone(),
                                            Some(idx.clone()),
                                            next_expr,
                                        )
                                    }
                                    _ => unimplemented!(),
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                    _ => {
                        Err(
                            first_expr.debug.to_string() +
                                "; left-hand side of assignment must be an identifier or array slot.",
                        )?
                    }
                }
            }
            "[" => {
                tokenizer
                    .next()
                    .ok_or(
                        debug.to_string() +
                            ", encountered end of input while parsing array access operator",
                    )?
                    .expect_operator_specific("]")?;
                let assignment = match tokenizer.peek().ok_or(
                    debug.to_string() +
                        ", encountered end of input while parsing array access operator",
                )? {
                    &Token::Operator(ref op, _) if op == "=" => true,
                    _ => false,
                };
                let res = Operator::ArrayAccess(first_expr, next_expr);
                if assignment {
                    let res = Expr::new(Node::Op(Box::new(res)), debug.clone());
                    return parse_binary_operator(op_precendence("["), res, tokenizer);
                } else {
                    res
                }
            }
            _ => {
                return Err(
                    debug.to_string() +
                        ", which caused Rush to experience an internal error! Please report this!",
                )?
            }
        })),
        debug.clone(),
    ))
}

fn parse_binary_operator(
    last_precedence: i32,
    first_expr: Expr,
    tokenizer: &mut Tokenizer,
) -> Result<Expr, String> {

    #[cfg(test)]
    println!("parse_binary");

    let next_token = tokenizer
        .peek()
        .ok_or("Expected operator, found end of input")?
        .clone();

    let (first_expr, op, debug) = match next_token {
        Token::Operator(ref op, _) if op == "(" => {
            let first_expr = parse_fn_call(first_expr, tokenizer)?;
            let next_token = tokenizer
                .peek()
                .ok_or("Expected operator, found end of input")?
                .clone();
            match next_token {
                Token::Operator(ref op, _) if op == ")" => return Ok(first_expr),
                Token::Operator(op, debug) => (first_expr, op, debug),
                token => Err(token.get_debug_info().to_string() + ", expected operator.")?,
            }
        }
        Token::Operator(op, debug) => (first_expr, op, debug),
        _ => unimplemented!(),
    };

    tokenizer.next();

    if op == "!" {
        return Err(
            debug.to_string() + ", but there is an expression preceding it and '" + &op +
                "' is not a binary operator!",
        )?;
    } else if op == ";" {
        return Ok(first_expr);
    }

    let precendence = op_precendence(&op);

    #[cfg(test)]
    println!(
        "parse_binary: {:?}, {:?}, {} < {}",
        op,
        debug,
        precendence,
        last_precedence
    );

    tokenizer.peek().ok_or(
        debug.to_string() + ", then encountered end of input! '" + &op +
            "' must be followed by an expression.",
    )?;

    let next_expr = parse_primary(tokenizer)?;

    if precendence < last_precedence {
        return Ok(operator_expr_from(
            &op,
            &debug,
            first_expr,
            next_expr,
            tokenizer,
        )?);
    }

    let mut next_precendence = {
        match tokenizer.peek() {
            Some(token) => {
                match *token {
                    Token::Operator(ref op, _) => op_precendence(op),
                    _ => -1,
                }
            }
            None => -1,
        }
    };

    #[cfg(test)]
    println!("precedence: {} < {}", precendence, next_precendence);

    let next_expr = if precendence < next_precendence {
        next_precendence = -1;
        parse_binary_operator(precendence + 1, next_expr, tokenizer)?
    } else {
        next_expr
    };

    let expr = operator_expr_from(&op, &debug, first_expr, next_expr, tokenizer)?;

    if next_precendence != -1 {
        parse_binary_operator(precendence, expr, tokenizer)
    } else {
        Ok(expr)
    }
}

fn parse_arr(op: &str, debug: DebugInfo, tokenizer: &mut Tokenizer) -> Result<Expr, String> {
    if op != "[" {
        panic!("bad parse_arr");
    }

    let mut vals = Vec::new();

    loop {
        let expr = parse_expr(tokenizer)?;
        vals.push(expr);
        match tokenizer.next().ok_or(
            debug.to_string() +
                ", reached end of input while trying to parse array literal",
        )? {
            Token::Operator(ref op, _) if op == "," => {}
            Token::Operator(ref op, _) if op == "]" => break,
            token => {
                Err(
                    debug.to_string() + ", while parsing array literal, " +
                        &token.get_debug_info().to_string(),
                )?
            }
        }
    }

    Ok(Expr::new(Node::Array(vals), debug))
}

fn parse_parenthetic_expr(
    op: &str,
    debug: &DebugInfo,
    tokenizer: &mut Tokenizer,
) -> Result<Expr, String> {

    #[cfg(test)]
    println!("parse_parenthetic: {:?}, {:?}", op, debug);

    if op != "(" {
        return Err(
            debug.to_string() +
                ", expected '(', which caused Rush to experience an internal error!" +
                " Please report this!",
        )?;
    }

    tokenizer.peek().ok_or(
        debug.to_string() +
            ", then encountered end of input! '(' must be followed by an expression.",
    )?;

    let expr = parse_expr(tokenizer)?;

    tokenizer
        .next()
        .ok_or(
            expr.debug.to_string() +
                ", then encountered end of input! A closing parenthesis was missing.",
        )?
        .expect_operator_specific(")")
        .map_err(|mut err| {
            err.push_str(", which has caused an internal error! Please report this!");
            err
        })?;

    #[cfg(test)]
    println!("returning from parenthetic");

    Ok(expr)
}

fn check_for_end_of_expr(tokenizer: &mut Tokenizer) -> Result<(), String> {
    // this function currently just consumes a semicolon, if semicolon is the next token.
    // it used to do more stuff.

    let fake_semicolon = Token::Operator(";".to_string(), DebugInfo::new(";".to_string(), 0));

    let op = match tokenizer.peek().unwrap_or_else(|| &fake_semicolon) {
        &Token::Operator(ref op, _) if op == ";" => op.clone(),
        _ => String::from(""),
    };
    // use up the semicolon if it is there
    if op == ";" {
        tokenizer.next();
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_factorial() {
        println!(
            "{:#?}",
            Expr::parse_main(
                r#"
            fn $factorial($n) {
                if $n == 1 {
                    return 1;
                }
                return $n * $factorial($n - 1);
            }

            return $factorial($int($arg[0]));
        "#,
            ).unwrap()
        );
    }

    #[test]
    fn parse_inline_command() {
        println!(
            "{:#?}",
            Expr::parse_main(
                r#"
$echo("hello test");
        "#,
            ).unwrap()
        );
    }

    #[test]
    fn parse_command() {
        let expr = Expr::parse_main("df -kh $var . #");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Command(
                                "df".to_string(),
                                vec![
                                    Expr {
                                        node: Node::Str("-kh".to_string()),
                                        debug: DebugInfo::new("-kh", 3),
                                    },
                                    Expr {
                                        node: Node::Ident(Ident("$var".to_string())),
                                        debug: DebugInfo::new("$var", 7),
                                    },
                                    Expr {
                                        node: Node::Str(".".to_string()),
                                        debug: DebugInfo::new(".", 12),
                                    },
                                ],
                            ))),
                            debug: DebugInfo::new("df", 0),
                        },
                    ],
                ))),
                debug: DebugInfo::new("df -kh $var . #", 0),
            })
        );
    }

    #[test]
    fn parse_negate() {
        let expr = Expr::parse_main("!$someBool");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Not(Expr {
                                node: Node::Ident(Ident("$someBool".to_string())),
                                debug: DebugInfo::new("$someBool", 1),
                            }))),
                            debug: DebugInfo::new("!", 0),
                        },
                    ],
                ))),
                debug: DebugInfo::new("!$someBool", 0),
            })
        );
    }

    #[test]
    fn parse_assign() {
        let expr = Expr::parse_main("$someInt = $otherInt");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Assign(
                                Ident("$someInt".to_string()),
                                None,
                                Expr {
                                    node: Node::Ident(Ident("$otherInt".to_string())),
                                    debug: DebugInfo::new("$otherInt", 11),
                                },
                            ))),
                            debug: DebugInfo::new("=", 9),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$someInt = $otherInt", 0),
            })
        );
    }

    #[test]
    fn parse_array_assign() {
        let expr = Expr::parse_main("$someInt[3] = $otherInt");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Assign(
                                Ident("$someInt".to_string()),
                                Some(Expr {
                                    node: Node::Int(3),
                                    debug: DebugInfo::new("3", 9),
                                }),
                                Expr {
                                    node: Node::Ident(Ident("$otherInt".to_string())),
                                    debug: DebugInfo::new("$otherInt", 14),
                                },
                            ))),
                            debug: DebugInfo::new("=", 12),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$someInt[3] = $otherInt", 0),
            })
        );
    }

    #[test]
    fn parse_array() {
        let expr = Expr::parse_main("$x = [3, 2, 4]; return $x[0];");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Assign(
                                Ident("$x".to_string()),
                                None,
                                Expr {
                                    node: Node::Array(vec![
                                        Expr {
                                            node: Node::Int(3),
                                            debug: DebugInfo::new("3", 6),
                                        },
                                        Expr {
                                            node: Node::Int(2),
                                            debug: DebugInfo::new("2", 9),
                                        },
                                        Expr {
                                            node: Node::Int(4),
                                            debug: DebugInfo::new("4", 12),
                                        },
                                    ]),
                                    debug: DebugInfo::new("[", 5),
                                },
                            ))),
                            debug: DebugInfo::new("=", 3),
                        },
                        Expr {
                            node: Node::Return(Box::new(Expr {
                                node: Node::Op(Box::new(Operator::ArrayAccess(
                                    Expr {
                                        node: Node::Ident(Ident("$x".to_string())),
                                        debug: DebugInfo::new("$x", 23),
                                    },
                                    Expr {
                                        node: Node::Int(0),
                                        debug: DebugInfo::new("0", 26),
                                    },
                                ))),
                                debug: DebugInfo::new("[", 25),
                            })),
                            debug: DebugInfo::new("return", 16),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$x = [3, 2, 4]; return $x[0];", 0),
            })
        );
    }

    #[test]
    fn parse_function() {
        let expr = Expr::parse_main("fn $test($otherInt) { $someInt = $otherInt; }");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Function(Box::new(Function::new(
                                Ident(String::from("$test")),
                                vec![Ident("$otherInt".to_string())],
                                vec![
                                    Expr {
                                        node: Node::Op(Box::new(Operator::Assign(
                                            Ident("$someInt".to_string()),
                                            None,
                                            Expr {
                                                node: Node::Ident(Ident("$otherInt".to_string())),
                                                debug: DebugInfo::new("$otherInt", 33),
                                            },
                                        ))),
                                        debug: DebugInfo::new("=", 31),
                                    },
                                ],
                            ))),
                            debug: DebugInfo::new("fn", 0),
                        },
                    ],
                ))),
                debug: DebugInfo::new("fn $test($otherInt) { $someInt = $otherInt; }", 0),
            })
        );
    }


    #[test]
    fn parse_for() {
        let expr = Expr::parse_main("for $otherInt in $array { $someInt = $otherInt }");
        assert_eq!(
            expr,
            Ok(Expr {
                debug: DebugInfo::new("for $otherInt in $array { $someInt = $otherInt }", 0),
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            debug: DebugInfo::new("for", 0),
                            node: Node::For(Box::new(For {
                                loopvar: Ident("$otherInt".to_string()),
                                iterator: Expr {
                                    debug: DebugInfo::new("$array", 17),
                                    node: Node::Ident(Ident("$array".to_string())),
                                },
                                body: vec![
                                    Expr {
                                        debug: DebugInfo::new("=", 35),
                                        node: Node::Op(Box::new(Operator::Assign(
                                            Ident("$someInt".to_string()),
                                            None,
                                            Expr {
                                                debug: DebugInfo::new("$otherInt", 37),
                                                node: Node::Ident(Ident("$otherInt".to_string())),
                                            },
                                        ))),
                                    },
                                ],
                            })),
                        },
                    ],
                ))),
            })
        );
    }


    #[test]
    fn parse_while() {
        let expr = Expr::parse_main("while $otherInt == $something { $otherInt = $someInt }");
        assert_eq!(
            expr,
            Ok(Expr {
                debug: DebugInfo::new("while $otherInt == $something { $otherInt = $someInt }", 0),
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            debug: DebugInfo::new("while", 0),
                            node: Node::While(Box::new(While {
                                condition: Expr {
                                    node: Node::Op(Box::new(Operator::Equals(
                                        Expr {
                                            node: Node::Ident(Ident("$otherInt".to_string())),
                                            debug: DebugInfo::new("$otherInt", 6),
                                        },
                                        Expr {
                                            node: Node::Ident(Ident("$something".to_string())),
                                            debug: DebugInfo::new("$something", 19),
                                        },
                                    ))),
                                    debug: DebugInfo::new("==", 16),
                                },
                                body: vec![
                                    Expr {
                                        debug: DebugInfo::new("=", 42),
                                        node: Node::Op(Box::new(Operator::Assign(
                                            Ident("$otherInt".to_string()),
                                            None,
                                            Expr {
                                                debug: DebugInfo::new("$someInt", 44),
                                                node: Node::Ident(Ident("$someInt".to_string())),
                                            },
                                        ))),
                                    },
                                ],
                            })),
                        },
                    ],
                ))),
            })
        );
    }


    #[test]
    fn parse_if() {
        let expr = Expr::parse_main(
            "if $otherInt == 4 { $someInt = $otherInt } else { $someInt = 3 }",
        );
        assert_eq!(
            expr,
            Ok(Expr {
                debug: DebugInfo::new(
                    "if $otherInt == 4 { $someInt = $otherInt } else { $someInt = 3 }",
                    0,
                ),
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            debug: DebugInfo::new("if", 0),
                            node: Node::If(Box::new(If {
                                condition: Expr {
                                    node: Node::Op(Box::new(Operator::Equals(
                                        Expr {
                                            node: Node::Ident(Ident("$otherInt".to_string())),
                                            debug: DebugInfo::new("$otherInt", 3),
                                        },
                                        Expr {
                                            node: Node::Int(4),
                                            debug: DebugInfo::new("4", 16),
                                        },
                                    ))),
                                    debug: DebugInfo::new("==", 13),
                                },
                                true_body: vec![
                                    Expr {
                                        debug: DebugInfo::new("=", 29),
                                        node: Node::Op(Box::new(Operator::Assign(
                                            Ident("$someInt".to_string()),
                                            None,
                                            Expr {
                                                debug: DebugInfo::new("$otherInt", 31),
                                                node: Node::Ident(Ident("$otherInt".to_string())),
                                            },
                                        ))),
                                    },
                                ],
                                else_body: vec![
                                    Expr {
                                        debug: DebugInfo::new("=", 59),
                                        node: Node::Op(Box::new(Operator::Assign(
                                            Ident("$someInt".to_string()),
                                            None,
                                            Expr {
                                                node: Node::Int(3),
                                                debug: DebugInfo::new("3", 61),
                                            },
                                        ))),
                                    },
                                ],
                            })),
                        },
                    ],
                ))),
            })
        );
    }


    #[test]
    fn parse_add() {
        let expr = Expr::parse_main("$someInt + $otherInt");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Add(
                                Expr {
                                    node: Node::Ident(Ident("$someInt".to_string())),
                                    debug: DebugInfo::new("$someInt", 0),
                                },
                                Expr {
                                    node: Node::Ident(Ident("$otherInt".to_string())),
                                    debug: DebugInfo::new("$otherInt", 11),
                                },
                            ))),
                            debug: DebugInfo::new("+", 9),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$someInt + $otherInt", 0),
            })
        );
    }

    #[test]
    fn parse_add2() {
        let expr = Expr::parse_main("$someInt + $otherInt; $someInt + $otherInt;");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Add(
                                Expr {
                                    node: Node::Ident(Ident("$someInt".to_string())),
                                    debug: DebugInfo::new("$someInt", 0),
                                },
                                Expr {
                                    node: Node::Ident(Ident("$otherInt".to_string())),
                                    debug: DebugInfo::new("$otherInt", 11),
                                },
                            ))),
                            debug: DebugInfo::new("+", 9),
                        },
                        Expr {
                            node: Node::Op(Box::new(Operator::Add(
                                Expr {
                                    node: Node::Ident(Ident("$someInt".to_string())),
                                    debug: DebugInfo::new("$someInt", 22),
                                },
                                Expr {
                                    node: Node::Ident(Ident("$otherInt".to_string())),
                                    debug: DebugInfo::new("$otherInt", 33),
                                },
                            ))),
                            debug: DebugInfo::new("+", 31),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$someInt + $otherInt; $someInt + $otherInt;", 0),
            })
        );
    }

    #[test]
    fn parse_sub_add() {
        let expr = Expr::parse_main("$someInt - $otherInt + $neat");
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Add(
                                Expr {
                                    node: Node::Op(Box::new(Operator::Sub(
                                        Expr {
                                            node: Node::Ident(Ident("$someInt".to_string())),
                                            debug: DebugInfo::new("$someInt", 0),
                                        },
                                        Expr {
                                            node: Node::Ident(Ident("$otherInt".to_string())),
                                            debug: DebugInfo::new("$otherInt", 11),
                                        },
                                    ))),
                                    debug: DebugInfo::new("-", 9),
                                },
                                Expr {
                                    node: Node::Ident(Ident("$neat".to_string())),
                                    debug: DebugInfo::new("$neat", 23),
                                },
                            ))),
                            debug: DebugInfo::new("+", 21),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$someInt - $otherInt + $neat", 0),
            })
        );
    }

    #[test]
    fn parse_parenthetic() {
        let expr = Expr::parse_main("$someInt + ($otherInt - $coolInt) / $neat");
        let div = Expr {
            node: Node::Op(Box::new(Operator::Div(
                Expr {
                    node: Node::Op(Box::new(Operator::Sub(
                        Expr {
                            node: Node::Ident(Ident("$otherInt".to_string())),
                            debug: DebugInfo::new("$otherInt", 12),
                        },
                        Expr {
                            node: Node::Ident(Ident("$coolInt".to_string())),
                            debug: DebugInfo::new("$coolInt", 24),
                        },
                    ))),
                    debug: DebugInfo::new("-", 22),
                },
                Expr {
                    node: Node::Ident(Ident("$neat".to_string())),
                    debug: DebugInfo::new("$neat", 36),
                },
            ))),
            debug: DebugInfo::new("/", 34),
        };
        assert_eq!(
            expr,
            Ok(Expr {
                node: Node::Function(Box::new(Function::new(
                    Ident(String::from("<anonymous>")),
                    Vec::new(),
                    vec![
                        Expr {
                            node: Node::Op(Box::new(Operator::Add(
                                Expr {
                                    node: Node::Ident(Ident("$someInt".to_string())),
                                    debug: DebugInfo::new("$someInt", 0),
                                },
                                div,
                            ))),
                            debug: DebugInfo::new("+", 9),
                        },
                    ],
                ))),
                debug: DebugInfo::new("$someInt + ($otherInt - $coolInt) / $neat", 0),
            })
        );
    }
}
