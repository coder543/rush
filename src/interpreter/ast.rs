#![allow(unused)]

use interpreter::tokenizer::*;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Node {
    Noop,
    Ident(Ident),
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Op(Box<Operator>),
    For(Box<For>),
    While(Box<While>),
    Function(Box<Function>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Operator {
    Call(Ident, Vec<Expr>),
    Assign(Ident, Expr),
    Add(Expr, Expr),
    Sub(Expr, Expr),
    Mul(Expr, Expr),
    Div(Expr, Expr),
    Negate(Expr),
    Not(Expr),
    And(Expr, Expr),
    Or(Expr, Expr),
    Equals(Expr, Expr),
    NotEquals(Expr, Expr),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Function {
    name: Ident,
    args: Vec<Ident>,
    body: Vec<Expr>,
}

impl Function {
    pub fn new(name: Ident, args: Vec<Ident>, body: Vec<Expr>) -> Function {
        Function { name, args, body }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct For {
    loopvar: Ident,
    iterator: Expr,
    body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct While {
    condition: Expr,
    body: Vec<Expr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Expr {
    node: Node,
    debug: DebugInfo,
}

use std::iter::Peekable;
type Tokenizer<'a> = Peekable<RushTokenizer<'a>>;

impl Expr {
    pub fn new(node: Node, debug: DebugInfo) -> Expr {
        Expr { node, debug }
    }

    pub fn parse(buffer: &str) -> Result<Expr, String> {
        let tokenizer = &mut RushTokenizer::new(buffer).peekable();
        let body = parse_exprs(tokenizer, true)?;
        Ok(Expr {
            node: Node::Function(Box::new(Function::new(
                Ident(String::from("<anonymous>")),
                Vec::new(),
                body,
            ))),
            debug: DebugInfo::new(buffer, 0),
        })
    }
}

fn parse_exprs(mut tokenizer: &mut Tokenizer, outermost: bool) -> Result<Vec<Expr>, String> {
    if !outermost {
        tokenizer
            .next()
            .ok_or("Expected '{', reached end of input instead!")?
            .expect_operator_specific("{")?;
    }

    let mut exprs = Vec::new();

    loop {
        if let Some(token) = tokenizer.peek() {
            if !outermost {
                if token.expect_operator_specific("}").is_ok() {
                    return Ok(exprs);
                }
            }
        } else {
            break;
        }
        exprs.push(parse_expr(tokenizer)?)
    }

    if outermost {
        Ok(exprs)
    } else {
        Err("Expected '}', reached end of input instead!")?
    }
}

fn parse_primary(tokenizer: &mut Tokenizer) -> Result<Expr, String> {
    let token = tokenizer.next().ok_or(
        "Reached end of input while trying to parse_primary",
    )?;
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
            } else if op != "(" {
                parse_unary_operator(op, debug, tokenizer)?
            } else {
                parse_parenthetic_expr(op, debug, tokenizer)?
            }
        }
        _ => return Err("Unexpected token")?,
    })
}

fn parse_expr(tokenizer: &mut Tokenizer) -> Result<Expr, String> {
    let primary_expr = parse_primary(tokenizer)?;

    println!("parse_expr");

    {
        let peek = match tokenizer.peek() {
            Some(token) => token,
            None => return Ok(primary_expr),
        };
        match peek {
            &Token::Operator(ref op, _) => {
                if op == ")" || op == "(" || op == "!" || op == ";" {
                    return Ok(primary_expr);
                }
            }
            _ => {
                return Ok(primary_expr);
            }
        }
    }

    let expr = parse_binary_operator(0, primary_expr, tokenizer)?;

    Ok(expr)
}

fn parse_unary_operator(
    op: String,
    debug: DebugInfo,
    tokenizer: &mut Tokenizer,
) -> Result<Expr, String> {

    println!("parse_unary: {:?}, {:?}", op, debug);

    if !(op == "!" || op == "-") {
        return Err(
            debug.to_string() + ", but there is no expression preceding it and '" + &op +
                "' is not a unary operator!",
        )?;
    }

    tokenizer.peek().ok_or(
        debug.to_string() + ", then encountered end of input! '" + &op +
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
        "+" => 10,
        "-" => 20,
        "*" => 30,
        "/" => 40,
        _ => -1,
    }
}

fn operator_expr_from(
    op: &str,
    debug: &DebugInfo,
    first_expr: Expr,
    next_expr: Expr,
) -> Result<Expr, String> {
    Ok(Expr::new(
        Node::Op(Box::new(match op {
            "+" => Operator::Add(first_expr, next_expr),
            "-" => Operator::Sub(first_expr, next_expr),
            "*" => Operator::Mul(first_expr, next_expr),
            "/" => Operator::Div(first_expr, next_expr),
            "&&" => Operator::And(first_expr, next_expr),
            "||" => Operator::Or(first_expr, next_expr),
            "==" => Operator::Equals(first_expr, next_expr),
            "!=" => Operator::NotEquals(first_expr, next_expr),
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

    println!("parse_binary");

    let next_token = tokenizer.next().ok_or(
        "Expected operator, found end of input",
    )?;

    let (op, debug) = match next_token {
        Token::Operator(op, debug) => (op, debug),
        _ => unimplemented!(),
    };

    if op == "!" {
        return Err(
            debug.to_string() + ", but there is an expression preceding it and '" + &op +
                "' is not a binary operator!",
        )?;
    }

    let precendence = op_precendence(&op);

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
        return Ok(operator_expr_from(&op, &debug, first_expr, next_expr)?);
    }

    let mut next_precendence = {
        match tokenizer.peek() {
            Some(token) => {
                match token {
                    &Token::Operator(ref op, _) => op_precendence(op),
                    _ => -1,
                }
            }
            None => -1,
        }
    };

    println!("precedence: {} < {}", precendence, next_precendence);

    let next_expr = if precendence < next_precendence {
        next_precendence = -1;
        parse_binary_operator(precendence + 1, next_expr, tokenizer)?
    } else {
        next_expr
    };

    let expr = operator_expr_from(&op, &debug, first_expr, next_expr)?;

    if next_precendence != -1 {
        parse_binary_operator(precendence, expr, tokenizer)
    } else {
        Ok(expr)
    }
}

fn parse_parenthetic_expr(
    op: String,
    debug: DebugInfo,
    tokenizer: &mut Tokenizer,
) -> Result<Expr, String> {

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

    println!("returning from parenthetic");

    Ok(expr)
}

fn check_for_semicolon(tokenizer: &mut Tokenizer) -> Result<(), String> {
    // ensure that there is a semicolon at the end of this expression
    // or, ensure that this is the final expression,
    // by providing a fake semicolon if no token is returned at all
    tokenizer
        .next()
        .or(Some(Token::Operator(
            ";".to_string(),
            DebugInfo::new(";".to_string(), 0),
        )))
        .unwrap()
        .expect_operator_specific(";")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_negate() {
        let expr = Expr::parse("!$someBool");
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
    fn parse_add() {
        let expr = Expr::parse("$someInt + $otherInt");
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
    fn parse_sub_add() {
        let expr = Expr::parse("$someInt - $otherInt + $neat");
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
        let expr = Expr::parse("$someInt + ($otherInt - $coolInt) / $neat");
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
