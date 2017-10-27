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

impl Expr {
    pub fn new(node: Node, debug: DebugInfo) -> Expr {
        Expr { node, debug }
    }

    pub fn parse(buffer: &str) -> Result<Expr, String> {
        let tokenizer = &mut RushTokenizer::new(buffer);
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

fn parse_exprs(mut tokenizer: &mut RushTokenizer, outermost: bool) -> Result<Vec<Expr>, String> {
    if !outermost {
        tokenizer
            .next()
            .ok_or("Expected '{', reached end of input instead!")?
            .expect_operator_specific("{")?;
    }

    let mut exprs = Vec::new();

    while let Some(token) = tokenizer.next() {
        if !outermost {
            if token.expect_operator_specific("}").is_ok() {
                return Ok(exprs);
            }
        }
        exprs.push(parse_expr(token, tokenizer)?)
    }

    if outermost {
        Ok(exprs)
    } else {
        Err("Expected '}', reached end of input instead!")?
    }
}

fn parse_expr(first_token: Token, tokenizer: &mut RushTokenizer) -> Result<Expr, String> {

    let first_expr = match first_token {
        Token::Operator(op, debug) => {
            if op == ";" {
                return Ok(Expr::new(Node::Noop, debug));
            }

            let expr = parse_unary_operator(op, debug, tokenizer)?;
            check_for_semicolon(tokenizer)?;
            return Ok(expr);
        }
        Token::Ident(id, debug) => Expr::new(Node::Ident(id), debug),
        _ => unimplemented!(),
    };

    if let Some(second_token) = tokenizer.next() {
        match second_token {
            Token::Operator(op, debug) => {
                if op == ";" {
                    Ok(first_expr)
                } else {
                    let expr = parse_binary_operator(first_expr, op, debug, tokenizer)?;
                    check_for_semicolon(tokenizer)?;
                    Ok(expr)
                }
            }
            _ => unimplemented!(),
        }
    } else {
        Ok(first_expr)
    }
}

fn parse_unary_operator(
    op: String,
    debug: DebugInfo,
    tokenizer: &mut RushTokenizer,
) -> Result<Expr, String> {

    if !(op == "!" || op == "-") {
        return Err(
            debug.to_string() + ", but there is no expression preceding it and '" + &op +
                "' is not a unary operator!",
        )?;
    }

    let next = tokenizer.next().ok_or(
        debug.to_string() + ", then encountered end of input! '" + &op +
            "' must be followed by an expression.",
    )?;

    let next_expr = parse_expr(next, tokenizer)?;

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

fn parse_binary_operator(
    first_expr: Expr,
    op: String,
    debug: DebugInfo,
    tokenizer: &mut RushTokenizer,
) -> Result<Expr, String> {

    if op == "!" {
        return Err(
            debug.to_string() + ", but there is an expression preceding it and '" + &op +
                "' is not a binary operator!",
        )?;
    }

    let next = tokenizer.next().ok_or(
        debug.to_string() + ", then encountered end of input! '" + &op +
            "' must be followed by an expression.",
    )?;

    let next_expr = parse_expr(next, tokenizer)?;

    let operator = match op.as_ref() {
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
    };

    let expr = Expr::new(Node::Op(Box::new(operator)), debug);


    Ok(expr)
}

fn parse_parenthetic_expr(
    first_token: Token,
    tokenizer: &mut RushTokenizer,
) -> Result<Expr, String> {

    first_token.expect_operator_specific("(").map_err(
        |mut err| {
            err.push_str(", which has caused an internal error! Please report this!");
            err
        },
    )?;

    unimplemented!();
}

fn check_for_semicolon(tokenizer: &mut RushTokenizer) -> Result<(), String> {
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
}
