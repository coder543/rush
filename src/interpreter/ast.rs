#![allow(unused)]

use interpreter::tokenizer::*;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Expr {
    node: Node,
    debug: DebugInfo,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Node {
    Ident(Ident),
    Int(i64),
    Float(f64),
    Op(Box<Operator>),
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

impl Expr {
    pub fn parse(buffer: &str) -> Result<Expr, String> {
        let tokenizer = RushTokenizer::new(buffer);
        let (body, _) = parse_exprs(tokenizer)?;
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

fn parse_exprs(tokenizer: RushTokenizer) -> Result<(Vec<Expr>, RushTokenizer), String> {
    unimplemented!();
}
