#![allow(unused)]

use interpreter::tokenizer::Ident;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Expr {
    raw: String,
    node: Node,
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
    name: String,
    args: Vec<Ident>,
    body: Expr,
}
