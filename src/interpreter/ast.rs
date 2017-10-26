#![allow(unused)]

pub struct Expr {
    raw: String,
    node: Node,
}

pub enum Node {
    Ident(Ident),
    Int(i64),
    Float(f64),
    Str(String),
    Op(Box<Operator>),
    For(Box<For>),
    While(Box<While>),
    Function(Box<Function>),
}

pub struct Ident(String);

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

pub struct Function {
    name: Ident,
    args: Vec<Ident>,
    body: Expr
}

pub struct For {
    loopvar: Ident,
    iterator: Expr,
    body: Expr
}

pub struct While {
    condition: Expr,
    body: Expr
}