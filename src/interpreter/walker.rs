#![allow(unused)]

use interpreter::ast::*;
use interpreter::tokenizer::*;

use std::collections::HashMap;

impl Expr {
    fn run(&mut self, memory: &mut HashMap<Ident, Expr>) -> Result<Expr, String> {
        match self.node {
            Node::Ident(ref id) => {
                memory
                    .get(id)
                    .ok_or_else(|| {
                        self.debug.to_string() + ", but " + &id.0 + " has not been defined yet."
                    })
                    .map(|val| val.clone())
            }
            Node::Op(ref mut operator) => operator.run(memory),
            Node::Function(ref mut function) => function.run(memory, Vec::new()),
            _ => Ok(self.clone()),
        }
    }
}

impl Operator {
    fn run(&mut self, memory: &mut HashMap<Ident, Expr>) -> Result<Expr, String> {
        match *self {
            Operator::Assign(ref id, ref mut expr) => {
                let val = expr.run(memory)?;
                memory.insert(id.clone(), val);
                Ok(Expr::new(Node::Noop, expr.debug.clone()))
            },
            Operator::Add(ref mut left, ref mut right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 + val2), DebugInfo::none()))
                    }
                    _ => unimplemented!()
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl Function {
    fn run(&mut self, memory: &mut HashMap<Ident, Expr>, args: Vec<Expr>) -> Result<Expr, String> {
        if self.args.len() != args.len() {
            return Err(format!("Function {} called with an incorrect number of arguments. Expected {} but found {}.", self.name, self.args.len(), args.len()))?
        }
        for (id, val) in self.args.iter().zip(args) {
            memory.insert(id.clone(), val);
        }
        for mut expr in &mut self.body {
            match expr.node {
                Node::Return(_) => return expr.run(memory),
                _ => expr.run(memory)?,
            };
        }

        Ok(Expr::new(Node::Noop, DebugInfo::none()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn walk_assign() {
        let mut expr = Expr::parse("$someInt = 35").unwrap();
        let mut memory = HashMap::new();
        let result = expr.run(&mut memory);
        println!("{:#?} -> {:#?}\n memory: {:#?}", expr, result, memory);
    }

    #[test]
    fn walk_assign2() {
        let mut expr = Expr::parse("$someInt = 35; $otherInt = $someInt + 4;").unwrap();
        let mut memory = HashMap::new();
        let result = expr.run(&mut memory);
        println!("{:#?}\n memory: {:#?}", result, memory);
    }
}
