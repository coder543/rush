#![allow(unused)]

use interpreter::ast::*;
use interpreter::tokenizer::*;

use std::collections::HashMap;

pub type Memory = HashMap<Ident, Expr>;

impl Expr {
    fn run(&self, memory: &mut Memory) -> Result<Expr, String> {
        match self.node {
            Node::Ident(ref id) => {
                memory
                    .get(id)
                    .ok_or_else(|| {
                        self.debug.to_string() + ", but " + &id.0 + " has not been defined yet."
                    })
                    .map(|val| val.clone())
            }
            Node::Op(ref operator) => operator.run(memory),
            Node::Function(ref function) => function.run(memory, Vec::new()),
            _ => Ok(self.clone()),
        }
    }
}

impl Operator {
    fn run(&self, memory: &mut Memory) -> Result<Expr, String> {
        match *self {
            Operator::Assign(ref id, ref expr) => {
                let val = expr.run(memory)?;
                memory.insert(id.clone(), val);
                Ok(Expr::new(Node::Noop, expr.debug.clone()))
            }
            Operator::Add(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 + val2), DebugInfo::none()))
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }
}

/// get the list of visible identifiers at this scope level
fn get_scope(memory: &mut Memory) -> Vec<Ident> {
    memory.keys().cloned().collect()
}

/// remove any identifiers that were created during the scoped region
fn revert_scope(scope: Vec<Ident>, memory: &mut Memory) {
    memory.retain(|ident, _| scope.contains(ident));
}

fn revert_scope_after<T>(val: T, scope: Vec<Ident>, memory: &mut Memory) -> T {
    revert_scope(scope, memory);
    val
}

impl Function {
    fn run(&self, memory: &mut Memory, args: Vec<Expr>) -> Result<Expr, String> {
        if self.args.len() != args.len() {
            return Err(format!(
                "Function {} called with incorrect number of arguments. Expected {} but found {}.",
                self.name,
                self.args.len(),
                args.len()
            ))?;
        }
        let scope = get_scope(memory);
        for (id, val) in self.args.iter().zip(args) {
            memory.insert(id.clone(), val);
        }
        for mut expr in &self.body {
            match expr.node {
                Node::Return(_) => return revert_scope_after(expr.run(memory), scope, memory),
                _ => expr.run(memory)?,
            };
        }

        revert_scope(scope, memory);

        Ok(Expr::new(Node::Noop, DebugInfo::none()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_global(name: &str, memory: &mut Memory) {
        memory.insert(
            Ident(name.to_string()),
            Expr::new(Node::Noop, DebugInfo::none()),
        );
    }

    #[test]
    fn walk_assign() {
        let expr = Expr::parse("$someInt = 35").unwrap();
        let mut memory = &mut HashMap::new();

        // create $someInt outside the scope of the anonymous outer function that
        // Expr::parse returns, that way the effect of the assignment can be seen here.
        create_global("$someInt", memory);

        let result = expr.run(memory).unwrap();
        println!("{:#?}\n memory: {:#?}", result, memory);
    }

    #[test]
    fn walk_assign2() {
        let expr = Expr::parse("$otherInt = 35; $someInt = $otherInt + 2;").unwrap();
        let mut memory = &mut HashMap::new();

        // create $someInt outside the scope of the anonymous outer function that
        // Expr::parse returns, that way the effect of the assignment can be seen here.
        create_global("$someInt", memory);

        let result = expr.run(memory).unwrap();
        println!("{:#?}\n memory: {:#?}", result, memory);
    }

    #[test]
    #[should_panic]
    fn walk_assign_undefined() {
        let expr = Expr::parse("$someInt = 35; $otherInt = $someInt + $whatIsThis;").unwrap();
        let mut memory = &mut HashMap::new();
        let result = expr.run(memory).unwrap();
    }
}
