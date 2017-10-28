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

            // Most of the following are the opposite of DRY. I should
            // figure out a way to reduce the needless duplication.
            Operator::Add(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 + val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Float(val1 + val2), DebugInfo::none()))
                    }
                    _ => unimplemented!(),
                }
            }
            Operator::Sub(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 - val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Float(val1 - val2), DebugInfo::none()))
                    }
                    _ => unimplemented!(),
                }
            }
            Operator::Mul(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 * val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Float(val1 * val2), DebugInfo::none()))
                    }
                    _ => unimplemented!(),
                }
            }
            Operator::Div(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 / val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Float(val1 / val2), DebugInfo::none()))
                    }
                    _ => unimplemented!(),
                }
            }
            Operator::Mod(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Int(val1 % val2), DebugInfo::none()))
                    }
                    _ => unimplemented!(),
                }
            }

            Operator::Negate(ref expr) => {
                let expr_res = expr.run(memory)?;
                match expr_res.node {
                    Node::Int(val) => Ok(Expr::new(Node::Int(-val), DebugInfo::none())),
                    Node::Float(val) => Ok(Expr::new(Node::Float(-val), DebugInfo::none())),
                    _ => {
                        Err(
                            expr.debug.to_string() +
                                ", attempt to negate something which is not a number",
                        )?
                    }
                }
            }

            Operator::Not(ref expr) => {
                let expr_res = expr.run(memory)?;
                match expr_res.node {
                    Node::Bool(val) => Ok(Expr::new(Node::Bool(!val), DebugInfo::none())),
                    _ => {
                        Err(
                            expr.debug.to_string() +
                                ", attempt to use logical NOT on something which is not a boolean",
                        )?
                    }
                }
            }
            Operator::And(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 && val2), DebugInfo::none()))
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to use logical AND on something which is not a boolean",
                        )?
                    }
                }
            }
            Operator::Or(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 || val2), DebugInfo::none()))
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to use logical AND on something which is not a boolean",
                        )?
                    }
                }
            }
            Operator::Equals(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 == val2), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 == val2), DebugInfo::none()))
                    }
                    (Node::Str(val1), Node::Str(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 == val2), DebugInfo::none()))
                    }
                    (Node::Function(val1), Node::Function(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 == val2), DebugInfo::none()))
                    }
                    (Node::Float(_), _) => {
                        Err(
                            left.debug.to_string() +
                                ", floating point equality testing is not supported",
                        )?
                    }
                    (_, Node::Float(_)) => {
                        Err(
                            right.debug.to_string() +
                                ", floating point equality testing is not supported",
                        )?
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to compare two values for equality which cannot be compared",
                        )?
                    }
                }
            }
            Operator::NotEquals(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 != val2), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 != val2), DebugInfo::none()))
                    }
                    (Node::Str(val1), Node::Str(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 != val2), DebugInfo::none()))
                    }
                    (Node::Function(val1), Node::Function(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 != val2), DebugInfo::none()))
                    }
                    (Node::Float(_), _) => {
                        Err(
                            left.debug.to_string() +
                                ", floating point equality testing is not supported",
                        )?
                    }
                    (_, Node::Float(_)) => {
                        Err(
                            right.debug.to_string() +
                                ", floating point equality testing is not supported",
                        )?
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to compare two values for equality which cannot be compared",
                        )?
                    }
                }
            }
            Operator::Less(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 < val2), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 < val2), DebugInfo::none()))
                    }
                    (Node::Str(val1), Node::Str(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 < val2), DebugInfo::none()))
                    }
                    (Node::Function(val1), Node::Function(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 < val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 < (val2 as f64)), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Bool((val1 as f64) < val2), DebugInfo::none()))
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to compare two values for equality which cannot be compared",
                        )?
                    }
                }
            }
            Operator::LessOrEquals(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 <= val2), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 <= val2), DebugInfo::none()))
                    }
                    (Node::Str(val1), Node::Str(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 <= val2), DebugInfo::none()))
                    }
                    (Node::Function(val1), Node::Function(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 <= val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 <= (val2 as f64)), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Bool((val1 as f64) <= val2), DebugInfo::none()))
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to compare two values for equality which cannot be compared",
                        )?
                    }
                }
            }
            Operator::Greater(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 > val2), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 > val2), DebugInfo::none()))
                    }
                    (Node::Str(val1), Node::Str(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 > val2), DebugInfo::none()))
                    }
                    (Node::Function(val1), Node::Function(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 > val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 > (val2 as f64)), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Bool((val1 as f64) > val2), DebugInfo::none()))
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to compare two values for equality which cannot be compared",
                        )?
                    }
                }
            }
            Operator::GreaterOrEquals(ref left, ref right) => {
                let left_res = left.run(memory)?;
                let right_res = right.run(memory)?;
                match (left_res.node, right_res.node) {
                    (Node::Bool(val1), Node::Bool(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 >= val2), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 >= val2), DebugInfo::none()))
                    }
                    (Node::Str(val1), Node::Str(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 >= val2), DebugInfo::none()))
                    }
                    (Node::Function(val1), Node::Function(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 >= val2), DebugInfo::none()))
                    }
                    (Node::Float(val1), Node::Int(val2)) => {
                        Ok(Expr::new(Node::Bool(val1 >= (val2 as f64)), DebugInfo::none()))
                    }
                    (Node::Int(val1), Node::Float(val2)) => {
                        Ok(Expr::new(Node::Bool((val1 as f64) >= val2), DebugInfo::none()))
                    }
                    _ => {
                        Err(
                            left.debug.to_string() + &right.debug.to_string() +
                                ", attempt to compare two values for equality which cannot be compared",
                        )?
                    }
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
