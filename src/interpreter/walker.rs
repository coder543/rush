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
            Node::Return(ref expr) => {
                let mut res = self.clone();
                res.node = Node::Return(Box::new(expr.run(memory)?));
                Ok(res)
            }
            Node::Op(ref operator) => operator.run(memory),
            Node::Function(ref function) => function.run(memory, Vec::new()),
            Node::If(ref if_stmt) => if_stmt.run(memory),
            Node::For(ref for_stmt) => for_stmt.run(memory),
            Node::While(ref while_stmt) => while_stmt.run(memory),
            _ => Ok(self.clone()),
        }
    }
}

/// avoids needlessly cloning an Expr if it is already resolved to
/// a primitive value. unfortunately, this requires passing in an
/// external storage variable for this case where the value has not
/// yet been resolved to a primitive, for lifetime reasons.
macro_rules! resolve {
    ($exp:ident, $memory:ident, $store:ident) => (
        match $exp.node {
            Node::Int(_) => $exp,
            Node::Float(_) => $exp,
            Node::Str(_) => $exp,
            Node::Bool(_) => $exp,
            _ => {$store = $exp.run($memory)?; &$store}
        }
    )
}

macro_rules! do_binary_op {
    ($op:tt, $memory:ident, $left:ident, $right:ident, $(($typeA:tt, $typeB:tt -> ($asType:tt:$asTypePrim:tt))),*; $(($typeA2:tt, $typeB2:tt -> ($asType2:tt))),*; _ => $defaultCase:expr) => ({
        let store1;
        let store2;
        let left_res = resolve!($left, $memory, store1);
        let right_res = resolve!($right, $memory, store2);
        match (&left_res.node, &right_res.node) {
            $(
                (&Node::$typeA(val1), &Node::$typeB(val2)) => {
                    Ok(Expr::new(Node::$asType((val1 as $asTypePrim) $op (val2 as $asTypePrim)), DebugInfo::none()))
                },
            )*
            $(
                (&Node::$typeA2(ref val1), &Node::$typeB2(ref val2)) => {
                    Ok(Expr::new(Node::$asType2(val1 $op val2), DebugInfo::none()))
                },
            )*
            _ => $defaultCase,
        }
    })
}

impl Operator {
    fn run(&self, memory: &mut Memory) -> Result<Expr, String> {
        match *self {
            Operator::Assign(ref id, ref expr) => {
                let val = expr.run(memory)?;
                memory.insert(id.clone(), val);
                Ok(Expr::new(Node::Noop, expr.debug.clone()))
            }

            Operator::Call(ref id, ref args) => unimplemented!(),

            Operator::Add(ref left, ref right) => {
                do_binary_op!(+, memory, left, right, (Int, Int -> (Int:i64)), (Float, Float -> (Float:f64)), (Float, Int -> (Float:f64)), (Int, Float -> (Float:f64));; _ => unimplemented!())
            }
            Operator::Sub(ref left, ref right) => {
                do_binary_op!(-, memory, left, right, (Int, Int -> (Int:i64)), (Float, Float -> (Float:f64)), (Float, Int -> (Float:f64)), (Int, Float -> (Float:f64));; _ => unimplemented!())
            }
            Operator::Mul(ref left, ref right) => {
                do_binary_op!(*, memory, left, right, (Int, Int -> (Int:i64)), (Float, Float -> (Float:f64)), (Float, Int -> (Float:f64)), (Int, Float -> (Float:f64));; _ => unimplemented!())
            }
            Operator::Div(ref left, ref right) => {
                do_binary_op!(/, memory, left, right, (Int, Int -> (Int:i64)), (Float, Float -> (Float:f64)), (Float, Int -> (Float:f64)), (Int, Float -> (Float:f64));; _ => unimplemented!())
            }
            Operator::Mod(ref left, ref right) => {
                do_binary_op!(%, memory, left, right, (Int, Int -> (Int:i64));; _ => unimplemented!())
            }
            Operator::And(ref left, ref right) => {
                do_binary_op!(&&, memory, left, right, (Bool, Bool -> (Bool:bool));; _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to use logical AND on something which is not a boolean")?)
            }
            Operator::Or(ref left, ref right) => {
                do_binary_op!(||, memory, left, right, (Bool, Bool -> (Bool:bool));; _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to use logical OR on something which is not a boolean")?)
            }
            Operator::Less(ref left, ref right) => {
                do_binary_op!(<, memory, left, right, (Int, Int -> (Bool:i64)), (Float, Float -> (Bool:f64)), (Float, Int -> (Bool:f64)), (Int, Float -> (Bool:f64)); (Str, Str -> (Bool)); _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to compare two values for equality which cannot be compared")?)
            }
            Operator::LessOrEquals(ref left, ref right) => {
                do_binary_op!(<=, memory, left, right, (Int, Int -> (Bool:i64)), (Float, Float -> (Bool:f64)), (Float, Int -> (Bool:f64)), (Int, Float -> (Bool:f64)); (Str, Str -> (Bool)); _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to compare two values for equality which cannot be compared")?)
            }
            Operator::Greater(ref left, ref right) => {
                do_binary_op!(>, memory, left, right, (Int, Int -> (Bool:i64)), (Float, Float -> (Bool:f64)), (Float, Int -> (Bool:f64)), (Int, Float -> (Bool:f64)); (Str, Str -> (Bool)); _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to compare two values for equality which cannot be compared")?)
            }
            Operator::GreaterOrEquals(ref left, ref right) => {
                do_binary_op!(>=, memory, left, right, (Int, Int -> (Bool:i64)), (Float, Float -> (Bool:f64)), (Float, Int -> (Bool:f64)), (Int, Float -> (Bool:f64)); (Str, Str -> (Bool)); _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to compare two values for equality which cannot be compared")?)
            }
            Operator::Equals(ref left, ref right) => {
                do_binary_op!(==, memory, left, right,; (Int, Int -> (Bool)), (Float, Float -> (Bool)), (Str, Str -> (Bool)), (Bool, Bool -> (Bool)); _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to compare two values for equality which cannot be compared")?)
            }
            Operator::NotEquals(ref left, ref right) => {
                do_binary_op!(!=, memory, left, right,; (Int, Int -> (Bool)), (Float, Float -> (Bool)), (Str, Str -> (Bool)), (Bool, Bool -> (Bool)); _ => Err(
                            left.debug.to_string() + &right.debug.to_string() +
                            ", attempt to compare two values for equality which cannot be compared")?)
            }

            Operator::Negate(ref expr) => {
                let store;
                let expr_res = resolve!(expr, memory, store);
                match &expr_res.node {
                    &Node::Int(val) => Ok(Expr::new(Node::Int(-val), DebugInfo::none())),
                    &Node::Float(val) => Ok(Expr::new(Node::Float(-val), DebugInfo::none())),
                    _ => {
                        Err(
                            expr.debug.to_string() +
                                ", attempt to negate something which is not a number",
                        )?
                    }
                }
            }

            Operator::Not(ref expr) => {
                let store;
                let expr_res = resolve!(expr, memory, store);
                match &expr_res.node {
                    &Node::Bool(val) => Ok(Expr::new(Node::Bool(!val), DebugInfo::none())),
                    _ => {
                        Err(
                            expr.debug.to_string() +
                                ", attempt to use logical NOT on something which is not a boolean",
                        )?
                    }
                }
            }
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

fn run_exprs(exprs: &Vec<Expr>, memory: &mut Memory) -> Result<Expr, String> {
    for mut expr in exprs {
        let expr = expr.run(memory)?;
        match expr.node {
            Node::Return(_) => return Ok(expr),
            _ => {}
        };
    }
    Ok(Expr::new(Node::Noop, DebugInfo::none()))
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
            match expr.run(memory)?.node {
                Node::Return(ret_expr) => return revert_scope_after(Ok(*ret_expr), scope, memory),
                _ => {}
            };
        }

        revert_scope(scope, memory);

        Ok(Expr::new(Node::Noop, DebugInfo::none()))
    }
}

impl If {
    fn run(&self, memory: &mut Memory) -> Result<Expr, String> {
        let conditional = self.condition.run(memory)?;
        let conditional = match conditional.node {
            Node::Bool(bool_val) => bool_val,
            _ => Err("")?,
        };

        if conditional {
            run_exprs(&self.true_body, memory)
        } else {
            run_exprs(&self.else_body, memory)
        }
    }
}

impl For {
    fn run(&self, memory: &mut Memory) -> Result<Expr, String> {
        unimplemented!();
    }
}

impl While {
    fn run(&self, memory: &mut Memory) -> Result<Expr, String> {
        unimplemented!();
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
