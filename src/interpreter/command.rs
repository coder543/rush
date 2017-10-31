use std::time::Instant;

use interpreter::ast::*;
use interpreter::{Ident, DebugInfo};

use interpreter::builtins::add_builtins;

// Obviously just a rough sketch
// These fields will need to be rethought
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ExpressionOutput {
    pub command: String,
    pub stdout: String,
    pub stderr: String,
    pub interleaved: String, //how it actually happened, stdout + stderr + any echoed stdin
    pub started: Instant,
    pub completed: Instant,
}

pub fn run_expression(
    buffer: &str,
    mut commandline: Vec<String>,
) -> Result<ExpressionOutput, String> {

    let mut expr = Expr::parse(buffer)?;

    let mut args = Vec::new();
    for arg in commandline.drain(..) {
        args.push(Expr::parse_one(&arg)?);
    }

    expr.memory.insert(
        Ident("$arg".to_string()),
        Expr::new(Node::Array(args), DebugInfo::none()),
    );

    add_builtins(&mut expr.memory);

    let result = expr.run()?;

    let output = format!("{:?}", result.node);

    Ok(ExpressionOutput {
        command: buffer.to_string(),
        stdout: output.clone(),
        stderr: "".to_string(),
        interleaved: output,
        started: Instant::now(),
        completed: Instant::now(),
    })
}
