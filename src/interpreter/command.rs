use std::time::Instant;
use std::collections::HashMap;

use interpreter::ast::*;
use interpreter::Ident;

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

    for (i, arg) in commandline.drain(..).enumerate() {
        expr.memory.insert(
            Ident(format!("$arg{}", i)),
            Expr::parse_one(&arg)?,
        );
    }

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
