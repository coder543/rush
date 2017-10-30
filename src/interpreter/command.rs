use std::time::Instant;
use std::collections::HashMap;

use interpreter::ast::*;
use interpreter::tokenizer::Ident;

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

    let expr = Expr::parse(buffer)?;
    let memory = &mut HashMap::new();

    for (i, arg) in commandline.drain(..).enumerate() {
        memory.insert(Ident(format!("$arg{}", i)), Expr::parse_one(&arg)?);
    }

    let result = expr.run(memory)?;

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
