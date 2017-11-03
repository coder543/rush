use std::time::Instant;
use std::env;

use interpreter::ast::*;
use interpreter::{Ident, DebugInfo};

use interpreter::builtins::register_builtins;

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
        args.push(Expr::new(Node::Str(arg), DebugInfo::none()));
    }

    expr.memory.insert(
        Ident("$arg".to_string()),
        Expr::new(Node::Array(args), DebugInfo::none()),
    );

    // insert the environment variables
    for (key, value) in env::vars_os() {
        let (key, value) = (
            key.to_string_lossy().to_string(),
            value.to_string_lossy().to_string(),
        );
        let key = "$".to_string() + &key;
        if key == "$PATH" {
            let values = value
                .split(":")
                .map(|val| {
                    Expr::new(Node::Str(val.to_string()), DebugInfo::none())
                })
                .collect::<Vec<_>>();
            expr.memory.insert(
                Ident(key),
                Expr::new(Node::Array(values), DebugInfo::none()),
            );
        } else {
            expr.memory.insert(
                Ident(key),
                Expr::new(Node::Str(value), DebugInfo::none()),
            );
        }
    }

    register_builtins(&mut expr.memory);

    let started = Instant::now();
    let result = expr.run()?;
    let completed = Instant::now();

    let output = format!("{:?}", result.node);
    let output = if output == "Noop" {
        String::new()
    } else {
        output
    };

    Ok(ExpressionOutput {
        started,
        completed,
        command: buffer.to_string(),
        stdout: output.clone(),
        stderr: "".to_string(),
        interleaved: output,
    })
}
