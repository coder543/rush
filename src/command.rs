use std::time::Instant;

use builtins::echo;

// Obviously just a rough sketch
// These fields will need to be rethought
pub struct ExpressionOutput {
    pub command: String,
    pub stdout: String,
    pub stderr: String,
    pub interleaved: String, //how it actually happened, stdout + stderr + any echoed stdin
    pub started: Instant,
    pub completed: Instant,
}

pub fn run_expression(buffer: &str) -> Result<ExpressionOutput, &'static str> {
    let mut words = buffer.split(' ');
    let command = words.next();
    let args = words.collect();

    command
        .and_then(|command| match command {
            "echo" => Some(echo(buffer, args)),
            _ => None,
        })
        .or(Some(Err("could not find command")))
        .unwrap()
}
