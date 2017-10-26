use std::time::Instant;
use pancurses;

use interpreter::builtins::*;

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

pub fn run_expression(buffer: &str) -> Result<ExpressionOutput, String> {
    let mut words = buffer.split(' ');
    let command = words.next();
    let args = words.collect();

    command
        .and_then(|command| match command {
            "echo" => Some(echo(buffer, args)),
            "cat" => Some(cat(buffer, args)),
            "exit" => {
                pancurses::endwin();
                ::std::process::exit(0);
            }
            _ => None,
        })
        .or(Some(Err(format!("could not find {}", buffer))))
        .unwrap()
}
