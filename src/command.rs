use std::time::Instant;

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
    let stdout;
    if buffer.starts_with("echo") {
        stdout = &buffer[5..]
    } else {
        stdout = ""
    }
    Ok(ExpressionOutput {
        command: buffer.to_string(),
        stdout: stdout.to_string(),
        stderr: "".to_string(),
        interleaved: stdout.to_string(),
        started: Instant::now(),
        completed: Instant::now(),
    })
}
