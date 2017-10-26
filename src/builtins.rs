use command::ExpressionOutput;
use std::time::Instant;

pub fn echo(buffer: &str, args: Vec<&str>) -> Result<ExpressionOutput, &'static str> {
    let output = args.join(" ");
    Ok(ExpressionOutput {
        command: buffer.to_string(),
        stdout: output.clone(),
        stderr: "".to_string(),
        interleaved: output,
        started: Instant::now(),
        completed: Instant::now(),
    })
}
