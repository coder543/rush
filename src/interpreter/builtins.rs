#![allow(unused)]

use interpreter::command::ExpressionOutput;
use std::time::Instant;
use std::fs::File;
use std::io::Read;

pub fn echo(buffer: &str, args: Vec<&str>) -> Result<ExpressionOutput, String> {
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

pub fn cat(buffer: &str, args: Vec<&str>) -> Result<ExpressionOutput, String> {
    let mut output = String::new();
    for file in args {
        File::open(file)
            .and_then(|mut file| file.read_to_string(&mut output))
            .map_err(|err| format!("Error reading {}: {}", file, err))?;
    }
    Ok(ExpressionOutput {
        command: buffer.to_string(),
        stdout: output.clone(),
        stderr: "".to_string(),
        interleaved: output,
        started: Instant::now(),
        completed: Instant::now(),
    })
}
