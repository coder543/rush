#![allow(unused)]

use interpreter::command::ExpressionOutput;
use std::time::Instant;
use std::fs::File;
use std::io::Read;
use interpreter::ast::*;
use interpreter::{Ident, DebugInfo};
use interpreter::Memory;

pub fn add_builtins(memory: &mut Memory) {
    memory.insert(
        Ident("$exit".to_string()),
        Expr::new(
            Node::Function(Box::new(Function {
                name: Ident("$exit".to_string()),
                args: vec![Ident("$exitCode".to_string())],
                body: vec![
                    Expr::new(Node::Builtin(Builtin(exit)), DebugInfo::new("$exit", 0)),
                ],
            })),
            DebugInfo::new("$exit", 0),
        ),
    );
}

pub fn exit(memory: &mut Memory) -> Result<Expr, String> {
    let code = match memory.get(&Ident("$exitCode".to_string())) {
        Some(val) => {
            match val.node {
                Node::Int(int) => int,
                _ => 0,
            }
        }
        _ => 0,
    };

    ::std::process::exit(code as i32);
}

// pub fn echo(buffer: &str, args: Vec<&str>) -> Result<ExpressionOutput, String> {
//     let output = args.join(" ");
//     Ok(ExpressionOutput {
//         command: buffer.to_string(),
//         stdout: output.clone(),
//         stderr: "".to_string(),
//         interleaved: output,
//         started: Instant::now(),
//         completed: Instant::now(),
//     })
// }

// pub fn cat(buffer: &str, args: Vec<&str>) -> Result<ExpressionOutput, String> {
//     let mut output = String::new();
//     for file in args {
//         File::open(file)
//             .and_then(|mut file| file.read_to_string(&mut output))
//             .map_err(|err| format!("Error reading {}: {}", file, err))?;
//     }
//     Ok(ExpressionOutput {
//         command: buffer.to_string(),
//         stdout: output.clone(),
//         stderr: "".to_string(),
//         interleaved: output,
//         started: Instant::now(),
//         completed: Instant::now(),
//     })
// }
