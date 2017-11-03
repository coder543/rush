#![allow(unused)]

use interpreter::command::ExpressionOutput;
use std::time::Instant;
use std::fs::File;
use std::io::Read;
use interpreter::ast::*;
use interpreter::{Ident, DebugInfo};
use interpreter::Memory;

macro_rules! register_builtin(
    ($memory:ident, $name:ident, $($arg:ident),+) => (
        $memory.insert(
            Ident(concat!("$", stringify!($name)).to_string()),
            Expr::new(
                Node::Function(Box::new(Function {
                    name: Ident(concat!("$", stringify!($name)).to_string()),
                    args: vec![$(Ident(concat!("$", stringify!($arg)).to_string()))+],
                    body: vec![
                        Expr::new(Node::Builtin(Builtin($name)), DebugInfo::new(concat!("$", stringify!($name)), 0)),
                    ],
                })),
                DebugInfo::new(concat!("$", stringify!($name)), 0),
            ),
        );
    )
);

pub fn register_builtins(memory: &mut Memory) {
    register_builtin!(memory, exit, exitCode);
    register_builtin!(memory, echo, val);
    register_builtin!(memory, panic, val);
    register_builtin!(memory, type_of, val);
    register_builtin!(memory, string, val);
    register_builtin!(memory, int, val);
    register_builtin!(memory, float, val);
    register_builtin!(memory, boolean, val);
}

pub fn exit(memory: &mut Memory) -> Result<Expr, String> {
    let code = match Ident("$exitCode".to_string()).run(&mut DebugInfo::none(), memory)?.node {
        Node::Int(int) => int,
        _ => 0,
    };

    ::std::process::exit(code as i32);
}

pub fn string(memory: &mut Memory) -> Result<Expr, String> {
    let val = Ident("$val".to_string()).run(&mut DebugInfo::none(), memory)?;
    let str_val = match val.node {
        Node::Int(int) => int.to_string(),
        Node::Float(float) => float.to_string(),
        Node::Str(ref str_val) => str_val.clone(),
        Node::Bool(bool_val) => bool_val.to_string(),
        _ => {
            Err(
                val.debug.to_string() + ", which cannot be converted into a string",
            )?
        }
    };

    Ok(Expr::new(Node::Str(str_val), DebugInfo::none()))
}

pub fn int(memory: &mut Memory) -> Result<Expr, String> {
    let val = Ident("$val".to_string()).run(&mut DebugInfo::none(), memory)?;
    let int_val = match val.node {
        Node::Int(int) => int,
        Node::Float(float) => float as i64,
        Node::Str(ref str_val) => {
            str_val.parse().map_err(|_| {
                val.debug.to_string() + " with value " + str_val + " cannot be converted to an int."
            })?
        }
        Node::Bool(bool_val) => if bool_val { 1 } else { 0 },
        _ => {
            Err(
                val.debug.to_string() + ", which cannot be converted into an int",
            )?
        }
    };

    Ok(Expr::new(Node::Int(int_val), DebugInfo::none()))
}

pub fn float(memory: &mut Memory) -> Result<Expr, String> {
    let val = Ident("$val".to_string()).run(&mut DebugInfo::none(), memory)?;
    let float_val = match val.node {
        Node::Int(int) => int as f64,
        Node::Float(float) => float,
        Node::Str(ref str_val) => {
            str_val.parse().map_err(|_| {
                val.debug.to_string() + " with value " + str_val +
                    " cannot be converted to a float."
            })?
        }
        Node::Bool(bool_val) => if bool_val { 1.0 } else { 0.0 },
        _ => {
            Err(
                val.debug.to_string() + ", which cannot be converted into a float",
            )?
        }
    };

    Ok(Expr::new(Node::Float(float_val), DebugInfo::none()))
}

pub fn boolean(memory: &mut Memory) -> Result<Expr, String> {
    let val = Ident("$val".to_string()).run(&mut DebugInfo::none(), memory)?;
    let bool_val = match val.node {
        Node::Str(ref str_val) => {
            str_val.to_lowercase().parse().map_err(|_| {
                val.debug.to_string() + " with value " + str_val +
                    " cannot be converted to a boolean."
            })?
        }
        Node::Bool(bool_val) => bool_val,
        _ => {
            Err(
                val.debug.to_string() + ", which cannot be converted into a boolean",
            )?
        }
    };

    Ok(Expr::new(Node::Bool(bool_val), DebugInfo::none()))
}

pub fn echo(memory: &mut Memory) -> Result<Expr, String> {
    let val = Ident("$val".to_string()).run(&mut DebugInfo::none(), memory)?;
    let str_val = match val.node {
        Node::Int(int) => int.to_string(),
        Node::Float(float) => float.to_string(),
        Node::Str(ref str_val) => str_val.clone(),
        Node::Bool(bool_val) => bool_val.to_string(),
        ref node => format!("{:#?}", node),
    };

    println!("{}", str_val);

    Ok(Expr::new(Node::Noop, DebugInfo::none()))
}

pub fn panic(memory: &mut Memory) -> Result<Expr, String> {
    let val = Ident("$val".to_string()).run(&mut DebugInfo::none(), memory)?;
    let str_val = match val.node {
        Node::Str(ref str_val) => str_val.clone(),
        ref node => format!("{:#?}", node),
    };

    Err(format!("script panicked with error message \"{}\"", str_val))
}

pub fn type_of(memory: &mut Memory) -> Result<Expr, String> {
    let val = { 
        let val = memory.get(&Ident("$val".to_string()));
        if val.is_none() {
            return Ok(Expr::new(Node::Str("undefined".to_string()), DebugInfo::none()));
        }
        val.unwrap().clone()
    };
    let str_val = match val.run(memory)?.node {
        Node::Int(_) => "int".to_string(),
        Node::Float(_) => "float".to_string(),
        Node::Str(_) => "string".to_string(),
        Node::Bool(_) => "boolean".to_string(),
        Node::Array(_) => "array".to_string(),
        Node::Function(_) => "function".to_string(),
        ref node => format!("{:#?}", node),
    };
    
    Ok(Expr::new(Node::Str(str_val), DebugInfo::none()))
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
