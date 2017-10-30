use std::fs::File;
use std::io::Read;
use interpreter::command::run_expression;

pub fn run_file(path: &str, args: Vec<String>) {
    let mut script = String::new();
    File::open(path)
        .expect("could not open file")
        .read_to_string(&mut script)
        .unwrap();
    let result = run_expression(&script, args);
    match result {
        Ok(result) => println!("{}", result.stdout),
        Err(error) => println!("Error running script: {}", error),
    }
}
