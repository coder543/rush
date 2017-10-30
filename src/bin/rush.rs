extern crate rush;
use rush::script::run_file;
use std::env::args;

fn main() {
    if args().len() == 1 {
        rush::start_interactive();
    } else {
        for arg in args().skip(1) {
            run_file(&arg);
        }
    }
}
