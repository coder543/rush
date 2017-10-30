extern crate rush;
use rush::script::run_file;
use std::env::args;

fn main() {
    if args().len() == 1 {
        rush::start_interactive();
    } else {
        let mut arglist = args().skip(1);
        let script = arglist.next().unwrap();
        let args = arglist.collect();
        run_file(&script, args);
    }
}
