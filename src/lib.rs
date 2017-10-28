#[macro_use]
extern crate try_opt;

extern crate pancurses;

mod errext;

mod windowext;

mod interactive;
use interactive::input_loop;

mod interpreter;

pub fn start_interactive() {
    let screen = &pancurses::initscr();
    pancurses::noecho();
    pancurses::set_title("rush");
    screen.keypad(true);
    input_loop(screen).unwrap();
}
