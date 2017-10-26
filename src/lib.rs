extern crate pancurses;
use pancurses::{Input, Window};

mod errext;
use errext::ErrExt;

fn print_buffer(screen: &Window, buffer: &str) {
    let height = (screen.get_max_y() - 1) as usize;
    let width = screen.get_max_x() as usize;
    screen.mv(0, 0).as_err().unwrap();
    for line in buffer.lines().take(height) {
        let max = ::std::cmp::min(width, line.len());
        screen.addstr(&line[..max]).as_err().unwrap();
        screen.addch('\n');
    }
}

fn print_command(screen: &Window, buffer: &str) {
    let height = screen.get_max_y() - 1;
    let width = screen.get_max_x() - 1;
    screen.mv(height - 1, 0).as_err().unwrap();
    screen.hline(0, width).as_err().unwrap();
    screen.mv(height, 0).as_err().unwrap();
    let max = ::std::cmp::min(width as usize, buffer.len());
    screen.addstr(&buffer[..max]).as_err().unwrap();
}

fn input_loop(screen: Window) {
    let mut output_buffer = String::from("this is a test\nwith line breaks. ");
    let mut command_buffer = String::new();
    // let (mut x, mut y) = (0, 0);
    loop {
        screen.clear().as_err().unwrap();
        print_buffer(&screen, &output_buffer);
        print_command(&screen, &command_buffer);
        screen.refresh().as_err().unwrap();
        let next = screen.getch().unwrap();
        match next {
            Input::KeyBackspace |
            Input::Character('\x08') => {
                command_buffer.pop();
            }
            Input::Character('\n') => {
                output_buffer.push_str("\n");
                output_buffer.push_str(&command_buffer);
                command_buffer.clear();
            }
            Input::Character(chr) => {
                command_buffer.push(chr);
            }
            _ => {}
        };
    }
}

pub fn interactive() {
    let screen = pancurses::initscr();
    pancurses::noecho();
    pancurses::set_title("rush");
    screen.keypad(true);
    input_loop(screen);
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
