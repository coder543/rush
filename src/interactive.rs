use pancurses::{Input, Window};
use errext::{ErrExt, PResult};
use interpreter::command::run_expression;

fn print_buffer(screen: &Window, buffer: &str) -> PResult {
    let height = (screen.get_max_y() - 3) as usize;
    let width = (screen.get_max_x() - 1) as usize;
    screen.mv(0, 0).unwrap();
    for line in buffer.lines().take(height) {
        let max = ::std::cmp::min(width, line.len());
        screen.addstr(&line[..max]).unwrap();
        screen.addch('\n').unwrap();
    }
    Ok(())
}

fn print_command(screen: &Window, buffer: &str) -> PResult {
    let height = screen.get_max_y() - 1;
    let width = screen.get_max_x() - 1;
    screen.mv(height - 1, 0).unwrap();
    screen.hline(0, width).unwrap();
    screen.mv(height, 0).unwrap();
    let max = ::std::cmp::min(width as usize, buffer.len());
    screen.addstr(&buffer[..max]).unwrap();
    Ok(())
}

pub fn input_loop(screen: &Window) -> PResult {
    let mut output_buffer = String::from("");
    let mut command_buffer = String::new();
    // let (mut x, mut y) = (0, 0);
    loop {
        screen.clear().unwrap();
        print_buffer(screen, &output_buffer)?;
        print_command(screen, &command_buffer)?;
        screen.refresh().unwrap();
        let maybe_next = screen.getch();
        if let Some(next) = maybe_next {
            match next {
                Input::KeyBackspace |
                Input::Character('\x08') |
                Input::Character('\x7f') => {
                    command_buffer.pop();
                }
                Input::Character('\n') => {
                    if command_buffer == "" {
                        continue;
                    }
                    output_buffer += "$ ";
                    let output = run_expression(&command_buffer, vec![]);
                    match output {
                        Ok(output) => {
                            output_buffer += &output.command;
                            output_buffer += "\n";
                            output_buffer += &output.interleaved;
                        }
                        Err(message) => {
                            output_buffer += &command_buffer;
                            output_buffer += "\n";
                            output_buffer += &message;
                        }
                    }
                    output_buffer += "\n";
                    command_buffer.clear();
                }
                Input::Character(chr) => {
                    command_buffer.push(chr);
                }
                _ => {}
            }
        }
    }
}
