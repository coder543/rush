use pancurses::Window;
use errext::{ErrExt, PResult};

pub trait WindowExt {
    fn backspace(&self) -> PResult;
}

impl WindowExt for Window {
    fn backspace(&self) -> PResult {
        let (y, x) = self.get_cur_yx();
        self.mvaddch(y, x - 1, ' ').as_err()?;
        self.mv(y, x - 1).as_err()
    }
}
