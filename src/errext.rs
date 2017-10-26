use pancurses;

pub type PResult = Result<(), &'static str>;

pub trait ErrExt {
    fn as_err(&self) -> PResult;
}

impl ErrExt for i32 {
    fn as_err(&self) -> PResult {
        if *self == pancurses::ERR {
            Err("screen is broken?")
        } else {
            Ok(())
        }
    }
}
