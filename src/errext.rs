use pancurses;

pub type PResult = Result<(), &'static str>;

pub trait ErrExt {
    fn as_err(&self) -> PResult;
    fn unwrap(&self);
}

impl ErrExt for i32 {
    fn as_err(&self) -> PResult {
        if *self == pancurses::ERR {
            Err("screen is broken?")
        } else {
            Ok(())
        }
    }

    fn unwrap(&self) {
        assert_ne!(*self, pancurses::ERR, "screen is broken?");
    }
}
