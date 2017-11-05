use std::time::Instant;
use std::sync::mpsc::{Receiver, channel};

pub mod recorder;
pub mod uploader;
mod input;

use self::recorder::Recorder;
use self::uploader::Uploader;
pub use self::input::input_loop;

pub struct Record {
    pub text: String,
    pub time: Instant,
}

impl Record {
    pub fn new(text: String) -> Record {
        Record {
            text,
            time: Instant::now(),
        }
    }
}

pub enum Message {
    StdOut(Record),
    StdErr(Record),
    StdIn(Record),
    NewRecorder(Receiver<Message>),
}

pub fn make_channel() -> (Uploader, Recorder) {
    let (uplink, downlink) = channel();
    let uploader = Uploader { uplink };

    let recorder = Recorder {
        stdout: vec![],
        stderr: vec![],
        stdin: vec![],
        downlink,
    };

    (uploader, recorder)
}
