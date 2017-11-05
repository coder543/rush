use std::sync::mpsc::{Sender, channel};
use interactive::{Record, Message};

pub struct Uploader {
    pub uplink: Sender<Message>,
}

impl Uploader {
    pub fn stdout<IntoString: Into<String>>(&self, text: IntoString) {
        self.uplink
            .send(Message::StdOut(Record::new(text.into())))
            .unwrap();
    }

    pub fn stderr<IntoString: Into<String>>(&self, text: IntoString) {
        self.uplink
            .send(Message::StdErr(Record::new(text.into())))
            .unwrap();
    }

    pub fn stdin<IntoString: Into<String>>(&self, text: IntoString) {
        self.uplink
            .send(Message::StdIn(Record::new(text.into())))
            .unwrap();
    }

    pub fn subrecorder(&self) -> Uploader {
        let (uplink, downlink) = channel();
        self.uplink.send(Message::NewRecorder(downlink)).unwrap();
        Uploader { uplink }
    }
}
