use std::sync::mpsc::{Receiver, Sender, channel};
use interactive::{Record, Message};

pub struct Uploader {
    pub uplink: Sender<Message>,
}

impl Uploader {
    pub fn stdout<IntoString: Into<String>>(&self, text: IntoString) {
        self.uplink.send(Message::StdOut(Record::new(text.into())));
    }

    pub fn stderr<IntoString: Into<String>>(&self, text: IntoString) {
        self.uplink.send(Message::StdErr(Record::new(text.into())));
    }

    pub fn stdin<IntoString: Into<String>>(&self, text: IntoString) {
        self.uplink.send(Message::StdIn(Record::new(text.into())));
    }

    pub fn subrecorder(&self) -> Uploader {
        let (uplink, downlink) = channel();
        self.uplink.send(Message::NewRecorder(downlink));
        Uploader { uplink }
    }
}
