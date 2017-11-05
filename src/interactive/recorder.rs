use std::sync::mpsc::Receiver;
use interactive::{Record, Message};

pub struct Recorder {
    pub stdout: Vec<Record>,
    pub stderr: Vec<Record>,
    pub stdin: Vec<Record>,
    pub downlink: Receiver<Message>,
}
