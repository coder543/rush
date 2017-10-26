#![allow(unused)]

pub struct Ident(String);

pub struct DebugInfo {
    raw: String,
    pos: u64,
}

impl DebugInfo {
    fn new(raw: String, pos: u64) -> DebugInfo {
        DebugInfo { raw, pos }
    }
}

pub enum Token {
    Operator(String, DebugInfo),
    Str(String, DebugInfo),
    Int(i64, DebugInfo),
    Float(f64, DebugInfo),
    Ident(Ident, DebugInfo),
    Unknown(String, DebugInfo),
}

use std::str::Split;
pub struct RushTokenizer<'a> {
    token_stream: Split<'a, &'static [char]>,
    pos: u64,
}

impl<'a> RushTokenizer<'a> {
    fn new(buffer: &'a str) -> RushTokenizer<'a> {
        RushTokenizer {
            token_stream: buffer.split(&[' ', '\n']),
            pos: 0,
        }
    }

    fn next_raw_token(&mut self) -> Option<String> {
        let raw_token = match self.token_stream.next() {
            Some(raw_token) => raw_token,
            None => return None,
        };

        self.pos += raw_token.len() as u64;

        Some(raw_token.to_string())
    }
}

impl<'a> Iterator for RushTokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let raw_token = match self.next_raw_token() {
            Some(raw_token) => raw_token,
            None => return None,
        };
        Some(Token::Unknown(
            raw_token.clone(),
            DebugInfo::new(raw_token, self.pos),
        ))
    }
}
