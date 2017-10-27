#![allow(unused)]

use std::fmt;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct DebugInfo {
    pub raw: String,
    pub pos: u64,
}

impl DebugInfo {
    pub fn new<T: Into<String>>(raw: T, pos: u64) -> DebugInfo {
        let raw = raw.into();
        DebugInfo { raw, pos }
    }
}

impl fmt::Display for DebugInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Found {} at character {}", self.raw, self.pos)
    }
}

impl From<DebugInfo> for String {
    fn from(debug: DebugInfo) -> String {
        debug.to_string()
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Ident(pub String);

impl Ident {
    fn try_new(buffer: &str) -> Option<Ident> {
        if buffer.len() < 2 {
            return None;
        }

        let mut chars = buffer.chars();

        // all identifiers must start with $
        if try_opt!(chars.next()) != '$' {
            return None;
        }

        // all identifiers must begin with an "alphabetic" character
        if !(try_opt!(chars.next()).is_alphabetic()) {
            return None;
        }

        for c in chars {
            if !c.is_alphabetic() && !c.is_numeric() {
                return None;
            }
        }
        Some(Ident(buffer.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Operator(String, DebugInfo),
    Str(String, DebugInfo),
    Int(i64, DebugInfo),
    Float(f64, DebugInfo),
    Ident(Ident, DebugInfo),
    Unknown(String, DebugInfo),
}

impl Token {
    pub fn get_debug_info(&self) -> DebugInfo {
        match self {
            &Token::Operator(_, ref debug) => debug.clone(),
            &Token::Str(_, ref debug) => debug.clone(),
            &Token::Int(_, ref debug) => debug.clone(),
            &Token::Float(_, ref debug) => debug.clone(),
            &Token::Ident(_, ref debug) => debug.clone(),
            &Token::Unknown(_, ref debug) => debug.clone(),
        }
    }

    pub fn expect_operator(&self, err: &str) -> Result<(), String> {
        match self {
            &Token::Operator(_, _) => Ok(()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_operator_specific(&self, op: &str) -> Result<(), String> {
        match self {
            &Token::Operator(ref operator, _) if operator == op => Ok(()),
            _ => Err(format!("{}, expected '{}'", self.get_debug_info(), op)),
        }
    }

    pub fn expect_str(&self, err: &str) -> Result<(), String> {
        match self {
            &Token::Str(_, _) => Ok(()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_int(&self, err: &str) -> Result<(), String> {
        match self {
            &Token::Int(_, _) => Ok(()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_float(&self, err: &str) -> Result<(), String> {
        match self {
            &Token::Float(_, _) => Ok(()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_ident(&self, err: &str) -> Result<(), String> {
        match self {
            &Token::Ident(_, _) => Ok(()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_ident_specific(&self, id: Ident) -> Result<(), String> {
        match self {
            &Token::Ident(ref ident, _) if *ident == id => Ok(()),
            _ => Err(format!("{}, expected '{}'", self.get_debug_info(), id.0)),
        }
    }

    pub fn expect_unknown(&self, err: &str) -> Result<(), String> {
        match self {
            &Token::Unknown(_, _) => Ok(()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_unknown_specific(&self, unknown: String, err: &str) -> Result<(), String> {
        match self {
            &Token::Unknown(ref unk, _) if *unk == unknown => Ok(()),
            _ => Err(format!("{}, expected '{}'", self.get_debug_info(), unknown)),
        }
    }
}

static SYM_OPS: [&str; 14] = [
    "+",
    "-",
    "*",
    "/",
    "=",
    "!",
    ";",
    "(",
    ")",
    "{",
    "}",
    "==",
    "&&",
    "||",
];
static WORD_OPS: [&str; 3] = ["or", "and", "not"];

use std::str::Split;
use std::collections::VecDeque;
pub struct RushTokenizer<'a> {
    token_stream: Split<'a, &'static [char]>,
    ready_tokens: VecDeque<String>,
    pos: u64,
    last_len: u64,
}

impl<'a> RushTokenizer<'a> {
    pub fn new(buffer: &'a str) -> RushTokenizer<'a> {
        RushTokenizer {
            token_stream: buffer.split(&[' ', '\n']),
            ready_tokens: VecDeque::new(),
            pos: 0,
            last_len: 0,
        }
    }

    /// Separate function to encapsulate the logic of incrementing the position counter
    fn next_raw_token(&mut self) -> Option<String> {
        self.pos += self.last_len;

        let raw_token;

        //check if we prepared an extra raw_token last time around
        if let Some(ready_token) = self.ready_tokens.pop_front() {
            raw_token = ready_token;
        } else {
            raw_token = try_opt!(self.token_stream.next()).to_string();
        }
        self.last_len = raw_token.len() as u64 + 1; // + 1 to account for the space

        Some(raw_token.to_string())
    }

    fn get_string_literal(&mut self, raw_token: &str) -> Option<Token> {
        if !raw_token.starts_with('"') {
            return None;
        }

        // remove the opening quote from the string literal
        // and allocate a local mutable string
        let mut raw_token = raw_token[1..].to_string();

        // save the position of the opening quote
        let position = self.pos;

        let mut next_token = raw_token.clone();
        loop {
            if next_token.contains('"') {
                if next_token.ends_with('"') {
                    raw_token.pop();
                    let strlit = raw_token.clone();
                    raw_token = ["\"", &raw_token, "\""].concat();
                    let debug = DebugInfo::new(raw_token, position);
                    return Some(Token::Str(strlit, debug));
                }
            }
            next_token = try_opt!(self.next_raw_token());
            raw_token.push(' ');
            raw_token.push_str(&next_token);
        }
    }

    fn get_ident(&mut self, raw_token: &str) -> Option<Ident> {
        Ident::try_new(raw_token)
    }

    fn get_int(&mut self, raw_token: &str) -> Option<i64> {
        raw_token.parse().ok()
    }

    fn get_float(&mut self, raw_token: &str) -> Option<f64> {
        raw_token.parse().ok()
    }

    fn is_operator(&mut self, raw_token: &str) -> bool {
        SYM_OPS.contains(&raw_token) || WORD_OPS.contains(&raw_token)
    }

    fn subdivide_token(&mut self, raw_token: &mut String) {
        println!("token to subdivide is {}", raw_token);
        if raw_token.len() == 1 {
            return;
        }

        for i in 0..raw_token.len() {
            for op in SYM_OPS.iter() {
                if raw_token[i..].starts_with(op) {
                    let new = if i == 0 {
                        raw_token.split_off(op.len())
                    } else {
                        raw_token.split_off(i)
                    };

                    self.last_len -= new.len() as u64;
                    self.ready_tokens.push_front(new);
                    self.last_len -= 1;
                    return;
                }
            }
        }
    }
}

impl<'a> Iterator for RushTokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut raw_token = try_opt!(self.next_raw_token());

        self.subdivide_token(&mut raw_token);

        //this print statement only exists during unit testing
        #[cfg(test)]
        println!("token at pos {} is {:?}", self.pos, raw_token);

        if self.is_operator(&raw_token) {
            let debug = DebugInfo::new(raw_token.clone(), self.pos);
            return Some(Token::Operator(raw_token, debug));
        }

        if let Some(ident) = self.get_ident(&raw_token) {
            return Some(Token::Ident(ident, DebugInfo::new(raw_token, self.pos)));
        }

        if let Some(token_string) = self.get_string_literal(&raw_token) {
            return Some(token_string);
        }

        if let Some(int) = self.get_int(&raw_token) {
            return Some(Token::Int(int, DebugInfo::new(raw_token, self.pos)));
        }

        if let Some(float) = self.get_float(&raw_token) {
            return Some(Token::Float(float, DebugInfo::new(raw_token, self.pos)));
        }

        Some(Token::Unknown(
            raw_token.clone(),
            DebugInfo::new(raw_token, self.pos),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_ident() {
        let mut tokenizer = RushTokenizer::new("$test");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Ident(
                Ident("$test".to_string()),
                DebugInfo::new("$test", 0),
            ))
        );
    }

    #[test]
    fn tokenize_int() {
        let mut tokenizer = RushTokenizer::new("32");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Int(32, DebugInfo::new("32", 0)))
        );
    }

    #[test]
    fn tokenize_float() {
        let mut tokenizer = RushTokenizer::new("2.2");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Float(2.2, DebugInfo::new("2.2", 0)))
        );
    }

    #[test]
    fn tokenize_string() {
        let mut tokenizer = RushTokenizer::new("\"does this work?\"");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Str(
                "does this work?".to_string(),
                DebugInfo::new("\"does this work?\"", 0),
            ))
        );
    }

    #[test]
    fn tokenize_operator() {
        let mut tokenizer = RushTokenizer::new("+ -");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("+".to_string(), DebugInfo::new("+", 0)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("-".to_string(), DebugInfo::new("-", 2)))
        );
    }

    #[test]
    fn tokenize_unknown() {
        let mut tokenizer = RushTokenizer::new("htop");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Unknown(
                "htop".to_string(),
                DebugInfo::new("htop", 0),
            ))
        );
    }

    #[test]
    fn tokenize_subdivision() {
        let mut tokenizer = RushTokenizer::new("(32&&15)||23;");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("(".to_string(), DebugInfo::new("(", 0)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Int(32, DebugInfo::new("32", 1)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("&&".to_string(), DebugInfo::new("&&", 3)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Int(15, DebugInfo::new("15", 5)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator(")".to_string(), DebugInfo::new(")", 7)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("||".to_string(), DebugInfo::new("||", 8)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Int(23, DebugInfo::new("23", 10)))
        );
    }

    #[test]
    fn tokenize_misc() {
        let mut tokenizer = RushTokenizer::new("$x = 2.2; $cool = \"awesome\";");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Ident(
                Ident("$x".to_string()),
                DebugInfo::new("$x", 0),
            ))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("=".to_string(), DebugInfo::new("=", 3)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Float(2.2, DebugInfo::new("2.2", 5)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator(";".to_string(), DebugInfo::new(";", 8)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Ident(
                Ident("$cool".to_string()),
                DebugInfo::new("$cool", 10),
            ))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("=".to_string(), DebugInfo::new("=", 16)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Str(
                "awesome".to_string(),
                DebugInfo::new("\"awesome\"", 18),
            ))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator(";".to_string(), DebugInfo::new(";", 27)))
        );
    }
}
