use interpreter::{Ident, DebugInfo};

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Token {
    Operator(String, DebugInfo),
    Str(String, DebugInfo),
    Bool(bool, DebugInfo),
    Int(i64, DebugInfo),
    Float(f64, DebugInfo),
    Ident(Ident, DebugInfo),
    Unknown(String, DebugInfo),
}

#[allow(unused)]
impl Token {
    pub fn get_debug_info(&self) -> DebugInfo {
        match *self {
            Token::Operator(_, ref debug) |
            Token::Str(_, ref debug) |
            Token::Int(_, ref debug) |
            Token::Float(_, ref debug) |
            Token::Bool(_, ref debug) |
            Token::Ident(_, ref debug) |
            Token::Unknown(_, ref debug) => debug.clone(),
        }
    }

    pub fn expect_operator(&self, err: &str) -> Result<String, String> {
        match *self {
            Token::Operator(ref op, _) => Ok(op.clone()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_operator_specific(&self, op: &str) -> Result<(), String> {
        match *self {
            Token::Operator(ref operator, _) if operator == op => Ok(()),
            _ => Err(format!("{}, expected '{}'", self.get_debug_info(), op)),
        }
    }

    pub fn expect_str(&self, err: &str) -> Result<String, String> {
        match *self {
            Token::Str(ref str_val, _) => Ok(str_val.clone()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_int(&self, err: &str) -> Result<i64, String> {
        match *self {
            Token::Int(int, _) => Ok(int),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_float(&self, err: &str) -> Result<f64, String> {
        match *self {
            Token::Float(float, _) => Ok(float),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_ident(&self, err: &str) -> Result<Ident, String> {
        match *self {
            Token::Ident(ref id, _) => Ok(id.clone()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_ident_specific(&self, id: Ident) -> Result<(), String> {
        match *self {
            Token::Ident(ref ident, _) if *ident == id => Ok(()),
            _ => Err(format!("{}, expected '{}'", self.get_debug_info(), id.0)),
        }
    }

    pub fn expect_unknown(&self, err: &str) -> Result<String, String> {
        match *self {
            Token::Unknown(ref unknown, _) => Ok(unknown.clone()),
            _ => Err(format!("{}, {}", self.get_debug_info(), err)),
        }
    }

    pub fn expect_unknown_specific(&self, unknown: &str) -> Result<(), String> {
        match *self {
            Token::Unknown(ref unk, _) if *unk == unknown => Ok(()),
            _ => Err(format!("{}, expected '{}'", self.get_debug_info(), unknown)),
        }
    }
}

static SYM_OPS: [&str; 23] = [
    ">=",
    "<=",
    "==",
    "!=",
    "&&",
    "||",
    "+",
    "-",
    "*",
    "/",
    "%",
    ",",
    "=",
    "!",
    ";",
    "[",
    "]",
    "(",
    ")",
    "{",
    "}",
    ">",
    "<",
];

static WORD_OPS: [&str; 3] = ["or", "and", "not"];

use std::str::Split;
use std::collections::VecDeque;
pub struct RushTokenizer<'a> {
    token_stream: Split<'a, &'static [char]>,
    ready_tokens: VecDeque<String>,
    pos: u64,
    last_len: u64,
    peeked: Option<Token>,
}

impl<'a> RushTokenizer<'a> {
    pub fn new(buffer: &'a str) -> RushTokenizer<'a> {
        RushTokenizer {
            token_stream: buffer.split(&[' ', '\n']),
            ready_tokens: VecDeque::new(),
            pos: 0,
            last_len: 0,
            peeked: None,
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
            raw_token = loop {
                let tmp = try_opt!(self.token_stream.next()).to_string();
                if tmp.len() > 0 {
                    break tmp;
                }
            }
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
                if !next_token.ends_with('"') {
                    let quote_pos = raw_token.find('"').unwrap() + 1;
                    let new = String::from(&raw_token[quote_pos..]);
                    self.last_len -= new.len() as u64 + 1;
                    self.ready_tokens.push_front(new);
                    raw_token = String::from(&raw_token[..quote_pos]);
                }
                raw_token.pop();
                let strlit = raw_token.clone();
                raw_token = ["\"", &raw_token, "\""].concat();
                let debug = DebugInfo::new(raw_token, position);
                return Some(Token::Str(strlit, debug));
            }
            next_token = try_opt!(self.next_raw_token());
            raw_token.push(' ');
            raw_token.push_str(&next_token);
        }
    }

    fn get_ident(&mut self, raw_token: &str) -> Option<Ident> {
        Ident::try_new(raw_token)
    }

    fn get_bool(&mut self, raw_token: &str) -> Option<bool> {
        if raw_token == "true" {
            Some(true)
        } else if raw_token == "false" {
            Some(false)
        } else {
            None
        }
    }

    fn get_int(&mut self, raw_token: &str) -> Option<i64> {
        raw_token.parse().ok()
    }

    fn get_float(&mut self, raw_token: &str) -> Option<f64> {
        raw_token.parse().ok()
    }

    fn is_operator(&self, raw_token: &str) -> bool {
        SYM_OPS.contains(&raw_token) || WORD_OPS.contains(&raw_token)
    }

    pub fn peek(&mut self) -> Option<&Token> {
        if let Some(_) = self.peeked {
            return self.peeked.as_ref();
        }
        let next = try_opt!(self.next());
        self.peeked = Some(next);
        return self.peeked.as_ref();
    }

    pub fn next_basic(&mut self) -> Option<Token> {
        assert!(
            self.peeked.is_none(),
            "can't call next_basic while there is a peeked token!"
        );

        let raw_token = try_opt!(self.next_raw_token());

        if let Some(ident) = self.get_ident(&raw_token) {
            return Some(Token::Ident(ident, DebugInfo::new(raw_token, self.pos)));
        }

        Some(Token::Str(
            raw_token.to_string(),
            DebugInfo::new(raw_token, self.pos),
        ))
    }

    fn subdivide_token(&mut self, raw_token: &mut String) {
        #[cfg(test)]
        println!("token to subdivide is {}", raw_token);
        if raw_token.len() == 1 {
            return;
        }

        for i in 0..raw_token.len() {
            for op in &SYM_OPS {
                if raw_token[i..].starts_with(op) {
                    let new = if i == 0 {
                        raw_token.split_off(op.len())
                    } else {
                        raw_token.split_off(i)
                    };

                    if new.len() == 0 {
                        return;
                    }
                    self.last_len -= new.len() as u64 + 1;
                    self.ready_tokens.push_front(new);
                    return;
                }
            }
        }
    }
}

impl<'a> Iterator for RushTokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        if let Some(token) = self.peeked.take() {
            return Some(token);
        }

        let mut raw_token = try_opt!(self.next_raw_token());

        self.subdivide_token(&mut raw_token);

        //this print statement only exists during unit testing
        #[cfg(test)]
        println!("token at pos {} is {:?}", self.pos, raw_token);

        if self.is_operator(&raw_token) {
            let mut raw_token = raw_token;
            let debug = DebugInfo::new(raw_token.clone(), self.pos);

            raw_token = match raw_token.as_str() { 
                "not" => "!",
                "and" => "&&",
                "or" => "||",
                _ => &raw_token,
            }.to_string();

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

        if let Some(bool_val) = self.get_bool(&raw_token) {
            return Some(Token::Bool(bool_val, DebugInfo::new(raw_token, self.pos)));
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
    fn tokenize_bool() {
        let mut tokenizer = RushTokenizer::new("true false other");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Bool(true, DebugInfo::new("true", 0)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Bool(false, DebugInfo::new("false", 5)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Unknown(
                "other".to_string(),
                DebugInfo::new("other", 11),
            ))
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
    fn tokenize_string2() {
        let mut tokenizer = RushTokenizer::new("\"hello test\")");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Str(
                "hello test".to_string(),
                DebugInfo::new("\"hello test\"", 0),
            ))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator(")".to_string(), DebugInfo::new(")", 12)))
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
    fn tokenize_word_operators() {
        let mut tokenizer = RushTokenizer::new("and or not");
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("&&".to_string(), DebugInfo::new("and", 0)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("||".to_string(), DebugInfo::new("or", 4)))
        );
        assert_eq!(
            tokenizer.next(),
            Some(Token::Operator("!".to_string(), DebugInfo::new("not", 7)))
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
