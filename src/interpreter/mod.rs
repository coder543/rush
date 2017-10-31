pub mod ast;
pub mod builtins;
pub mod command;
pub mod tokenizer;
pub mod walker;

use self::ast::Expr;

use std::fmt;
use std::collections::HashMap;

pub type Memory = HashMap<Ident, Expr>;

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

    pub fn none() -> DebugInfo {
        DebugInfo {
            raw: String::from("<unknown>"),
            pos: 0,
        }
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

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Hash)]
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

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
