use std::{collections::HashMap, num::ParseIntError};

use logos::{Logos, Skip};
use thiserror::Error;

use crate::Hint;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(extras = (u32,))] // line number
#[logos(skip r"([ \t\r]+|;.*)")] // ignore whitespace and comments
pub enum Token {
    #[regex(r"[xX][0-9a-fA-F]{1,4}", |lex| u16::from_str_radix(&lex.slice()[1..], 16))]
    Addr(u16),

    // NB we're going to disallow a label beginning with `_` so we can use it for assembler directive hints
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Label(String),

    #[token("_FILL")]
    HintFill,

    #[token("_STRINGZ")]
    HintStringz,

    #[token("\n", |lex| lex.extras.0 += 1; Skip)]
    NEWLINE,
}

macro_rules! expect_token {
    ($lexer:expr) => {
        match $lexer.next() {
            Some(Ok(result)) => Ok(result),
            Some(Err(e)) => Err(ParseError::LexError(
                e,
                $lexer.extras.0 + 1,
                $lexer.slice().to_string(),
            )),
            None => Err(ParseError::UnexpectedEOF($lexer.extras.0 + 1)),
        }
    };
    ($lexer:expr, $expected:pat) => {
        match $lexer.next() {
            Some(Ok($expected)) => Ok(()),
            Some(Ok(unexpected)) => Err(ParseError::UnexpectedToken(
                unexpected,
                $lexer.extras.0 + 1,
                $lexer.slice().to_string(),
            )),
            Some(Err(e)) => Err(ParseError::LexError(
                e,
                $lexer.extras.0 + 1,
                $lexer.slice().to_string(),
            )),
            None => Err(ParseError::UnexpectedEOF($lexer.extras.0 + 1)),
        }
    };
    ($lexer:expr, $expected:pat => $result:expr) => {
        match $lexer.next() {
            Some(Ok($expected)) => Ok($result),
            Some(Ok(unexpected)) => Err(ParseError::UnexpectedToken(
                unexpected,
                $lexer.extras.0 + 1,
                $lexer.slice().to_string(),
            )),
            Some(Err(e)) => Err(ParseError::LexError(
                e,
                $lexer.extras.0 + 1,
                $lexer.slice().to_string(),
            )),
            None => Err(ParseError::UnexpectedEOF($lexer.extras.0 + 1)),
        }
    };
}

pub struct SymbolTable {
    symbols: HashMap<String, usize>,
    hints: HashMap<usize, Hint>,
}

// format: ADDR (SYMBOL|HINT)
fn load_symbol_table(source: &str) -> Result<SymbolTable, LexError> {
    let symbols: HashMap<String, usize> = HashMap::new();
    let hints: HashMap<usize, Hint> = HashMap::new();

    let mut lexer = Token::lexer(source);

    while let Some(res) = lexer.next() {
        
    }

    Ok(SymbolTable::new(symbols, hints))
}

impl SymbolTable {
    pub fn new(symbols: HashMap<String, usize>, hints: HashMap<usize, Hint>) -> SymbolTable {
        SymbolTable { symbols, hints }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new(HashMap::new(), HashMap::new())
    }
}

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum LexError {
    #[error("error parsing u16: {0}")]
    ParseIntError(#[from] ParseIntError),

    #[default]
    #[error("unknown token")]
    UnknownToken,
}

#[derive(Error, Clone, Debug, PartialEq)]
pub enum ParseError {
    #[error("lex error on line {1}: {0} ({2})")]
    LexError(LexError, u32, String),

    #[error("unexpected token on line {1}: {0:?} ({2})")]
    UnexpectedToken(Token, u32, String),

    #[error("unexpected EOF on line {0}")]
    UnexpectedEOF(u32),
}
