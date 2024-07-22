use std::{collections::HashMap, fmt::Display, num::ParseIntError};

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

    // NB disallow labels beginning with `_` so they can be used for assembler directive hints
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Label(String),

    #[token("_FILL", |_| Hint::Fill)]
    HintFill(Hint),

    #[token("_STRINGZ", |_| Hint::Stringz)]
    HintStringz(Hint),

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

impl SymbolTable {
    pub fn new(symbols: HashMap<String, usize>, hints: HashMap<usize, Hint>) -> SymbolTable {
        SymbolTable { symbols, hints }
    }

    /// Loads a symbol table from input.
    pub fn load(source: &str) -> Result<SymbolTable, ParseError> {
        let mut symbols: HashMap<String, usize> = HashMap::new();
        let mut hints: HashMap<usize, Hint> = HashMap::new();

        let mut lexer = Token::lexer(source);

        while let Some(res) = lexer.next() {
            match res {
                Ok(Token::Addr(addr)) => {
                    let token = expect_token!(lexer)?;
                    match token {
                        Token::Label(label) => {
                            // TODO check for dupes
                            symbols.insert(label, addr.into());
                        }
                        Token::HintFill(hint) | Token::HintStringz(hint) => {
                            hints.insert(addr.into(), hint);
                        }
                        _ => {
                            return Err(ParseError::UnexpectedToken(
                                token,
                                lexer.extras.0,
                                lexer.slice().into(),
                            ));
                        }
                    }
                }
                Ok(token) => {
                    return Err(ParseError::UnexpectedToken(
                        token,
                        lexer.extras.0,
                        lexer.slice().into(),
                    ));
                }
                Err(e) => {
                    return Err(ParseError::LexError(
                        e,
                        lexer.extras.0,
                        lexer.slice().into(),
                    ))
                }
            }
        }

        Ok(SymbolTable::new(symbols, hints))
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new(HashMap::new(), HashMap::new())
    }
}

/* formatting */

impl Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (symbol, addr) in self.symbols.iter() {
            writeln!(f, "x{addr:04x} {symbol}")?;
        }
        for (addr, hint) in self.hints.iter() {
            writeln!(f, "x{addr:04x} {hint}")?;
        }
        Ok(())
    }
}

/* errors */

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
