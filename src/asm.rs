use crate::{Instruction, Program};
use lalrpop_util::lalrpop_mod;
use logos::{Lexer, Logos};
use std::{
    io::{Error, Read},
    num::ParseIntError,
};
use thiserror::Error;

lalrpop_mod!(#[allow(overflowing_literals)] pub parser);

pub fn assemble(r: &mut dyn Read) -> Result<Program, Error> {
    let mut prog = Program::default();
    let parser = parser::ProgramParser::new();
    let mut buf = String::new();

    r.read_to_string(&mut buf)?;
    parser.parse(&mut prog, buf.as_str()).unwrap(); // TODO unsafe unwrap

    prog.resolve_symbols();

    Ok(prog)
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = ParseError)]
#[logos(skip r"([ \t\r\n]+|;.*)")] // ignore whitespace and comments
pub enum Token {
    /* op codes */
    /// ```
    /// use lc3::asm::Token;
    /// use logos::Logos;
    ///
    /// assert_eq!(Token::lexer("BR").next(),    Some(Ok(Token::BR(0b111))));
    /// assert_eq!(Token::lexer("BRnzp").next(), Some(Ok(Token::BR(0b111))));
    /// assert_eq!(Token::lexer("BRn").next(),   Some(Ok(Token::BR(0b100))));
    /// assert_eq!(Token::lexer("BRz").next(),   Some(Ok(Token::BR(0b010))));
    /// assert_eq!(Token::lexer("BRp").next(),   Some(Ok(Token::BR(0b001))));
    /// assert_eq!(Token::lexer("BRnz").next(),  Some(Ok(Token::BR(0b110))));
    /// assert_eq!(Token::lexer("BRnp").next(),  Some(Ok(Token::BR(0b101))));
    /// assert_eq!(Token::lexer("BRzp").next(),  Some(Ok(Token::BR(0b011))));
    /// ```
    #[regex(r"BRn?z?p?",
     callback = |lex| {
        let mut flags: u16 = 0;
        for c in lex.slice()[2..].chars() {
            match c {
                'n' | 'N' => flags |= 1 << 2,
                'z' | 'Z' => flags |= 1 << 1,
                'p' | 'P' => flags |= 1 << 0,
                _ => unreachable!(),
            }
        }
        // NB "The assembly language opcode BR is interpreted the same as BRnzp;
        // that is, always branch to the target address."
        if flags == 0 {
            flags = 0b111;
        }
        flags
    }, ignore(ascii_case))]
    BR(u16), /* branch */
    #[token("ADD", ignore(ascii_case))]
    ADD, /* add  */
    #[token("LD", ignore(ascii_case))]
    LD, /* load */
    #[token("ST", ignore(ascii_case))]
    ST, /* store */
    #[token("JSR", ignore(ascii_case))]
    JSR, /* jump register */
    #[token("JSRR", ignore(ascii_case))]
    JSRR, /* JSR, but takes a register argument */
    #[token("AND", ignore(ascii_case))]
    AND, /* bitwise and */
    #[token("LDR", ignore(ascii_case))]
    LDR, /* load register */
    #[token("STR", ignore(ascii_case))]
    STR, /* store register */
    #[token("RTI", ignore(ascii_case))]
    RTI, /* unused */
    #[token("NOT", ignore(ascii_case))]
    NOT, /* bitwise not */
    #[token("LDI", ignore(ascii_case))]
    LDI, /* load indirect */
    #[token("STI", ignore(ascii_case))]
    STI, /* store indirect */
    #[token("JMP", ignore(ascii_case))]
    JMP, /* jump */
    #[token("RET", ignore(ascii_case))]
    RET, /* equivalent to JMP R7 */
    #[token("RES", ignore(ascii_case))]
    RES, /* reserved (unused) */
    #[token("LEA", ignore(ascii_case))]
    LEA, /* load effective address */
    #[token("TRAP", ignore(ascii_case))]
    TRAP, /* execute trap */

    // traps
    #[token("GETC", ignore(ascii_case))]
    GETC, /* get character from keyboard, not echoed */
    #[token("OUT", ignore(ascii_case))]
    OUT, /* output a character */
    #[token("PUTS", ignore(ascii_case))]
    PUTS, /* output a word string */
    #[token("IN", ignore(ascii_case))]
    IN, /* get character from keyboard, echoed onto the terminal */
    #[token("PUTSP", ignore(ascii_case))]
    PUTSP, /* output a byte string */
    #[token("HALT", ignore(ascii_case))]
    HALT, /* halt the program */

    // registers
    /// ```
    /// use lc3::asm::Token;
    /// use logos::Logos;
    ///
    /// assert_eq!(Token::lexer("R0").next(), Some(Ok(Token::REG(0))));
    /// assert_eq!(Token::lexer("R1").next(), Some(Ok(Token::REG(1))));
    /// assert_eq!(Token::lexer("R2").next(), Some(Ok(Token::REG(2))));
    /// assert_eq!(Token::lexer("R3").next(), Some(Ok(Token::REG(3))));
    /// assert_eq!(Token::lexer("R4").next(), Some(Ok(Token::REG(4))));
    /// assert_eq!(Token::lexer("R5").next(), Some(Ok(Token::REG(5))));
    /// assert_eq!(Token::lexer("R6").next(), Some(Ok(Token::REG(6))));
    /// assert_eq!(Token::lexer("R7").next(), Some(Ok(Token::REG(7))));
    /// ```
    #[regex("R[0-7]", callback = |lex| {
        (lex.slice().as_bytes()[1] - b'0') as u16
    },
    ignore(ascii_case))]
    REG(u16),

    // assembler directives
    #[token(".ORIG", ignore(ascii_case))]
    ORIG, /* origin */
    #[token(".FILL", ignore(ascii_case))]
    FILL, /* fill a single address */
    #[token(".STRINGZ", ignore(ascii_case))]
    STRINGZ, /* ascii string literal */
    #[token(".END", ignore(ascii_case))]
    END, /* end of program */

    // literals
    #[regex(r"#-?[0-9]+|[xX][0-9a-fA-F]+", callback = |lex| {
        let radix = if lex.slice().chars().nth(0) == Some('#') {10} else {16};
        u16::from_str_radix(&lex.slice()[1..], radix)
    })]
    NumLit(u16),
    #[regex(r#""(?:[^"]|\\")*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StrLit(String),

    // labels
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Label(String),

    // punctuation
    #[token(",")]
    Comma,
}

pub struct ParseStream<'source> {
    lex: Lexer<'source, Token>,
}

impl<'source> ParseStream<'source> {
    pub fn new(source: &'source str) -> Self {
        ParseStream { lex: Token::lexer(source) }
    }

    pub fn expect_next_token(&mut self) -> Result<Token, ParseError> {
        if let Some(result) = self.lex.next() {
            result
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    pub fn expect_comma(&mut self) -> Result<(), ParseError> {
        let token = self.expect_next_token()?;

        if let Token::Comma = token {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "COMMA".to_string(),
                found: token,
            })
        }
    }

    pub fn expect_orig(&mut self) -> Result<(), ParseError> {
        let token = self.expect_next_token()?;

        if let Token::ORIG = token {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "ORIG".to_string(),
                found: token,
            })
        }
    }

    pub fn expect_numlit(&mut self) -> Result<u16, ParseError> {
        let token = self.expect_next_token()?;

        if let Token::NumLit(value) = token {
            Ok(value)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "NUMLIT".to_string(),
                found: token,
            })
        }
    }

    pub fn expect_reg(&mut self) -> Result<u16, ParseError> {
        let token = self.expect_next_token()?;

        if let Token::REG(value) = token {
            Ok(value)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "REG".to_string(),
                found: token,
            })
        }
    }
}

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum ParseError {
    #[error("unexpected token (expected {expected}, found {found:?}")]
    UnexpectedToken { expected: String, found: Token },
    #[error("unexpected EOF")]
    UnexpectedEOF,
    #[error("error parsing u16 from {0}")]
    ParseIntError(#[from] ParseIntError),
    #[default]
    #[error("unknown parse error")]
    Unknown,
}

pub fn parse<'source>(source: &'source impl ToString) -> Result<Program, ParseError> {
    let mut prog = Program::default();
    let binding = source.to_string();
    let mut stream = ParseStream::new(&binding);

    stream.expect_orig()?;
    prog.orig = stream.expect_numlit()?;

    let mut iaddr = 0u16;
    loop{
        let token = stream.expect_next_token()?;
        match token {
            // TODO what if there is add'l garbage after .END?
            Token::END => {
                break;
            }
            Token::Label(label) => {
                prog.syms.insert(label, iaddr);
            }
            _ => {
                parse_instruction(token, &mut stream)?;
            }
        }
        iaddr += 1;
    }

    Ok(prog)
}

fn parse_instruction(la: Token, stream: &mut ParseStream) -> Result<Instruction, ParseError> {
    match la {
        Token::ADD => {
            let dr = stream.expect_reg()?;
            stream.expect_comma()?;
            let sr1 = stream.expect_reg()?;
            stream.expect_comma()?;
            let token = stream.expect_next_token()?;
            match token {
                Token::REG(sr2) => Ok(Instruction::Add(dr, sr1, false, sr2)),
                Token::NumLit(imm5) => Ok(Instruction::Add(dr, sr1, true, imm5 & 0x1f)),
                _ => Err(ParseError::UnexpectedToken {
                    expected: "REG or imm5".to_string(),
                    found: token,
                }),
            }
        }
        _ => {
            unimplemented!()
        }
    }
}
