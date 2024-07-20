use crate::{Hint, Instruction, Op, Program, Trap};
use logos::{Lexer, Logos};
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = ParseError)]
#[logos(skip r"([ \t\r\n]+|;.*)")] // ignore whitespace and comments
pub enum Token {
    /* operations */
    /// ```
    /// use lc3::asm::Token;
    /// use logos::Logos;
    ///
    /// assert_eq!(Token::lexer("BR").next(),    Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b111 << 9)))));
    /// assert_eq!(Token::lexer("BRnzp").next(), Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b111 << 9)))));
    /// assert_eq!(Token::lexer("BRn").next(),   Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b100 << 9)))));
    /// assert_eq!(Token::lexer("BRz").next(),   Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b010 << 9)))));
    /// assert_eq!(Token::lexer("BRp").next(),   Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b001 << 9)))));
    /// assert_eq!(Token::lexer("BRnz").next(),  Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b110 << 9)))));
    /// assert_eq!(Token::lexer("BRnp").next(),  Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b101 << 9)))));
    /// assert_eq!(Token::lexer("BRzp").next(),  Some(Ok(Token::BR((Op::BR as u16) << 12 | (0b011 << 9)))));
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
        ((Op::BR as u16) << 12) | (flags << 9)
    }, ignore(ascii_case))]
    BR(u16),
    #[token("ADD", |_| ((Op::ADD as u16) << 12), ignore(ascii_case))]
    ADD(u16),
    #[token("LD", |_| ((Op::LD as u16) << 12), ignore(ascii_case))]
    LD(u16),
    #[token("ST", |_| ((Op::ST as u16) << 12), ignore(ascii_case))]
    ST(u16),
    #[token("JSR", |_| (((Op::JSR as u16) << 12) | (1 << 11)), ignore(ascii_case))]
    JSR(u16),
    #[token("JSRR", |_| ((Op::JSR as u16) << 12), ignore(ascii_case))]
    JSRR(u16),
    #[token("AND", |_| ((Op::AND as u16) << 12), ignore(ascii_case))]
    AND(u16),
    #[token("LDR", |_| ((Op::LDR as u16) << 12), ignore(ascii_case))]
    LDR(u16),
    #[token("STR", |_| ((Op::STR as u16) << 12), ignore(ascii_case))]
    STR(u16),
    #[token("RTI", |_| ((Op::RTI as u16) << 12), ignore(ascii_case))]
    RTI(u16),
    #[token("NOT", |_| ((Op::NOT as u16) << 12), ignore(ascii_case))]
    NOT(u16),
    #[token("LDI", |_| ((Op::LDI as u16) << 12), ignore(ascii_case))]
    LDI(u16),
    #[token("STI", |_| ((Op::STI as u16) << 12), ignore(ascii_case))]
    STI(u16),
    #[token("JMP", |_| ((Op::JMP as u16) << 12), ignore(ascii_case))]
    JMP(u16),
    #[token("RET", |_| (((Op::JMP as u16) << 12) | (7 << 6)), ignore(ascii_case))]
    RET(u16),
    #[token("LEA", |_| ((Op::LEA as u16) << 12), ignore(ascii_case))]
    LEA(u16),
    #[token("TRAP", |_| ((Op::TRAP as u16) << 12), ignore(ascii_case))]
    TRAP(u16),

    /* traps */
    #[token("GETC", |_| (((Op::TRAP as u16) << 12) | (Trap::GETC as u16)), ignore(ascii_case))]
    GETC(u16),
    #[token("OUT", |_| (((Op::TRAP as u16) << 12) | (Trap::OUT as u16)), ignore(ascii_case))]
    OUT(u16),
    #[token("PUTS", |_| (((Op::TRAP as u16) << 12) | (Trap::PUTS as u16)), ignore(ascii_case))]
    PUTS(u16),
    #[token("IN", |_| (((Op::TRAP as u16) << 12) | (Trap::IN as u16)), ignore(ascii_case))]
    IN(u16),
    #[token("PUTSP", |_| (((Op::TRAP as u16) << 12) | (Trap::PUTSP as u16)), ignore(ascii_case))]
    PUTSP(u16),
    #[token("HALT", |_| (((Op::TRAP as u16) << 12) | (Trap::HALT as u16)), ignore(ascii_case))]
    HALT(u16),

    /* registers */
    /// ```rust
    /// use lc3::Reg;
    /// use lc3::asm::Token;
    /// use logos::Logos;
    ///
    /// assert_eq!(Token::lexer("R0").next(), Some(Ok(Token::REG(Reg::R0))));
    /// assert_eq!(Token::lexer("R1").next(), Some(Ok(Token::REG(Reg::R1))));
    /// assert_eq!(Token::lexer("R2").next(), Some(Ok(Token::REG(Reg::R2))));
    /// assert_eq!(Token::lexer("R3").next(), Some(Ok(Token::REG(Reg::R3))));
    /// assert_eq!(Token::lexer("R4").next(), Some(Ok(Token::REG(Reg::R4))));
    /// assert_eq!(Token::lexer("R5").next(), Some(Ok(Token::REG(Reg::R5))));
    /// assert_eq!(Token::lexer("R6").next(), Some(Ok(Token::REG(Reg::R6))));
    /// assert_eq!(Token::lexer("R7").next(), Some(Ok(Token::REG(Reg::R7))));
    /// ```
    #[regex("R[0-7]", callback = |lex| {
        (lex.slice().as_bytes()[1] - b'0') as u16
    },
    ignore(ascii_case))]
    REG(u16),

    /* assembler directives */
    #[token(".ORIG", ignore(ascii_case))]
    ORIG, /* origin */
    #[token(".FILL", ignore(ascii_case))]
    FILL, /* fill a single address */
    #[token(".STRINGZ", ignore(ascii_case))]
    STRINGZ, /* ascii string literal */
    #[token(".END", ignore(ascii_case))]
    END, /* end of program */

    /* literals */ // TODO tests!
    #[regex(r"#-?[0-9]+|[xX][0-9a-fA-F]+", callback = |lex| {
        let radix = if lex.slice().chars().nth(0) == Some('#') {10} else {16};
        let value = i16::from_str_radix(&lex.slice()[1..], radix)?;
        Ok::<u16, ParseError>(value as u16)
    })]
    NUMLIT(u16),
    #[regex(r#""(?:[^"]|\\")*""#, callback = |lex| {
        lex.slice()[1..lex.slice().len()-1].as_bytes().into_iter().map(|b| *b as u16).collect::<Vec<u16>>()
    })]
    STRLIT(Vec<u16>),

    /* labels */
    // NB we're going to disallow a label beginning with `_` so we can use it for assembler directive hints
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    LABEL(String),

    /* punctuation */
    #[token(",")]
    COMMA,
}

macro_rules! expect_token {
    ($lexer:expr) => {
        match $lexer.next() {
            Some(result) => result,
            None => Err(ParseError::UnexpectedEOF),
        }
    };
    ($lexer:expr, $expected:pat) => {
        match $lexer.next() {
            Some(Ok($expected)) => Ok(()),
            Some(Ok(unexpected)) => Err(ParseError::UnexpectedToken { found: unexpected }),
            Some(Err(e)) => Err(e),
            None => Err(ParseError::UnexpectedEOF),
        }
    };
    ($lexer:expr, $expected:pat => $result:expr) => {
        match $lexer.next() {
            Some(Ok($expected)) => Ok($result),
            Some(Ok(unexpected)) => Err(ParseError::UnexpectedToken { found: unexpected }),
            Some(Err(e)) => Err(e),
            None => Err(ParseError::UnexpectedEOF),
        }
    };
}

pub fn assemble_program(source: &str) -> Result<Program, ParseError> {
    let prog = parse_program(source)?;

    Ok(prog)
}

pub fn parse_program(source: &str) -> Result<Program, ParseError> {
    let mut prog = Program::default();
    let mut lexer = Token::lexer(source);

    expect_token!(lexer, Token::ORIG => ())?;
    prog.origin = expect_token!(lexer, Token::NUMLIT(addr) => addr)?;

    loop {
        let token = expect_token!(lexer)?;
        match token {
            Token::END => {
                break;
            }
            Token::LABEL(label) => {
                // TODO check for duplicate symbols
                prog.symbols.insert(label, prog.instructions.len());
            }
            /* assembler directives */
            Token::FILL => {
                let token = expect_token!(lexer)?;
                match token {
                    Token::NUMLIT(word) => prog
                        .instructions
                        .push(Instruction::new(word + prog.origin, None, Some(Hint::Fill))),
                    Token::LABEL(label) => prog.instructions.push(Instruction::new(0, Some(label), Some(Hint::Fill))),
                    _ => return Err(ParseError::UnexpectedToken { found: token }),
                }
            }
            Token::STRINGZ => {
                let strlit = expect_token!(lexer, Token::STRLIT(s) => s)?;
                prog.instructions
                    .push(Instruction::new(strlit[0], None, Some(Hint::Stringz)));
                for c in strlit[1..].into_iter() {
                    prog.instructions.push(Instruction::new(*c, None, None));
                }
            }
            _ => {
                prog.instructions
                    .push(parse_instruction_la(&mut lexer, token)?);
            }
        }
    }

    Ok(prog)
}

/// Parse a single LC3 instruction (outside of the context of a full program).
///
/// ```rust
/// use lc3::{Instruction, Op, Trap};
/// use lc3::asm::parse_instruction;
/// /* operations */
/// assert_eq!(parse_instruction("ADD R0, R1, R2"), Ok(Instruction::new((Op::ADD as u16) << 12 | 0 << 9 | 1 << 6 | 2, None, None)));
/// assert_eq!(parse_instruction("AND R3, R4, R5"), Ok(Instruction::new((Op::AND as u16) << 12 | 3 << 9 | 4 << 6 | 5, None, None)));
/// assert_eq!(parse_instruction("ADD R6, R7, #-7"), Ok(Instruction::new((Op::ADD as u16) << 12 | 6 << 9 | 7 << 6 | 1 << 5 | ((-7i16 & 0x1f) as u16), None, None)));
/// assert_eq!(parse_instruction("AND R6, R7, #-7"), Ok(Instruction::new((Op::AND as u16) << 12 | 6 << 9 | 7 << 6 | 1 << 5 | ((-7i16 & 0x1f) as u16), None, None)));
/// assert_eq!(parse_instruction("BR x123"), Ok(Instruction::new((Op::BR as u16) << 12 | 7 << 9 | (0x123 & 0x1ff), None, None)));
/// assert_eq!(parse_instruction("BR LABEL"), Ok(Instruction::new((Op::BR as u16) << 12 | 7 << 9, Some(String::from("LABEL")), None)));
/// assert_eq!(parse_instruction("JMP R1"), Ok(Instruction::new((Op::JMP as u16) << 12 | 1 << 6, None, None)));
/// assert_eq!(parse_instruction("JSR x123"), Ok(Instruction::new((Op::JSR as u16) << 12 | 1 << 11 | (0x123 & 0x7ff), None, None)));
/// assert_eq!(parse_instruction("JSRR R1"), Ok(Instruction::new((Op::JSR as u16) << 12 | 1 << 6, None, None)));
/// assert_eq!(parse_instruction("LD R1, x123"), Ok(Instruction::new((Op::LD as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None, None)));
/// assert_eq!(parse_instruction("LD R1, LABEL"), Ok(Instruction::new((Op::LD as u16) << 12 | 1 << 9, Some(String::from("LABEL")), None)));
/// assert_eq!(parse_instruction("LDI R1, x123"), Ok(Instruction::new((Op::LDI as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None, None)));
/// assert_eq!(parse_instruction("LDI R1, LABEL"), Ok(Instruction::new((Op::LDI as u16) << 12 | 1 << 9, Some(String::from("LABEL")), None)));
/// assert_eq!(parse_instruction("LEA R1, x123"), Ok(Instruction::new((Op::LEA as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None, None)));
/// assert_eq!(parse_instruction("LEA R1, LABEL"), Ok(Instruction::new((Op::LEA as u16) << 12 | 1 << 9, Some(String::from("LABEL")), None)));
/// assert_eq!(parse_instruction("RET"), Ok(Instruction::new((Op::JMP as u16) << 12 | 7 << 6, None, None)));
/// assert_eq!(parse_instruction("ST R1, x123"), Ok(Instruction::new((Op::ST as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None, None)));
/// assert_eq!(parse_instruction("ST R1, LABEL"), Ok(Instruction::new((Op::ST as u16) << 12 | 1 << 9, Some(String::from("LABEL")), None)));
/// assert_eq!(parse_instruction("STI R1, x123"), Ok(Instruction::new((Op::STI as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None, None)));
/// assert_eq!(parse_instruction("STI R1, LABEL"), Ok(Instruction::new((Op::STI as u16) << 12 | 1 << 9, Some(String::from("LABEL")), None)));
/// assert_eq!(parse_instruction("LDR R6, R7, #-7"), Ok(Instruction::new((Op::LDR as u16) << 12 | 6 << 9 | 7 << 6 | ((-7i16 & 0x3f) as u16), None, None)));
/// assert_eq!(parse_instruction("STR R6, R7, #-7"), Ok(Instruction::new((Op::STR as u16) << 12 | 6 << 9 | 7 << 6 | ((-7i16 & 0x3f) as u16), None, None)));
/// /* traps */
/// assert_eq!(parse_instruction("TRAP x23"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (0x23 & 0xff), None, None)));
/// assert_eq!(parse_instruction("GETC"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::GETC as u16), None, None)));
/// assert_eq!(parse_instruction("OUT"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::OUT as u16), None, None)));
/// assert_eq!(parse_instruction("PUTS"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::PUTS as u16), None, None)));
/// assert_eq!(parse_instruction("IN"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::IN as u16), None, None)));
/// assert_eq!(parse_instruction("PUTSP"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::PUTSP as u16), None, None)));
/// assert_eq!(parse_instruction("HALT"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::HALT as u16), None, None)));
/// ```
pub fn parse_instruction(source: &str) -> Result<Instruction, ParseError> {
    let mut lexer = Token::lexer(source);
    let token = expect_token!(lexer)?;
    parse_instruction_la(&mut lexer, token)
}

fn parse_instruction_la(lexer: &mut Lexer<Token>, la: Token) -> Result<Instruction, ParseError> {
    match la {
        /* operations */
        Token::ADD(op) | Token::AND(op) => {
            let r1 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let r2 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let token = expect_token!(lexer)?;
            match token {
                Token::REG(r3) => Ok(Instruction::new(op | r1 << 9 | r2 << 6 | r3, None, None)),
                Token::NUMLIT(imm5) => Ok(Instruction::new(
                    op | r1 << 9 | r2 << 6 | 1 << 5 | (imm5 & 0x1f),
                    None, None
                )),
                _ => Err(ParseError::UnexpectedToken { found: token }),
            }
        }
        Token::BR(op) => {
            let token = expect_token!(lexer)?;
            match token {
                Token::NUMLIT(pcoffset9) => Ok(Instruction::new(op | (pcoffset9 & 0x1ff), None, None)),
                Token::LABEL(label) => Ok(Instruction::new(op, Some(label), None)),
                _ => Err(ParseError::UnexpectedToken { found: token }),
            }
        }
        Token::JMP(op) | Token::JSRR(op) => {
            let r1 = expect_token!(lexer, Token::REG(r) => r)?;
            Ok(Instruction::new(op | r1 << 6, None, None))
        }
        Token::RET(op) => Ok(Instruction::new(op, None, None)),
        Token::JSR(op) => {
            let token = expect_token!(lexer)?;
            match token {
                Token::NUMLIT(pcoffset11) => Ok(Instruction::new(op | (pcoffset11 & 0x7ff), None, None)),
                Token::LABEL(label) => Ok(Instruction::new(op, Some(label), None)),
                _ => Err(ParseError::UnexpectedToken { found: token }),
            }
        }
        Token::LD(op) | Token::LDI(op) | Token::LEA(op) | Token::ST(op) | Token::STI(op) => {
            let r1 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let token = expect_token!(lexer)?;
            match token {
                Token::NUMLIT(pcoffset9) => {
                    Ok(Instruction::new(op | r1 << 9 | (pcoffset9 & 0x1ff), None, None))
                }
                Token::LABEL(label) => Ok(Instruction::new(op | r1 << 9, Some(label), None)),
                _ => Err(ParseError::UnexpectedToken { found: token }),
            }
        }
        Token::LDR(op) | Token::STR(op) => {
            let r1 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let r2 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let offset6 = expect_token!(lexer, Token::NUMLIT(offset6) => offset6)?;
            Ok(Instruction::new(
                op | r1 << 9 | r2 << 6 | (offset6 & 0x3f),
                None, None
            ))
        }
        Token::NOT(op) => {
            let r1 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let r2 = expect_token!(lexer, Token::REG(r) => r)?;
            Ok(Instruction::new(op | r1 << 9 | r2 << 6 | 0b111111, None, None))
        }
        Token::RTI(op) => Ok(Instruction::new(op, None, None)),

        /* traps */
        Token::TRAP(op) => {
            let trapvect8 = expect_token!(lexer, Token::NUMLIT(trapvect8) => trapvect8)?;
            Ok(Instruction::new(op | (trapvect8 & 0xff), None, None))
        }
        Token::GETC(op)
        | Token::OUT(op)
        | Token::PUTS(op)
        | Token::IN(op)
        | Token::PUTSP(op)
        | Token::HALT(op) => Ok(Instruction::new(op, None, None)),
        _ => Err(ParseError::UnexpectedToken { found: la }),
    }
}

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum ParseError {
    #[error("unexpected token: {found:?}")]
    UnexpectedToken {
        /* expected: String,*/ found: Token,
    },
    #[error("unexpected EOF")]
    UnexpectedEOF,
    #[error("error parsing u16 from {0}")]
    ParseIntError(#[from] ParseIntError),
    #[default]
    #[error("unknown parse error")]
    Unknown,
}
