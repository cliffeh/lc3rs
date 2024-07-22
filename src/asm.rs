use crate::{Hint, Instruction, Op, Program, Trap};
use logos::{Lexer, Logos, Skip};
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(extras = (u32,))] // line number
#[logos(skip r"([ \t\r]+|;.*)")] // ignore whitespace and comments
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

    /* literals */
    #[regex(r"#-?[0-9]+|[xX][0-9a-fA-F]{1,4}", callback = |lex| {
        let radix = if lex.slice().chars().nth(0) == Some('#') {10} else {16};
        let value = i32::from_str_radix(&lex.slice()[1..], radix)?;
        Ok::<u16, LexError>(value as u16)
    })]
    NUMLIT(u16),
    /// Returns the original unescaped string as a byte vector.
    /// TODO write a test for this
    #[regex(r#""(?:[^"]|\\")*""#, |lex| String::from(&lex.slice()[1..lex.slice().len()-1]))]
    STRLIT(String),

    /* labels */
    // NB we're going to disallow a label beginning with `_` so we can use it for assembler directive hints
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    LABEL(String),

    /* punctuation */
    #[token(",")]
    COMMA,

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

impl Program {
    /// Parse LC3 source assembly into a binary program and resolve all symbol references.
    pub fn assemble(source: &str) -> Result<Program, ParseError> {
        let mut prog = Program::parse(source)?;
        if let Err(e) = prog.resolve_symbols() {
            return Err(ParseError::SymbolError(e));
        }
        Ok(prog)
    }

    /// Parse LC3 source assembly into a binary program, _without_ resolving symbols references into addresses.
    fn parse(source: &str) -> Result<Program, ParseError> {
        let mut prog = Program::default();
        let mut lexer = Token::lexer(source);

        expect_token!(lexer, Token::ORIG)?;
        prog.origin = expect_token!(lexer, Token::NUMLIT(addr) => addr)?;

        loop {
            let token = expect_token!(lexer)?;
            match token {
                Token::END => {
                    break;
                }
                Token::LABEL(label) => {
                    if let Some(_) = prog.symbols.get(&label) {
                        return Err(ParseError::SymbolError(format!(
                            "duplicate symbol: {}",
                            label
                        )));
                    }
                    prog.symbols.insert(label, prog.instructions.len());
                }
                /* assembler directives */
                Token::FILL => {
                    prog.hints.insert(prog.instructions.len(), Hint::Fill);
                    let token = expect_token!(lexer)?;
                    match token {
                        Token::NUMLIT(word) => prog.instructions.push(Instruction::new(word, None)),
                        Token::LABEL(label) => {
                            prog.instructions.push(Instruction::new(0, Some(label)))
                        }
                        _ => {
                            return Err(ParseError::UnexpectedToken(
                                token,
                                lexer.extras.0 + 1,
                                lexer.slice().to_string(),
                            ))
                        }
                    }
                }
                Token::STRINGZ => {
                    prog.hints.insert(prog.instructions.len(), Hint::Stringz);
                    let escaped = expect_token!(lexer, Token::STRLIT(s) => s)?;
                    for b in unescape(&escaped).as_bytes() {
                        prog.instructions.push(Instruction::new(*b as u16, None));
                    }
                    prog.instructions.push(Instruction::new(0, None));
                }
                _ => {
                    prog.instructions
                        .push(Instruction::parse_la(&mut lexer, token)?);
                }
            }
        }

        Ok(prog)
    }
}

impl Instruction {
    /// Parse a single LC3 instruction (outside of the context of a full program).
    ///
    /// Most users won't actually ever need to use this, but it's handy for testing/debugging.
    ///
    /// ```rust
    /// use lc3::{Instruction, Op, Trap};
    ///
    /// /* operations */
    /// assert_eq!(Instruction::parse_instruction("ADD R0, R1, R2"), Ok(Instruction::new((Op::ADD as u16) << 12 | 0 << 9 | 1 << 6 | 2, None)));
    /// assert_eq!(Instruction::parse_instruction("AND R3, R4, R5"), Ok(Instruction::new((Op::AND as u16) << 12 | 3 << 9 | 4 << 6 | 5, None)));
    /// assert_eq!(Instruction::parse_instruction("ADD R6, R7, #-7"), Ok(Instruction::new((Op::ADD as u16) << 12 | 6 << 9 | 7 << 6 | 1 << 5 | ((-7i16 & 0x1f) as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("AND R6, R7, #-7"), Ok(Instruction::new((Op::AND as u16) << 12 | 6 << 9 | 7 << 6 | 1 << 5 | ((-7i16 & 0x1f) as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("BR x123"), Ok(Instruction::new((Op::BR as u16) << 12 | 7 << 9 | (0x123 & 0x1ff), None)));
    /// assert_eq!(Instruction::parse_instruction("BR LABEL"), Ok(Instruction::new((Op::BR as u16) << 12 | 7 << 9, Some(String::from("LABEL")))));
    /// assert_eq!(Instruction::parse_instruction("JMP R1"), Ok(Instruction::new((Op::JMP as u16) << 12 | 1 << 6, None)));
    /// assert_eq!(Instruction::parse_instruction("JSR x123"), Ok(Instruction::new((Op::JSR as u16) << 12 | 1 << 11 | (0x123 & 0x7ff),None)));
    /// assert_eq!(Instruction::parse_instruction("JSRR R1"), Ok(Instruction::new((Op::JSR as u16) << 12 | 1 << 6, None)));
    /// assert_eq!(Instruction::parse_instruction("LD R1, x123"), Ok(Instruction::new((Op::LD as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None)));
    /// assert_eq!(Instruction::parse_instruction("LD R1, LABEL"), Ok(Instruction::new((Op::LD as u16) << 12 | 1 << 9, Some(String::from("LABEL")))));
    /// assert_eq!(Instruction::parse_instruction("LDI R1, x123"), Ok(Instruction::new((Op::LDI as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None)));
    /// assert_eq!(Instruction::parse_instruction("LDI R1, LABEL"), Ok(Instruction::new((Op::LDI as u16) << 12 | 1 << 9, Some(String::from("LABEL")))));
    /// assert_eq!(Instruction::parse_instruction("LEA R1, x123"), Ok(Instruction::new((Op::LEA as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None)));
    /// assert_eq!(Instruction::parse_instruction("LEA R1, LABEL"), Ok(Instruction::new((Op::LEA as u16) << 12 | 1 << 9, Some(String::from("LABEL")))));
    /// assert_eq!(Instruction::parse_instruction("RET"), Ok(Instruction::new((Op::JMP as u16) << 12 | 7 << 6, None)));
    /// assert_eq!(Instruction::parse_instruction("ST R1, x123"), Ok(Instruction::new((Op::ST as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None)));
    /// assert_eq!(Instruction::parse_instruction("ST R1, LABEL"), Ok(Instruction::new((Op::ST as u16) << 12 | 1 << 9, Some(String::from("LABEL")))));
    /// assert_eq!(Instruction::parse_instruction("STI R1, x123"), Ok(Instruction::new((Op::STI as u16) << 12 | 1 << 9 | (0x123 & 0x1ff), None)));
    /// assert_eq!(Instruction::parse_instruction("STI R1, LABEL"), Ok(Instruction::new((Op::STI as u16) << 12 | 1 << 9, Some(String::from("LABEL")))));
    /// assert_eq!(Instruction::parse_instruction("LDR R6, R7, #-7"), Ok(Instruction::new((Op::LDR as u16) << 12 | 6 << 9 | 7 << 6 | ((-7i16 & 0x3f) as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("STR R6, R7, #-7"), Ok(Instruction::new((Op::STR as u16) << 12 | 6 << 9 | 7 << 6 | ((-7i16 & 0x3f) as u16), None)));
    ///
    /// /* traps */
    /// assert_eq!(Instruction::parse_instruction("TRAP x23"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (0x23 & 0xff), None)));
    /// assert_eq!(Instruction::parse_instruction("GETC"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::GETC as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("OUT"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::OUT as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("PUTS"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::PUTS as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("IN"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::IN as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("PUTSP"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::PUTSP as u16), None)));
    /// assert_eq!(Instruction::parse_instruction("HALT"), Ok(Instruction::new((Op::TRAP as u16) << 12 | (Trap::HALT as u16), None)));
    /// ```
    pub fn parse_instruction(source: &str) -> Result<Instruction, ParseError> {
        let mut lexer = Token::lexer(source);
        let token = expect_token!(lexer)?;
        Instruction::parse_la(&mut lexer, token)
    }

    fn parse_la(lexer: &mut Lexer<Token>, la: Token) -> Result<Instruction, ParseError> {
        match la {
            /* operations */
            Token::ADD(op) | Token::AND(op) => {
                let r1 = expect_token!(lexer, Token::REG(r) => r)?;
                expect_token!(lexer, Token::COMMA)?;
                let r2 = expect_token!(lexer, Token::REG(r) => r)?;
                expect_token!(lexer, Token::COMMA)?;
                let token = expect_token!(lexer)?;
                match token {
                    Token::REG(r3) => Ok(Instruction::new(op | r1 << 9 | r2 << 6 | r3, None)),
                    Token::NUMLIT(imm5) => Ok(Instruction::new(
                        op | r1 << 9 | r2 << 6 | 1 << 5 | (imm5 & 0x1f),
                        None,
                    )),
                    _ => Err(ParseError::UnexpectedToken(
                        token,
                        lexer.extras.0 + 1,
                        lexer.slice().to_string(),
                    )),
                }
            }
            Token::BR(op) => {
                let token = expect_token!(lexer)?;
                match token {
                    Token::NUMLIT(pcoffset9) => {
                        Ok(Instruction::new(op | (pcoffset9 & 0x1ff), None))
                    }
                    Token::LABEL(label) => Ok(Instruction::new(op, Some(label))),
                    _ => Err(ParseError::UnexpectedToken(
                        token,
                        lexer.extras.0 + 1,
                        lexer.slice().to_string(),
                    )),
                }
            }
            Token::JMP(op) | Token::JSRR(op) => {
                let r1 = expect_token!(lexer, Token::REG(r) => r)?;
                Ok(Instruction::new(op | r1 << 6, None))
            }
            Token::RET(op) => Ok(Instruction::new(op, None)),
            Token::JSR(op) => {
                let token = expect_token!(lexer)?;
                match token {
                    Token::NUMLIT(pcoffset11) => {
                        Ok(Instruction::new(op | (pcoffset11 & 0x7ff), None))
                    }
                    Token::LABEL(label) => Ok(Instruction::new(op, Some(label))),
                    _ => Err(ParseError::UnexpectedToken(
                        token,
                        lexer.extras.0 + 1,
                        lexer.slice().to_string(),
                    )),
                }
            }
            Token::LD(op) | Token::LDI(op) | Token::LEA(op) | Token::ST(op) | Token::STI(op) => {
                let r1 = expect_token!(lexer, Token::REG(r) => r)?;
                expect_token!(lexer, Token::COMMA)?;
                let token = expect_token!(lexer)?;
                match token {
                    Token::NUMLIT(pcoffset9) => {
                        Ok(Instruction::new(op | r1 << 9 | (pcoffset9 & 0x1ff), None))
                    }
                    Token::LABEL(label) => Ok(Instruction::new(op | r1 << 9, Some(label))),
                    _ => Err(ParseError::UnexpectedToken(
                        token,
                        lexer.extras.0 + 1,
                        lexer.slice().to_string(),
                    )),
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
                    None,
                ))
            }
            Token::NOT(op) => {
                let r1 = expect_token!(lexer, Token::REG(r) => r)?;
                expect_token!(lexer, Token::COMMA)?;
                let r2 = expect_token!(lexer, Token::REG(r) => r)?;
                Ok(Instruction::new(op | r1 << 9 | r2 << 6 | 0b111111, None))
            }
            Token::RTI(op) => Ok(Instruction::new(op, None)),

            /* traps */
            Token::TRAP(op) => {
                let trapvect8 = expect_token!(lexer, Token::NUMLIT(trapvect8) => trapvect8)?;
                Ok(Instruction::new(op | (trapvect8 & 0xff), None))
            }
            Token::GETC(op)
            | Token::OUT(op)
            | Token::PUTS(op)
            | Token::IN(op)
            | Token::PUTSP(op)
            | Token::HALT(op) => Ok(Instruction::new(op, None)),
            _ => Err(ParseError::UnexpectedToken(
                la,
                lexer.extras.0 + 1,
                lexer.slice().to_string(),
            )),
        }
    }
}

/* errors */

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum LexError {
    #[error("error parsing u16: {0}")]
    ParseIntError(#[from] ParseIntError),

    #[error("error unescaping string literal: {0}")]
    UnescapeError(String),

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
    #[error("symbol error: {0}")]
    SymbolError(String),
}

/* utilities */

/// Unescapes the input string.
/// // TODO write a test
fn unescape(input: &str) -> String {
    // NB There are a handful of crates that do this, none of which actually escapes everything
    // we need escaped (notably, ANSI escapes like `\e[`). `snailquote` handles these, but also
    // has its own agenda about escaping things inside single quotes and suchlike.
    let mut result = String::new();
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '\\' => result.push('\\'),
                    '\'' => result.push('\''),
                    '"' => result.push('"'),
                    '0' => result.push('\0'),
                    'x' => {
                        // Handle hexadecimal escape sequence (e.g., \x41)
                        let mut hex = String::new();
                        if let Some(h1) = chars.next() {
                            hex.push(h1);
                        }
                        if let Some(h2) = chars.next() {
                            hex.push(h2);
                        }
                        if let Ok(value) = u8::from_str_radix(&hex, 16) {
                            result.push(value as char);
                        }
                    }
                    'u' => {
                        // Handle Unicode escape sequence (e.g., \u{1F600})
                        if chars.next() == Some('{') {
                            let mut hex = String::new();
                            while let Some(h) = chars.next() {
                                if h == '}' {
                                    break;
                                }
                                hex.push(h);
                            }
                            if let Ok(value) = u32::from_str_radix(&hex, 16) {
                                if let Some(ch) = std::char::from_u32(value) {
                                    result.push(ch);
                                }
                            }
                        }
                    }
                    'e' => {
                        // Handle ANSI escape sequence (e.g., \e[31m)
                        result.push('\x1B');
                        if let Some('[') = chars.peek() {
                            result.push(chars.next().unwrap());
                            while let Some(&next) = chars.peek() {
                                result.push(next);
                                chars.next();
                                if next.is_alphabetic() {
                                    break;
                                }
                            }
                        }
                    }
                    _ => result.push(next),
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}
