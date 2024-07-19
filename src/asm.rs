use crate::{Instruction, Op, Program, Trap};
use logos::{Lexer, Logos};
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = ParseError)]
#[logos(skip r"([ \t\r\n]+|;.*)")] // ignore whitespace and comments
pub enum Token {
    /* op codes */
    /// ```
    /// use lc3::asm::Token;
    /// use logos::Logos;
    ///
    /// assert_eq!(Token::lexer("BR").next(),    Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b111))));
    /// assert_eq!(Token::lexer("BRnzp").next(), Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b111))));
    /// assert_eq!(Token::lexer("BRn").next(),   Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b100))));
    /// assert_eq!(Token::lexer("BRz").next(),   Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b010))));
    /// assert_eq!(Token::lexer("BRp").next(),   Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b001))));
    /// assert_eq!(Token::lexer("BRnz").next(),  Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b110))));
    /// assert_eq!(Token::lexer("BRnp").next(),  Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b101))));
    /// assert_eq!(Token::lexer("BRzp").next(),  Some(Ok(Token::BR((Op::BR as u16) << 12 | 0b011))));
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
        ((Op::BR as u16) << 12) | flags
    }, ignore(ascii_case))]
    BR(u16),
    #[token("ADD", |_| ((Op::ADD as u16) << 12), ignore(ascii_case))]
    ADD(u16),
    #[token("LD", |_| ((Op::LD as u16) << 12), ignore(ascii_case))]
    LD(u16),
    #[token("ST", |_| ((Op::ST as u16) << 12), ignore(ascii_case))]
    ST(u16),
    #[token("JSR", |_| ((Op::JSR as u16) << 12) | 1 << 11, ignore(ascii_case))]
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
    #[token("RET", |_| ((Op::JMP as u16) << 12) | 0x7 << 6, ignore(ascii_case))]
    RET(u16),
    #[token("LEA", |_| ((Op::LEA as u16) << 12), ignore(ascii_case))]
    LEA(u16),
    #[token("TRAP", |_| ((Op::TRAP as u16) << 12), ignore(ascii_case))]
    TRAP(u16),

    /* traps */
    #[token("GETC", |_| ((Op::TRAP as u16) << 12) | (Trap::GETC as u16), ignore(ascii_case))]
    GETC(u16),
    #[token("OUT", |_| ((Op::TRAP as u16) << 12) | (Trap::OUT as u16), ignore(ascii_case))]
    OUT(u16),
    #[token("PUTS", |_| ((Op::TRAP as u16) << 12) | (Trap::PUTS as u16), ignore(ascii_case))]
    PUTS(u16),
    #[token("IN", |_| ((Op::TRAP as u16) << 12) | (Trap::IN as u16), ignore(ascii_case))]
    IN(u16),
    #[token("PUTSP", |_| ((Op::TRAP as u16) << 12) | (Trap::PUTSP as u16), ignore(ascii_case))]
    PUTSP(u16),
    #[token("HALT", |_| ((Op::TRAP as u16) << 12) | (Trap::HALT as u16), ignore(ascii_case))]
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
        let value = i16::from_str_radix(&lex.slice()[1..], radix)?;
        Ok::<u16, ParseError>(value as u16)
    })]
    NUMLIT(u16),
    #[regex(r#""(?:[^"]|\\")*""#, callback = |lex| {
        lex.slice()[1..lex.slice().len()-1].as_bytes().into_iter().map(|b| *b as u16).collect::<Vec<u16>>()
    })]
    STRLIT(Vec<u16>),

    // labels
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    LABEL(String),

    // punctuation
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
            Token::STRINGZ => {
                let strlit = expect_token!(lexer, Token::STRLIT(s) => s)?;
                for c in strlit {
                    prog.instructions.push(Instruction::new(c, None));
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
/// use lc3::{Instruction, Op};
/// use lc3::asm::parse_instruction;
/// assert_eq!(parse_instruction("ADD R0, R1, R2").unwrap(), Instruction::new((Op::ADD as u16) << 12 | 0 << 9 | 1 << 6 | 2, None));
/// assert_eq!(parse_instruction("AND R3, R4, R5").unwrap(), Instruction::new((Op::AND as u16) << 12 | 3 << 9 | 4 << 6 | 5, None));
/// assert_eq!(parse_instruction("ADD R6, R7, #-7").unwrap(), Instruction::new((Op::ADD as u16) << 12 | 6 << 9 | 7 << 6 | (-7i16 as u16), None));
/// assert_eq!(parse_instruction("AND R6, R7, #-7").unwrap(), Instruction::new((Op::AND as u16) << 12 | 6 << 9 | 7 << 6 | (-7i16 as u16), None));
/// 
/// ```
pub fn parse_instruction(source: &str) -> Result<Instruction, ParseError> {
    let mut lexer = Token::lexer(source);
    let token = expect_token!(lexer)?;
    parse_instruction_la(&mut lexer, token)
}

fn parse_instruction_la(lexer: &mut Lexer<Token>, la: Token) -> Result<Instruction, ParseError> {
    match la {
        Token::ADD(op) | Token::AND(op) => {
            let r1 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let r2 = expect_token!(lexer, Token::REG(r) => r)?;
            expect_token!(lexer, Token::COMMA)?;
            let token = expect_token!(lexer)?;
            match token {
                Token::REG(r3) => Ok(Instruction::new(op | r1 << 9 | r2 << 6 | r3, None)),
                Token::NUMLIT(imm5) => Ok(Instruction::new(
                    op | r1 << 9 | r2 << 6 | 1 << 5 | imm5,
                    None,
                )),
                _ => Err(ParseError::UnexpectedToken { found: token }),
            }
        }
        _ => unimplemented!(),
    }
}

// pub struct Parser<'source> {
//     lexer: Lexer<'source, Token>,
// }

// impl<'source> Parser<'source> {
//     pub fn new(source: &'source str) -> Self {
//         Parser {
//             lexer: Token::lexer(source),
//         }
//     }

//     pub fn parse_program(&mut self) -> Result<Program, ParseError> {
//         let mut prog = Program::default();

//         self.expect_orig()?;
//         prog.origin = self.expect_numlit()?;

//         let mut iaddr = 0usize;
//         loop {
//             let token = self.lexer.next().unwrap().unwrap(); // self.expect_next_token()?;
//             match token {
//                 // TODO what if there is add'l garbage after .END?
//                 Token::END => {
//                     break;
//                 }
//                 // TODO check for duplicate labels
//                 Token::LABEL(label) => {
//                     prog.symbols.insert(label, iaddr);
//                 }
//                 _ => {
//                     let instructions = self.parse_instruction_la(token)?;
//                     for instruction in instructions {
//                         prog.instructions.push(instruction.to_owned());
//                     }
//                 }
//             }
//             iaddr += 1;
//         }

//         Ok(prog)
//     }

//     fn parse_instruction_la(&mut self, la: Token) -> Result<&[Instruction], ParseError> {
//         match la {
//             // ops
//             Token::ADD(op) | Token::AND(op) => {
//                 let r1 = self.expect_reg()?;
//                 self.expect_comma()?;
//                 let r2 = self.expect_reg()?;
//                 self.expect_comma()?;
//                 let token = self.expect_next_token()?;
//                 match token {
//                     Token::REG(r3) => Ok(&[Instruction::new(op | r1 << 9 | r2 << 6 | r3, None)]),
//                     Token::NUMLIT(imm5) => Ok(&[Instruction::new(op | r1 << 9 | r2 << 6 | 1 << 5 | imm5, None)]),
//                     _ => Err(ParseError::UnexpectedToken {
//                         expected: "REG or imm5".to_string(),
//                         found: token,
//                     }),
//                 }
//             }

//             Token::BR(op) => {
//                 let token = self.expect_next_token()?;
//                 match token {
//                     Token::NUMLIT(pcoffset9) => Ok(&[Instruction::new(op | pcoffset9, None)]),
//                     Token::LABEL(label) => Ok(&[Instruction::new(op, Some(label))]),
//                     _ => Err(ParseError::UnexpectedToken {
//                         expected: "LABEL or pcoffset9".to_string(),
//                         found: token,
//                     }),
//                 }
//             }

//             Token::JMP(op) | Token::RET(op) => Ok(&[Instruction::new(op | self.expect_reg()? << 6, None)]),

//             Token::JSR(op) => {
//                 let token = self.expect_next_token()?;
//                 match token {
//                     Token::NUMLIT(pcoffset11) =>Ok(&[Instruction::new(op | 1 << 11 | pcoffset11, None)]),
//                     Token::LABEL(label) => Ok(&[Instruction::new(op | 1 << 11, Some(label))]),
//                     _ => Err(ParseError::UnexpectedToken {
//                         expected: "LABEL or pcoffset11".to_string(),
//                         found: token,
//                     }),
//                 }
//             }

//             Token::JSRR(op) => Ok(&[Instruction::new(op | self.expect_reg()? << 6, None)]),

//             Token::LD(op) | Token::LDI(op) | Token::LEA(op) | Token::ST(op) | Token::STI(op) => {
//                 let r1 = self.expect_reg()?;
//                 self.expect_comma()?;
//                 let token = self.expect_next_token()?;
//                 match token {
//                     Token::NUMLIT(pcoffset9) => Ok(&[Instruction::new(op | r1 << 9 | pcoffset9, None)]),
//                     Token::LABEL(label) => Ok(&[Instruction::new(op | r1 << 9, Some(label))]),
//                     _ => Err(ParseError::UnexpectedToken {
//                         expected: "LABEL or pcoffset9".to_string(),
//                         found: token,
//                     }),
//                 }
//             }

//             Token::LDR(op) | Token::STR(op) => {
//                 let r1 = self.expect_reg()?;
//                 self.expect_comma()?;
//                 let r2 = self.expect_reg()?;
//                 self.expect_comma()?;
//                 let offset6 = self.expect_numlit()?;
//                 Ok(&[Instruction::new(op | r1 << 9 | r2 << 6 | offset6, None)])
//             }

//             Token::NOT(op) => {
//                 let r1 = self.expect_reg()?;
//                 self.expect_comma()?;
//                 let r2 = self.expect_reg()?;
//                 Ok(&[Instruction::new(op | r1 << 9 | r2 << 6 | 0b111111, None)])
//             }
//             Token::RTI(op) => Ok(&[Instruction::new(op, None)]),

//             Token::TRAP(op) => Ok(&[Instruction::new(op | self.expect_numlit()?, None)]),

//             // traps
//             Token::GETC(op)
//             | Token::OUT(op)
//             | Token::PUTS(op)
//             | Token::IN(op)
//             | Token::PUTSP(op)
//             | Token::HALT(op) => Ok(&[Instruction::new(op, None)]),

//             // assembler directives
//             Token::FILL => {
//                 let token = self.expect_next_token()?;
//                 match token {
//                     Token::NUMLIT(value) => Ok(&[Instruction::new(value, None)]),
//                     Token::LABEL(label) => Ok(&[Instruction::new(0u16, Some(label))]),
//                     _ => Err(ParseError::UnexpectedToken {
//                         expected: "LABEL or NUMLIT".to_string(),
//                         found: token,
//                     }),
//                 }
//             }

//             Token::STRINGZ => Ok((&self.expect_strlit()?[..], None)),
//             _ => Err(ParseError::UnexpectedToken {
//                 expected: "operation or assembler directive".to_string(),
//                 found: la,
//             }),
//         }
//     }

//     fn expect_next_token(&mut self) -> Result<Token, ParseError> {
//         if let Some(result) = self.lexer.next() {
//             result
//         } else {
//             Err(ParseError::UnexpectedEOF)
//         }
//     }

//     fn expect_comma(&mut self) -> Result<(), ParseError> {
//         let token = self.expect_next_token()?;

//         if let Token::COMMA = token {
//             Ok(())
//         } else {
//             Err(ParseError::UnexpectedToken {
//                 expected: "COMMA".to_string(),
//                 found: token,
//             })
//         }
//     }

//     fn expect_numlit(&mut self) -> Result<u16, ParseError> {
//         let token = self.expect_next_token()?;

//         if let Token::NUMLIT(value) = token {
//             Ok(value)
//         } else {
//             Err(ParseError::UnexpectedToken {
//                 expected: "NUMLIT".to_string(),
//                 found: token,
//             })
//         }
//     }

//     fn expect_orig(&mut self) -> Result<(), ParseError> {
//         let token = self.expect_next_token()?;

//         if let Token::ORIG = token {
//             Ok(())
//         } else {
//             Err(ParseError::UnexpectedToken {
//                 expected: "ORIG".to_string(),
//                 found: token,
//             })
//         }
//     }

//     fn expect_reg(&mut self) -> Result<u16, ParseError> {
//         let token = self.expect_next_token()?;

//         if let Token::REG(value) = token {
//             Ok(value)
//         } else {
//             Err(ParseError::UnexpectedToken {
//                 expected: "REG".to_string(),
//                 found: token,
//             })
//         }
//     }

//     fn expect_strlit(&mut self) -> Result<Vec<u16>, ParseError> {
//         let token = self.expect_next_token()?;

//         if let Token::STRLIT(value) = token {
//             Ok(value)
//         } else {
//             Err(ParseError::UnexpectedToken {
//                 expected: "STRLIT".to_string(),
//                 found: token,
//             })
//         }
//     }
// }

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

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_parse_add() {
//         let (&[instruction], optlabel) = Parser::new("ADD R0, R1, R2").parse_instruction().unwrap()
//         else {
//             unreachable!()
//         };
//         assert_eq!(
//             (instruction, optlabel),
//             ((Op::ADD as u16) << 12 | 0 << 9 | 1 << 6 | 2, None)
//         );

//         let Ok((&[instruction], optlabel)) = Parser::new("ADD R3, R4, #-7").parse_instruction();
//         assert_eq!(
//             (instruction, optlabel),
//             (
//                 (Op::ADD as u16) << 12 | 3 << 9 | 4 << 6 | -7i16 as u16,
//                 None
//             )
//         );
//     }

//     #[test]
//     fn test_parse_and() {
//         let (&[instruction], optlabel) = parse_instruction("AND R5, R6, R7");
//         assert_eq!(
//             (instruction, optlabel),
//             ((Op::AND as u16) << 12 | 5 << 9 | 6 << 6 | 7, None)
//         );

//         let (&[instruction], optlabel) = parse_instruction("AND R3, R4, #-7");
//         assert_eq!(
//             (instruction, optlabel),
//             (
//                 (Op::AND as u16) << 12 | 3 << 9 | 4 << 6 | -7i16 as u16,
//                 None
//             )
//         );
//     }

//     #[test]
//     fn test_parse_br() {
//         let (&[instruction], optlabel) = parse_instruction("BR x1234");
//         assert_eq!((instruction, optlabel), ((Op::BR << 12) | 0x1234, None));
//         let mut parser = Parser::new("BR LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Br(0b111, 0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_jmp() {
//         let mut parser = Parser::new("JMP R6");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Jmp(Reg::R6)
//         );
//     }

//     #[test]
//     fn test_parse_jsr() {
//         let mut parser = Parser::new("JSR x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Jsr(0x1234, None)
//         );
//         let mut parser = Parser::new("JSR LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Jsr(0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_jsrr() {
//         let mut parser = Parser::new("JSRR R6");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Jsrr(Reg::R6)
//         );
//     }

//     #[test]
//     fn test_parse_ld() {
//         let mut parser = Parser::new("LD R1, x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Ld(Reg::R1, 0x1234, None)
//         );
//         let mut parser = Parser::new("LD R1, LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Ld(Reg::R1, 0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_ldi() {
//         let mut parser = Parser::new("LDI R1, x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Ldi(Reg::R1, 0x1234, None)
//         );
//         let mut parser = Parser::new("LDI R1, LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Ldi(Reg::R1, 0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_ldr() {
//         let mut parser = Parser::new("LDR R0, R1, #20");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Ldr(Reg::R0, Reg::R1, 20)
//         );
//     }

//     #[test]
//     fn test_parse_lea() {
//         let mut parser = Parser::new("LEA R1, x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Lea(Reg::R1, 0x1234, None)
//         );
//         let mut parser = Parser::new("LEA R1, LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Lea(Reg::R1, 0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_not() {
//         let mut parser = Parser::new("NOT R0, R1");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Not(Reg::R0, Reg::R1)
//         );
//     }

//     #[test]
//     fn test_parse_ret() {
//         let mut parser = Parser::new("RET");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Jmp(Reg::R7)
//         );
//     }

//     #[test]
//     fn test_parse_rti() {
//         let mut parser = Parser::new("RTI");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(parser.parse_instruction(la).unwrap(), Instruction::Rti);
//     }

//     #[test]
//     fn test_parse_st() {
//         let mut parser = Parser::new("ST R1, x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::St(Reg::R1, 0x1234, None)
//         );
//         let mut parser = Parser::new("ST R1, LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::St(Reg::R1, 0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_sti() {
//         let mut parser = Parser::new("STI R1, x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Sti(Reg::R1, 0x1234, None)
//         );
//         let mut parser = Parser::new("STI R1, LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Sti(Reg::R1, 0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_str() {
//         let mut parser = Parser::new("STR R0, R1, #20");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Str(Reg::R0, Reg::R1, 20)
//         );
//     }

//     #[test]
//     fn test_parse_trap() {
//         let mut parser = Parser::new("TRAP x20");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::GETC)
//         );
//     }

//     #[test]
//     fn test_parse_getc() {
//         let mut parser = Parser::new("GETC");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::GETC)
//         );
//     }

//     #[test]
//     fn test_parse_out() {
//         let mut parser = Parser::new("OUT");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::OUT)
//         );
//     }

//     #[test]
//     fn test_parse_puts() {
//         let mut parser = Parser::new("PUTS");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::PUTS)
//         );
//     }

//     #[test]
//     fn test_parse_in() {
//         let mut parser = Parser::new("IN");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::IN)
//         );
//     }

//     #[test]
//     fn test_parse_putsp() {
//         let mut parser = Parser::new("PUTSP");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::PUTSP)
//         );
//     }

//     #[test]
//     fn test_parse_halt() {
//         let mut parser = Parser::new("HALT");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Trap(Trap::HALT as u16)
//         );
//     }

//     #[test]
//     fn test_parse_fill() {
//         let mut parser = Parser::new(".FILL x1234");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Fill(0x1234, None)
//         );
//         let mut parser = Parser::new(".FILL LABEL");
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Fill(0, Some("LABEL".to_string()))
//         );
//     }

//     #[test]
//     fn test_parse_stringz() {
//         let mut parser = Parser::new(r#".STRINGZ "hello, world!\n""#);
//         let la = parser.expect_next_token().unwrap();
//         assert_eq!(
//             parser.parse_instruction(la).unwrap(),
//             Instruction::Stringz(r"hello, world!\n".as_bytes().to_vec())
//         );
//     }
// }
