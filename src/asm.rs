use crate::{Instruction, Program, Reg, Trap};
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
        Reg::from(lex.slice().as_bytes()[1] - b'0')
    },
    ignore(ascii_case))]
    REG(Reg),

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
    #[regex(r#""(?:[^"]|\\")*""#, |lex| lex.slice()[1..lex.slice().len()-1].as_bytes().to_vec())]
    STRLIT(Vec<u8>),

    // labels
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    LABEL(String),

    // punctuation
    #[token(",")]
    COMMA,
}

pub struct Parser<'source> {
    lexer: Lexer<'source, Token>,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        Parser {
            lexer: Token::lexer(source),
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut prog = Program::default();

        self.expect_orig()?;
        prog.orig = self.expect_numlit()?;

        let mut iaddr = 0u16;
        loop {
            let token = self.expect_next_token()?;
            match token {
                // TODO what if there is add'l garbage after .END?
                Token::END => {
                    break;
                }
                Token::LABEL(label) => {
                    prog.syms.insert(label, iaddr);
                }
                _ => {
                    self.parse_instruction(token)?;
                }
            }
            iaddr += 1;
        }

        Ok(prog)
    }

    fn parse_instruction(&mut self, la: Token) -> Result<Instruction, ParseError> {
        match la {
            // ops
            Token::ADD => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let sr1 = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::REG(sr2) => Ok(Instruction::AddReg(dr, sr1, sr2)),
                    Token::NUMLIT(imm5) => Ok(Instruction::AddImm5(dr, sr1, imm5)),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "REG or imm5".to_string(),
                        found: token,
                    }),
                }
            }
            Token::AND => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let sr1 = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::REG(sr2) => Ok(Instruction::AndReg(dr, sr1, sr2)),
                    Token::NUMLIT(imm5) => Ok(Instruction::AndImm5(dr, sr1, imm5)),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "REG or imm5".to_string(),
                        found: token,
                    }),
                }
            }
            Token::BR(flags) => {
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset9) => Ok(Instruction::Br(flags, pcoffset9, None)),
                    Token::LABEL(label) => Ok(Instruction::Br(flags, 0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset9".to_string(),
                        found: token,
                    }),
                }
            }
            Token::JMP => Ok(Instruction::Jmp(self.expect_reg()?)),
            Token::JSR => {
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset11) => Ok(Instruction::Jsr(pcoffset11, None)),
                    Token::LABEL(label) => Ok(Instruction::Jsr(0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset11".to_string(),
                        found: token,
                    }),
                }
            }
            Token::JSRR => Ok(Instruction::Jsrr(self.expect_reg()?)),
            Token::LD => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset9) => Ok(Instruction::Ld(dr, pcoffset9, None)),
                    Token::LABEL(label) => Ok(Instruction::Ld(dr, 0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset9".to_string(),
                        found: token,
                    }),
                }
            }
            Token::LDI => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset9) => Ok(Instruction::Ldi(dr, pcoffset9, None)),
                    Token::LABEL(label) => Ok(Instruction::Ldi(dr, 0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset9".to_string(),
                        found: token,
                    }),
                }
            }
            Token::LDR => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let base_r = self.expect_reg()?;
                self.expect_comma()?;
                let offset6 = self.expect_numlit()?;
                Ok(Instruction::Ldr(dr, base_r, offset6))
            }
            Token::LEA => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset9) => Ok(Instruction::Lea(dr, pcoffset9, None)),
                    Token::LABEL(label) => Ok(Instruction::Lea(dr, 0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset9".to_string(),
                        found: token,
                    }),
                }
            }
            Token::NOT => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let sr = self.expect_reg()?;
                Ok(Instruction::Not(dr, sr))
            }
            Token::RET => Ok(Instruction::Jmp(Reg::R7)),
            Token::RTI => Ok(Instruction::Rti),
            Token::ST => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset9) => Ok(Instruction::St(dr, pcoffset9, None)),
                    Token::LABEL(label) => Ok(Instruction::St(dr, 0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset9".to_string(),
                        found: token,
                    }),
                }
            }
            Token::STI => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(pcoffset9) => Ok(Instruction::Sti(dr, pcoffset9, None)),
                    Token::LABEL(label) => Ok(Instruction::Sti(dr, 0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or pcoffset9".to_string(),
                        found: token,
                    }),
                }
            }
            Token::STR => {
                let dr = self.expect_reg()?;
                self.expect_comma()?;
                let base_r = self.expect_reg()?;
                self.expect_comma()?;
                let offset6 = self.expect_numlit()?;
                Ok(Instruction::Str(dr, base_r, offset6))
            }
            Token::TRAP => Ok(Instruction::Trap(self.expect_numlit()?)),

            // traps
            Token::GETC => Ok(Instruction::Trap(Trap::GETC as u16)),
            Token::OUT => Ok(Instruction::Trap(Trap::OUT as u16)),
            Token::PUTS => Ok(Instruction::Trap(Trap::PUTS as u16)),
            Token::IN => Ok(Instruction::Trap(Trap::IN as u16)),
            Token::PUTSP => Ok(Instruction::Trap(Trap::PUTSP as u16)),
            Token::HALT => Ok(Instruction::Trap(Trap::HALT as u16)),

            // assembler directives
            Token::FILL => {
                let token = self.expect_next_token()?;
                match token {
                    Token::NUMLIT(value) => Ok(Instruction::Fill(value, None)),
                    Token::LABEL(label) => Ok(Instruction::Fill(0, Some(label))),
                    _ => Err(ParseError::UnexpectedToken {
                        expected: "LABEL or NUMLIT".to_string(),
                        found: token,
                    }),
                }
            }

            Token::STRINGZ => Ok(Instruction::Stringz(self.expect_strlit()?)),

            _ => Err(ParseError::UnexpectedToken {
                expected: "operation or assembler directive".to_string(),
                found: la,
            }),
        }
    }

    fn expect_next_token(&mut self) -> Result<Token, ParseError> {
        if let Some(result) = self.lexer.next() {
            result
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    fn expect_comma(&mut self) -> Result<(), ParseError> {
        let token = self.expect_next_token()?;

        if let Token::COMMA = token {
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "COMMA".to_string(),
                found: token,
            })
        }
    }

    fn expect_numlit(&mut self) -> Result<u16, ParseError> {
        let token = self.expect_next_token()?;

        if let Token::NUMLIT(value) = token {
            Ok(value)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "NUMLIT".to_string(),
                found: token,
            })
        }
    }

    fn expect_orig(&mut self) -> Result<(), ParseError> {
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

    fn expect_reg(&mut self) -> Result<Reg, ParseError> {
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

    fn expect_strlit(&mut self) -> Result<Vec<u8>, ParseError> {
        let token = self.expect_next_token()?;

        if let Token::STRLIT(value) = token {
            Ok(value)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "STRLIT".to_string(),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_add() {
        let mut parser = Parser::new("ADD R0, R1, R2");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::AddReg(Reg::R0, Reg::R1, Reg::R2)
        );
        let mut parser = Parser::new("ADD R3, R4, #-7");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::AddImm5(Reg::R3, Reg::R4, -7i16 as u16)
        );
    }

    #[test]
    fn test_parse_and() {
        let mut parser = Parser::new("AND R0, R1, R2");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::AndReg(Reg::R0, Reg::R1, Reg::R2)
        );
        let mut parser = Parser::new("AND R3, R4, #-7");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::AndImm5(Reg::R3, Reg::R4, -7i16 as u16)
        );
    }

    #[test]
    fn test_parse_br() {
        let mut parser = Parser::new("BR x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Br(0b111, 0x1234, None)
        );
        let mut parser = Parser::new("BR LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Br(0b111, 0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_jmp() {
        let mut parser = Parser::new("JMP R6");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Jmp(Reg::R6)
        );
    }

    #[test]
    fn test_parse_jsr() {
        let mut parser = Parser::new("JSR x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Jsr(0x1234, None)
        );
        let mut parser = Parser::new("JSR LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Jsr(0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_jsrr() {
        let mut parser = Parser::new("JSRR R6");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Jsrr(Reg::R6)
        );
    }

    #[test]
    fn test_parse_ld() {
        let mut parser = Parser::new("LD R1, x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Ld(Reg::R1, 0x1234, None)
        );
        let mut parser = Parser::new("LD R1, LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Ld(Reg::R1, 0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_ldi() {
        let mut parser = Parser::new("LDI R1, x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Ldi(Reg::R1, 0x1234, None)
        );
        let mut parser = Parser::new("LDI R1, LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Ldi(Reg::R1, 0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_ldr() {
        let mut parser = Parser::new("LDR R0, R1, #20");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Ldr(Reg::R0, Reg::R1, 20)
        );
    }

    #[test]
    fn test_parse_lea() {
        let mut parser = Parser::new("LEA R1, x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Lea(Reg::R1, 0x1234, None)
        );
        let mut parser = Parser::new("LEA R1, LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Lea(Reg::R1, 0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_not() {
        let mut parser = Parser::new("NOT R0, R1");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Not(Reg::R0, Reg::R1)
        );
    }

    #[test]
    fn test_parse_ret() {
        let mut parser = Parser::new("RET");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Jmp(Reg::R7)
        );
    }

    #[test]
    fn test_parse_rti() {
        let mut parser = Parser::new("RTI");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(parser.parse_instruction(la).unwrap(), Instruction::Rti);
    }

    #[test]
    fn test_parse_st() {
        let mut parser = Parser::new("ST R1, x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::St(Reg::R1, 0x1234, None)
        );
        let mut parser = Parser::new("ST R1, LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::St(Reg::R1, 0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_sti() {
        let mut parser = Parser::new("STI R1, x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Sti(Reg::R1, 0x1234, None)
        );
        let mut parser = Parser::new("STI R1, LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Sti(Reg::R1, 0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_str() {
        let mut parser = Parser::new("STR R0, R1, #20");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Str(Reg::R0, Reg::R1, 20)
        );
    }

    #[test]
    fn test_parse_trap() {
        let mut parser = Parser::new("TRAP x20");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(0x20)
        );
    }

    #[test]
    fn test_parse_getc() {
        let mut parser = Parser::new("GETC");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(Trap::GETC as u16)
        );
    }

    #[test]
    fn test_parse_out() {
        let mut parser = Parser::new("OUT");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(Trap::OUT as u16)
        );
    }

    #[test]
    fn test_parse_puts() {
        let mut parser = Parser::new("PUTS");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(Trap::PUTS as u16)
        );
    }

    #[test]
    fn test_parse_in() {
        let mut parser = Parser::new("IN");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(Trap::IN as u16)
        );
    }

    #[test]
    fn test_parse_putsp() {
        let mut parser = Parser::new("PUTSP");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(Trap::PUTSP as u16)
        );
    }

    #[test]
    fn test_parse_halt() {
        let mut parser = Parser::new("HALT");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Trap(Trap::HALT as u16)
        );
    }

    #[test]
    fn test_parse_fill() {
        let mut parser = Parser::new(".FILL x1234");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Fill(0x1234, None)
        );
        let mut parser = Parser::new(".FILL LABEL");
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Fill(0, Some("LABEL".to_string()))
        );
    }

    #[test]
    fn test_parse_stringz() {
        let mut parser = Parser::new(r#".STRINGZ "hello, world!\n""#);
        let la = parser.expect_next_token().unwrap();
        assert_eq!(
            parser.parse_instruction(la).unwrap(),
            Instruction::Stringz(r"hello, world!\n".as_bytes().to_vec())
        );
    }
}
