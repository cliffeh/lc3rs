use logos::Logos;
use std::collections::HashMap;
use std::io::{Error, Write};

pub const MEMORY_MAX: usize = 1 << 16;

#[repr(u16)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"([ \t\r\n]+|;.*)")] // ignore whitespace and comments
pub enum Token {
    // op codes
    #[regex(r"BRn?z?p?",
     callback = |lex| {
        let mut flags: u16 = 0;
        for c in lex.slice()[2..].chars() {
            match c {
                'n' | 'N' => flags |= 1 << 11,
                'z' | 'Z' => flags |= 1 << 10,
                'p' | 'P' => flags |= 1 << 9,
                _ => unreachable!(),
            }
        }
        // NB "The assembly language opcode BR is interpreted the same as BRnzp;
        // that is, always branch to the target address."
        if flags == 0 {
            flags = 1 << 11 | 1 << 10 | 1 << 9;
        }
        flags
    }, ignore(ascii_case))]
    BR(u16) = 0, /* branch */
    #[token("ADD", |_| 1 << 12, ignore(ascii_case))]
    ADD(u16), /* add  */
    #[token("LD", |_| 2 << 12, ignore(ascii_case))]
    LD(u16), /* load */
    #[token("ST", |_| 3 << 12, ignore(ascii_case))]
    ST(u16), /* store */
    #[token("JSR", |_| 4 << 12 | 1 << 11, ignore(ascii_case))]
    JSR(u16), /* jump register */
    #[token("AND", |_| 5 << 12, ignore(ascii_case))]
    AND(u16), /* bitwise and */
    #[token("LDR", |_| 6 << 12, ignore(ascii_case))]
    LDR(u16), /* load register */
    #[token("STR", |_| 7 << 12, ignore(ascii_case))]
    STR(u16), /* store register */
    #[token("RTI", |_| 8 << 12, ignore(ascii_case))]
    RTI(u16), /* unused */
    #[token("NOT", |_| 9 << 12, ignore(ascii_case))]
    NOT(u16), /* bitwise not */
    #[token("LDI", |_| 10 << 12, ignore(ascii_case))]
    LDI(u16), /* load indirect */
    #[token("STI", |_| 11 << 12, ignore(ascii_case))]
    STI(u16), /* store indirect */
    #[token("JMP", |_| 12 << 12, ignore(ascii_case))]
    JMP(u16), /* jump */
    #[token("RES", |_| 13 << 12, ignore(ascii_case))]
    RES(u16), /* reserved (unused) */
    #[token("LEA", |_| 14 << 12, ignore(ascii_case))]
    LEA(u16), /* load effective address */
    #[token("TRAP", |_| 15 << 12, ignore(ascii_case))]
    TRAP(u16), /* execute trap */

    // alternative ops
    #[token("RET", |_| 12 << 12 | 7 << 6, ignore(ascii_case))]
    RET(u16), /* equivalent to JMP R7 */
    #[token("JSRR", |_| 4 << 12, ignore(ascii_case))]
    JSRR(u16), /* JSR, but takes a register argument */

    // traps
    #[token("GETC", |_| 15 << 12 | 0x20, ignore(ascii_case))]
    GETC(u16) = 0x20, /* get character from keyboard, not echoed */
    #[token("OUT", |_| 15 << 12 | 0x21, ignore(ascii_case))]
    OUT(u16) = 0x21,   /* output a character */
    #[token("PUTS", |_| 15 << 12 | 0x22, ignore(ascii_case))]
    PUTS(u16) = 0x22,  /* output a word string */
    #[token("IN", |_| 15 << 12 | 0x23, ignore(ascii_case))]
    IN(u16) = 0x23,    /* get character from keyboard, echoed onto the terminal */
    #[token("PUTSP", |_| 15 << 12 | 0x24, ignore(ascii_case))]
    PUTSP(u16) = 0x24, /* output a byte string */
    #[token("HALT", |_| 15 << 12 | 0x25, ignore(ascii_case))]
    HALT(u16) = 0x25,  /* halt the program */

    // registers
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
    #[regex(r"#-?[0-9]+", |lex| i16::from_str_radix(&lex.slice()[1..], 10).ok())]
    DecLit(i16),
    #[regex(r"[xX][0-9a-fA-F]+", |lex| u16::from_str_radix(&lex.slice()[1..], 16).ok())]
    HexLit(u16), // TODO allow negative hex literals?
    #[regex(r#""(?:[^"]|\\")*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StrLit(String),

    // labels
    #[regex(r"[a-zA-Z][_\-a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Label(String),

    // punctuation
    #[token(",")]
    Comma,
}

pub struct Program {
    pub orig: usize,
    pub mem: Vec<u16>,
    pub syms: HashMap<String, usize>,
    pub refs: HashMap<usize, String>,
    pub mask: HashMap<usize, u16>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            orig: 0x0, // NB using 0x3000 as a default masks errors...
            mem: vec![],
            syms: HashMap::new(),
            refs: HashMap::new(),
            mask: HashMap::new(),
        }
    }

    pub fn resolve_symbols(&mut self) -> () {
        for (iaddr, label) in self.refs.iter() {
            if let Some(saddr) = self.syms.get(label) {
                if let Some(mask) = self.mask.get(iaddr) {
                    match mask {
                        0 => self.mem[*iaddr] = (self.orig + saddr) as u16, // .FILL
                        _ => {
                            // self.mem[iaddr] |= (saddr - iaddr - 1) & mask
                            self.mem[*iaddr] |=
                                ((*saddr as i16 - *iaddr as i16 - 1) & *mask as i16) as u16;
                        }
                    }
                } else {
                    panic!("undefined mask at addr {}", iaddr);
                }
            } else {
                panic!("undefined label: {}", label);
            }
        }
    }

    pub fn write(self, out: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += out.write(&u16::to_be_bytes(self.orig as u16))?;
        for inst in self.mem {
            n += out.write(&u16::to_be_bytes(inst))?;
        }
        Ok(n)
    }
}
