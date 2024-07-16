use std::collections::HashMap;
use std::io::{BufRead, BufReader, Error, Read, Write};
pub mod asm;
// pub mod vm;
use std::fmt;
// use vm::{COND_NEG, COND_POS, COND_ZRO};

pub const MEMORY_MAX: usize = 1 << 16;

pub enum Op {
    BR = 0, /* branch */
    ADD,    /* add  */
    LD,     /* load */
    ST,     /* store */
    JSR,    /* jump register */
    AND,    /* bitwise and */
    LDR,    /* load register */
    STR,    /* store register */
    RTI,    /* unused */
    NOT,    /* bitwise not */
    LDI,    /* load indirect */
    STI,    /* store indirect */
    JMP,    /* jump */
    RES,    /* reserved (unused) */
    LEA,    /* load effective address */
    TRAP,   /* execute trap */
}

#[derive(Clone, Debug, PartialEq)]
pub enum Trap {
    GETC = 0x20,  /* get character from keyboard, not echoed */
    OUT = 0x21,   /* output a character */
    PUTS = 0x22,  /* output a word string */
    IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
    PUTSP = 0x24, /* output a byte string */
    HALT = 0x25,  /* halt the program */
}

#[derive(Clone, Debug, PartialEq)]
pub enum Reg {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    // ops
    AddReg(Reg, Reg, Reg),
    AddImm5(Reg, Reg, u16),
    AndReg(Reg, Reg, Reg),
    AndImm5(Reg, Reg, u16),
    Br(u16, u16, Option<String>),
    Jmp(Reg),
    Jsr(u16, Option<String>),
    Jsrr(Reg),
    Ld(Reg, u16, Option<String>),
    Ldi(Reg, u16, Option<String>),
    Ldr(Reg, Reg, u16),
    Lea(Reg, u16, Option<String>),
    Not(Reg, Reg),
    Rti,
    St(Reg, u16, Option<String>),
    Sti(Reg, u16, Option<String>),
    Str(Reg, Reg, u16),
    Trap(Trap),

    // assembler directives
    Fill(u16, Option<String>),
    Stringz(Vec<u8>),
}

pub struct Program {
    /// Origin address of the program
    pub orig: u16,
    /// Instructions of the program
    pub instructions: Vec<Instruction>,
    /// Symbol table
    pub syms: HashMap<String, u16>,
}

impl Program {
    /// Creates a new program.
    pub fn new(orig: u16, instructions: Vec<Instruction>) -> Program {
        Program {
            orig,
            instructions,
            syms: HashMap::new(),
        }
    }

    /// Dumps the symbol table for this program. The format is one symbol per line,
    /// relative to `self.orig` sorted in address order.
    ///
    /// # Example
    ///
    /// ```rust
    /// use lc3::Program;
    ///
    /// let mut prog = Program::default();
    /// prog.orig = 0x3000;
    /// prog.syms.insert(String::from("foo"), 0x20);
    /// prog.syms.insert(String::from("bar"), 0x1200);
    ///
    /// let mut buf: Vec<u8> = vec![];
    /// prog.dump_symbols(&mut buf);
    ///
    /// let mut s = String::from_utf8(buf).unwrap();
    /// let mut actual = s.split("\n");
    ///
    /// assert_eq!(actual.next().unwrap(), "x3020 foo");
    /// assert_eq!(actual.next().unwrap(), "x4200 bar");
    /// ```
    pub fn dump_symbols(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        let mut symvec: Vec<(&String, &u16)> = self.syms.iter().collect();
        symvec.sort_by(|(_, addr1), (_, addr2)| addr1.cmp(addr2));
        for (sym, saddr) in symvec {
            n += w.write(format!("x{:04x} {}\n", saddr + self.orig, sym).as_bytes())?;
        }
        Ok(n)
    }

    /// Loads the symbol table for this program, per the format specified in `dump_symbols()`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use lc3::Program;
    /// use std::io::BufReader;
    ///
    /// let mut prog = Program::default();
    ///
    /// let mut br = BufReader::new("x3020 foo x1,x2\nx4200 bar".as_bytes());
    /// let _ = prog.load_symbols(&mut br);
    ///
    /// assert_eq!(prog.syms.get("foo").unwrap(), &0x3020u16);
    /// assert_eq!(prog.syms.get("bar").unwrap(), &0x4200u16);
    /// ```
    pub fn load_symbols(&mut self, r: &mut dyn Read) -> Result<(), Error> {
        for res in BufReader::new(r).lines().into_iter() {
            let line = res.unwrap();
            let mut split = line.split(" ").into_iter();
            let s = &split.next().unwrap()[1..];

            let addr = u16::from_str_radix(&s, 16).unwrap();
            let symbol = String::from(split.next().unwrap());

            self.syms.insert(symbol.clone(), addr);
        }

        Ok(())
    }

    /// Does a reverse lookup of a symbol, given its address.
    ///
    /// Returns Option<u16>, as the address passed in might not reference a symbol
    /// (i.e., might be intended as a literal).
    ///
    /// # Example
    ///
    /// ```rust
    /// use lc3::Program;
    /// use std::io::BufReader;
    ///
    /// let mut prog = Program::default();
    ///
    /// let mut br = BufReader::new("x3020 foo\nx4200 bar".as_bytes());
    /// let _ = prog.load_symbols(&mut br);
    ///
    /// assert_eq!(prog.lookup_symbol_by_address(0x3020).unwrap(), &"foo".to_string());
    /// assert_eq!(prog.lookup_symbol_by_address(0x4200).unwrap(), &"bar".to_string());
    /// ```
    pub fn lookup_symbol_by_address(&self, addr: u16) -> Option<&String> {
        for (symbol, saddr) in self.syms.iter() {
            if *saddr == addr {
                return Some(symbol);
            }
        }
        None
    }

    /// Writes the program out to `w`.
    pub fn write(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += w.write(&u16::to_be_bytes(self.orig as u16))?;
        for instruction in &self.instructions {
            n += w.write(&u16::to_be_bytes(0 /*u16::from(instruction)*/))?; // TODO!
        }
        Ok(n)
    }
}

impl Default for Program {
    fn default() -> Self {
        Program {
            orig: 0x3000,
            instructions: vec![],
            syms: HashMap::new(),
        }
    }
}

impl From<u16> for Op {
    fn from(value: u16) -> Self {
        match value {
            0x0 => Op::BR,
            0x1 => Op::ADD,
            0x2 => Op::LD,
            0x3 => Op::ST,
            0x4 => Op::JSR,
            0x5 => Op::AND,
            0x6 => Op::LDR,
            0x7 => Op::STR,
            0x8 => Op::RTI,
            0x9 => Op::NOT,
            0xA => Op::LDI,
            0xB => Op::STI,
            0xC => Op::JMP,
            0xD => Op::RES,
            0xE => Op::LEA,
            0xF => Op::TRAP,
            _ => unreachable!(),
        }
    }
}

impl From<u8> for Reg {
    fn from(value: u8) -> Self {
        match value {
            0 => Reg::R0,
            1 => Reg::R1,
            2 => Reg::R2,
            3 => Reg::R3,
            4 => Reg::R4,
            5 => Reg::R5,
            6 => Reg::R6,
            7 => Reg::R7,
            _ => unreachable!(),
        }
    }
}

impl From<Reg> for u16 {
    fn from(value: Reg) -> Self {
        match value {
            Reg::R0 => 0,
            Reg::R1 => 1,
            Reg::R2 => 2,
            Reg::R3 => 3,
            Reg::R4 => 4,
            Reg::R5 => 5,
            Reg::R6 => 6,
            Reg::R7 => 7,
        }
    }
}

impl TryFrom<u16> for Trap {
    type Error = ();

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            0x20 => Ok(Trap::GETC),
            0x21 => Ok(Trap::OUT),
            0x22 => Ok(Trap::PUTS),
            0x23 => Ok(Trap::IN),
            0x24 => Ok(Trap::PUTSP),
            0x25 => Ok(Trap::HALT),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::AddReg(dr, sr1, sr2) => {
                write!(f, "ADD {:#?}, {:#?}, {:#?}", dr, sr1, sr2)?;
            }
            Instruction::AddImm5(dr, sr1, imm5) => {
                write!(f, "ADD {:#?}, {:#?}, #{}", dr, sr1, *imm5 as i16)?;
            }
            Instruction::AndReg(dr, sr1, sr2) => {
                write!(f, "AND {:#?}, {:#?}, {:#?}", dr, sr1, sr2)?;
            }
            Instruction::AndImm5(dr, sr1, imm5) => {
                write!(f, "AND {:#?}, {:#?}, #{}", dr, sr1, *imm5 as i16)?;
            }
            Instruction::Br(flags, pcoffset9, optlabel) => {
                write!(f, "BR")?;
                if flags & 0x4 /*COND_NEG*/ != 0 {
                    // TODO
                    write!(f, "n")?;
                }
                if flags & 0x2 /* COND_ZRO */ != 0 {
                    // TODO
                    write!(f, "z")?;
                }
                if flags & 0x1 /* COND_POS */ != 0 {
                    // TODO
                    write!(f, "p")?;
                }
                if let Some(label) = optlabel {
                    write!(f, " {}", label)?;
                } else {
                    write!(f, " x{:04x}", pcoffset9)?;
                }
            }
            Instruction::Jmp(base_r) => {
                if *base_r == Reg::R7 {
                    write!(f, "RET")?;
                } else {
                    write!(f, "JMP {:#?}", base_r)?;
                }
            }
            Instruction::Jsr(pcoffset11, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, "JSR {}", label)?;
                } else {
                    write!(f, "JSR x{:04x}", pcoffset11)?;
                }
            }
            Instruction::Jsrr(base_r) => {
                write!(f, "JSRR {:#?}", base_r)?;
            }
            Instruction::Ld(dr, pcoffset9, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, "LD {:#?}, {}", dr, label)?;
                } else {
                    write!(f, "LD {:#?}, x{:04x}", dr, pcoffset9)?;
                }
            }
            Instruction::Ldi(dr, pcoffset9, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, "LDI {:#?}, {}", dr, label)?;
                } else {
                    write!(f, "LDI {:#?}, x{:04x}", dr, pcoffset9)?;
                }
            }
            Instruction::Ldr(dr, base_r, offset6) => {
                write!(f, "LDR {:#?}, {:#?}, #{}", dr, base_r, *offset6 as i16)?;
            }
            Instruction::Lea(dr, pcoffset9, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, "LEA {:#?}, {}", dr, label)?;
                } else {
                    write!(f, "LEA {:#?}, x{:04x}", dr, pcoffset9)?;
                }
            }
            Instruction::Not(dr, sr) => {
                write!(f, "NOT {:#?}, {:#?}", dr, sr)?;
            }
            Instruction::Rti => {
                write!(f, "RTI")?;
            }
            Instruction::St(sr, pcoffset9, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, "ST {:#?}, {}", sr, label)?;
                } else {
                    write!(f, "ST {:#?}, x{:04x}", sr, pcoffset9)?;
                }
            }
            Instruction::Sti(sr, pcoffset9, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, "STI {:#?}, {}", sr, label)?;
                } else {
                    write!(f, "STI {:#?}, x{:04x}", sr, pcoffset9)?;
                }
            }
            Instruction::Str(sr, base_r, offset6) => {
                write!(f, "STR {:#?}, {:#?}, #{}", sr, base_r, *offset6 as i16)?;
            }
            Instruction::Trap(trap) => {
                write!(f, "{}", trap)?;
            }
            Instruction::Fill(value, optlabel) => {
                if let Some(label) = optlabel {
                    write!(f, ".FILL {}", label)?;
                } else {
                    write!(f, ".FILL x{:04x}", value)?;
                }
            }
            Instruction::Stringz(bytes) => {
                write!(f, ".STRINGZ \"")?;
                for b in bytes {
                    write!(f, "{}", *b as char)?;
                }
                write!(f, "\"")?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ".ORIG x{:04X}", self.orig)?;

        eprintln!("instruction len: {}", self.instructions.len());

        for iaddr in 0..self.instructions.len() {
            // write!(f, "x{:04X} ", iaddr)?;

            if let Some(symbol) = self.lookup_symbol_by_address(iaddr as u16 + self.orig) {
                write!(f, "{} ", symbol)?;
            }

            writeln!(f, "{}", self.instructions[iaddr])?;
        }

        writeln!(f, ".END")?;

        Ok(())
    }
}

impl fmt::Display for Trap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Trap::GETC => write!(f, "GETC"),
            Trap::OUT => write!(f, "OUT"),
            Trap::PUTS => write!(f, "PUTS"),
            Trap::IN => write!(f, "IN"),
            Trap::PUTSP => write!(f, "PUTSP"),
            Trap::HALT => write!(f, "HALT"),
        }
    }
}

// utility
pub fn sign_extend(x: u16, count: usize) -> u16 {
    if ((x >> (count - 1)) & 1) != 0 {
        x | (0xFFFF << count)
    } else {
        x
    }
}
