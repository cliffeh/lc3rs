use std::collections::HashMap;
use std::io::{Error, Read, Write};
mod asm;
pub use asm::assemble;

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

pub enum Trap {
    GETC = 0x20,  /* get character from keyboard, not echoed */
    OUT = 0x21,   /* output a character */
    PUTS = 0x22,  /* output a word string */
    IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
    PUTSP = 0x24, /* output a byte string */
    HALT = 0x25,  /* halt the program */
}

pub struct Program {
    pub orig: u16,
    pub mem: Vec<u16>,
    pub syms: HashMap<String, u16>,
    pub refs: HashMap<u16, (String, u16)>,
}

impl Program {
    /// Creates a new program.
    pub fn new(orig: u16, mem: Vec<u16>) -> Program {
        Program {
            orig,
            mem,
            syms: HashMap::new(),
            refs: HashMap::new(),
        }
    }

    /// Dumps the symbol table for this program. The format is one symbol per line,
    /// relative to `self.orig` and sorted in address order.
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
        for (sym, addr) in symvec {
            n += w.write(format!("x{:04x} {}\n", addr + self.orig, sym).as_bytes())?;
        }
        Ok(n)
    }

    /// Walks through all symbol references in `self.refs` and updates their respective
    /// instructions using the actual address of the symbol, masked appropriately for
    /// the instruction. Panics if there is an undefined symbol.
    pub fn resolve_symbols(&mut self) {
        // instruction address
        for (iaddr, (symbol, mask)) in self.refs.iter() {
            // symbol address
            if let Some(saddr) = self.syms.get(symbol) {
                match mask {
                    0 => self.mem[*iaddr as usize] = (self.orig + saddr) as u16, // .FILL
                    _ => {
                        // relative to the incremented PC
                        self.mem[*iaddr as usize] |=
                            ((*saddr as i16 - *iaddr as i16 - 1) & *mask as i16) as u16;
                    }
                }
            } else {
                panic!("undefined symbol: {}", symbol);
            }
        }
    }

    /// Reads an LC3 program from `r`.
    pub fn read(r: &mut dyn Read) -> Result<Program, Error> {
        let mut buf: [u8; 2] = [0, 0];
        r.read_exact(&mut buf)?;
        let orig = u16::from_be_bytes(buf);

        let mut mem: Vec<u16> = vec![];

        loop {
            if r.read(&mut buf)? == 0 {
                // TODO error on size other than 0 or 2
                break;
            }
            mem.push(u16::from_be_bytes(buf));
        }

        Ok(Program::new(orig, mem))
    }

    /// Writes the program out to `w`.
    pub fn write(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += w.write(&u16::to_be_bytes(self.orig as u16))?;
        for inst in &self.mem {
            n += w.write(&u16::to_be_bytes(*inst))?;
        }
        Ok(n)
    }
}

impl Default for Program {
    fn default() -> Self {
        Program {
            orig: 0x0, // NB using 0x3000 as a default masks errors...
            mem: vec![],
            syms: HashMap::new(),
            refs: HashMap::new(),
        }
    }
}

impl From<u16> for Op {
    fn from(value: u16) -> Op {
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
