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
    pub refs: HashMap<u16, String>,
    pub mask: HashMap<u16, u16>,
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

    pub fn dump_symbols(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        let mut symvec: Vec<(&String, &u16)> = self.syms.iter().collect();
        symvec.sort_by(|(_, addr1), (_, addr2)| addr1.cmp(addr2));
        for (sym, addr) in symvec {
            n += w.write(format!("x{:04x} {}\n", addr + self.orig, sym).as_bytes())?;
        }
        Ok(n)
    }

    pub fn resolve_symbols(&mut self) {
        // instruction address
        for (iaddr, label) in self.refs.iter() {
            // symbol address
            if let Some(saddr) = self.syms.get(label) {
                if let Some(mask) = self.mask.get(iaddr) {
                    match mask {
                        0 => self.mem[*iaddr as usize] = (self.orig + saddr) as u16, // .FILL
                        _ => {
                            // relative to the incremented PC
                            self.mem[*iaddr as usize] |=
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

    pub fn read(&mut self, r: &mut dyn Read) -> Result<usize, Error> {
        let mut n = 0;
        let mut buf: [u8; 2] = [0, 0];

        r.read_exact(&mut buf)?;
        self.orig = u16::from_be_bytes(buf);

        loop {
            if r.read(&mut buf)? == 0 {
                // TODO error on size other than 0 or 2
                break;
            }
            self.mem.push(u16::from_be_bytes(buf));
            n += 2;
        }
        Ok(n)
    }

    pub fn write(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += w.write(&u16::to_be_bytes(self.orig as u16))?;
        for inst in &self.mem {
            n += w.write(&u16::to_be_bytes(*inst))?;
        }
        Ok(n)
    }
}
