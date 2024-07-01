use std::collections::HashMap;
use std::io::{Error, Write};

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

impl Op {
    pub fn from_u16(op: u16) -> Op {
        match op {
            0 => Op::BR,
            1 => Op::ADD,
            2 => Op::LD,
            3 => Op::ST,
            4 => Op::JSR,
            5 => Op::AND,
            6 => Op::LDR,
            7 => Op::STR,
            8 => Op::RTI,
            9 => Op::NOT,
            10 => Op::LDI,
            11 => Op::STI,
            12 => Op::JMP,
            13 => Op::RES,
            14 => Op::LEA,
            15 => Op::TRAP,
            _ => unreachable!("unknown op code {}", op),
        }
    }
}

pub enum Trap {
    GETC = 0x20,  /* get character from keyboard, not echoed */
    OUT = 0x21,   /* output a character */
    PUTS = 0x22,  /* output a word string */
    IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
    PUTSP = 0x24, /* output a byte string */
    HALT = 0x25,  /* halt the program */
}

pub const MEMORY_MAX: usize = 1 << 16;

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

    pub fn dump_symbols(&mut self, out: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        let mut symvec : Vec<(&String, &usize)> = self.syms.iter().collect();
        symvec.sort_by(|(_, addr1), (_, addr2)| addr1.cmp(addr2));
        for (sym, addr) in symvec {
            n += out.write(format!("x{:04x} {}\n", addr+self.orig, sym).as_bytes())?;
        }
        Ok(n)
    }

    pub fn resolve_symbols(&mut self) {
        for (iaddr, label) in self.refs.iter() {
            if let Some(saddr) = self.syms.get(label) {
                if let Some(mask) = self.mask.get(iaddr) {
                    match mask {
                        0 => self.mem[*iaddr] = (self.orig + saddr) as u16, // .FILL
                        _ => {
                            // self.mem[iaddr] |= (saddr - iaddr - 1) & mask
                            self.mem[*iaddr] |= ((*saddr as i16 - *iaddr as i16 - 1) & *mask as i16) as u16;
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
