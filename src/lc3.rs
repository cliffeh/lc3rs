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

pub enum Reg {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC, /* program counter */
    COND,
    Count, /* hax */
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
    pub len: usize,
    pub mem: [u16; MEMORY_MAX],
    pub reg: [u16; Reg::Count as usize],
    pub syms: HashMap<String, usize>,
    pub refs: HashMap<String, usize>,
    pub hint: HashMap<usize, u16>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            orig: 0x3000,
            len: 0,
            mem: [0; MEMORY_MAX],
            reg: [0; Reg::Count as usize],
            syms: HashMap::new(),
            refs: HashMap::new(),
            hint: HashMap::new(),
        }
    }

    pub fn write(self, out: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += out.write(&u16::to_be_bytes(self.orig as u16))?;
        for i in 0..self.len {
            n += out.write(&u16::to_be_bytes(self.mem[self.orig + i]))?;
        }
        Ok(n)
    }
}
