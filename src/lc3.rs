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

pub const MEMORY_MAX: usize = 1 << 16;

pub struct Program {
    pub orig: u16,
    pub len: u16,
    pub mem: [u16; MEMORY_MAX],
    pub reg: [u16; Reg::Count as usize],
}

impl Program {
    pub fn new() -> Program {
        Program {
            orig: 0x3000,
            len: 0,
            mem: [0; MEMORY_MAX],
            reg: [0; Reg::Count as usize],
        }
    }
}
