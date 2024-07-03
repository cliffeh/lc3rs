use crate::{sign_extend, Op, Program, Trap, MEMORY_MAX};
use std::io::{stdout, Error, Read, Write};

pub struct VirtualMachine {
    pc: u16,
    cond: u16,
    mem: [u16; MEMORY_MAX],
    // 8 general-purpose registers
    reg: [u16; 8],
}

const COND_N: u16 = 2;
const COND_Z: u16 = 1;
const COND_P: u16 = 0;

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            pc: 0u16,
            cond: 0u16,
            mem: [0u16; MEMORY_MAX],
            reg: [0u16; 8],
        }
    }

    pub fn load(&mut self, r: &mut dyn Read) -> Result<(), Error> {
        let prog = Program::read(r)?;
        let orig = prog.orig as usize;
        for pos in 0..prog.mem.len() {
            self.mem[pos + orig] = prog.mem[pos];
        }
        Ok(())
    }

    pub fn execute(&mut self) {
        self.pc = 0x3000;

        loop {
            let inst = self.mem[self.pc as usize];
            self.pc += 1;
            let op = inst >> 12;
            match Op::from(op) {
                Op::LEA => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    self.reg[dr] = self.pc + pcoffset9;
                    self.setcc(pcoffset9);
                }
                Op::TRAP => {
                    // store the PC
                    self.reg[7] = self.pc;

                    let trapvect8 = inst & 0x00ff;
                    match Trap::from(trapvect8) {
                        Trap::PUTS => {
                            let mut addr = self.reg[0] as usize;
                            let mut out = stdout();

                            while self.mem[addr] != 0 {
                                let b = (self.mem[addr] & 0x00ff) as u8;
                                // TODO handle result
                                let _ = out.write(&[b]);
                                addr += 1;
                            }
                            // TODO handle result
                            let _ = out.flush();
                        }
                        Trap::HALT => {
                            break;
                        }
                        _ => {
                            unimplemented!();
                        }
                    }
                }
                _ => {
                    unimplemented!();
                }
            }
        }
    }

    fn setcc(&mut self, value: u16) {
        // in 2s complement if the uppermost bit is set it's negative
        if value & 0x8000 == 0 {
            self.cond = COND_N;
        } else if value == 0 {
            self.cond = COND_Z;
        } else {
            self.cond = COND_P;
        }
    }
}
