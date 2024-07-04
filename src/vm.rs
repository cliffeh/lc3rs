use crate::{sign_extend, Op, Program, Trap, MEMORY_MAX};
use std::io::{stdin, stdout, Error, Read, Write};

pub struct VirtualMachine {
    // program counter
    pc: u16,
    // processor status register
    psr: u16,
    mem: [u16; MEMORY_MAX],
    // 8 general-purpose registers
    reg: [u16; 8],
}

const COND_POS: u16 = 1 << 0;
const COND_ZRO: u16 = 1 << 1;
const COND_NEG: u16 = 1 << 2;

const MEM_KBSR: usize = 0xfe00; /* keyboard status register */
const MEM_KBDR: usize = 0xfe02; /* keyboard data register */

// unused in this implementation, but included for completeness' sake
#[allow(dead_code)]
const MEM_DSR: usize = 0xfe04; /* display status register */
#[allow(dead_code)]
const MEM_DDR: usize = 0xfe06; /* display data register */

const MEM_MCR: usize = 0xfffe; /* machine control register */

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            pc: 0u16,
            psr: 0u16,
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
        self.mem[MEM_MCR] = 1 << 15;

        while self.mem[MEM_MCR] >> 15 == 1 {
            let inst = self.mem[self.pc as usize];
            // DEBUG: eprintln!("PC {:04x}; inst: {:04x}", self.pc, inst);
            self.pc += 1;
            let op = inst >> 12;
            match Op::from(op) {
                Op::ADD => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let sr1 = ((inst >> 6) & 0x7) as usize;
                    let imm = (inst >> 5) & 0x1;

                    if imm == 0 {
                        // 3 registers
                        let sr2 = (inst & 0x7) as usize;
                        self.reg[dr] = self.reg[sr1].wrapping_add(self.reg[sr2]);
                    } else {
                        let imm5 = sign_extend(inst & 0x1f, 5);
                        self.reg[dr] = self.reg[sr1].wrapping_add(imm5);
                    }

                    self.setcc(self.reg[dr]);
                }
                Op::AND => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let sr1 = ((inst >> 6) & 0x7) as usize;
                    let imm = (inst >> 5) & 0x1;

                    if imm == 0 {
                        // 3 registers
                        let sr2 = (inst & 0x7) as usize;
                        self.reg[dr] = self.reg[sr1] & self.reg[sr2];
                    } else {
                        let imm5 = sign_extend(inst & 0x1f, 5);
                        self.reg[dr] = self.reg[sr1] & imm5;
                    }

                    self.setcc(self.reg[dr]);
                }
                Op::BR => {
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    let flags = (inst >> 9) & 0x7;

                    if flags & (self.psr & 0x7) != 0 {
                        self.pc = self.pc.wrapping_add(pcoffset9);
                    }
                }
                Op::JMP => {
                    let base_r = ((inst >> 6) & 0x7) as usize;
                    self.pc = self.reg[base_r];
                }
                Op::JSR => {
                    // store our return address
                    self.reg[7] = self.pc;

                    let flag = (inst >> 11) & 0x1;
                    if flag == 0 {
                        // JSRR
                        let base_r = ((inst >> 6) & 0x7) as usize;
                        self.pc = self.reg[base_r];
                    } else {
                        let pcoffset11 = sign_extend(inst & 0x7ff, 11);
                        self.pc = self.pc.wrapping_add(pcoffset11);
                    }
                }
                Op::LD => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    let addr = (self.pc.wrapping_add(pcoffset9)) as usize;

                    self.reg[dr] = self.read_mem(addr);
                    self.setcc(self.reg[dr]);
                }
                Op::LDI => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    let mut addr = (self.pc.wrapping_add(pcoffset9)) as usize;
                    addr = self.read_mem(addr) as usize;

                    self.reg[dr] = self.read_mem(addr);
                    self.setcc(self.reg[dr]);
                }
                Op::LDR => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let base_r = ((inst >> 6) & 0x7) as usize;
                    let offset6 = sign_extend(inst & 0x3f, 6);
                    let addr = (self.reg[base_r].wrapping_add(offset6)) as usize;

                    self.reg[dr] = self.read_mem(addr);
                    self.setcc(self.reg[dr]);
                }
                Op::LEA => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);

                    self.reg[dr] = self.pc.wrapping_add(pcoffset9);
                    self.setcc(pcoffset9);
                }
                Op::NOT => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let sr = ((inst >> 6) & 0x7) as usize;

                    self.reg[dr] = !self.reg[sr];
                    self.setcc(self.reg[dr]);
                }
                Op::ST => {
                    let sr = ((inst >> 9) & 0x7) as usize;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    let addr = (self.pc.wrapping_add(pcoffset9)) as usize;

                    self.mem[addr] = self.reg[sr];
                }
                Op::STI => {
                    let sr = ((inst >> 9) & 0x7) as usize;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    let mut addr = (self.pc.wrapping_add(pcoffset9)) as usize;

                    addr = self.read_mem(addr) as usize;
                    self.mem[addr] = self.reg[sr];
                }
                Op::STR => {
                    let sr = ((inst >> 9) & 0x7) as usize;
                    let base_r = ((inst >> 6) & 0x7) as usize;
                    let offset6 = sign_extend(inst & 0x3f, 6);
                    let addr = (self.reg[base_r].wrapping_add(offset6)) as usize;

                    self.mem[addr] = self.reg[sr];
                }
                Op::TRAP => {
                    // store the PC
                    self.reg[7] = self.pc;

                    let trapvect8 = inst & 0x00ff;
                    match Trap::from(trapvect8) {
                        Trap::GETC => {
                            let b = read_byte();
                            self.reg[0] = b as u16;
                            self.setcc(self.reg[0]);
                        }
                        Trap::OUT => {
                            let b = self.reg[0] as u8;
                            let mut out = stdout();
                            // TODO handle results
                            let _ = out.write(&[b]);
                            let _ = out.flush();
                        }
                        Trap::PUTS => {
                            let mut addr = self.reg[0] as usize;
                            let mut out = stdout();

                            while self.mem[addr] != 0 {
                                let b = self.mem[addr] as u8;
                                // TODO handle result
                                let _ = out.write(&[b]);
                                addr += 1;
                            }
                            // TODO handle result
                            let _ = out.flush();
                        }
                        Trap::IN => {
                            println!("Enter a character: ");
                            let b = read_byte();
                            let mut out = stdout();
                            let _ = out.write(&[b]);
                            // TODO handle result
                            let _ = out.flush();
                            self.reg[0] = b as u16;
                            self.setcc(self.reg[0]);
                        }
                        Trap::PUTSP => {
                            // two bytes per address
                            let mut addr = self.reg[0] as usize;
                            let mut out = stdout();

                            while self.mem[addr] != 0 {
                                let mut b = (self.mem[addr] & 0xff) as u8;
                                // TODO handle result
                                let _ = out.write(&[b]);
                                b = (self.mem[addr] >> 8) as u8;
                                // A character string consisting of an odd number of characters
                                // to be written will have x00 in bits [15:8] of the memory location
                                // containing the last character to be written.
                                if b != 0 {
                                    let _ = out.write(&[b]);
                                }
                                addr += 1;
                            }
                            // TODO handle result
                            let _ = out.flush();
                        }
                        Trap::HALT => {
                            // clearing the uppermost bit stops processing
                            self.mem[MEM_MCR] = self.mem[MEM_MCR] & 0x7fff;
                        }
                    }
                }
                Op::RTI | Op::RES => {
                    // RTI is unimplemented, RES is reserved
                    unimplemented!();
                }
            }
        }
    }

    fn read_mem(&mut self, addr: usize) -> u16 {
        if addr == MEM_KBSR {
            let b = read_byte();
            self.mem[MEM_KBSR] = 1 << 15;
            self.mem[MEM_KBDR] = b as u16;
        }

        return self.mem[addr];
    }

    fn setcc(&mut self, value: u16) {
        // clear the old condition code before setting the new one
        self.psr = self.psr & 0xfff8;

        // in 2s complement if the uppermost bit is set it's negative
        if value >> 15 == 1 {
            self.psr = self.psr | COND_NEG;
        } else if value == 0 {
            self.psr = self.psr | COND_ZRO;
        } else {
            self.psr = self.psr | COND_POS;
        }
    }
}

fn read_byte() -> u8 {
    let mut buf: [u8; 1] = [0];
    // TODO handle result
    let _ = stdin().read_exact(&mut buf);
    buf[0]
}
