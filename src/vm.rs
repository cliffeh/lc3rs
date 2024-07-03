use crate::{sign_extend, Op, Program, Trap, MEMORY_MAX};
use crossterm::event::{read, Event, KeyCode};
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
const MEM_KBSR: usize = 0xfe00; /* keyboard status */
const MEM_KBDR: usize = 0xfe02; /* keyboard data */

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
                Op::ADD => {
                    let dr = ((inst >> 9) & 0x7) as usize;
                    let sr1 = ((inst >> 6) & 0x7) as usize;
                    let imm = (inst >> 5) & 0x1;

                    if imm == 0 {
                        // 3 registers
                        let sr2 = (inst & 0x7) as usize;
                        self.reg[dr] = self.reg[sr1] + self.reg[sr2];
                    } else {
                        let imm5 = sign_extend(inst & 0x1f, 5);
                        self.reg[dr] = self.reg[sr1] + imm5;
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

    fn read_mem(&mut self, addr: usize) -> u16 {
        if addr == MEM_KBSR {
            let event = read().unwrap();
            match event {
                Event::Key(key) => {
                    self.mem[MEM_KBSR] = 1 << 15;
                    match key.code {
                        KeyCode::Char(c) => {
                            self.mem[MEM_KBDR] = c as u16;
                        }
                        _ => {
                            // TODO match other key codes?
                        }
                    }
                }
                _ => {
                    // TODO match other event types?
                }
            }
        }

        return self.mem[addr];
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
