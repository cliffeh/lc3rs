use std::collections::HashMap;
use std::io::{BufRead, BufReader, Error, Read, Write};
mod asm;
pub mod vm;
pub use asm::assemble;
use std::fmt;
use vm::{COND_NEG, COND_POS, COND_ZRO};

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

pub enum Instruction {
    Add(u16, u16, bool, u16),
}

pub struct Program {
    /// Origin address of the program
    pub orig: u16,
    /// Instructions of the program
    pub mem: Vec<u16>,
    /// Symbol table
    pub syms: HashMap<String, u16>,
    /// Used for assembly, this maps relative instruction addresses onto the
    /// symbols they reference and the mask to use when resolving the address
    /// of the symbol.
    pub refs: HashMap<u16, (String, Option<u16>)>,
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
    /// relative to `self.orig` sorted in address order. The symbol table will also
    /// optionally include a comma-delimited list of the instructions that reference
    /// each symbol.
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
            n += w.write(format!("x{:04x} {}", saddr + self.orig, sym).as_bytes())?;

            let refs: Vec<String> = self
                .lookup_references_by_symbol(sym)
                .into_iter()
                .map(|iaddr| format!("x{:04X}", iaddr))
                .collect();
            if refs.len() > 0 {
                n += w.write(&[b' '])?;
                n += w.write(refs.join(",").as_bytes())?;
            }
            n += w.write(&[b'\n'])?;
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
    /// // also check that it loaded refs correctly
    /// assert_eq!(prog.refs.get(&0x1u16).unwrap().0, "foo".to_string());
    /// assert_eq!(prog.refs.get(&0x2u16).unwrap().0, "foo".to_string());
    ///
    /// ```
    pub fn load_symbols(&mut self, r: &mut dyn Read) -> Result<(), Error> {
        for res in BufReader::new(r).lines().into_iter() {
            let line = res.unwrap();
            let mut split = line.split(" ").into_iter();
            let s = &split.next().unwrap()[1..];

            let addr = u16::from_str_radix(&s, 16).unwrap();
            let symbol = String::from(split.next().unwrap());

            self.syms.insert(symbol.clone(), addr);

            if let Some(rest) = split.next() {
                let refstr = rest.split(",");
                for s in refstr {
                    let iaddr = u16::from_str_radix(&s[1..], 16).unwrap();
                    self.refs.insert(iaddr, (symbol.clone(), None));
                }
            }
        }

        Ok(())
    }

    /// Walks through all symbol references in `self.refs` and updates their respective
    /// instructions using the actual address of the symbol, masked appropriately for
    /// the instruction. Panics if there is an undefined symbol.
    pub fn resolve_symbols(&mut self) {
        // instruction address
        for (iaddr, (symbol, maskopt)) in self.refs.iter() {
            // symbol address
            if let Some(saddr) = self.syms.get(symbol) {
                if let Some(mask) = maskopt {
                    // relative to the incremented PC
                    self.mem[*iaddr as usize] |=
                        ((*saddr as i16 - *iaddr as i16 - 1) & *mask as i16) as u16;
                } else {
                    self.mem[*iaddr as usize] = (self.orig + saddr) as u16; // .FILL
                }
            } else {
                panic!("undefined symbol: {}", symbol);
            }
        }
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

    /// Does a reverse lookup of all relative instruction addresses that reference `sym`.
    ///
    /// # Example
    ///
    /// ```rust
    /// use lc3::Program;
    /// use std::io::BufReader;
    ///
    /// let mut prog = Program::default();
    ///
    /// let _ = prog.refs.insert(0x1, ("foo".to_string(), None));
    /// let _ = prog.refs.insert(0x2, ("foo".to_string(), None));
    ///
    /// let expected: Vec<u16> = vec![1u16, 2u16];
    /// let mut actual = prog.lookup_references_by_symbol(&"foo".to_string());
    /// actual.sort(); // not guaranteed to be in-order
    ///
    /// assert_eq!(actual, expected);
    /// ```
    pub fn lookup_references_by_symbol(&self, sym: &String) -> Vec<u16> {
        let mut refs: Vec<u16> = vec![];
        for (iaddr, (symbol, _mask)) in self.refs.iter() {
            if *symbol == *sym {
                refs.push(*iaddr);
            }
        }
        refs
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ".ORIG x{:04X}", self.orig)?;

        for iaddr in 0..self.mem.len() {
            // write!(f, "x{:04X} ", iaddr)?;

            if let Some(symbol) = self.lookup_symbol_by_address(iaddr as u16 + self.orig) {
                write!(f, "{} ", symbol)?;
            }

            let inst = self.mem[iaddr];

            if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                if let Some(saddr) = self.syms.get(symbol) {
                    // do you believe in coincidence?
                    if *saddr == inst {
                        writeln!(f, ".FILL {}", symbol)?;
                        continue;
                    }
                }
            }

            match Op::from(inst >> 12) {
                Op::ADD => {
                    let dr = (inst >> 9) & 0x7;
                    let sr1 = (inst >> 6) & 0x7;
                    let imm = (inst >> 5) & 0x1;

                    if imm == 0 {
                        // 3 registers
                        let sr2 = inst & 0x7;
                        if inst & (0x7 << 2) == 0 {
                            writeln!(f, "ADD R{}, R{}, R{}", dr, sr1, sr2)?;
                        } else {
                            writeln!(f, ".FILL x{:04X}", inst)?;
                        }
                    } else {
                        let imm5 = sign_extend(inst & 0x1f, 5) as i16;
                        writeln!(f, "ADD R{}, R{}, #{}", dr, sr1, imm5)?;
                    }
                }
                Op::AND => {
                    let dr = (inst >> 9) & 0x7;
                    let sr1 = (inst >> 6) & 0x7;
                    let imm = (inst >> 5) & 0x1;

                    if imm == 0 {
                        // 3 registers
                        let sr2 = inst & 0x7;
                        if inst & (0x7 << 2) == 0 {
                            writeln!(f, "AND R{}, R{}, R{}", dr, sr1, sr2)?;
                        } else {
                            writeln!(f, ".FILL x{:04X}", inst)?;
                        }
                    } else {
                        let imm5 = sign_extend(inst & 0x1f, 5) as i16;
                        writeln!(f, "AND R{}, R{}, #{}", dr, sr1, imm5)?;
                    }
                }
                Op::BR => {
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);
                    let flags = (inst >> 9) & 0x7;

                    write!(f, "BR")?;

                    if flags & COND_NEG == COND_NEG {
                        write!(f, "n")?;
                    }
                    if flags & COND_ZRO == COND_ZRO {
                        write!(f, "z")?;
                    }
                    if flags & COND_POS == COND_POS {
                        write!(f, "p")?;
                    }

                    if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                        writeln!(f, " {}", symbol)?;
                    } else {
                        writeln!(f, " x{:04X}", pcoffset9)?;
                    }
                }
                Op::JMP => {
                    if inst & (7 << 9) == 0 && inst & 0x3f == 0 {
                        let base_r = (inst >> 6) & 0x7;
                        if base_r == 7 {
                            writeln!(f, "RET")?;
                        } else {
                            writeln!(f, "JMP R{}", base_r)?;
                        }
                    } else {
                        writeln!(f, ".FILL x{:04X}", inst)?;
                    }
                }
                Op::JSR => {
                    let flag = (inst >> 11) & 0x1;
                    if flag == 0 {
                        // JSRR
                        if inst & (7 << 9) == 0 && inst & 0x3f == 0 {
                            let base_r = (inst >> 6) & 0x7;
                            writeln!(f, "JSRR R{}", base_r)?;
                        } else {
                            writeln!(f, ".FILL {:04X}", inst)?;
                        }
                    } else {
                        let pcoffset11 = inst & 0x7ff;

                        write!(f, "JSR")?;

                        if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                            writeln!(f, " {}", symbol)?;
                        } else {
                            writeln!(f, " x{:04X}", pcoffset11)?;
                        }
                    }
                }
                Op::LD => {
                    let dr = (inst >> 9) & 0x7;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);

                    write!(f, "LD R{},", dr)?;

                    if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                        writeln!(f, " {}", symbol)?;
                    } else {
                        writeln!(f, " x{:04X}", pcoffset9)?;
                    }
                }
                Op::LDI => {
                    let dr = (inst >> 9) & 0x7;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);

                    write!(f, "LDI R{},", dr)?;

                    if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                        writeln!(f, " {}", symbol)?;
                    } else {
                        writeln!(f, " x{:04X}", pcoffset9)?;
                    }
                }
                Op::LDR => {
                    let dr = (inst >> 9) & 0x7;
                    let base_r = (inst >> 6) & 0x7;
                    let offset6 = sign_extend(inst & 0x3f, 6) as i16;
                    writeln!(f, "LDR R{}, R{}, #{}", dr, base_r, offset6)?;
                }
                Op::LEA => {
                    let dr = (inst >> 9) & 0x7;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);

                    write!(f, "LEA R{},", dr)?;

                    if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                        writeln!(f, " {}", symbol)?;
                    } else {
                        writeln!(f, " x{:04X}", pcoffset9)?;
                    }
                }
                Op::NOT => {
                    let dr = (inst >> 9) & 0x7;
                    let sr = (inst >> 6) & 0x7;

                    writeln!(f, "NOT R{}, R{}", dr, sr)?;
                }
                Op::ST => {
                    let sr = (inst >> 9) & 0x7;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);

                    write!(f, "ST R{},", sr)?;

                    if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                        writeln!(f, " {}", symbol)?;
                    } else {
                        writeln!(f, " x{:04X}", pcoffset9)?;
                    }
                }
                Op::STI => {
                    let sr = (inst >> 9) & 0x7;
                    let pcoffset9 = sign_extend(inst & 0x1ff, 9);

                    write!(f, "STI R{},", sr)?;

                    if let Some((symbol, _mask)) = self.refs.get(&(iaddr as u16)) {
                        writeln!(f, " {}", symbol)?;
                    } else {
                        writeln!(f, " x{:04X}", pcoffset9)?;
                    }
                }
                Op::STR => {
                    let sr = (inst >> 9) & 0x7;
                    let base_r = (inst >> 6) & 0x7;
                    let offset6 = sign_extend(inst & 0x3f, 6) as i16;
                    writeln!(f, "STR R{}, R{}, #{}", sr, base_r, offset6)?;
                }
                Op::TRAP => {
                    if inst & (0xf << 8) == 0 {
                        if let Ok(trap) = Trap::try_from(inst & 0x00ff) {
                            writeln!(f, "{}", trap)?;
                        } else {
                            let trapvect8 = inst & 0x00ff;
                            writeln!(f, "TRAP x{:04X}", trapvect8)?;
                        }
                    } else {
                        writeln!(f, ".FILL x{:04X}", inst)?;
                    }
                }
                Op::RTI => {
                    writeln!(f, "RTI")?;
                }
                Op::RES => {
                    unimplemented!();
                }
            }
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
