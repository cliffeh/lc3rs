use std::collections::HashMap;
use std::io::{BufRead, BufReader, Error, Read, Write};
pub mod asm;
// pub mod vm;
use std::fmt;
// use vm::{COND_NEG, COND_POS, COND_ZRO};

pub const MEMORY_MAX: usize = 1 << 16;

/// Top 4 bits of an instruction, indicating what oparation it is
#[repr(u16)]

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

/// Trap vectors
pub enum Trap {
    GETC = 0x20,  /* get character from keyboard, not echoed */
    OUT = 0x21,   /* output a character */
    PUTS = 0x22,  /* output a word string */
    IN = 0x23,    /* get character from keyboard, echoed onto the terminal */
    PUTSP = 0x24, /* output a byte string */
    HALT = 0x25,  /* halt the program */
}

#[derive(Clone, PartialEq)]
pub enum Hint {
    Fill,
    Stringz,
}

#[derive(Clone, PartialEq)]
pub struct Instruction {
    pub word: u16,
    pub label: Option<String>,
}

pub struct Program {
    /// Origin address of the program
    pub origin: u16,
    /// List of instructions that comprise the program
    pub instructions: Vec<Instruction>,
    /// Symbol table, indexed by position in `instructions`
    pub symbols: HashMap<String, usize>,
    /// Assembler directive type hints
    pub hints: HashMap<usize, Hint>,
}

impl Instruction {
    pub fn new(word: u16, label: Option<String>) -> Self {
        Instruction { word, label }
    }
}

impl<'p> Program {
    /// Creates a new program.
    pub fn new(
        origin: u16,
        instructions: Vec<Instruction>,
        symbols: HashMap<String, usize>,
        hints: HashMap<usize, Hint>,
    ) -> Self {
        Program {
            origin,
            instructions,
            symbols,
            hints,
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
        let mut symvec: Vec<(&String, &usize)> = self.symbols.iter().collect();
        symvec.sort_by(|(_, addr1), (_, addr2)| addr1.cmp(addr2));
        for (sym, saddr) in symvec {
            n += w.write(format!("x{:04x} {}\n", (*saddr as u16) + self.origin, sym).as_bytes())?;
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

            let addr = usize::from_str_radix(&s, 16).unwrap();
            let symbol = String::from(split.next().unwrap());

            self.symbols.insert(symbol.clone(), addr);
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
    pub fn lookup_symbol_by_address(&self, addr: usize) -> Option<&String> {
        for (symbol, saddr) in self.symbols.iter() {
            if *saddr == addr {
                return Some(symbol);
            }
        }
        None
    }

    /// Resolves symbols to their actual addresses and populates instructions accordingly.
    pub fn resolve_symbols(&mut self) -> Result<(), String> {
        // for each instruction
        for iaddr in 0..self.instructions.len() {
            // if that instruction references a label
            if let Some(label) = &self.instructions[iaddr].label {
                // get the address of that label
                if let Some(&saddr) = &self.symbols.get(label) {
                    if let Some(Hint::Fill) = self.hints.get(&iaddr) {
                        // for .FILL we want the "raw" address of the label relative to the program's origin
                        self.instructions[iaddr].word = self.origin.wrapping_add(saddr as u16);
                    } else {
                        // for operations we want the offset of the label relative to the incremented PC
                        let op = Op::from(self.instructions[iaddr].word >> 12);
                        match op {
                            Op::BR | Op::LD | Op::LDI | Op::LEA | Op::ST | Op::STI => { // PCoffset9
                                self.instructions[iaddr].word |= ((saddr as isize - iaddr as isize - 1) as u16) & 0x1ff;
                            }
                            Op::JSR => { // PCoffset11
                                self.instructions[iaddr].word |= ((saddr as isize - iaddr as isize - 1) as u16) & 0x7ff;
                            }
                            _ => { return Err(format!("unexpected symbol on {} operation", op)); }
                        }
                    }
                } else {
                    return Err(format!("undefined symbol: {}", label));
                }
            }
        }
        Ok(())
    }

    /// Writes the program out to `w`.
    pub fn write(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += w.write(&u16::to_be_bytes(self.origin as u16))?;
        for instruction in &self.instructions {
            n += w.write(&u16::to_be_bytes(instruction.word))?; // TODO!
        }
        Ok(n)
    }
}

impl Default for Program {
    fn default() -> Self {
        Program::new(0x3000, vec![], HashMap::new(), HashMap::new())
    }
}

/* conversion functions */

impl From<u16> for Op {
    fn from(value: u16) -> Self {
        unsafe { std::mem::transmute(value) }
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

/* utilities */

pub fn sign_extend(x: u16, count: usize) -> u16 {
    if ((x >> (count - 1)) & 1) != 0 {
        x | (0xFFFF << count)
    } else {
        x
    }
}

/* formatting */

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            Op::BR => "BR",
            Op::ADD => "ADD",
            Op::LD => "LD",
            Op::ST => "ST",
            Op::JSR => "JSR",
            Op::AND => "AND",
            Op::LDR => "LDR",
            Op::STR => "STR",
            Op::RTI => "RTI",
            Op::NOT => "NOT",
            Op::LDI => "LDI",
            Op::STI => "STI",
            Op::JMP => "JMP",
            Op::RES => "RES",
            Op::LEA => "LEA",
            Op::TRAP => "TRAP",
        })
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

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ".ORIG x{:04X}", self.origin)?;

        let mut iaddr = 0;
        while iaddr < self.instructions.len() {
            if let Some(label) = self.lookup_symbol_by_address(iaddr) {
                write!(f, "{} ", label)?;
            }

            if let Some(hint) = self.hints.get(&iaddr) {
                match hint {
                    Hint::Fill => {
                        if let Some(label) = &self.instructions[iaddr].label {
                            writeln!(f, ".FILL {}", label)?;
                        } else {
                            writeln!(f, ".FILL x{:04X}", self.instructions[iaddr].word)?;
                        }
                    }
                    Hint::Stringz => {
                        f.write_str(".STRINGZ \"")?;
                        while self.instructions[iaddr].word != 0 {
                            let b = (self.instructions[iaddr].word & 0xff) as u8;
                            write!(f, "{}", b as char)?;
                            iaddr += 1;
                        }
                        writeln!(f, "\"")?;
                    }
                }
            } else {
                write!(f, "{}\n", self.instructions[iaddr])?;
            }

            iaddr += 1;
        }
        writeln!(f, ".END")
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = Op::from(self.word >> 12);
        let mut s: String = format!("{}", op);
        match op {
            Op::ADD | Op::AND => {
                s += format!(
                    " R{}, R{}, ",
                    (self.word >> 9) & 0b111,
                    (self.word >> 6) & 0b111
                )
                .as_str();
                if (self.word & (1 << 5)) != 0 {
                    s += format!("#{}", sign_extend(self.word & 0x1f, 5) as i16).as_str();
                } else if self.word & (0b11 << 3) != 0 {
                    // invalid ADD|AND if these bits are set
                    s = format!(".FILL x{:04X}", self.word);
                } else {
                    s += format!("R{}", self.word & 0b111).as_str();
                }
            }
            Op::BR => {
                if self.word & (0b111 << 9) == 0 {
                    // invalid BR if these bits aren't set
                    s = format!(".FILL x{:04X}", self.word);
                }
                if self.word & (1 << 11) != 0 {
                    s += "n";
                }
                if self.word & (1 << 10) != 0 {
                    s += "z";
                }
                if self.word & (1 << 9) != 0 {
                    s += "p";
                }
                match &self.label {
                    Some(label) => s += format!(" {}", label).as_str(),
                    None => s += format!(" x{:04X}", self.word & 0x1ff).as_str(),
                }
            }
            Op::JMP => {
                if self.word & ((0b111 << 9) | (0b11111)) != 0 {
                    // invalid JMP|RET if these bits are set
                    s = format!(".FILL x{:04X}", self.word);
                } else {
                    let r1 = (self.word >> 6) & 0b111;
                    if r1 == 7 {
                        s = String::from("RET");
                    } else {
                        s += format!(" R{}", (self.word >> 6) & 0b111).as_str();
                    }
                }
            }
            Op::JSR => {
                if self.word & (1 << 11) != 0 {
                    match &self.label {
                        Some(label) => s += format!(" {}", label).as_str(),
                        None => s += format!(" x{:04X}", self.word & 0x7ff).as_str(),
                    }
                } else {
                    s = format!("JSRR R{}", (self.word >> 6) & 0b111);
                }
            }
            Op::LD | Op::LDI | Op::LEA | Op::ST | Op::STI => {
                let r1 = (self.word >> 9) & 0b111;
                s += format!(" R{}, ", r1).as_str();
                match &self.label {
                    Some(label) => s += format!("{}", label).as_str(),
                    None => s += format!("x{:04X}", self.word & 0x1ff).as_str(),
                }
            }
            Op::LDR | Op::STR => {
                s += format!(
                    " R{}, R{}, #{}",
                    (self.word >> 9) & 0b111,
                    (self.word >> 6) & 0b111,
                    sign_extend(self.word & 0x3ff, 6) as i16
                )
                .as_str();
            }
            Op::NOT => {
                if self.word & 0b111111 == 0 {
                    // invalid NOT if these bits aren't set
                    s = format!(".FILL x{:04X}", self.word);
                } else {
                    s += format!(
                        " R{}, R{}",
                        (self.word >> 9) & 0b111,
                        (self.word >> 6) & 0b111
                    )
                    .as_str();
                }
            }
            Op::TRAP => {
                if self.word & (0b1111 << 8) != 0 {
                    // invalid TRAP if these bits are set
                    s = format!(".FILL x{:04X}", self.word);
                } else {
                    let trapvect8 = self.word & 0xff;
                    if let Ok(trap) = Trap::try_from(trapvect8) {
                        s = format!("{}", trap);
                    } else {
                        s += format!(" x{:04X}", trapvect8).as_str();
                    }
                }
            }
            Op::RTI => {
                if self.word & 0xfff != 0 {
                    // invalid RTI if these bits are set
                    s = format!(".FILL x{:04X}", self.word);
                }
            }
            Op::RES => unimplemented!(),
        }
        f.write_str(&s)
    }
}

impl fmt::Debug for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Instruction word: x{:04X}, label: {}",
            self.word,
            match &self.label {
                Some(label) => format!("{}", label),
                None => "None".to_string(),
            }
        )
    }
}
