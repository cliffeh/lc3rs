use std::collections::HashMap;
use std::io::{BufRead, BufReader, Error, Read, Write};
pub mod asm;
// pub mod vm;
use std::fmt;
// use vm::{COND_NEG, COND_POS, COND_ZRO};

pub const MEMORY_MAX: usize = 1 << 16;

/// Top 4 bits of an instruction, indicating what oparation it is
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

pub struct Program {
    /// Origin address of the program
    pub origin: u16,
    /// List of instructions that comprise the program
    pub instructions: Vec<u16>,
    /// Symbol table, indexed by position in `instructions`
    pub symbols: HashMap<String, usize>,
    /// Referenced symbols, indexed by position in `instructions`
    pub refs: HashMap<usize, String>,
}

impl Program {
    /// Creates a new program.
    pub fn new(origin: u16, instructions: Vec<u16>) -> Program {
        Program {
            origin,
            instructions,
            symbols: HashMap::new(),
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
        let mut symvec: Vec<(&String, &u16)> = self.symbols.iter().collect();
        symvec.sort_by(|(_, addr1), (_, addr2)| addr1.cmp(addr2));
        for (sym, saddr) in symvec {
            n += w.write(format!("x{:04x} {}\n", saddr + self.origin, sym).as_bytes())?;
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

            let addr = u16::from_str_radix(&s, 16).unwrap();
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
    pub fn lookup_symbol_by_address(&self, addr: u16) -> Option<&String> {
        for (symbol, saddr) in self.symbols.iter() {
            if *saddr == addr {
                return Some(symbol);
            }
        }
        None
    }

    /// Writes the program out to `w`.
    pub fn write(&self, w: &mut dyn Write) -> Result<usize, Error> {
        let mut n: usize = 0;
        n += w.write(&u16::to_be_bytes(self.origin as u16))?;
        for instruction in &self.instructions {
            n += w.write(&u16::to_be_bytes(*instruction))?; // TODO!
        }
        Ok(n)
    }
}

impl Default for Program {
    fn default() -> Self {
        Program {
            origin: 0x3000,
            instructions: vec![],
            symbols: HashMap::new(),
        }
    }
}

/* conversion functions */

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
        match *self {
            Op::BR => write!(f, "BR"),
            Op::ADD => write!(f, "ADD"),
            Op::LD => write!(f, "LD"),
            Op::ST => write!(f, "ST"),
            Op::JSR => write!(f, "JSR"),
            Op::AND => write!(f, "AND"),
            Op::LDR => write!(f, "LDR"),
            Op::STR => write!(f, "STR"),
            Op::RTI => write!(f, "RTI"),
            Op::NOT => write!(f, "NOT"),
            Op::LDI => write!(f, "LDI"),
            Op::STI => write!(f, "STI"),
            Op::JMP => write!(f, "JMP"),
            Op::RES => write!(f, "RES"),
            Op::LEA => write!(f, "LEA"),
            Op::TRAP => write!(f, "TRAP"),
        }
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
