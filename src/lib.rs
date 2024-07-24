use std::collections::HashMap;
use std::io::{BufRead, BufReader, Error, Read, Write};
pub mod asm;
use std::fmt;

use thiserror::Error;
// use vm::{COND_NEG, COND_POS, COND_ZRO};

pub const MEMORY_MAX: usize = 1 << 16;

/// Top 4 bits of an instruction, indicating what operation it is
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

#[derive(Clone, Debug, PartialEq)]
pub enum Hint {
    Fill,
    Stringz,
}

pub struct Program {
    /// Origin address of the program
    origin: u16,
    /// List of instructions that comprise the program
    instructions: Vec<u16>,
    /// Symbol table, containing addresses of symbols and assembler directive hints
    symtab: SymbolTable,
}

impl<'p> Program {
    /// Creates a new program.
    pub fn new(origin: u16, instructions: Vec<u16>, symtab: SymbolTable) -> Self {
        Program {
            origin,
            instructions,
            symtab,
        }
    }

    /// Resolves symbols to their actual addresses and populates instructions accordingly.
    pub fn resolve_symbols(&mut self) -> Result<(), SymbolError> {
        // for each instruction
        for iaddr in 0..self.instructions.len() {
            // if that instruction references a label
            if let Some(label) = self.symtab.get_ref(&iaddr) {
                // get the address of that label
                if let Some(&saddr) = &self.symtab.get_symbol_address(label) {
                    if let Some(Hint::Fill) = self.symtab.get_hint(&iaddr) {
                        // for .FILL we want the "raw" address of the label relative to the program's origin
                        self.instructions[iaddr] = self.origin.wrapping_add(saddr as u16);
                    } else {
                        // for operations we want the offset of the label relative to the incremented PC
                        let op = Op::from(self.instructions[iaddr] >> 12);
                        match op {
                            Op::BR | Op::LD | Op::LDI | Op::LEA | Op::ST | Op::STI => {
                                // PCoffset9
                                self.instructions[iaddr] |=
                                    ((saddr as isize - iaddr as isize - 1) as u16) & 0x1ff;
                            }
                            Op::JSR => {
                                // PCoffset11
                                self.instructions[iaddr] |=
                                    ((saddr as isize - iaddr as isize - 1) as u16) & 0x7ff;
                            }
                            _ => {
                                return Err(SymbolError::UnexpectedSymbol(format!(
                                    "{} doesn't take a symbol argument",
                                    op
                                )));
                            }
                        }
                    }
                } else {
                    return Err(SymbolError::UndefinedSymbol(label.into()));
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
            n += w.write(&u16::to_be_bytes(*instruction))?; // TODO!
        }
        Ok(n)
    }
}

pub struct SymbolTable {
    /// Map of symbols to their respective addresses within a program
    symbols: HashMap<String, usize>,
    /// Map of addresses to assembler directive hints
    hints: HashMap<usize, Hint>,
    /// Map of addresses to symbol references
    refs: HashMap<usize, String>,
}

#[derive(Error, Clone, Debug, PartialEq)]
pub enum SymbolError {
    #[error("duplicate symbol: {0}")]
    DuplicateSymbol(String),

    #[error("undefined symbol: {0}")]
    UndefinedSymbol(String),

    #[error("unexpected symbol: {0}")]
    UnexpectedSymbol(String),
}

impl SymbolTable {
    pub fn new(
        symbols: HashMap<String, usize>,
        hints: HashMap<usize, Hint>,
        refs: HashMap<usize, String>,
    ) -> SymbolTable {
        SymbolTable {
            symbols,
            hints,
            refs,
        }
    }
    pub fn insert_symbol(&mut self, label: String, addr: usize) -> Option<usize> {
        self.symbols.insert(label, addr)
    }

    pub fn get_symbol_address(&self, label: &String) -> Option<&usize> {
        self.symbols.get(label)
    }

    pub fn insert_hint(&mut self, addr: usize, hint: Hint) -> Option<Hint> {
        self.hints.insert(addr, hint)
    }

    pub fn get_hint(&self, addr: &usize) -> Option<&Hint> {
        self.hints.get(addr)
    }

    pub fn insert_ref(&mut self, addr: usize, label: String) -> Option<String> {
        self.refs.insert(addr, label)
    }

    pub fn get_ref(&self, addr: &usize) -> Option<&String> {
        self.refs.get(addr)
    }

    /// Does a reverse lookup of a symbol, given its address
    pub fn lookup_symbol_by_address(&self, addr: usize) -> Option<&String> {
        for (symbol, saddr) in self.symbols.iter() {
            if *saddr == addr {
                return Some(symbol);
            }
        }
        None
    }
}

/* defaults */

impl Default for Program {
    fn default() -> Self {
        Program::new(0x3000, vec![], SymbolTable::default())
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable::new(HashMap::new(), HashMap::new(), HashMap::new())
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

/// Escapes the input character.
/// // TODO write a test
fn escape(c: char) -> String {
    let mut result = String::new();

    match c {
        '\n' => result.push_str("\\n"),
        '\t' => result.push_str("\\t"),
        '\r' => result.push_str("\\r"),
        '\\' => result.push_str("\\\\"),
        '\'' => result.push_str("\\'"),
        '\"' => result.push_str("\\\""),
        '\0' => result.push_str("\\0"),
        '\x1B' => result.push_str("\\e"), // ANSI escape character
        _ if c.is_control() => {
            // Handle other control characters with hexadecimal escape sequence
            result.push_str(&format!("\\x{:02X}", c as u8));
        }
        _ => result.push(c),
    }

    result
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

impl fmt::Display for Hint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Hint::Fill => f.write_str("_FILL"),
            Hint::Stringz => f.write_str("_STRINGZ"),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, ".ORIG x{:04X}", self.origin)?;

        let mut iaddr = 0;
        while iaddr < self.instructions.len() {
            if let Some(label) = self.symtab.lookup_symbol_by_address(iaddr) {
                write!(f, "{} ", label)?;
            }

            if let Some(hint) = self.symtab.get_hint(&iaddr) {
                match hint {
                    Hint::Fill => {
                        if let Some(label) = self.symtab.get_ref(&iaddr) {
                            writeln!(f, ".FILL {}", label)?;
                        } else {
                            writeln!(f, ".FILL x{:04X}", self.instructions[iaddr])?;
                        }
                    }
                    Hint::Stringz => {
                        f.write_str(".STRINGZ \"")?;
                        while self.instructions[iaddr] != 0 {
                            let b = (self.instructions[iaddr] & 0xff) as u8;
                            write!(f, "{}", escape(b as char))?;
                            iaddr += 1;
                        }
                        writeln!(f, "\"")?;
                    }
                }
            } else {
                let inst = self.instructions[iaddr];
                let op: Op = inst.into();
                let mut s: String = format!("{}", op);

                match op {
                    Op::ADD | Op::AND => {
                        s += format!(" R{}, R{}, ", (inst >> 9) & 0b111, (inst >> 6) & 0b111)
                            .as_str();
                        if (inst & (1 << 5)) != 0 {
                            s += format!("#{}", sign_extend(inst & 0x1f, 5) as i16).as_str();
                        } else if inst & (0b11 << 3) != 0 {
                            // invalid ADD|AND if these bits are set
                            s = format!(".FILL x{:04X}", inst);
                        } else {
                            s += format!("R{}", inst & 0b111).as_str();
                        }
                    }
                    Op::BR => {
                        if inst & (0b111 << 9) == 0 {
                            // invalid BR if these bits aren't set
                            s = format!(".FILL x{:04X}", inst);
                        }
                        if inst & (1 << 11) != 0 {
                            s += "n";
                        }
                        if inst & (1 << 10) != 0 {
                            s += "z";
                        }
                        if inst & (1 << 9) != 0 {
                            s += "p";
                        }
                        if let Some(label) = self.symtab.get_ref(&iaddr) {
                            s += format!(" {}", label).as_str();
                        } else {
                            s += format!(" x{:04X}", inst & 0x1ff).as_str();
                        }
                    }
                    Op::JMP => {
                        if inst & ((0b111 << 9) | (0b11111)) != 0 {
                            // invalid JMP|RET if these bits are set
                            s = format!(".FILL x{:04X}", inst);
                        } else {
                            let r1 = (inst >> 6) & 0b111;
                            if r1 == 7 {
                                s = String::from("RET");
                            } else {
                                s += format!(" R{}", (inst >> 6) & 0b111).as_str();
                            }
                        }
                    }
                    Op::JSR => {
                        if inst & (1 << 11) != 0 {
                            if let Some(label) = self.symtab.get_ref(&iaddr) {
                                s += format!(" {}", label).as_str();
                            } else {
                                s += format!(" x{:04X}", inst & 0x7ff).as_str();
                            }
                        } else {
                            s = format!("JSRR R{}", (inst >> 6) & 0b111);
                        }
                    }
                    Op::LD | Op::LDI | Op::LEA | Op::ST | Op::STI => {
                        let r1 = (inst >> 9) & 0b111;
                        s += format!(" R{}, ", r1).as_str();
                        if let Some(label) = self.symtab.get_ref(&iaddr) {
                            s += format!("{}", label).as_str();
                        } else {
                            s += format!("x{:04X}", inst & 0x1ff).as_str();
                        }
                    }
                    Op::LDR | Op::STR => {
                        s += format!(
                            " R{}, R{}, #{}",
                            (inst >> 9) & 0b111,
                            (inst >> 6) & 0b111,
                            sign_extend(inst & 0x3ff, 6) as i16
                        )
                        .as_str();
                    }
                    Op::NOT => {
                        if inst & 0b111111 == 0 {
                            // invalid NOT if these bits aren't set
                            s = format!(".FILL x{:04X}", inst);
                        } else {
                            s += format!(" R{}, R{}", (inst >> 9) & 0b111, (inst >> 6) & 0b111)
                                .as_str();
                        }
                    }
                    Op::TRAP => {
                        if inst & (0b1111 << 8) != 0 {
                            // invalid TRAP if these bits are set
                            s = format!(".FILL x{:04X}", inst);
                        } else {
                            let trapvect8 = inst & 0xff;
                            if let Ok(trap) = Trap::try_from(trapvect8) {
                                s = format!("{}", trap);
                            } else {
                                s += format!(" x{:04X}", trapvect8).as_str();
                            }
                        }
                    }
                    Op::RTI => {
                        if inst & 0xfff != 0 {
                            // invalid RTI if these bits are set
                            s = format!(".FILL x{:04X}", inst);
                        }
                    }
                    Op::RES => unimplemented!(),
                }
                write!(f, "{}", s);
            }

            iaddr += 1;
        }
        writeln!(f, ".END")
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        for (symbol, addr) in self.symbols.iter() {
            writeln!(f, "x{addr:04x} {symbol}")?;
        }
        for (addr, hint) in self.hints.iter() {
            writeln!(f, "x{addr:04x} {hint}")?;
        }
        Ok(())
    }
}
