use pest::Parser;
use pest_derive::Parser;
use std::io::{stdin, Read};

#[derive(Parser)]
#[grammar = "parser/lc3.pest"]
struct LC3Parser;

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
    COUNT, /* hax */
}

pub const MEMORY_MAX: usize = 1 << 16;

pub struct Program {
    pub orig: u16,
    pub mem: [u16; MEMORY_MAX],
    pub reg: [u16; Reg::COUNT as usize],
}

fn main() {
    // let mut stdin = stdin();
    // let mut buf : String = String::new();
    // let result = stdin.read_to_string(&mut buf);

    // let x: u16 = 65535;

    let input = "; this is a small program\nadd r0, r1, r2\nand r3, r4, #7";

    let pairs = LC3Parser::parse(Rule::program, input).unwrap_or_else(|e| panic!("{}", e));

    for pair in pairs {
        // A pair is a combination of the rule which matched and a span of input
        println!("Rule:    {:?}", pair.as_rule());
        println!("Span:    {:?}", pair.as_span());
        println!("Text:    {}", pair.as_str());

        // // A pair can be converted to an iterator of the tokens which make it up:
        // for inner_pair in pair.into_inner() {
        //     match inner_pair.as_rule() {
        //         Rule::add => println!("add:  {}", inner_pair.as_str()),
        //         Rule::and => println!("and:  {}", inner_pair.as_str()),
        //         _ => println!("i don't know what this is: {}", inner_pair.as_str()),
        //     };
        // }
    }
}
