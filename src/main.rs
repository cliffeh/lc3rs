use lalrpop_util::lalrpop_mod;

use lc3::Program;

lalrpop_mod!(pub asm);

fn main() {
    let parser = asm::InstructionParser::new();

    let mut prog = Program::new();

    let op = parser.parse(&mut prog, "add r0, r1, r2").unwrap();

    println!("got: {}", op as u16);
}
