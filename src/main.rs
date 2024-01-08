use lalrpop_util::lalrpop_mod;

use lc3::Program;

lalrpop_mod!(pub asm);

fn main() {
    let parser = asm::ProgramParser::new();

    let mut prog = Program::new();

    parser.parse(&mut prog, ".orig x3000\nadd r0, r1, r2\nand r3, r4, r5\n.end").unwrap();

    // println!("got: {}", op as u16);
}
