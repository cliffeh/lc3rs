use lalrpop_util::lalrpop_mod;

use lc3::Program;

lalrpop_mod!(pub asm);

fn main() {
    let parser = asm::ProgramParser::new();

    let mut prog = Program::new();

    parser
        .parse(
            &mut prog,
            ".orig x3000\n.fill x4000\nadd r0, r1, r2\ntest_label and r3, r4, r5\n.end",
        )
        .unwrap();

    for i in 0..prog.len {
        let addr = (prog.orig + i) as usize;
        println!("{:#06x}: {:#06x}", addr, prog.mem[addr]);
    }
    // println!("got: {}", op as u16);
}
