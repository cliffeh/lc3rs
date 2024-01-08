use lalrpop_util::lalrpop_mod;
use lc3::Program;
use std::io::{stdin, stdout, Error, Read};
use std::str;

lalrpop_mod!(pub asm);

fn main() -> Result<(), Error> {
    let parser = asm::ProgramParser::new();

    let mut prog = Program::new();

    // let input = ".orig x3000\n.fill x4000\nadd r0, r1, r2\ntest_label and r3, r4, r5\n.end";
    let mut buffer = Vec::new();
    stdin().read_to_end(&mut buffer)?;
    let input = str::from_utf8(&buffer).unwrap();

    parser.parse(&mut prog, input).unwrap();

    // DEBUG dump symbol table to stderr
    // for (k, v) in prog.syms.iter() {
    //     eprintln!("{}: {:#06x}", k, (prog.orig + v));
    // }

    prog.resolve_symbols();

    prog.write(&mut stdout())?;

    // for i in 0..prog.len {
    //     let addr = (prog.orig + i) as usize;
    //     println!("{:#06x}: {:#06x}", addr, prog.mem[addr]);
    // }
    // println!("got: {}", op as u16);

    Ok(())
}
