use lalrpop_util::lalrpop_mod;
use lc3::Program;
use std::io::{stdin, stdout, stderr, Error, Read};
use std::str;

lalrpop_mod!(#[allow(overflowing_literals)] pub asm);
// lalrpop_mod!(pub asm);

fn main() -> Result<(), Error> {
    let parser = asm::ProgramParser::new();

    let mut prog = Program::new();

    // let input = ".orig x3000\n.fill x4000\nadd r0, r1, r2\ntest_label and r3, r4, r5\n.end";
    let mut buffer = Vec::new();
    stdin().lock().read_to_end(&mut buffer)?;
    let input = str::from_utf8(&buffer).unwrap();

    parser.parse(&mut prog, input).unwrap();

    // DEBUG dump symbol table to stderr
    prog.dump_symbols(&mut stderr().lock())?;

    prog.resolve_symbols();

    prog.write(&mut stdout().lock())?;

    // for i in 0..prog.len {
    //     let addr = (prog.orig + i) as usize;
    //     println!("{:#06x}: {:#06x}", addr, prog.mem[addr]);
    // }
    // println!("got: {}", op as u16);

    Ok(())
}
