use lalrpop_util::lalrpop_mod;
use lc3::Program;
use std::io::{stdin, stdout, stderr, Error, Read};
use std::str;

lalrpop_mod!(#[allow(overflowing_literals)] pub asm);

fn main() -> Result<(), Error> {
    let parser = asm::ProgramParser::new();

    let mut prog = Program::new();

    let mut buffer = Vec::new();
    stdin().lock().read_to_end(&mut buffer)?;
    let input = str::from_utf8(&buffer).unwrap();

    parser.parse(&mut prog, input).unwrap();

    // TODO allow dumping symbol table to file
    prog.dump_symbols(&mut stderr().lock())?;

    prog.resolve_symbols();

    // TODO allow writing program to file
    prog.write(&mut stdout().lock())?;

    Ok(())
}
