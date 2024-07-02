use crate::Program;
use lalrpop_util::lalrpop_mod;
use std::io::{Error, Read};

lalrpop_mod!(#[allow(overflowing_literals)] pub parser);

pub fn assemble(r: &mut dyn Read) -> Result<Program, Error> {
    let mut prog = Program::default();
    let parser = parser::ProgramParser::new();
    let mut buf = String::new();

    r.read_to_string(&mut buf)?;
    parser.parse(&mut prog, buf.as_str()).unwrap(); // TODO unsafe unwrap

    prog.resolve_symbols();

    Ok(prog)
}
