use lc3::{assemble, Program};
use rstest::rstest;
use std::{fs, path::PathBuf};

#[rstest]
fn test_assemble(#[files("examples/*.asm")] infile: PathBuf) {
    let mut outfile = infile.clone();
    outfile.set_extension("obj");

    let mut input = fs::File::open(infile).unwrap();
    let prog = assemble(&mut input).unwrap();

    let mut output = fs::File::open(outfile).unwrap();
    let expected = Program::read(&mut output).unwrap();

    assert_eq!(prog.mem, expected.mem);

    // TODO also test symbol table
}
