use lc3::asm::assemble_program;
use lc3::Program;
use rstest::rstest;
use std::io::{Cursor, Read, Write};
use std::{fs, path::PathBuf};

#[rstest]
fn test_assemble(#[files("examples/*.asm")] infile: PathBuf) {
    let mut outfile = infile.clone();
    outfile.set_extension("obj");
    let mut expected: Vec<u8> = vec![];
    let _ = fs::File::open(outfile).unwrap().read_to_end(&mut expected);

    let source = fs::read_to_string(&infile).unwrap();
    let prog = assemble_program(&source).unwrap();
    let mut actual: Vec<u8> = vec![];
    let _ = prog.write(&mut actual);

    assert_eq!(actual, expected);
}

// Test that the disassembled program re-assembles to the same object code.
#[rstest]
fn test_disassemble_no_symbols(#[files("examples/*.obj")] infile: PathBuf) {
    // read in the object file
    let mut input = fs::File::open(infile.clone()).unwrap();
    let prog = Program::read(&mut input).unwrap();
    let mut expected: Vec<u8> = vec![];
    let _ = prog.write(&mut expected);

    // write out the disassembled program
    let disassembled_prog = format!("{}", prog);

    // re-assemble the program from what we've written out
    let reassembled_prog = assemble_program(&disassembled_prog).unwrap();

    let mut actual: Vec<u8> = vec![];
    let _ = reassembled_prog.write(&mut actual);

    assert_eq!(actual, expected);
}

// Test that the disassembled program re-assembles to the same object code.
#[rstest]
fn test_disassemble_with_symbols(#[files("examples/*.obj")] infile: PathBuf) {
    // read in the object file
    let mut input = fs::File::open(infile.clone()).unwrap();
    let mut prog = Program::read(&mut input).unwrap();
    let mut symfile = infile.clone();
    symfile.set_extension("sym");
    let mut symin = fs::File::open(symfile).unwrap();
    let mut buf = String::new();
    let _ = symin.read_to_string(&mut buf);
    let _ = prog.load_symbols(&buf);
    prog.infer_references();

    let mut expected: Vec<u8> = vec![];
    let _ = prog.write(&mut expected);

    // write out the disassembled program
    let disassembled_prog = format!("{}", prog);

    // re-assemble the program from what we've written out
    let reassembled_prog = assemble_program(&disassembled_prog).unwrap();

    let mut actual: Vec<u8> = vec![];
    let _ = reassembled_prog.write(&mut actual);

    assert_eq!(actual, expected);
}
