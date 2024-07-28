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

// // Test that the disassembled program re-assembles to the same object code.
// #[rstest]
// fn test_disassemble_with_symbols(#[files("examples/*.obj")] infile: PathBuf) {
//     // read in the object file
//     let mut input = fs::File::open(infile.clone()).unwrap();
//     let mut expected = Program::read(&mut input).unwrap();

//     // ...and load the symbol table
//     let mut symfile = infile.clone();
//     symfile.set_extension("sym");
//     let _ = expected.load_symbols(&mut fs::File::open(symfile.clone()).unwrap());

//     // write out the disassembled program
//     let mut output: Vec<u8> = vec![];
//     let _ = write!(output, "{}", expected);

//     // re-assemble the program from what we've written out
//     let actual = assemble(&mut Cursor::new(output)).unwrap();

//     // ...and test that the assembled object code is the same
//     for pos in 0..actual.mem.len() {
//         assert_eq!(
//             actual.mem[pos], expected.mem[pos],
//             "checking mem[{}]: x{:04X} x{:04X}",
//             pos, actual.mem[pos], expected.mem[pos]
//         );
//     }
// }
