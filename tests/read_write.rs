use lc3::Program;
use rstest::rstest;
use std::{fs, io::Read, path::PathBuf};

#[rstest]
fn test_read_write(#[files("examples/*.obj")] infile: PathBuf) {
    let mut expected: Vec<u8> = vec![];
    let _ = fs::File::open(infile.clone()).unwrap().read_to_end(&mut expected);

    let prog = Program::read(&mut fs::File::open(infile).unwrap()).unwrap();
    
    let mut actual: Vec<u8> = vec![];
    let _ = prog.write(&mut actual);

    assert_eq!(actual, expected);
}
