use lc3::{assemble, Program};
use std::fs;

fn main() {
    for example in ["2048", "gammut", "hello", "rogue"] {
        // TODO use PathBuf here...
        let infile =format!("{}/examples/{}.asm", env!("CARGO_MANIFEST_DIR"), example);
        let outfile = format!("{}/examples/{}.obj", env!("CARGO_MANIFEST_DIR"), example);

        print!("assembling example from {}...", infile);

        let mut input = fs::File::open(infile).unwrap();
        let prog = assemble(&mut input).unwrap();

        let mut output = fs::File::open(outfile).unwrap();
        let mut expected = Program::new();
        expected.read(&mut output).unwrap();

        assert_eq!(prog.mem, expected.mem);
        println!("OK");

        // TODO also test symbol table
    }
}
