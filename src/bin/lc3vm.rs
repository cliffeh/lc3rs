use lc3::vm::VirtualMachine;
use std::env;
use std::fs;
use crossterm::terminal::{enable_raw_mode, disable_raw_mode};

pub fn main() {
    // TODO handle result
    let _ = enable_raw_mode();

    let mut vm = VirtualMachine::new();
    let filename = env::args().nth(1).expect("Expected file argument");
    let mut infile = fs::File::open(filename).unwrap();

    // TODO handle result
    let _ = vm.load(&mut infile);
    vm.execute();

    // TODO handle result
    let _ = disable_raw_mode();
}
