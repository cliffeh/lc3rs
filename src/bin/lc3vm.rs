use lc3::vm::VirtualMachine;
use std::env;
use std::fs;

pub fn main() {
    let mut vm = VirtualMachine::new();
    let filename = env::args().nth(1).expect("Expected file argument");
    let mut infile = fs::File::open(filename).unwrap();

    // TODO handle result
    let _ = vm.load(&mut infile);
    vm.execute();
}
