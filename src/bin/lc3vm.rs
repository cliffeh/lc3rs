use lc3::vm::VirtualMachine;
use std::env;
use std::fs;
use termios::{tcsetattr, Termios, ECHO, ICANON, TCSANOW};

pub fn main() {
    // Save the current terminal settings (0 == stdin)
    let termios = Termios::from_fd(0).unwrap();
    let mut new_termios = termios.clone();

    // Disable canonical mode and echo
    new_termios.c_lflag &= !(ICANON | ECHO);

    // Apply the new settings immediately
    tcsetattr(0, TCSANOW, &new_termios).unwrap();

    let mut vm = VirtualMachine::new();
    let filename = env::args().nth(1).expect("Expected file argument");
    let mut infile = fs::File::open(filename).unwrap();

    // TODO handle result
    let _ = vm.load(&mut infile);
    vm.execute();

    // Restore the original terminal settings
    tcsetattr(0, TCSANOW, &termios).unwrap();
}
