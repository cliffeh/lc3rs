// use lc3::vm::VirtualMachine;
// use std::io::Write; // for flush()
// use std::{env, fs, io, process};
// use termios::{tcsetattr, Termios, ECHO, ICANON, TCSANOW};

pub fn main() {
//     // Save the current terminal settings (0 == stdin)
//     let termios = Termios::from_fd(0).unwrap();
//     let mut new_termios = termios.clone();

//     // Disable canonical mode and echo
//     new_termios.c_lflag &= !(ICANON | ECHO);

//     // Apply the new settings immediately
//     tcsetattr(0, TCSANOW, &new_termios).unwrap();

//     // Hide the cursor
//     print!("\x1B[?25l");
//     let _ = io::stdout().flush();

//     let _ = ctrlc::set_handler(move || {
//         // Show the cursor before exiting
//         print!("\x1B[?25h");
//         let _ = io::stdout().flush();

//         // Restore the original terminal settings
//         tcsetattr(0, TCSANOW, &termios).unwrap();

//         eprintln!("Ctrl-C pressed. Exiting...");
//         process::exit(1);
//     });

//     let mut vm = VirtualMachine::new();
//     let filename = env::args().nth(1).expect("Expected file argument");
//     let mut infile = fs::File::open(filename).unwrap();

//     // TODO handle result
//     let _ = vm.load(&mut infile);
//     vm.execute();

//     // Show the cursor before exiting
//     print!("\x1B[?25h");
//     let _ = io::stdout().flush();

//     // Restore the original terminal settings
//     tcsetattr(0, TCSANOW, &termios).unwrap();
}
