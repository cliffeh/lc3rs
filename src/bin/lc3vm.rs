use clap::Parser;
use lc3::vm::VirtualMachine;
use std::io::Write; // for flush()
use std::{fs, io, path, process};
use termios::{tcsetattr, Termios, ECHO, ICANON, TCSANOW};

static LONG_ABOUT: &str = r#"
This program loads and executes LC3 programs.
"#;

#[derive(Parser)]
#[command(author, version, about = "an LC3 virtual machine", long_about=LONG_ABOUT)]
struct Args {
    /// Read input from FILE [required]
    #[arg(value_name = "INFILE", required = true)]
    input: path::PathBuf,
}

pub fn main() {
    let args = Args::parse();

    // Save the current terminal settings (0 == stdin)
    let termios = Termios::from_fd(0).unwrap();
    let mut new_termios = termios.clone();

    // Disable canonical mode and echo
    new_termios.c_lflag &= !(ICANON | ECHO);

    // Apply the new settings immediately
    tcsetattr(0, TCSANOW, &new_termios).unwrap();

    // Hide the cursor
    print!("\x1B[?25l");
    let _ = io::stdout().flush();

    let _ = ctrlc::set_handler(move || {
        // Show the cursor before exiting
        print!("\x1B[?25h");
        let _ = io::stdout().flush();

        // Restore the original terminal settings
        tcsetattr(0, TCSANOW, &termios).unwrap();

        eprintln!("Ctrl-C pressed. Exiting...");
        process::exit(1);
    });

    let mut vm = VirtualMachine::new();
    let mut infile = fs::File::open(args.input).unwrap();

    // TODO handle result
    let _ = vm.load(&mut infile);
    vm.execute();

    // Show the cursor before exiting
    print!("\x1B[?25h");
    let _ = io::stdout().flush();

    // Restore the original terminal settings
    tcsetattr(0, TCSANOW, &termios).unwrap();
}
