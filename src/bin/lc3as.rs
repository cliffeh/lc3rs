use clap::Parser;
use lc3::asm::assemble_program;
use lc3::Program;
use std::io::Write;
use std::path::PathBuf;
use std::{error, io};
use std::{fs, path};

static LONG_ABOUT: &str = r#"
This program assembles and disassembles LC3 programs.

The default mode of operation is to read an input file of LC3 assembly code,
assemble the program, and output the resulting object code. Passing the -D flag
puts the program into disassembly mode, in which the program will read in object
code, disassemble it, and output the resulting assembly code.

To the degree possible it attempts to be "smart" about what the filenames should
be, inferring their names using .asm, .obj, and .sym extensions for assembly,
object, and symbol files based on the input filename and mode of operation.

The program accepts "-" to mean stdout for output and stdin for input. Note that
specifying it more than once for either input or output results in undefined
behavior.

Examples:

     # assemble prog.asm and write the object code to prog.obj
     lc3as prog.asm

     # assemble prog.asm, write object code to prog.obj, write symbols to prog.sym
     lc3as prog.asm -s

     # assemble prog.asm, write object code to myfile.obj, write symbols to stdout
     lc3as prog.asm -s- -o myfile.obj

     # disassemble prog.obj, write assembly to prog.asm
     lc3as -D prog.obj

     # disassemble prog.obj, read symbols from prog.sym, write assembly to prog.asm
     lc3as -D prog.obj -s

     # disassemble prog.obj, read symbols from stdin, write assembly to myfile.asm
     lc3as -D prog.obj -s- -o myfile.asm
"#;

#[derive(Parser)]
#[command(author, version, about = "an LC3 assembler/disassembler", long_about=LONG_ABOUT)]
struct Args {
    /// Disassemble object code (default: <INFILE>.obj)
    #[arg(short = 'D', long, default_value_t = false)]
    dissasemble: bool,

    /// Write output to FILE
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Write symbol table to FILE (or read it from FILE, if -D is specified)
    #[arg(short, long, value_name = "FILE")]
    symbols: Option<Option<PathBuf>>,

    /// Read input from FILE [required]
    #[arg(value_name = "INFILE", required = true)]
    input: path::PathBuf,
}

impl Args {
    fn get_assembly_source(&self) -> Result<String, io::Error> {
        let mut input: Box<dyn io::Read> = if self.input == PathBuf::from("-") {
            Box::new(io::stdin().lock()) as Box<dyn io::Read>
        } else {
            Box::new(fs::File::open(&self.input)?) as Box<dyn io::Read>
        };
        let mut buf = String::new();
        input.read_to_string(&mut buf)?;

        Ok(buf)
    }

    fn get_object_read(&self) -> Result<impl io::Read, io::Error> {
        let input: Box<dyn io::Read> = if self.input == PathBuf::from("-") {
            Box::new(io::stdin().lock()) as Box<dyn io::Read>
        } else {
            Box::new(fs::File::open(&self.input)?) as Box<dyn io::Read>
        };

        Ok(input)
    }

    fn get_output_write(&self, ext: &str) -> Result<impl io::Write, io::Error> {
        Ok(if let Some(path) = &self.output {
            if *path == PathBuf::from("-") {
                Box::new(io::stdout().lock()) as Box<dyn io::Write>
            } else {
                Box::new(fs::File::create(&path)?) as Box<dyn io::Write>
            }
        } else {
            let mut path = self.input.clone();
            path.set_extension(ext);
            Box::new(fs::File::create(&path)?) as Box<dyn io::Write>
        })
    }

    fn get_symbol_write(&self) -> Result<impl io::Write, io::Error> {
        if let Some(opt) = &self.symbols {
            if let Some(path) = opt {
                if *path == PathBuf::from("-") {
                    Ok(Box::new(io::stdout().lock()) as Box<dyn io::Write>)
                } else {
                    Ok(Box::new(fs::File::create(&path)?) as Box<dyn io::Write>)
                }
            } else {
                // construct a filename from the input file
                let mut path = self.input.clone();
                path.set_extension("sym");
                Ok(Box::new(fs::File::create(&path)?) as Box<dyn io::Write>)
            }
        } else {
            // we don't want to dump the symbol table
            Ok(Box::new(io::empty()) as Box<dyn io::Write>)
        }
    }

    fn get_symbol_source(&self) -> Result<String, io::Error> {
        if let Some(opt) = &self.symbols {
            let path = if let Some(userpath) = opt {
                userpath.clone()
            } else {
                let mut inferredpath = self.input.clone();
                inferredpath.set_extension("sym");
                inferredpath
            };

            let mut input: Box<dyn io::Read> = if path == PathBuf::from("-") {
                Box::new(io::stdin().lock()) as Box<dyn io::Read>
            } else {
                Box::new(fs::File::open(path)?) as Box<dyn io::Read>
            };

            let mut buf = String::new();
            input.read_to_string(&mut buf)?;
            Ok(buf)
        } else {
            // we don't want to read the symbol table
            Ok(String::new())
        }
    }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = Args::parse();

    if args.dissasemble {
        let mut r = args.get_object_read()?;
        let mut prog = Program::read(&mut r)?;
        let symbols = args.get_symbol_source()?;
        prog.load_symbols(&symbols)?;
        prog.infer_references();
        let mut output = args.get_output_write("asm")?;
        output.write(format!("{}", prog).as_bytes())?;
    } else {
        let source = args.get_assembly_source()?;
        let prog = assemble_program(&source)?;
        let mut output = args.get_output_write("obj")?;
        let mut symbols = args.get_symbol_write()?;
        prog.write(&mut output)?;
        prog.dump_symbols(&mut symbols)?;
    }

    Ok(())
}
