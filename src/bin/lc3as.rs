use clap::Parser;
use lc3::assemble;
use std::io::{stdin, stdout, Error, Read, Write};
use std::path::PathBuf;
use std::{fs, path};

#[derive(Parser)]
#[command(author, version, about = "an LC3 (dis-)assembler", long_about = None)]
struct Args {
    /// turn on debug logging
    #[arg(short, long)]
    debug: bool,

    /// output program to FILE; defaults to the input filename with a .obj extension,
    /// or "prog.obj" if reading from stdin
    #[arg(short, long, value_name = "FILE")]
    output: Option<path::PathBuf>,

    /// output symbols to FILE; defaults to the input filename with a .sym extension,
    /// or "prog.sym" if reading from stdin
    #[arg(short, long, value_name = "FILE")]
    symbols: Option<path::PathBuf>,

    /// read input from FILE, or stdin if FILE is not specified
    #[arg(value_name = "FILE")]
    input: Option<path::PathBuf>,
}

impl Args {
    fn get_input_output_symbols_filenames(&self) -> (PathBuf, PathBuf, PathBuf) {
        match &self.input {
            // we're reading from stdin
            None => {
                let outfile = match &self.output {
                    None => PathBuf::from("prog.obj"),
                    Some(path) => path.clone(),
                };
                let symfile = match &self.symbols {
                    None => PathBuf::from("prog.sym"),
                    Some(path) => path.clone(),
                };
                return (PathBuf::from("-"), outfile, symfile);
            }
            // we have an input file to read from
            Some(infile) => {
                let outfile = match &self.output {
                    None => {
                        let mut outfile = infile.clone();
                        outfile.set_extension("obj");
                        outfile
                    }
                    Some(path) => path.clone(),
                };
                let symfile = match &self.symbols {
                    None => {
                        let mut symfile = infile.clone();
                        symfile.set_extension("sym");
                        symfile
                    }
                    Some(path) => path.clone(),
                };
                return (infile.clone(), outfile, symfile);
            }
        }
    }

    fn get_input_output_symbols(&self) -> (impl Read, impl Write, impl Write) {
        let filenames = self.get_input_output_symbols_filenames();
        let input =
            if filenames.0 == PathBuf::from("-") {
                Box::new(stdin().lock()) as Box<dyn Read>
            } else {
                Box::new(fs::File::open(&filenames.0).expect(
                    format!("unable to open input file: {}", filenames.0.display()).as_str(),
                )) as Box<dyn Read>
            };

        let output = if filenames.1 == PathBuf::from("-") {
            Box::new(stdout().lock()) as Box<dyn Write>
        } else {
            Box::new(fs::File::create(&filenames.1).expect(
                format!("unable to create output file: {}", filenames.1.display()).as_str(),
            )) as Box<dyn Write>
        };

        let symbols = if filenames.2 == PathBuf::from("-") {
            Box::new(stdout().lock()) as Box<dyn Write>
        } else {
            Box::new(fs::File::create(&filenames.2).expect(
                format!("unable to create symbol file: {}", filenames.2.display()).as_str(),
            )) as Box<dyn Write>
        };

        (input, output, symbols)
    }
}

fn assemble_program(
    input: &mut impl Read,
    output: &mut impl Write,
    symbols: &mut impl Write,
) -> Result<(), Error> {
    let prog = assemble(input)?;

    // TODO do something with results?
    let _ = prog.write(output);
    let _ = prog.dump_symbols(symbols);

    Ok(())
}

fn main() -> Result<(), Error> {
    let args = Args::parse();
    if args.debug {
        let (infile, outfile, symfile) = args.get_input_output_symbols_filenames();
        eprintln!("using input file: {}", infile.display());
        eprintln!("using output file: {}", outfile.display());
        eprintln!("using symbol file: {}", symfile.display());
    }

    let (mut input, mut output, mut symbols) = args.get_input_output_symbols();

    assemble_program(&mut input, &mut output, &mut symbols)
}
