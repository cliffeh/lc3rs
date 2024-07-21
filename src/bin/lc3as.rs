use clap::Parser;
use lc3::asm::assemble_program;
use std::path::PathBuf;
use std::{error, io};
use std::{fs, path};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// disassemble object code
    #[arg(short = 'D', long, default_value_t = false)]
    dissasemble: bool,

    /// write output to FILE; defaults to the input filename with a .obj
    /// extension, or a .asm extension if -D is specified
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// file to write the symbol table to (or read it from, if -D is specified);
    /// by default no symbol table will be read/written
    #[arg(short, long, value_name = "FILE")]
    symbols: Option<PathBuf>,

    /// read input from FILE
    #[arg(value_name = "FILE", required = true)]
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

    //     fn get_input_output_symbols_filenames(&self) -> (PathBuf, PathBuf, PathBuf) {
    //         let infile = self.input.clone();
    //         let outfile = match &self.output {
    //             None => {
    //                 let mut path = self.input.clone();
    //                 path.set_extension(if self.dissasemble { "asm" } else { "obj" });
    //                 path
    //             }
    //             Some(path) => path.clone(),
    //         };
    //         let symfile = match &self.symbols {
    //             None => {
    //                 let mut path = self.input.clone();
    //                 path.set_extension("sym");
    //                 path
    //             }
    //             Some(path) => path.clone(),
    //         };

    //         (infile, outfile, symfile)
    //     }

    //     fn get_assembly_input_output_symbols(&self) -> (impl Read, impl Write, impl Write) {
    //         let filenames = self.get_input_output_symbols_filenames();
    //         let input = fs::File::open(&filenames.0)
    //             .expect(format!("unable to open input file: {}", filenames.0.display()).as_str());

    //         let output = if filenames.1 == PathBuf::from("-") {
    //             Box::new(stdout().lock()) as Box<dyn Write>
    //         } else {
    //             Box::new(fs::File::create(&filenames.1).expect(
    //                 format!("unable to create output file: {}", filenames.1.display()).as_str(),
    //             )) as Box<dyn Write>
    //         };

    //         let symbols = if filenames.2 == PathBuf::from("-") {
    //             Box::new(stdout().lock()) as Box<dyn Write>
    //         } else {
    //             Box::new(fs::File::create(&filenames.2).expect(
    //                 format!("unable to create symbol file: {}", filenames.2.display()).as_str(),
    //             )) as Box<dyn Write>
    //         };

    //         (input, output, symbols)
    //     }

    //     fn get_disassembly_input_output_symbols(&self) -> (impl Read, impl Write, impl Read) {
    //         let filenames = self.get_input_output_symbols_filenames();
    //         let input = fs::File::open(&filenames.0)
    //             .expect(format!("unable to open input file: {}", filenames.0.display()).as_str());

    //         let output = if filenames.1 == PathBuf::from("-") {
    //             Box::new(stdout().lock()) as Box<dyn Write>
    //         } else {
    //             Box::new(fs::File::create(&filenames.1).expect(
    //                 format!("unable to create output file: {}", filenames.1.display()).as_str(),
    //             )) as Box<dyn Write>
    //         };

    //         let symbols =
    //             if filenames.2 == PathBuf::from("-") {
    //                 Box::new(stdin().lock()) as Box<dyn Read>
    //             } else {
    //                 Box::new(fs::File::open(&filenames.2).expect(
    //                     format!("unable to open symbol file: {}", filenames.2.display()).as_str(),
    //                 )) as Box<dyn Read>
    //             };

    //         (input, output, symbols)
    //     }
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let args = Args::parse();

    if args.dissasemble {
        todo!()
    } else {
        let source = args.get_assembly_source()?;
        let prog = assemble_program(&source)?;
        let mut output = args.get_output_write("obj")?;
        prog.write(&mut output)?;
    }

    Ok(())
}
