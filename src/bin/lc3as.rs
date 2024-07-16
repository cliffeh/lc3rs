use clap::Parser;
use lc3::{Program};
use std::io::{stdin, stdout, Error, Read, Write};
use std::path::PathBuf;
use std::{fs, path};
use lc3::asm::Parser as LC3Parser;

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

// impl Args {
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
// }

// fn assemble_program(
//     input: &mut impl Read,
//     output: &mut impl Write,
//     symbols: &mut impl Write,
// ) -> Result<(), Error> {
//     let prog = assemble(input)?;

//     // TODO do something with results?
//     let _ = prog.write(output);
//     let _ = prog.dump_symbols(symbols);

//     Ok(())
// }

// fn disassemble_program(
//     input: &mut impl Read,
//     output: &mut impl Write,
//     symbols: &mut impl Read, // TODO use me!
// ) -> Result<(), Error> {
//     let mut prog = Program::read(input)?;
//     let _ = prog.load_symbols(symbols);

//     write!(output, "{}", prog)?;

//     Ok(())
// }

fn main() -> Result<(), Error> {
    let args = Args::parse();

    let input = fs::read_to_string(args.input)?;

    eprintln!("input: {}", input);

    let prog = LC3Parser::new(&input).parse_program().unwrap();

    print!("{}", prog);

    Ok(())
    // if args.debug {
    //     let (infile, outfile, symfile) = args.get_input_output_symbols_filenames();
    //     eprintln!("using input file: {}", infile.display());
    //     eprintln!("using output file: {}", outfile.display());
    //     eprintln!("using symbol file: {}", symfile.display());
    // }

    // if args.dissasemble {
    //     let (mut input, mut output, mut symbols) = args.get_disassembly_input_output_symbols();
    //     disassemble_program(&mut input, &mut output, &mut symbols)
    // } else {
    //     let (mut input, mut output, mut symbols) = args.get_assembly_input_output_symbols();
    //     assemble_program(&mut input, &mut output, &mut symbols)
    // }
}
