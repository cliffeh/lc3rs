use lc3::Token;
use logos::Logos;
use std::io::{stdin, BufRead, Error};

fn main() -> Result<(), Error> {
    // let mut buf = String::new();
    // stdin().lock().read_to_string(&mut buf)?;

    for result in stdin().lock().lines() {
        let line = result.unwrap();
        let lex = Token::lexer(&line);

        for token in lex {
            println!("{:?}", token);
        }
    }

    Ok(())
}
