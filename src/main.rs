use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub asm);

fn main() {
    let parser = asm::InstParser::new();

    let op = parser.parse("BRnzp").unwrap();

    println!("got: {}", op as u16);
}
