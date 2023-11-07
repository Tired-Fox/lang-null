extern crate null;

use null::compiler::{Compiler};
use null::error::Errors;

use null::lexer::Lexer;
use null::parser::Parser;

fn compile() {
    Compiler::init();
    Compiler::builder()
        .nasm("toolchain/nasm")
        .go_link("toolchain/GoLink")
        .path("assets/null/main.nl");
}

const SOURCE: &'static str = r#"
main :: fn(exit_code: i32) {
    exit(exit_code)
}
"#;

fn parse() {
    let mut parser = Parser::with_source(SOURCE);
    parser.parse();
}

fn lex() {
    // let mut lexer = Lexer::source(SOURCE);
    let mut lexer = Lexer::with_source(SOURCE);
    lexer.lex();

    for token in lexer.tokens.iter() {
        println!("{:?}", lexer.get_info(token));
    }
    Errors(lexer.errors()).render(lexer.source());
}

fn main() {
    // lex();
    // parse();
    compile();
}
