extern crate null;

use nasm_to_string::nasm;
use null::compiler::{Compiler, syscall};
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
}
main(12);
exit(12);
"#;

fn parse() {
    let mut parser = Parser::with_source(SOURCE);
    parser.parse();
}

fn lex() {
    // let mut lexer = Lexer::source(SOURCE);
    let mut lexer = Lexer::with_path("assets/null/main.nl");
    lexer.lex();

    Errors(lexer.errors()).render(lexer.source());
}

fn main() {
    // lex();
    // parse();
    compile();
}
