extern crate null;

use null::compiler::Compiler;
use null::lexer;
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
main(12)
exit(12); f256
"#;

fn parse() {
    let mut parser = Parser::source(SOURCE);
    parser.parse();
}

fn lex() {
    let mut lexer = Lexer::source(SOURCE);
    lexer.lex();

    for error in lexer.errors() {
        eprintln!("{}", error);
    }
}

fn main() {
    lex();
    // parse();
    // compile();
}
