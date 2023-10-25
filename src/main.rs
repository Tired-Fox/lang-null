extern crate null;

use null::{parser::Parser, lexer::Lexer};
use nasm_to_string::nasm;

fn main() {
    let source = r#"
exit(0);
"#;

        println!("{}\n", source);

        let mut lexer = Lexer::new("assets/null/main.nl");
        lexer.run();

        println!("Token count: {}", lexer.tokens.len());

        let mut parser = Parser::new(lexer);
        parser.parse();

    // let exit_code = 69;
    // println!(
    //     "{}",
    //     nasm![
    //                global  start
    //                section .text
    //         start:
    //                mov     ecx, {exit_code} ;Some Comment;
    //                call    ExitProcess
    //     ]
    // )
}
