extern crate null;
extern crate null_macros;

use null::lexer::Lexer;
use null::parser::token;
use null_macros::asm;

fn main() {
    //     let source = r#"
    // let x: i32 = 0;
    // fn main() {
    //     print("Hello, world!");
    // }
    // "#;
    //
    //     println!("{}\n", source);
    //
    //     let mut lexer = Lexer::new("main.nl");
    //     lexer.run();
    //
    //     println!("Token count: {}", lexer.tokens.len());
    //     for info in lexer.token_info {
    //         println!("{:?}", info);
    //     }
    //     println!("\nIdents: {:?}", lexer.identifiers);
    //     println!("Numbrs: {:?}", lexer.literal_numbers);
    //     println!("Strings: {:?}", lexer.literal_strings);
    //     println!("Errors: {:?}", lexer.errors);
    //
    let exit_code = 69;
    println!(
        "{}",
        asm![
                    global  start
                    extern  ExitProcess
                    incbin  "../bin/libs.o"
                    section .text

            start:
                    mov     ecx, {exit_code} ;Some Comment;
                    call    ExitProcess
        ]
    )
}
