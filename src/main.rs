extern crate null;

use null::lexer::Lexer;

fn main() {
    let source = r#"
let x: i32 = 0;
fn main() {
    print("Hello, world!");
}
"#;

    println!("{}\n", source);

    let mut lexer = Lexer::new("main.nl");
    lexer.run();

    for info in lexer.token_info {
        println!("{:?}", info);
    }
    println!("{:?}", lexer.identifiers);
    println!("{:?}", lexer.literal_numbers);
    println!("{:?}", lexer.literal_strings);
}
