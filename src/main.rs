extern crate null;

use null::compiler::Compiler;

fn main() {
    Compiler::init();
    Compiler::builder()
        .nasm("toolchain/nasm")
        .go_link("toolchain/GoLink")
        .path("assets/null/main.nl");
}
