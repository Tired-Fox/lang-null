use proc_macro2::Span;
use proc_macro_error::emit_error;

pub mod instruction;
pub mod lex;
pub mod parse;

#[derive(Debug)]
pub struct Expr(pub Span, pub Vec<lex::Token>);

pub struct NASM {
    pub tokens: Vec<lex::Token>,
}

impl syn::parse::Parse for NASM {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut nasm = NASM { tokens: Vec::new() };
        while !input.is_empty() {
            nasm.tokens.push(match lex::Token::parse(input) {
                Ok(t) => {
                    println!("{:?}", t);
                    t
                }
                Err(e) => {
                    emit_error!(e.span(), e.to_string());
                    lex::Token::Unkown
                }
            })
        }
        Ok(nasm)
    }
}
