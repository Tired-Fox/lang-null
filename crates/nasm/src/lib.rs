extern crate proc_macro;

mod lang;

use lang::{lex::Token, NASM};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, TokenStreamExt};
use syn::parse_macro_input;

#[proc_macro_error::proc_macro_error]
#[proc_macro]
pub fn nasm(input: TokenStream) -> TokenStream {
    let _nasm = parse_macro_input!(input as NASM);

    let mut injections: TokenStream2 = TokenStream2::new();
    for token in _nasm.tokens.iter() {
        match token {
            Token::Injection(_, v) => {
                injections.append_all(quote!{#v,});
            }
            _ => {}
        }
    }

    let fmt: TokenStream2 = format!("\"{}\"", _nasm)
        .parse::<TokenStream>()
        .unwrap()
        .into();

    quote!(format!(#fmt, #injections)).into()
}
