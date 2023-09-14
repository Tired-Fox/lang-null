extern crate proc_macro;

mod lang;

use lang::{lex::Token, NASM};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_error::proc_macro_error]
#[proc_macro]
pub fn nasm(input: TokenStream) -> TokenStream {
    let _nasm = parse_macro_input!(input as NASM);

    let injections: TokenStream2 = _nasm
        .tokens
        .iter()
        .filter_map(|t| match t {
            Token::Injection(_, v) => Some(v.clone()),
            _ => None,
        })
        .collect::<Vec<String>>()
        .join(", ")
        .parse::<TokenStream>()
        .unwrap()
        .into();
    println!("{}", injections);

    let fmt: TokenStream2 = format!("\"{}\"", _nasm)
        .parse::<TokenStream>()
        .unwrap()
        .into();
    println!("{}", fmt);

    quote!(format!(#fmt, #injections)).into()
}
