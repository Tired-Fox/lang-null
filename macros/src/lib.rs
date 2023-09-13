extern crate proc_macro;
use nasm::NASM;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

mod nasm;

use lazy_static::lazy_static;
use syn::parse_macro_input;
lazy_static! {
    static ref RE: regex::Regex = regex::Regex::new(r#"\{([^\{\}]+)\}"#).unwrap();
}

#[proc_macro_error::proc_macro_error]
#[proc_macro]
pub fn asm(input: TokenStream) -> TokenStream {
    let mut injections = Vec::new();
    let mut haystack = input.to_string();

    for injection in RE.find_iter(&haystack) {
        injections.push((&haystack[injection.start() + 1..injection.end() - 1]).to_string())
    }

    haystack = RE.replace_all(&haystack, "{}").to_string();

    let mut result = TokenStream2::new();
    if injections.len() > 0 {
        result = format!(", {}", injections.join(", "))
            .parse::<TokenStream>()
            .unwrap()
            .into();
    }

    let _nasm = parse_macro_input!(input as NASM);

    quote!(format!(#haystack #result)).into()
}