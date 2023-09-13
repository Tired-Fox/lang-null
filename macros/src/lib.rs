extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use regex::Captures;
use syn::parse_macro_input;

mod asm;

#[proc_macro_error::proc_macro_error]
#[proc_macro]
pub fn asm(input: TokenStream) -> TokenStream {
    // let REG = regex::Regex::new(r#"\{([^\{\}]+)\}"#).expect("Valid inj regex");
    //
    // let mut injections = Vec::new();
    // let mut haystack = input.to_string();
    // println!("{:?}", haystack);
    //
    // for injection in REG.find_iter(&haystack) {
    //     injections.push((&haystack[injection.start() + 1..injection.end() - 1]).to_string())
    // }
    //
    // haystack = REG.replace_all(&haystack, "{}").to_string();
    //
    // println!("{:?} {:?}", haystack, injections);
    // let mut result = TokenStream2::new();
    // if injections.len() > 0 {
    //     result = format!(", {}", injections.join(", "))
    //         .parse::<TokenStream>()
    //         .unwrap()
    //         .into();
    // }
    //
    // println!("{:?}", result.to_string());
    let asm = parse_macro_input!(input as asm::ASM);
    let fmt = asm.to_string();
    let mut cpt: TokenStream2 = TokenStream2::new();
    if asm.captures.len() > 0 {
        cpt = format!(", {}", asm.captures.join(", "))
            .parse::<TokenStream>()
            .unwrap()
            .into();
    }

    println!("{:?} {}", fmt, cpt);
    quote!(format!(#fmt #cpt)).into()
}
