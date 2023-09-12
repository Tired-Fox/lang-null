extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

mod asm;

#[proc_macro_error::proc_macro_error]
#[proc_macro]
pub fn asm(input: TokenStream) -> TokenStream {
    let asm = parse_macro_input!(input as asm::ASM);
    return quote!(format!("")).into();
}
