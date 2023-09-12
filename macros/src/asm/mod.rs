use proc_macro_error::abort;
use syn::{parse::Parse, Ident};

pub mod token;

pub enum AsmToken {
    Normal(String),
    Inject(String),
}

pub struct ASM {
    pub tokens: Vec<AsmToken>,
}

struct AsmParser<'a> {
    pub asm: ASM,
    input: syn::parse::ParseStream<'a>,
}

impl<'a> AsmParser<'a> {
    pub fn ident(&mut self) -> syn::Result<Ident> {
        match self.input.parse::<Ident>() {
            Ok(ident) => Ok(ident),
            Err(_) => {
                abort!(self.input.span(), "Expected ident");
            }
        }
    }

    pub fn new(input: syn::parse::ParseStream<'a>) -> Self {
        AsmParser {
            asm: ASM { tokens: Vec::new() },
            input,
        }
    }

    pub fn parse(mut self) -> syn::Result<ASM> {
        let next: Ident = self.ident()?;
        println!("{}", next);
        Ok(self.asm)
    }
}

/*
 Syntax:
   command (sub command options)
*/

impl Parse for ASM {
    fn parse<'a>(input: syn::parse::ParseStream<'a>) -> syn::Result<Self> {
        let parser = AsmParser::new(input);
        parser.parse()
    }
}
