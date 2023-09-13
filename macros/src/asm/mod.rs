use std::fmt::Display;

use proc_macro_error::abort;
use quote::quote;
use syn::{braced, parse::Parse, token::Brace, Expr, Ident, Token};

use crate::asm::token::Values;

use self::token::Instruction;

pub mod token;

pub enum NameOrInjection {
    Value(String),
    Injection,
}

pub enum AsmToken {
    Global(NameOrInjection),
    Section(NameOrInjection),
    Instruction(Instruction),
    Injection,
}

impl Display for AsmToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Instruction(inst) => write!(f, "{}\n", inst),
            Self::Injection => write!(f, "{{}}"),
            Self::Global(s) => write!(
                f,
                "global {}",
                match s {
                    NameOrInjection::Value(v) => v.clone(),
                    NameOrInjection::Injection => "{}".to_string(),
                }
            ),
            Self::Section(s) => write!(
                f,
                "section .{}",
                match s {
                    NameOrInjection::Value(v) => v.clone(),
                    NameOrInjection::Injection => "{}".to_string(),
                }
            ),
            _ => write!(f, "{}", self),
        }
    }
}

pub struct ASM {
    pub tokens: Vec<AsmToken>,
    pub captures: Vec<String>,
}

impl Display for ASM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in self.tokens.iter() {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

struct AsmParser<'a> {
    pub asm: ASM,
    input: syn::parse::ParseStream<'a>,
}

impl<'a> AsmParser<'a> {
    pub fn ident(&mut self) -> Ident {
        match self.input.parse::<Ident>() {
            Ok(ident) => ident,
            Err(_) => {
                abort!(self.input.span(), "Expected ident");
            }
        }
    }

    pub fn new(input: syn::parse::ParseStream<'a>) -> Self {
        AsmParser {
            asm: ASM {
                tokens: Vec::new(),
                captures: Vec::new(),
            },
            input,
        }
    }

    pub fn parse(mut self) -> syn::Result<ASM> {
        let mut instruction: Option<(Ident, Vec<Values>)> = None;
        while !self.input.is_empty() {
            if self.input.peek(Ident) {
                let ident = self.ident();
                // TODO: Constant variable assignment
                match ident.to_string().as_str() {
                    "global" => {
                        /* TODO: globals */
                        println!("Global")
                    }
                    "section" => {
                        /* TODO: sections */
                        println!("Section")
                    }
                    val if Instruction::is_instruction(&val.to_string()) => {
                        println!("New instruction: {}", val);
                        if let Some(inst) = instruction {
                            self.asm
                                .tokens
                                .push(AsmToken::Instruction(Instruction::parse(inst.0, inst.1)));
                        }
                        instruction = Some((ident, Vec::new()));
                    }
                    _ => {
                        println!("New param: {}", ident);
                        instruction.as_mut().unwrap().1.push(Values::from(ident));
                    }
                }
            } else if self.input.peek(Token![,]) {
                let _ = self.input.parse::<Token![,]>();
            } else if self.input.peek(Brace) {
                let brace;
                braced!(brace in self.input);
                let brace = brace.parse::<Expr>()?;
                self.asm.captures.push(quote!(#brace).to_string());
                if instruction.is_none() {
                    self.asm.tokens.push(AsmToken::Injection);
                } else {
                    instruction.as_mut().unwrap().1.push(Values::Injection);
                }
            } else {
                println!("Unkown");
                if instruction.is_none() {
                    abort!(self.input.span(), "Must first provide an instruction");
                } else {
                    instruction
                        .as_mut()
                        .unwrap()
                        .1
                        .push(Values::parse(&mut self.input)?);
                }
            }
        }

        if instruction.is_some() {
            let instruction = instruction.take().unwrap();
            self.asm
                .tokens
                .push(AsmToken::Instruction(Instruction::parse(
                    instruction.0,
                    instruction.1,
                )))
        }
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
