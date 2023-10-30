use std::collections::HashMap;
use std::iter::Peekable;

use crate::compiler::syscall;
use crate::lexer;
use crate::lexer::Lexer;
use crate::parser::token::{Argument, Parameter, Parse, Punctuated, Token};

pub mod token;
mod built_in;

// pub tokens: Vec<Token>,
// pub token_info: Vec<TokenInfo>,
// pub literal_numbers: Vec<String>,
// pub literal_strings: Vec<String>,
// pub errors: Vec<ErrorInfo>,
// pub identifiers: Vec<String>,

// TODO: Maintain scope
//      - Scope has methods
//      - Scope has variables
//      - Scope has functions
//      - Scope stack with all of methods, variables, and functions referencing what scope they come from

#[derive(Clone)]
pub struct Scope {
    variables: HashMap<String, usize>,
    functions: HashMap<String, usize>,
}

impl Scope {
    pub fn function(&mut self, name: String, function: usize) {
        self.functions.insert(name, function);
    }

    pub fn variable(&mut self, name: String, variable: usize) {
        self.variables.insert(name, variable);
    }

    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    // pub fn inherit(&self, variables: &mut HashMap<String, &Decleration>, functions: &mut HashMap<String, &Decleration>) {
    //     variables.extend(self.variables.clone());
    //     functions.extend(self.functions.clone());
    // }
}

pub struct Parser {
    lexer: Lexer,
    pub tokens: Vec<Token>,
    pub scopes: Vec<Scope>,
}

impl Parser {
    pub fn path<P: AsRef<std::path::Path>>(path: P) -> Self {
        Parser::new(Lexer::path(path))
    }

    pub fn source(source: &str) -> Self {
        Parser::new(Lexer::source(source))
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            tokens: Vec::new(),
            scopes: vec![
                // Global Scope
                Scope::new()
            ],
        }
    }

    pub fn group<'a, V: Parse, const N: char, T: Iterator<Item=&'a lexer::Token>>(&self, tokens: &mut Peekable<T>) -> token::Punctuated<V, N> {
        match tokens.next() {
            Some(next) => {
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::OpenParen) = self.lexer.get_kind(next) {
                    let args = token::Punctuated::<V, N>::parse(tokens, &self.lexer).unwrap();
                    match tokens.next() {
                        Some(next) => {
                            if let lexer::TokenKind::Symbol(lexer::TokenSymbol::CloseParen) = self.lexer.get_kind(next) {
                                return args;
                            } else {
                                panic!("Invalid syntax; expected `)` was {:?}", self.lexer.get_kind(next))
                            }
                        },
                        None => panic!("Invalid syntax; expected `)` was {:?}", self.lexer.get_kind(next))
                    }
                } else {
                    panic!("Invalid syntax; expected `(` was {:?}", self.lexer.get_kind(next))
                }
            },
            _ => panic!("Invalid syntax; expected `(`")
        }
    }

    pub fn function_call<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, token: &lexer::Token, tokens: &mut Peekable<T>) {
        let ident = self.lexer.get_ident(token);

        if let Some(ident) = ident {
            if let Some(scope) = self.scopes.last() {
                if let Some(_) = scope.functions.get(ident) {
                    println!("Call function {}()", ident);
                    let args = self.group::<Parameter, ',', T>(tokens);
                    self.tokens.push(Token::Call(token::Call::Call(
                        token::Ident(ident.clone()),
                        args
                    )));
                    return;
                } else if syscall::SYSCALLS.contains(&ident.as_str()) {
                    println!("Syscall {}()", ident);
                    let args = self.group::<Parameter, ',', T>(tokens);
                    self.tokens.push(Token::Call(token::Call::Syscall(
                        token::Ident(ident.clone()),
                        args
                    )));
                    return;
                }
            }
        }
        panic!("Function call not in scope")
    }

    pub fn ident_action<'a, T: Iterator<Item=&'a lexer::Token>>(
        &mut self,
        token: &lexer::Token,
        tokens: &mut Peekable<T>,
    ) {
        // let name = self.lexer.get_ident(token).unwrap();

        match tokens.peek() {
            Some(next) => {
                let info = &self.lexer.token_info[next.0];

                let need_semi = match info.kind {
                    lexer::TokenKind::Symbol(symbol) => {
                        use lexer::TokenSymbol;

                        match symbol {
                            TokenSymbol::ColonColon => {
                                // Function decleration
                                println!("Declare function");
                                true
                            }
                            TokenSymbol::Colon => {
                                // Variable decleration
                                println!("Declare variable");
                                true
                            }
                            TokenSymbol::Equal => {
                                // Variable assignment
                                println!("Assign to variable");
                                true
                            }
                            TokenSymbol::Dot => {
                                // Member access
                                println!("Member access");
                                false
                            }
                            TokenSymbol::OpenParen => {
                                // Function call... parse arguments
                                println!("Call function");
                                self.function_call(token, tokens);
                                false
                            }
                            _ => {
                                println!("No-Op");
                                false
                            }
                        }
                    }
                    _ => panic!("Expected ::, :, or = symbol following identifier"),
                };

                match tokens.peek() {
                    Some(next) => {
                        if lexer::TokenKind::Symbol(lexer::TokenSymbol::Semicolon) == self.lexer.get_kind(next) {
                            // TODO: Return continue scope
                            let _ = tokens.next();
                        } else {
                            // TODO: Return exit scope
                            if need_semi {
                                panic!("Expected semi-colon")
                            }
                        }
                    }
                    None => {
                        /* TODO: Return exit scope */
                        if need_semi {
                            panic!("Expected semi-colon")
                        }
                    }
                };
            }
            None => {
                panic!("Expected declaration or assignment");
            }
        }
    }

    pub fn parse(&mut self) {
        // First lex the source
        self.lexer.lex();

        let tokens = self.lexer.tokens.clone();
        let mut tokens = tokens.iter().peekable();

        while let Some(token) = tokens.next() {
            let info = &self.lexer.token_info[token.0];
            match info.kind {
                lexer::TokenKind::Identifier => {
                    println!(
                        "Identifier: {}",
                        self.lexer.identifiers[info.payload as usize]
                    );
                    self.ident_action(&token, &mut tokens);
                }
                lexer::TokenKind::EOF => {
                    println!("EOF");
                    break;
                }
                val => panic!("Unkown syntax {:?}", val),
            }
        }
    }
}

