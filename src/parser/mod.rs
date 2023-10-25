pub mod token;
use crate::lexer::{Lexer, Token, TokenKind, TokenSymbol};
use std::iter::Peekable;

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

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser { lexer }
    }

    pub fn function_call<'a, T: Iterator<Item = &'a Token>>(&mut self, token: &Token, tokens: &mut Peekable<T>) {
        // List of funtion names in scope
    }

    pub fn ident_action<'a, T: Iterator<Item = &'a Token>>(
        &mut self,
        token: &Token,
        tokens: &mut Peekable<T>,
    ) {
        let name = &self.lexer.identifiers[self.lexer.token_info[token.0].payload];

        match tokens.peek() {
            Some(next) => {
                let info = &self.lexer.token_info[next.0];
                match info.kind {
                    TokenKind::Symbol(symbol) => {
                        match symbol {
                            TokenSymbol::ColonColon => {
                                // Function decleration
                                println!("Declare function");
                            }
                            TokenSymbol::Colon => {
                                // Variable decleration
                                println!("Declare variable");
                            }
                            TokenSymbol::Equal => {
                                // Variable assignment
                                println!("Assign to variable");
                            }
                            TokenSymbol::Dot => {
                                // Member access
                                println!("Member access");
                            }
                            TokenSymbol::OpenParen => {
                                // Function call... parse arguments
                                println!("Call function");
                                self.function_call(token, tokens);
                            }
                            _ => println!("No-Op"),
                        }
                    }
                    _ => panic!("Expected ::, :, or = symbol following identifier"),
                }
                panic!("All done for now");
            }
            None => {
                panic!("Expected declaration or assignment");
            }
        }
    }

    pub fn parse(&mut self) {
        let tokens = self.lexer.tokens.clone();
        let mut tokens = tokens.iter().peekable();

        while let Some(token) = tokens.next() {
            let info = &self.lexer.token_info[token.0];
            match info.kind {
                TokenKind::Identifier => {
                    println!(
                        "Identifier: {}",
                        self.lexer.identifiers[info.payload as usize]
                    );
                    self.ident_action(&token, &mut tokens);
                }
                TokenKind::EOF => {
                    println!("EOF");
                    break;
                }
                _ => panic!("Unkown syntax"),
            }
        }
    }
}

