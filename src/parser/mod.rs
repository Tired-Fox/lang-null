use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Peekable;

use crate::{abort, err, first_positive, lexer, Location, Span};
use crate::error::{Error, Errors};
use crate::lexer::Lexer;
use crate::parser::token::{Argument, Block, Parameter, Parse, Token};

pub mod token;
mod built_in;

// pub tokens: Vec<Token>,
// pub token_info: Vec<TokenInfo>,
// pub literal_numbers: Vec<String>,
// pub literal_strings: Vec<String>,
// pub errors: Vec<ErrorInfo>,
// pub identifiers: Vec<String>,

pub struct Parser {
    lexer: Lexer,
    pub errors: Vec<Error>,
    pub tokens: Vec<Token>,
}

impl Parser {

    pub fn source(&self) -> &[char] {
        self.lexer.source()
    }

    pub fn file(&self) -> &Option<String> {
        &self.lexer.file
    }

    pub fn with_path<P: AsRef<std::path::Path>>(path: P) -> Self {
        Parser::new(Lexer::with_path(path))
    }

    pub fn with_source(source: &str) -> Self {
        Parser::new(Lexer::with_source(source))
    }

    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn abort(&mut self, error: Error) {
        self.errors.push(error);
        Errors(&self.errors).render(self.lexer.source());
        std::process::exit(1);
    }

    fn scope<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, tokens: &mut Peekable<T>) -> (Vec<Token>, Location) {
        // Parse all types like in the `parse()` method but exit on `}`
        let mut closed = false;
        let mut end = Location::default();
        let mut result = Vec::new();

        // Remove leading `{`
        let _ = tokens.next();

        while let Some(token) = tokens.next() {
            let info = &self.lexer.token_info[token.0];
            match info.kind {
                lexer::TokenKind::Identifier => {
                    let (token, location) = self.ident_action(token, tokens);
                    result.push(token);
                    end = location;
                }
                lexer::TokenKind::EOF => {
                    break;
                }
                lexer::TokenKind::Keyword(_) => {
                    let (token, location) = self.keyword_action(token, tokens);
                    result.push(token);
                    end = location;
                }
                lexer::TokenKind::Symbol(symbol) => {
                    match symbol {
                        lexer::TokenSymbol::CloseBrace => {
                            closed = true;
                            end = self.lexer.get_loc(token);
                            break;
                        }
                        lexer::TokenSymbol::OpenBrace => {
                            let (block, location) = self.block(tokens);
                            result.push(Token::Block(block));
                            end = location;
                        }
                        val => {
                            abort!(path=self.lexer.file.clone(),  span=self.lexer.curr_loc(), format!("Unknown syntax {:?}", val))
                        }
                    }
                }
                val => {
                    abort!(path=self.lexer.file.clone(),  span=self.lexer.curr_loc(), format!("Unknown syntax {:?}", val))
                }
            }
        }

        if !closed {
            self.abort(err!("Unclosed scope", help=["Try adding `}`"]));
        }
        (result, end)
    }

    fn block<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, tokens: &mut Peekable<T>) -> (Block, Location) {
        let mut inner = Vec::new();
        let mut start = Location::default();
        let mut end = start;
        match tokens.peek() {
            Some(next) => {
                println!("{:?}", self.lexer.get_kind(next));
                start = self.lexer.get_loc(next);
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::OpenBrace) = self.lexer.get_kind(next) {
                    let (result, location) = self.scope(tokens);
                    end = location;
                    inner = result;
                } else {
                    abort!(
                        path=self.lexer.file.clone(),
                        span=start,
                        "Expected function block",
                        help=["Try adding `{}`"]
                    )
                }
            }
            None => {
                let location = self.lexer.curr_loc();
                abort!(
                    path=self.lexer.file.clone(),
                    span=location,
                    "Expected `{`"
                )
            }
        };

        let location = Location::new(start.line, start.column, Span::new(start.span.start(), end.span.end()));
        (
            Block(inner, location),
            location
        )
    }

    fn group<'a, V: Parse + Debug, const N: char, T: Iterator<Item=&'a lexer::Token>>(&self, tokens: &mut Peekable<T>) -> (token::Punctuated<V, N>, usize) {
        match tokens.next() {
            Some(open) => {
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::OpenParen) = self.lexer.get_kind(open) {
                    let args = token::Punctuated::<V, N>::parse(tokens, &self.lexer).unwrap();
                    match tokens.next() {
                        Some(close) => {
                            if let lexer::TokenKind::Symbol(lexer::TokenSymbol::CloseParen) = self.lexer.get_kind(close) {
                                (args, self.lexer.get_loc(close).span.end())
                            } else {
                                panic!("Invalid syntax; expected `)` was {:?} | {:?}", self.lexer.get_kind(close), self.lexer.get_ident(close))
                            }
                        }
                        None => panic!("Invalid syntax; expected `)` was {:?}", self.lexer.get_kind(open))
                    }
                } else {
                    panic!("Invalid syntax; expected `(` was {:?}", self.lexer.get_kind(open))
                }
            }
            _ => panic!("Invalid syntax; expected `(`")
        }
    }

    fn function_call<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, token: &lexer::Token, tokens: &mut Peekable<T>) -> (Token, Location) {
        let ident = self.lexer.get_ident(token).unwrap();

        let (cargs, last) = self.group::<Parameter, ',', T>(tokens);
        let name = self.lexer.get_loc(token);
        let location = Location::new(
            name.line,
            name.column,
            Span::new(name.span.start(), last),
        );

        (
            Token::Call(token::Call::Call {
                name: token::Ident(ident.clone(), name),
                params: cargs,
                location,
            }),
            location
        )
    }

    fn function_decl<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, token: &lexer::Token, tokens: &mut Peekable<T>) -> (Token, Location) {
        match tokens.next() {
            Some(next) => {
                if let lexer::TokenKind::Keyword(lexer::TokenKeyword::Fn) = self.lexer.get_kind(next) {
                    let (args, last) = self.group::<Argument, ',', T>(tokens);
                    let block = match tokens.peek() {
                        Some(next) => {
                            if let lexer::TokenKind::Symbol(lexer::TokenSymbol::Semicolon) = self.lexer.get_kind(next) {
                                let _ = tokens.next();
                                None
                            } else if let lexer::TokenKind::Symbol(lexer::TokenSymbol::OpenBrace) = self.lexer.get_kind(next) {
                                let (block, _location) = self.block(tokens);
                                Some(block)
                            } else {
                                let location = self.lexer.get_loc(next);
                                abort!(path=self.lexer.file.clone(), span=location, "Expected `;` or `{`")
                            }
                        }
                        None => None
                    };
                    let name_loc = self.lexer.get_loc(token);
                    let location = Location::new(
                        name_loc.line,
                        name_loc.column,
                        Span::new(name_loc.span.start(), first_positive(&[
                            name_loc.span.end(),
                            last
                        ])),
                    );
                    (
                        Token::Decleration(token::Declaration::Function {
                            name: token::Ident(self.lexer.get_ident(token).unwrap().clone(), name_loc),
                            args: args,
                            body: block,
                            location,
                        }),
                        location
                    )
                } else {
                    abort!(path=self.lexer.file.clone(), span=self.lexer.get_loc(token), "Expected `fn` keyword")
                }
            }
            None => {
                abort!(path=self.lexer.file.clone(), span=self.lexer.get_loc(token), "Expected function declaration", help=["Try adding `fn`"])
            }
        }
    }

    fn ident_action<'a, T: Iterator<Item=&'a lexer::Token>>(
        &mut self,
        token: &lexer::Token,
        tokens: &mut Peekable<T>,
    ) -> (Token, Location) {
        let name = self.lexer.get_ident(token).unwrap();

        match tokens.peek() {
            Some(next) => {
                let info = &self.lexer.token_info[next.0];

                match &info.kind {
                    lexer::TokenKind::Symbol(symbol) => {
                        use lexer::TokenSymbol;

                        match symbol {
                            TokenSymbol::ColonColon => {
                                let _ = tokens.next();
                                match tokens.peek() {
                                    Some(next) => {
                                        match self.lexer.get_kind(next) {
                                            lexer::TokenKind::Keyword(lexer::TokenKeyword::Fn) => {
                                                return self.function_decl(token, tokens);
                                            }
                                            _ => {
                                                println!("Const assignment");
                                            }
                                        }
                                    }
                                    None => {
                                        abort!(path=self.lexer.file.clone(), span=self.lexer.get_loc(token), "Expected function declaration or const assignment");
                                    }
                                }
                            }
                            TokenSymbol::Colon => {
                                // Variable decl, expect type, `=`, then value
                                println!("Declare variable");
                            }
                            TokenSymbol::ColonEqual => {
                                // Variable assignment
                                println!("Assign to variable");
                            }
                            TokenSymbol::Dot => {
                                // Member access
                                println!("Member access");
                            }
                            TokenSymbol::OpenParen => {
                                return self.function_call(token, tokens);
                            }
                            _ => {
                                // No operation this value is either are a return value or does nothing.
                                // If does nothing don't add to parse tree
                                println!("No-Op");
                            }
                        }
                    }
                    _ => {
                        abort!(path=self.lexer.file.clone(), span=self.lexer.get_loc(next), "Expected ::, :, or = symbol following identifier")
                    }
                };
            }
            None => {
                abort!(path=self.lexer.file.clone(), span=self.lexer.curr_loc(), "Expected declaration or assignment");
            }
        }
        (Token::None, Location::default())
    }

    fn keyword_action<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, _token: &lexer::Token, _tokens: &mut Peekable<T>) -> (Token, Location) {
        (Token::None, Location::default())
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
                    let (token, _location) = self.ident_action(token, &mut tokens);
                    self.tokens.push(token);
                }
                lexer::TokenKind::EOF => {
                    break;
                }
                lexer::TokenKind::Keyword(_) => {
                    let (token, _location) = self.keyword_action(token, &mut tokens);
                    self.tokens.push(token);
                }
                val => {
                    abort!(path=self.lexer.file.clone(),  span=self.lexer.curr_loc(), format!("Unknown syntax {:?}", val))
                }
            }
        }

        println!("{:?}", self.tokens);
    }
}

