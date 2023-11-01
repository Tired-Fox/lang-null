use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::Peekable;

use crate::{abort, err, first_positive, lexer, Location, Span};
use crate::compiler::syscall;
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

// TODO: Maintain scope
//      - Scope has methods
//      - Scope has variables
//      - Scope has functions
//      - Scope stack with all of methods, variables, and functions referencing what scope they come from

#[derive(Clone, Default)]
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

    pub fn get_function(&self, name: &str) -> Option<usize> {
        self.functions.get(name).cloned()
    }

    pub fn get_variable(&self, name: &str) -> Option<usize> {
        self.functions.get(name).cloned()
    }

    pub fn new() -> Scope {
        Scope::default()
    }

    // pub fn inherit(&self, variables: &mut HashMap<String, &Decleration>, functions: &mut HashMap<String, &Decleration>) {
    //     variables.extend(self.variables.clone());
    //     functions.extend(self.functions.clone());
    // }
}

pub struct Parser {
    lexer: Lexer,
    pub errors: Vec<Error>,
    pub tokens: Vec<Token>,
    pub scopes: Vec<Scope>,
}

impl Parser {
    pub fn global(&self) -> &Scope {
        self.scopes.first().unwrap()
    }

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
            scopes: vec![
                // Global Scope
                Scope::new()
            ],
        }
    }

    fn abort(&mut self, error: Error) {
        self.errors.push(error);
        Errors(&self.errors).render(self.lexer.source());
        std::process::exit(1);
    }

    fn scope<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, tokens: &mut Peekable<T>) -> (Vec<Token>, usize) {
        // Parse all types like in the `parse()` method but exit on `}`
        let mut closed = false;
        let mut end = 0;
        let mut result = Vec::new();
        while let Some(token) = tokens.next().map(|t| (*t).clone()) {
            if let lexer::TokenKind::Symbol(lexer::TokenSymbol::CloseBrace) = self.lexer.get_kind(&token) {
                closed = true;
                break;
            } else if let lexer::TokenKind::Symbol(lexer::TokenSymbol::OpenBrace) = self.lexer.get_kind(&token) {
                result.push(self.block(tokens));
                end = self.lexer.get_loc(&token).span.end();
            }
        }
        if !closed {
            self.abort(err!("Unclosed block", help=["Try adding `}`"]));
        }
        (Vec::new(), end)
    }

    fn block<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, tokens: &mut Peekable<T>) -> Block {
        let mut inner = Vec::new();
        let mut end = 0;
        match tokens.next() {
            Some(next) => {
                let start = self.lexer.get_loc(next);
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::OpenBrace) = self.lexer.get_kind(next) {
                    let (result, last)= self.scope(tokens);
                    end = last;
                    inner = result;
                } else {
                    abort!(
                        path=self.lexer.file.clone(),
                        span=start,
                        "`{` was never closed",
                        help=["Try adding `}`"]
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
        // TODO: Change location to be based on the inner tokens
        Block(inner, Location::new(0, 0, Span::new(0, end)))
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
                                panic!("Invalid syntax; expected `)` was {:?}", self.lexer.get_kind(close))
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

    fn function_call<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, token: &lexer::Token, tokens: &mut Peekable<T>) {
        let ident = self.lexer.get_ident(token);

        if let Some(ident) = ident {
            if let Some(scope) = self.scopes.last() {
                if let Some(index) = scope.functions.get(&ident) {
                    if let Token::Decleration(token::Declaration::Function{args, ..}) = &self.tokens[*index] {
                        let (cargs, last) = self.group::<Parameter, ',', T>(tokens);
                        if args.len() != cargs.len() {
                            abort!(
                                path=self.lexer.file.clone(),
                                span=self.lexer.get_loc(token),
                                format!("Expected {} arguments; found {}", args.len(), cargs.len())
                            );
                        }
                        let name = self.lexer.get_loc(token);
                        let location = Location::new(
                            name.line,
                            name.column,
                            Span::new(name.span.start(), last)
                        );
                        self.tokens.push(Token::Call(token::Call::Call{
                            name: token::Ident(ident.clone(), name),
                            params: cargs,
                            location,
                        }));
                        return;
                    }
                } else if syscall::SYSCALLS.contains(&ident.as_str()) {
                    let (cargs, last) = self.group::<Parameter, ',', T>(tokens);
                    let name = self.lexer.get_loc(token);
                    let location = Location::new(
                        name.line,
                        name.column,
                        Span::new(name.span.start(), last)
                    );
                    self.tokens.push(Token::Call(token::Call::Syscall{
                        name: token::Ident(ident.clone(), name),
                        params: cargs,
                        location,
                    }));
                    return;
                }
            }
        }
        panic!("Function call not in scope")
    }

    fn function_decl<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, token: &lexer::Token, tokens: &mut Peekable<T>) {
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
                                Some(self.block(tokens))
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
                        ]))
                    );
                    self.tokens.push(Token::Decleration(token::Declaration::Function {
                        name: token::Ident( self.lexer.get_ident(token).unwrap().clone(), name_loc),
                        args: args,
                        body: block,
                        location
                    }))
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
    ) {
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
                                                self.scopes.last_mut().unwrap().function(
                                                    name.clone(),
                                                    self.tokens.len(),
                                                );
                                                self.function_decl(token, tokens);
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
                                self.function_call(token, tokens);
                                if self.scopes.len() == 1 {
                                    match tokens.next() {
                                        Some(next) => {
                                            match self.lexer.get_kind(next) {
                                                lexer::TokenKind::Symbol(lexer::TokenSymbol::Semicolon) => {}
                                                _ => {
                                                    abort!(path=self.lexer.file.clone(), span=self.lexer.get_loc(next), "Expected `;` after function call");
                                                }
                                            }
                                        }
                                        None => {
                                            abort!(path=self.lexer.file.clone(), span=self.lexer.get_loc(token), "Expected `;` after function call");
                                        }
                                    }
                                }
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
    }

    fn keyword_action<'a, T: Iterator<Item=&'a lexer::Token>>(&mut self, _token: &lexer::Token, _tokens: &mut Peekable<T>) {}

    pub fn parse(&mut self) {
        // First lex the source
        self.lexer.lex();

        let tokens = self.lexer.tokens.clone();
        let mut tokens = tokens.iter().peekable();

        while let Some(token) = tokens.next() {
            let info = &self.lexer.token_info[token.0];
            match info.kind {
                lexer::TokenKind::Identifier => {
                    self.ident_action(token, &mut tokens);
                }
                lexer::TokenKind::EOF => {
                    break;
                }
                lexer::TokenKind::Keyword(_) => {
                    self.keyword_action(token, &mut tokens);
                }
                val => {
                    abort!(path=self.lexer.file.clone(),  span=self.lexer.curr_loc(), format!("Unknown syntax {:?}", val))
                }
            }
        }

        println!("{:?}", self.tokens);
    }
}

