use std::fmt::{Debug, Display};
use std::iter::Peekable;
use std::ops::Index;
use crate::error::Error;

use crate::{err, lexer};
use crate::lexer::Lexer;

pub trait Parse:
    where Self: Sized
{
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self>;
}

pub type ParseResult<T> = Result<T, Error>;

pub struct Location {
    pub line: usize,
    pub column: usize,
}

pub enum Token {
    Call(Call),
    Ident(Ident),
    Literal(Literal),
    Decleration(Declaration),
    Block(Block),
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Call(call) => write!(f, "{:?}", call),
            Token::Ident(ident) => write!(f, "{:?}", ident),
            Token::Literal(literal) => write!(f, "{:?}", literal),
            Token::Decleration(declaration) => write!(f, "{:?}", declaration),
            Token::Block(block) => write!(f, "{:?}", block),
        }
    }
}

/// <name>: <type>
#[derive(Debug)]
pub struct Argument {
    pub name: Path,
    pub arg_type: Type,
}

#[derive(Debug)]
pub enum Type {
    IntegerType(u8),
    FloatType(u8),
    UnsignedType(u8),
    Path(Path),
}

impl Parse for Type {
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &Lexer) -> ParseResult<Self> {
        let next = stream.peek().map(|v| **v);
        match next {
            Some(next) => {
                match lexer.get_kind(&next) {
                    lexer::TokenKind::Identifier => {
                        stream.next();
                        Ok(Type::Path(Path::parse(stream, lexer)?))
                    }
                    lexer::TokenKind::IntegerTypeLiteral => {
                        stream.next();
                        Ok(Type::IntegerType(lexer.get_number(&next).unwrap().parse().unwrap()))
                    }
                    lexer::TokenKind::UnsignedTypeLiteral => {
                        stream.next();
                        Ok(Type::UnsignedType(lexer.get_number(&next).unwrap().parse().unwrap()))
                    }
                    lexer::TokenKind::FloatTypeLiteral => {
                        stream.next();
                        Ok(Type::FloatType(lexer.get_number(&next).unwrap().parse().unwrap()))
                    }
                    _ => {
                        Err(err!(ParseError, lexer.get_loc(&next), "Expected a type"))
                    }
                }
            }
            None => Err(err!(ParseError, "Expected a type".to_string()))
        }
    }
}

#[derive(Debug)]
pub struct Segment(pub Ident);

impl Parse for Segment {
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        if let Some(next) = stream.peek() {
            if let lexer::TokenKind::Identifier = lexer.get_kind(next) {
                if let Some(ident) = lexer.get_ident(next) {
                    stream.next();
                    return Ok(Segment(Ident(ident.clone())));
                }
            }
            eprintln!("Expected an identifier; was {:?}", lexer.get_kind(next));
            Err(err!(ParseError, lexer.get_loc(next), "Expected an identifier".to_string()))
        } else {
            eprintln!("Expected an identifier; no tokens left");
            Err(err!(ParseError, "Expected an identifier".to_string()))
        }
    }
}

#[derive(Debug)]
pub struct Path(pub Vec<Segment>);

impl Parse for Path {
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let start = match stream.peek() {
            Some(next) => lexer.get_loc(next),
            None => return Err(err!(ParseError, "Expected path".to_string())),
        };

        let mut segments = Vec::new();
        while let Ok(segment) = Segment::parse(stream, lexer) {
            segments.push(segment);
            if let Some(next) = stream.peek() {
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::ColonColon) = lexer.get_kind(next) {
                    stream.next();
                    continue;
                } else {
                    break;
                }
            }
        }

        if segments.is_empty() {
            Err(err!(ParseError, start.clone(), "Expected path".to_string()))
        } else {
            Ok(Path(segments))
        }
    }
}

impl Parse for Argument {
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let name = Path::parse(stream, lexer)?;
        let arg_type = match stream.next() {
            Some(next) => {
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::Colon) = lexer.get_kind(next) {
                    match Type::parse(stream, lexer) {
                        Ok(val) => val,
                        Err(err) => {
                            eprintln!("{}", err);
                            return Err(err);
                        }
                    }
                } else {
                    eprintln!("Invalid argument syntax; expected `:`");
                    return Err(err!(ParseError, lexer.get_loc(next), "Invalid argument syntax; expected `:`".to_string()));
                }
            }
            None => {
                eprintln!("Invalid argument syntax; expected argument type");
                return Err(err!(ParseError, "Invalid argument syntax; expected argument type".to_string()));
            }
        };
        Ok(Argument { name, arg_type })
    }
}

#[derive(Debug, Default)]
pub struct Block(pub Vec<Token>);

#[derive(Debug)]
pub struct Punctuated<T: Parse, const N: char = ','>(pub Vec<T>);

impl<V: Parse, const N: char> Parse for Punctuated<V, N> {
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let mut values = Vec::new();
        while let Ok(value) = V::parse(stream, lexer) {
            values.push(value);
            match stream.peek() {
                Some(next) => {
                    if let lexer::TokenKind::Symbol(symbol) = lexer.get_kind(next) {
                        if symbol.compare(N.to_string().as_str()) {
                            stream.next();
                            continue;
                        }
                    }
                    break;
                }
                None => break
            }
        }
        Ok(Punctuated(values))
    }
}

impl<V: Parse, const N: char> Punctuated<V, N> {
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl<T: Parse, const N: char> Index<usize> for Punctuated<T, N> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<T: Parse, const N: char> Default for Punctuated<T, N> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

#[derive(Debug)]
pub struct Ident(pub String);

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug)]
pub enum Literal {
    Number(String),
    String(String),
    Char(char),
}

#[derive(Debug)]
pub enum Parameter {
    Ident(Ident),
    Literal(Literal),
    Call(Call),
}

impl Parse for Parameter {
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let start = stream.peek().map(|v| (**v));
        match start {
            Some(next) => {
                match lexer.get_kind(&next) {
                    lexer::TokenKind::Identifier => {
                        let _ = stream.next();
                        Ok(Parameter::Ident(Ident(lexer.get_ident(&next).unwrap().clone())))
                    }
                    lexer::TokenKind::Literal(lit) => {
                        let _ = stream.next();
                        Ok(match lit {
                            lexer::TokenLiteral::Number => {
                                Parameter::Literal(Literal::Number(lexer.get_number(&next).unwrap().clone()))
                            }
                            lexer::TokenLiteral::String => {
                                Parameter::Literal(Literal::String(lexer.get_string(&next).unwrap().clone()))
                            }
                            lexer::TokenLiteral::Char => {
                                Parameter::Literal(Literal::Char(lexer.get_char(&next).unwrap()))
                            }
                        })
                    }
                    _ => Err(err!(ParseError, lexer.get_loc(&next), "Invalid parameter syntax".to_string()))
                }
            }
            None => Err(err!(ParseError, "Expected parameter".to_string()))
        }
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function(Ident, Punctuated<Argument>, Option<Block>),
    Variable(Ident, Literal),
}

#[derive(Debug)]
pub enum Call {
    Call(Ident, Punctuated<Parameter>),
    Syscall(Ident, Punctuated<Parameter>),
}