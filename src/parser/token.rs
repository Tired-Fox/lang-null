use std::fmt::{Debug, Display};
use std::iter::Peekable;
use std::ops::Index;

use crate::lexer;

pub trait Parse:
where Self: Sized
{
    fn parse<'a, T: Iterator<Item = &'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self>;
}

pub type ParseResult<T> = Result<T, Error>;

pub struct Location {
    pub line: usize,
    pub column: usize
}
pub struct Error(Location, String);
impl Error {
    pub fn new(loc: (usize, usize), message: String) -> Error {
        Error(Location { line: loc.0, column: loc.1}, message)
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}:{}]: {}",
            self.0.line,
            self.0.column,
            self.1
        )
    }
}
impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.1,
        )
    }
}
impl std::error::Error for Error {}

#[derive(Debug)]
pub enum Token {
    Call(Call),
    Ident(Ident),
    Literal(Literal),
    Decleration(Declaration),
}

/// <name>: <type>
#[derive(Debug)]
pub struct Argument {
    pub name: Path,
    pub arg_type: Path,
}

#[derive(Debug)]
pub struct Segment(pub Ident);
impl Parse for Segment {
    fn parse<'a, T: Iterator<Item = &'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        if let Some(next) = stream.next() {
            if let lexer::TokenKind::Identifier = lexer.get_kind(next) {
                if let Some(ident) = lexer.get_ident(next) {
                    return Ok(Segment(Ident(ident.clone())));
                }
            }
            Err(Error::new(lexer.get_loc(next), "Expected an identifier".to_string()))
        } else {
            Err(Error::new((0, 0), "Expected an identifier".to_string()))
        }
    }
}

#[derive(Debug)]
pub struct Path(pub Vec<Segment>);
impl Parse for Path {
    fn parse<'a, T: Iterator<Item = &'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let start = match stream.peek() {
            Some(next) => lexer.get_loc(next),
            None => return Err(Error::new((0, 0), "Expected path".to_string())),
        };

        let mut segments = Vec::new();
        while let Ok(segment) = Segment::parse(stream, lexer) {
            segments.push(segment);
            if let Some(next) = stream.peek() {
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::ColonColon) = lexer.get_kind(next) {
                    stream.next();
                    continue;
                } else {
                    return Err(Error::new(lexer.get_loc(next), "Invalid path syntax; expected `::`".to_string()));
                }
            }
        }

        if segments.is_empty() {
            Err(Error::new(start, "Expected path".to_string()))
        } else {
            Ok(Path(segments))
        }
    }
}

impl Parse for Argument {
    fn parse<'a, T: Iterator<Item = &'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        // Path: Path
        let name = Path::parse(stream, lexer)?;
        let arg_type = match stream.next() {
            Some(next) => {
                if let lexer::TokenKind::Symbol(lexer::TokenSymbol::Colon) = lexer.get_kind(next) {
                    Path::parse(stream, lexer)?
                } else {
                    return Err(Error::new(lexer.get_loc(next), "Invalid argument syntax; expected `:`".to_string()))
                }
            },
            None => return Err(Error::new((0, 0), "Invalid argument syntax; expected argument type".to_string()))
        };
        Ok(Argument { name, arg_type })
    }
}

#[derive(Debug, Default)]
pub struct Block(pub Vec<Token>);

#[derive(Debug)]
pub struct Punctuated<T: Parse, const N: char = ','>(pub Vec<T>);
impl<V: Parse, const N: char> Parse for Punctuated<V, N> {
    fn parse<'a, T: Iterator<Item = &'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let mut values = Vec::new();
        while let Ok(value) = V::parse(stream, lexer) {
            values.push(value);
            match stream.peek() {
                Some(next) => {
                    if let lexer::TokenKind::Symbol(symbol) = lexer.get_kind(next) {
                        if symbol.compare(N.to_string().as_str()) {
                            stream.next();
                            continue
                        }
                    }
                    break
                },
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

impl<T: Parse, const N: char> Default for  Punctuated<T, N>  {
    fn default() -> Self {
        Self(Vec::new())
    }
}

#[derive(Debug)]
pub struct Ident(pub String);
impl ToString for  Ident {
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
    fn parse<'a, T: Iterator<Item = &'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        match stream.next() {
            Some(next) => {
                match lexer.get_kind(next) {
                    lexer::TokenKind::Identifier => {
                        Ok(Parameter::Ident(Ident(lexer.get_ident(next).unwrap().clone())))
                    },
                    lexer::TokenKind::Literal(lit) => {
                        Ok(match lit {
                            lexer::TokenLiteral::Number => {
                                Parameter::Literal(Literal::Number(lexer.get_number(next).unwrap().clone()))
                            },
                            lexer::TokenLiteral::String => {
                                Parameter::Literal(Literal::String(lexer.get_string(next).unwrap().clone()))
                            },
                            lexer::TokenLiteral::Char => {
                                Parameter::Literal(Literal::Char(lexer.get_char(next).unwrap().clone()))
                            }
                        })
                    },
                    _ => Err(Error::new(lexer.get_loc(next), "Invalid parameter syntax".to_string()))
                }
            },
            None => Err(Error::new((0, 0), "Expected parameter".to_string()))
        }
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function(Ident, Punctuated<Argument>, Block),
    Variable(Ident, Literal),
}

#[derive(Debug)]
pub enum Call {
    Call(Ident, Punctuated<Parameter>),
    Syscall(Ident, Punctuated<Parameter>),
}
