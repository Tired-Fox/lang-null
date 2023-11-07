use std::fmt::{Debug};
use std::iter::Peekable;
use std::ops::Index;
use std::slice::Iter;
use crate::error::Error;

use crate::{err, lexer, Location, Span};
use crate::lexer::Lexer;

pub trait Parse:
    where Self: Sized
{
    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self>;
    fn location(&self) -> Location;
}

pub type ParseResult<T> = Result<T, Error>;

pub enum Token {
    None,
    Call(Call),
    Ident(Ident),
    Literal(Literal),
    Decleration(Declaration),
    Block(Block),
}

impl Token {
    pub fn location(&self) -> Location {
        match self {
            Token::None => Location::default(),
            Token::Call(call) => call.location(),
            Token::Ident(ident) => ident.location(),
            Token::Literal(literal) => literal.location(),
            Token::Decleration(declaration) => declaration.location(),
            Token::Block(block) => block.location(),
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::None => {Ok(())},
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
    location: Location,
}

#[derive(Debug)]
pub enum Type {
    IntegerType(u8, Location),
    FloatType(u8, Location),
    UnsignedType(u8, Location),
    Path(Path),
}

impl Parse for Type {
    fn location(&self) -> Location {
        match self {
            Type::IntegerType(_, location) => *location,
            Type::FloatType(_, location) => *location,
            Type::UnsignedType(_, location) => *location,
            Type::Path(path) => path.location(),
        }
    }

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
                        Ok(Type::IntegerType(lexer.get_number(&next).unwrap().parse().unwrap(), lexer.get_loc(&next)))
                    }
                    lexer::TokenKind::UnsignedTypeLiteral => {
                        stream.next();
                        Ok(Type::UnsignedType(lexer.get_number(&next).unwrap().parse().unwrap(), lexer.get_loc(&next)))
                    }
                    lexer::TokenKind::FloatTypeLiteral => {
                        stream.next();
                        Ok(Type::FloatType(lexer.get_number(&next).unwrap().parse().unwrap(), lexer.get_loc(&next)))
                    }
                    _ => {
                        Err(err!(span=lexer.get_loc(&next), "Expected a type"))
                    }
                }
            }
            None => Err(err!("Expected a type".to_string()))
        }
    }
}

#[derive(Debug)]
pub struct Segment(pub Ident);

impl Parse for Segment {
    fn location(&self) -> Location {
        self.0.1
    }

    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        if let Some(next) = stream.peek() {
            if let lexer::TokenKind::Identifier = lexer.get_kind(next) {
                if let Some(ident) = lexer.get_ident(next) {
                    let token = stream.next().unwrap();
                    return Ok(Segment(Ident(ident.clone(), lexer.get_loc(token))));
                }
            }
            // eprintln!("Expected an identifier; was {:?}", lexer.get_kind(next));
            Err(err!(span=lexer.get_loc(next), "Expected an identifier".to_string()))
        } else {
            // eprintln!("Expected an identifier; no tokens left");
            Err(err!("Expected an identifier".to_string()))
        }
    }
}

#[derive(Debug)]
pub struct Path(Location, pub Vec<Segment>);

impl Parse for Path {
    fn location(&self) -> Location {
        Location::default()
    }

    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let start = match stream.peek() {
            Some(next) => lexer.get_loc(next),
            None => return Err(err!("Expected path".to_string())),
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
            Err(err!(span=start.clone(), "Expected path".to_string()))
        } else {
            let first = segments.first().unwrap().location();
            let last = segments.last().unwrap().location();
            Ok(Path(Location::new(first.line, first.column, Span::new(first.span.start(), last.span.end())), segments))
        }
    }
}

impl Parse for Argument {
    fn location(&self) -> Location {
        self.location
    }

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
                    return Err(err!(span=lexer.get_loc(next), "Invalid argument syntax; expected `:`".to_string()));
                }
            }
            None => {
                eprintln!("Invalid argument syntax; expected argument type");
                return Err(err!("Invalid argument syntax; expected argument type".to_string()));
            }
        };
        let location = Location::new(
            name.location().line,
            name.location().column,
            Span::new(
                name.location().span.start(),
                arg_type.location().span.end()
            )
        );
        Ok(Argument {
            name,
            arg_type,
            location
        })
    }
}

#[derive(Debug, Default)]
pub struct Block(pub Vec<Token>, pub Location);
impl Block {
    pub fn location(&self) -> Location {
        self.1
    }
}

#[derive(Debug)]
pub struct Punctuated<T: Parse, const N: char = ','>(pub Vec<T>, pub Option<Location>);

impl<T: Parse, const N: char> Punctuated<T, N> {
    pub fn iter(&self) -> Iter<'_, T> {
        self.0.iter()
    }
}

impl<V: Parse, const N: char> Parse for Punctuated<V, N> {
    fn location(&self) -> Location {
        match &self.1 {
            Some(location) => *location,
            None => Location::default(),
        }
    }

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
        let location = match values.first() {
            Some(first) => {
                let first = first.location();
                let last = values.last().unwrap().location();
                Some(Location::new(
                    first.line,
                    first.column,
                    Span::new(
                        first.span.start(),
                        last.span.end(),
                    ),
                ))
            }
            None => None
        };

        Ok(Punctuated(values, location))
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
        Self(Vec::new(), None)
    }
}

#[derive(Debug)]
pub struct Ident(pub String, pub Location);
impl Ident {
    pub fn location(&self) -> Location {
        self.1
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug)]
pub enum Literal {
    Number{ value: String, location: Location },
    String{ value: String, location: Location },
    Char{ value: char, location: Location },
}

impl Literal {
    pub fn location(&self) -> Location {
        match self {
            Literal::Number{ location, .. } => *location,
            Literal::String{ location, .. } => *location,
            Literal::Char{ location, .. } => *location
        }
    }
}

#[derive(Debug)]
pub enum Parameter {
    Ident(Ident),
    Literal(Literal),
    Call(Call),
}

impl Parse for Parameter {
    fn location(&self) -> Location {
        Location::default()
    }

    fn parse<'a, T: Iterator<Item=&'a lexer::Token>>(stream: &mut Peekable<T>, lexer: &lexer::Lexer) -> ParseResult<Self> {
        let start = stream.peek().map(|v| (**v));
        match start {
            Some(next) => {
                match lexer.get_kind(&next) {
                    lexer::TokenKind::Identifier => {
                        let _ = stream.next();
                        Ok(Parameter::Ident(Ident(lexer.get_ident(&next).unwrap().clone(), lexer.get_loc(&next))))
                    }
                    lexer::TokenKind::Literal(lit) => {
                        let _ = stream.next();
                        Ok(match lit {
                            lexer::TokenLiteral::Number => {
                                Parameter::Literal(Literal::Number{value: lexer.get_number(&next).unwrap().clone(), location: lexer.get_loc(&next)})
                            }
                            lexer::TokenLiteral::String => {
                                Parameter::Literal(Literal::String{value: lexer.get_string(&next).unwrap().clone(), location: lexer.get_loc(&next)})
                            }
                            lexer::TokenLiteral::Char => {
                                Parameter::Literal(Literal::Char{value: lexer.get_char(&next).unwrap(), location: lexer.get_loc(&next)})
                            }
                        })
                    }
                    _ => Err(err!(span=lexer.get_loc(&next), "Invalid parameter syntax".to_string()))
                }
            }
            None => Err(err!("Expected parameter".to_string()))
        }
    }
}

#[derive(Debug)]
pub enum Declaration {
    Function{name: Ident, args: Punctuated<Argument>, body: Option<Block>, location: Location},
    Variable{name: Ident, value: Literal, location: Location},
}

impl Declaration {
    pub fn location(&self) -> Location {
        match self {
            Declaration::Function{location, ..} => *location,
            Declaration::Variable{location, ..} => *location,
        }
    }
}

#[derive(Debug)]
pub enum Call {
    Call{ name: Ident, params: Punctuated<Parameter>, location: Location},
    Syscall{ name: Ident, params: Punctuated<Parameter>, location: Location},
}

impl Call {
    pub fn location(&self) -> Location {
        match self {
            Call::Call{location, ..} => *location,
            Call::Syscall{location, ..} => *location
        }
    }
}