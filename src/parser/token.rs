use std::process::Command;

use crate::compiler::syscall::*;
use crate::lexer::Token;

pub struct Call();

/// <name>: <type>
pub struct Argument {
    name: Token,
    r#type: Vec<Token>,
}

/// <name> :: fn(<...args>) {
///
/// }
pub enum Function {
    Decleration(Token, Vec<Argument>, Vec<Token>, Vec<Token>),
    Call(Token, Vec<Token>),
}

///  main :: fn(a1: String) String {
///     return "hello";
///  }
///
/// Tokens as text
/// [main, ::, fn, (, a1, :, String, ), String, {, return, "hello", ;, }]
///
/// Parsed node
/// [Decleration {
///     name: Token(0),
///     args: vec![
///         Argument {
///             name: Token(4),
///             r#type: vec![Token(6)],
///         }
///     ],
///     return_type: vec![Token(8)],
///     body: vec![],
/// }]
pub fn teser() {}
