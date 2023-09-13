use proc_macro2::Span;

use super::instruction::Instruction;
use super::lex::*;
use super::Expr;

/*
                 # Labels    Instructions    Operands

 # Directives:               global          start
                             extern          ExitProcess
                             incbin          "path/to/binary"

 # Sections:                 section         .text
                start:
                             mov             rax, 0x02000004
                             mov             rdi, 1
                             mov             rsi, message
                             mov             rdx, 13
                             syscall

                             mov             rax, 0x02000001 ;Comment;
                             xor             rdi, rdi
                             syscall

                             section         .data
                 message:    db              "Hello, world", 10
*/

pub enum Directive {
    Global(Span, String),
    Extern(Span, String),
    Incbin(Span, String),
}

pub struct Section(String);

pub enum Node {
    Directive(Directive),
    Section(Section),
    Label(String),
    Instruction(Instruction, Vec<Expr>),
    Prefix(String, Vec<Expr>),
}
