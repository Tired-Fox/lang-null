use std::fmt::Display;

use proc_macro2::Span;

use super::instruction::Instruction;
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

#[derive(Debug)]
pub enum Directive {
    Global(Span, String),
    Extern(Span, String),
    Incbin(Span, String),
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Global(_, val) => write!(f, "global {}", val),
            Self::Extern(_, val) => write!(f, "extern {}", val),
            Self::Incbin(_, val) => write!(f, "incbin {}", val),
        }
    }
}

#[derive(Debug)]
pub enum Node {
    Directive(Directive),
    Section(Span, String),
    Label(Span, String),
    Instruction(Instruction, Vec<Expr>),
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Directive(directive) => write!(f, "{}", directive),
            Self::Section(_, val) => write!(f, "section .{}", val),
            Self::Label(_, val) => write!(f, "{}: ", val),
            Self::Instruction(inst, params) => {
                write!(
                    f,
                    "{} {}",
                    inst,
                    params
                        .iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}
