use std::fmt::{Debug, Display, Formatter};

pub mod compiler;
pub mod error;
pub mod lexer;
pub mod parser;

#[derive(Copy, Clone, Eq, PartialEq, Default)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start,
            end,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }
    pub fn end(&self) -> usize {
        self.end
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f, "[{}..{}]",
            self.start,
            self.end
        )
    }
}

#[derive(Copy, Clone, Default)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub span: Span,
}

impl Location {
    pub fn new(line: usize, column: usize, span: Span) -> Location {
        Location {
            line,
            column,
            span,
        }
    }
}

impl Debug for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{} {:?}", self.line, self.column, self.span)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub fn first_positive(sizes: &[usize]) -> usize {
    for size in sizes.iter() {
        if *size > 0 {
            return *size;
        }
    }
    0
}