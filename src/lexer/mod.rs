use std::collections::HashMap;

pub mod token;

use token::{Token, TokenInfo, TokenKind};

/// Information for location of a line in source
#[derive(Debug, Clone, Copy)]
pub struct Line {
    /// Starting offset of the line in the source buffer
    pub start: usize,
    /// Offset to the end of the line in the source buffer
    pub end: usize,
    /// The offset in the line where the first non whitespace character appears
    pub indent: usize,
}

impl Line {
    pub fn new(start: usize, end: usize, indent: usize) -> Line {
        Line { start, end, indent }
    }
}

pub struct Lexer {
    buffer: Vec<char>,
    lines: Vec<Line>,
    column: usize,

    tokens: Vec<token::Token>,
    token_info: Vec<token::TokenInfo>,
    literal_numbers: Vec<String>,
    literal_strings: Vec<String>,

    ident_map: HashMap<String, usize>,
    identifiers: Vec<String>,
}

impl Lexer {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            buffer: source.chars().collect(),
            lines: Vec::new(),
            column: 0,

            tokens: Vec::new(),
            token_info: Vec::new(),

            literal_numbers: Vec::new(),
            literal_strings: Vec::new(),

            ident_map: HashMap::new(),
            identifiers: Vec::new(),
        }
    }

    fn peekn(&self, n: usize) -> Option<char> {
        match self.lines.last() {
            None => None,
            Some(last) => {
                if last.start + self.column + n - 1 < last.end {
                    return Some(self.buffer[last.start + self.column + n - 1]);
                }
                None
            }
        }
    }

    fn peek(&self) -> Option<char> {
        match self.lines.last() {
            None => None,
            Some(last) => {
                if last.start + self.column < last.end {
                    return Some(self.buffer[last.start + self.column]);
                }
                None
            }
        }
    }

    fn next(&mut self) -> char {
        match self.lines.last() {
            None => panic!("No lines to read"),
            Some(last) => {
                if last.start + self.column >= last.end {
                    panic!("No more characters in the current line");
                }
                let n = self.buffer[last.start + self.column];
                self.column += 1;
                return n;
            }
        }
    }

    fn next_line(&mut self) -> bool {
        let mut start = 0;
        if let Some(last) = self.lines.last() {
            start = last.end + 1;
        }

        if start < self.buffer.len() {
            // Go from start to next \n char
            // Start - index(\n) inclusive
            // Walk until not whitespace record as indent
            let mut indent: Option<usize> = None;
            let mut i = start;
            while i < self.buffer.len() {
                match self.buffer[i] {
                    ' ' | '\t' | '\r' => {}
                    '\n' => {
                        self.lines.push(Line::new(start, i, indent.unwrap_or(0)));
                        self.column = indent.unwrap_or(0);
                        return true;
                    }
                    _ => {
                        if let None = indent {
                            indent = Some(i - start);
                        }
                    }
                }
                i += 1;
            }
            self.lines
                .push(Line::new(start, self.buffer.len(), indent.unwrap_or(0)));
            self.column = indent.unwrap_or(0);
            return true;
        }
        false
    }

    pub fn lex(source: &str) -> Lexer {
        let mut lexer = Lexer::new(source);

        while lexer.next_line() {
            'line: loop {
                match lexer.peek() {
                    None => break 'line,
                    Some(c) => {
                        match c {
                            ' ' | '\t' | '\r' => {
                                // TODO: Whitespace
                                println!("Whitespace: {:?}", c);
                            }
                            'a'..='z' | 'A'..='Z' | '_' => {
                                // TODO: Keyword or Ident
                                println!("Alpha: {:?}", c);
                            }
                            '0'..='9' => {
                                // TODO: Number
                                println!("Digit: {:?}", c);
                            }
                            '-' => {
                                match lexer.peekn(2) {
                                    Some(c) if c.is_digit(10) => {
                                        // TODO: Number
                                        println!("Negative digit: {:?}", c);
                                        lexer.next();
                                        lexer.next();
                                        continue 'line;
                                    }
                                    _ => {}
                                }
                                // TODO: Symbol
                                println!("Symbol: {:?}", c);
                            }
                            _ => {
                                // TODO: Symbol
                                println!("Symbol: {:?}", c);
                            }
                        }
                    }
                }
                lexer.next();
            }
        }

        // Close the token list with EOF token
        lexer.tokens.push(Token(lexer.tokens.len()));
        lexer.token_info.push(TokenInfo::new(
            TokenKind::EOF,
            lexer.lines.len(),
            lexer.column,
            0,
        ));
        lexer
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn peek() {
        let mut lexer = Lexer::new("    let x: i32 = 0;");
        lexer.next_line();
        assert_eq!(lexer.peek(), Some('l'));
    }

    #[test]
    fn peekn() {
        let mut lexer = Lexer::new("    let x: i32 = 0;");
        lexer.next_line();
        assert_eq!(lexer.peekn(5), Some('x'));
    }

    #[test]
    fn peek_no_next() {
        let mut lexer = Lexer::new("");
        lexer.next_line();
        assert_eq!(lexer.peek(), None);
    }

    #[test]
    fn next() {
        let mut lexer = Lexer::new("    let");
        lexer.next_line();
        assert_eq!(lexer.next(), 'l');
        assert_eq!(lexer.next(), 'e');
        assert_eq!(lexer.next(), 't');
    }

    #[test]
    #[should_panic]
    fn next_no_line() {
        let mut lexer = Lexer::new("");
        lexer.next();
    }

    #[test]
    #[should_panic]
    fn next_no_chars_in_line() {
        let mut lexer = Lexer::new("l");
        lexer.next_line();
        lexer.next();
        lexer.next();
    }

    use super::*;
    #[test]
    fn empty_lex() {
        let lexer = Lexer::lex("");
        assert_eq!(lexer.tokens.len(), 1);
        assert_eq!(lexer.token_info.len(), 1);
        assert_eq!(lexer.lines.len(), 0);
        assert_eq!(lexer.token_info[0].kind, TokenKind::EOF);
    }

    #[test]
    fn multi_line() {
        let source = "\
let x: i32 = 0;
fn main() {
    print(\"Hello, world!\");
}";

        let lexer = Lexer::lex(source);
        assert_eq!(lexer.lines.len(), 4);
        assert_eq!(lexer.tokens.len(), 22);
        assert_eq!(lexer.token_info.len(), 22);
    }
}
