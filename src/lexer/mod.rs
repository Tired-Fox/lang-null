use std::collections::HashMap;
use std::fs;

pub mod token;

use token::{Token, TokenInfo, TokenKind};

use self::token::{ErrorInfo, NumberLiteral, TokenKeyword, TokenLiteral, TokenSymbol};

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

    pub tokens: Vec<Token>,
    pub token_info: Vec<TokenInfo>,
    pub literal_numbers: Vec<String>,
    pub literal_strings: Vec<String>,

    pub errors: Vec<ErrorInfo>,

    ident_map: HashMap<String, usize>,
    pub identifiers: Vec<String>,
}

impl Lexer {
    fn read_file(path: &str) -> String {
        fs::read_to_string(path).unwrap()
    }

    pub fn new(path: &str) -> Lexer {
        Lexer {
            buffer: Lexer::read_file(path).chars().collect(),
            errors: Vec::new(),

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
                if last.start + self.column + n < last.end {
                    return Some(self.buffer[last.start + self.column + n]);
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

    fn whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => {
                    self.next();
                }
                _ => break,
            }
        }
    }

    fn get_start(&self) -> usize {
        if self.column == 0 {
            self.column + 1
        } else {
            self.column
        }
    }

    fn keyword_or_ident(&mut self) {
        let mut value = String::new();
        let start = self.get_start();

        loop {
            match self.peek() {
                Some(c) if c.is_alphabetic() || c == '_' || c.is_digit(10) => {
                    value.push(self.next());
                }
                _ => break,
            }
        }

        if value.len() == 0
            || (!value.chars().next().unwrap().is_alphabetic()
                && value.chars().next().unwrap() != '_')
        {
            self.tokens.push(Token(self.token_info.len()));
            self.token_info.push(TokenInfo::new(
                TokenKind::Error,
                self.lines.len(),
                self.get_start(),
                self.errors.len(),
            ));
            self.errors.push(ErrorInfo::new(
                value.len(),
                format!(
                    "Expected alpha char or underscore: found {:?}",
                    value.chars().next().unwrap()
                ),
            ));
            return;
        }

        // Check for TypeLiteral
        if value.chars().next().unwrap().is_alphabetic() {
            if (&value[1..]).chars().all(|c| c.is_digit(10)) {
                let kind = match value.chars().next().unwrap() {
                    'f' => Some(TokenKind::FloatTypeLiteral),
                    'i' => Some(TokenKind::IntegerTypeLiteral),
                    'u' => Some(TokenKind::UnsignedTypeLiteral),
                    _ => None,
                };

                match kind {
                    Some(k) => {
                        self.tokens.push(Token(self.token_info.len()));
                        self.token_info.push(TokenInfo::new(
                            k,
                            self.lines.len(),
                            start,
                            self.literal_numbers.len(),
                        ));
                        self.literal_numbers.push((&value[1..]).to_string());
                        return;
                    }
                    None => {}
                }
            }
        }

        // Keyword or Identifier
        let kind = match TokenKeyword::make(value.as_str()) {
            Some(keyword) => TokenKind::Keyword(keyword),
            None => TokenKind::Identifier,
        };

        let payload = match kind {
            TokenKind::Identifier => match self.ident_map.contains_key(&value) {
                true => self.ident_map.get(&value).unwrap().clone(),
                false => {
                    self.ident_map.insert(value.clone(), self.identifiers.len());
                    self.identifiers.push(value);
                    self.identifiers.len() - 1
                }
            },
            _ => 0,
        };

        self.tokens.push(Token(self.token_info.len()));
        self.token_info
            .push(TokenInfo::new(kind, self.lines.len(), start, payload));
    }

    fn symbol(&mut self) {
        let mut value = String::new();
        let start = self.get_start();

        loop {
            match self.peek() {
                Some(c) if !c.is_alphabetic() && !c.is_whitespace() && !c.is_digit(10) => {
                    value.push(self.next());
                }
                _ => break,
            }
        }

        // try longest to shortest combinations (Greedy)
        let mut kind: Option<TokenKind> = None;
        for i in 0..value.len() {
            match TokenSymbol::make(&value[..value.len() - i]) {
                Some(symbol) => {
                    kind = Some(TokenKind::Symbol(symbol));
                    // Move current char back to what was actually consumed
                    self.column -= i;
                    break;
                }
                None => {}
            }
        }

        match kind {
            Some(k) => {
                self.tokens.push(Token(self.token_info.len()));
                self.token_info.push(TokenInfo::new(
                    k,
                    self.lines.len(),
                    start,
                    0, /* Possible open or close linking */
                ))
            }
            None => {
                self.tokens.push(Token(self.token_info.len()));
                self.token_info.push(TokenInfo::new(
                    TokenKind::Error,
                    self.lines.len(),
                    start,
                    self.errors.len(),
                ));
                self.errors.push(ErrorInfo::new(
                    value.len(),
                    format!("Unkown symbol {:?}", value),
                ));
            }
        }
    }

    fn digit(&mut self) {
        let mut value = String::new();
        let start = self.get_start();

        loop {
            match self.peek() {
                Some(c) if c.is_digit(10) || c.is_alphabetic() || "_-+".contains(c) => {
                    value.push(self.next());
                }
                _ => break,
            }
        }

        if NumberLiteral::verify(&value) {
            self.tokens.push(Token(self.token_info.len()));
            self.token_info.push(TokenInfo::new(
                TokenKind::Literal(TokenLiteral::Number),
                self.lines.len(),
                start,
                self.literal_numbers.len(),
            ));
            self.literal_numbers.push(value);
        } else {
            self.tokens.push(Token(self.token_info.len()));
            self.token_info.push(TokenInfo::new(
                TokenKind::Error,
                self.lines.len(),
                start,
                self.errors.len(),
            ));
            self.errors.push(ErrorInfo::new(
                value.len(),
                format!("Invalid number format"),
            ))
        }
    }

    pub fn string(&mut self) {
        let mut value = String::new();
        let start = (self.lines.len(), self.get_start());
        self.next();

        let mut escaped = false;
        loop {
            match self.peek() {
                Some('\\') if !escaped => {
                    escaped = true;
                }
                Some('"') if !escaped => {
                    self.next();
                    break;
                }
                None => {
                    if !self.next_line() {
                        break;
                    }
                }
                _ => {
                    value.push(self.next());
                }
            }
        }

        self.tokens.push(Token(self.token_info.len()));
        self.token_info.push(TokenInfo::new(
            TokenKind::Literal(TokenLiteral::String),
            start.0,
            start.1,
            self.literal_strings.len(),
        ));
        self.literal_strings.push(value);
    }

    fn char(&mut self) {
        let mut value = String::new();
        let start = self.get_start();
        self.next();

        let mut escaped = false;
        loop {
            match self.peek() {
                Some('\\') if !escaped => {
                    escaped = true;
                }
                Some('\'') if !escaped => {
                    self.next();
                    break;
                }
                None => {
                    self.tokens.push(Token(self.token_info.len()));
                    self.token_info.push(TokenInfo::new(
                        TokenKind::Error,
                        self.lines.len(),
                        start,
                        self.errors.len(),
                    ));
                    self.errors.push(ErrorInfo::new(
                        self.column - start,
                        "Unterminated character literal".to_string(),
                    ))
                }
                _ => {
                    value.push(self.next());
                }
            }
        }

        if value.len() != 1 {
            self.tokens.push(Token(self.token_info.len()));
            self.token_info.push(TokenInfo::new(
                TokenKind::Error,
                self.lines.len(),
                start,
                self.errors.len(),
            ));
            self.errors.push(ErrorInfo::new(
                value.len() + 2,
                "Length of character literal must be 1".to_string(),
            ))
        }

        self.tokens.push(Token(self.token_info.len()));
        self.token_info.push(TokenInfo::new(
            TokenKind::Literal(TokenLiteral::Char),
            self.lines.len(),
            start,
            self.literal_strings.len(),
        ));
        self.literal_strings.push(value);
    }

    pub fn run(&mut self) {
        while self.next_line() {
            'line: loop {
                match self.peek() {
                    None => break 'line,
                    Some(c) => match c {
                        ' ' | '\t' | '\r' => {
                            self.whitespace();
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            self.keyword_or_ident();
                        }
                        '0'..='9' => {
                            self.digit();
                        }
                        '"' => {
                            self.string();
                        }
                        '\'' => {
                            self.char();
                        }
                        '/' if self.peekn(1) == Some('/') || Some('*') == self.peekn(1) => {
                            self.whitespace()
                        }
                        _ => {
                            self.symbol();
                        }
                    },
                }
            }
        }

        // Close the token list with EOF token
        self.tokens.push(Token(self.tokens.len()));
        self.token_info.push(TokenInfo::new(
            TokenKind::EOF,
            self.lines.len(),
            self.get_start(),
            0,
        ));

        // Deallocate buffer & map memory
        self.buffer = Vec::new();
        self.ident_map = HashMap::new();
    }
}

impl From<&str> for Lexer {
    fn from(source: &str) -> Self {
        Lexer {
            buffer: source.chars().collect(),
            errors: Vec::new(),

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
}

impl From<String> for Lexer {
    fn from(source: String) -> Self {
        Lexer {
            buffer: source.chars().collect(),
            errors: Vec::new(),

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
}

#[cfg(test)]
mod tests {
    #[test]
    fn peek() {
        let mut lexer = Lexer::from("    let x: i32 = 0;");
        lexer.next_line();
        assert_eq!(lexer.peek(), Some('l'));
    }

    #[test]
    fn peekn() {
        let mut lexer = Lexer::from("    let x: i32 = 0;");
        lexer.next_line();
        assert_eq!(lexer.peekn(5), Some(':'));
    }

    #[test]
    fn peek_no_next() {
        let mut lexer = Lexer::from("");
        lexer.next_line();
        assert_eq!(lexer.peek(), None);
    }

    #[test]
    fn next() {
        let mut lexer = Lexer::from("    let");
        lexer.next_line();
        assert_eq!(lexer.next(), 'l');
        assert_eq!(lexer.next(), 'e');
        assert_eq!(lexer.next(), 't');
    }

    #[test]
    #[should_panic]
    fn next_no_line() {
        let mut lexer = Lexer::from("");
        lexer.next();
    }

    #[test]
    #[should_panic]
    fn next_no_chars_in_line() {
        let mut lexer = Lexer::from("l");
        lexer.next_line();
        lexer.next();
        lexer.next();
    }

    use super::*;
    #[test]
    fn empty_lex() {
        let mut lexer = Lexer::from("");
        lexer.run();
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

        let mut lexer = Lexer::from(source);
        lexer.run();
        assert_eq!(lexer.lines.len(), 4);
        assert_eq!(lexer.tokens.len(), 19);
        assert_eq!(lexer.token_info.len(), 19);
    }
}
