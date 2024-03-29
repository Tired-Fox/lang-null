use std::collections::HashMap;
use std::fs;

pub use token::{Token, TokenInfo, TokenKind};

pub use self::token::{NumberLiteral, TokenKeyword, TokenLiteral, TokenSymbol};
use crate::error::{abort, Error, ErrorCommand};
use crate::{err, Location, Span};

pub mod token;

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
    pub file: Option<String>,
    lines: Vec<Line>,
    column: usize,

    pub tokens: Vec<Token>,
    pub token_info: Vec<TokenInfo>,
    pub literal_numbers: Vec<String>,
    pub literal_strings: Vec<String>,
    pub literal_chars: Vec<char>,

    pub errors: Vec<Error>,

    ident_map: HashMap<String, usize>,
    pub identifiers: Vec<String>,
}

impl Lexer {
    pub fn debug(&self, token: &Token) -> String {
        let info = self.get_info(token);
        match info.kind {
            TokenKind::Symbol(symbol) => symbol.to_string(),
            TokenKind::Literal(literal) => match literal {
                TokenLiteral::Number => {
                    format!("\x1b[35m{:?}\x1b[39m", self.get_number(token).unwrap())
                }
                TokenLiteral::String => {
                    format!("\x1b[32m{:?}\x1b[39m", self.get_string(token).unwrap())
                }
                TokenLiteral::Char => {
                    format!("\x1b[33m'{}'\x1b[39m", self.get_char(token).unwrap())
                }
            },
            TokenKind::Keyword(keyword) => keyword.to_string(),
            TokenKind::Identifier => format!("\x1b[33m{}\x1b[39m", self.get_ident(token).unwrap()),
            TokenKind::IntegerTypeLiteral => format!("\x1b[33mi{}\x1b[39m", self.get_number(token).unwrap()),
            TokenKind::UnsignedTypeLiteral => format!("\x1b[33mu{}\x1b[39m", self.get_number(token).unwrap()),
            TokenKind::FloatTypeLiteral => format!("\x1b[33mf{}\x1b[39m", self.get_number(token).unwrap()),
            TokenKind::Error => "\x1b[1;31mError\x1b[22;39m".to_string(),
            TokenKind::EOF => "\x1b[36mEOF\x1b[22;39m".to_string(),
        }
    }

    pub fn get_info(&self, token: &Token) -> &TokenInfo {
        &self.token_info[token.0]
    }

    pub fn get_ident(&self, token: &Token) -> Option<String> {
        if self.get_kind(token) == TokenKind::Identifier {
            Some(self.identifiers[self.get_info(token).payload].clone())
        } else {
            None
        }
    }

    pub fn get_kind(&self, token: &Token) -> TokenKind {
        self.get_info(token).kind
    }

    pub fn get_number(&self, token: &Token) -> Option<&String> {
        let kind = self.get_kind(token);
        if kind == TokenKind::Literal(TokenLiteral::Number)
            || kind == TokenKind::IntegerTypeLiteral
            || kind == TokenKind::UnsignedTypeLiteral
            || kind == TokenKind::FloatTypeLiteral {
            Some(&self.literal_numbers[self.get_info(token).payload])
        } else {
            None
        }
    }

    pub fn get_string(&self, token: &Token) -> Option<&String> {
        if self.get_kind(token) == TokenKind::Literal(TokenLiteral::String) {
            Some(&self.literal_strings[self.get_info(token).payload])
        } else {
            None
        }
    }

    pub fn get_char(&self, token: &Token) -> Option<char> {
        if self.get_kind(token) == TokenKind::Literal(TokenLiteral::Char) {
            Some(self.literal_chars[self.get_info(token).payload])
        } else {
            None
        }
    }

    pub fn get_loc(&self, token: &Token) -> Location {
        let info = self.get_info(token);
        Location::new(info.line, info.column, info.span)
    }

    pub fn curr_loc(&self) -> Location {
        let start = self.lines.last().unwrap().start;
        Location::new(self.lines.len(), self.column, Span::new(start, start + self.column))
    }

    pub fn extract(&self, span: &Span) -> &[char] {
        if span.start() > span.end() { panic!("Start must be less than the end value; was {}", span.start()) } else if span.end() > self.buffer.len() { panic!("End must be less than the source length; was {}", span.end()) } else {
            &self.buffer[span.start()..span.end()]
        }
    }

    pub fn source(&self) -> &[char] {
        self.buffer.as_slice()
    }

    pub fn errors(&self) -> &Vec<Error> {
        &self.errors
    }
}

impl Lexer {
    pub fn with_path<P: AsRef<std::path::Path>>(path: P) -> Self {
        let file = path.as_ref().to_str().unwrap().to_string();
        Lexer::new(fs::read_to_string(path).unwrap().as_str(), Some(file))
    }

    pub fn with_source(source: &str) -> Self {
        Lexer::new(source.trim(), None)
    }

    fn new(source: &str, file: Option<String>) -> Self {
        Lexer {
            buffer: source.chars().collect(),
            file,
            errors: Vec::new(),

            lines: Vec::new(),
            column: 0,

            tokens: Vec::new(),
            token_info: Vec::new(),

            literal_numbers: Vec::new(),
            literal_strings: Vec::new(),
            literal_chars: Vec::new(),

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
            None => abort!("No lines to read"),
            Some(last) => {
                if last.start + self.column >= last.end {
                    abort!("No more characters in the current line");
                }
                let n = self.buffer[last.start + self.column];
                self.column += 1;
                n
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

    fn get_column(&self, column: usize) -> usize {
        column + 1
    }

    fn keyword_or_ident(&mut self) {
        let mut value = String::new();
        let start = self.column;

        loop {
            match self.peek() {
                Some(c) if c.is_alphabetic() || c == '_' || c.is_ascii_digit() => {
                    value.push(self.next());
                }
                _ => break,
            }
        }

        let first = self.lines.last().unwrap().start;
        if value.is_empty()
            || (!value.chars().next().unwrap().is_alphabetic()
            && !value.starts_with('_'))
        {
            self.errors.push(err!(
                path=self.file,
                span=Location::new(
                    self.lines.len(),
                    self.get_column(start),
                    Span::new(first + start, first + start + value.len())
                ),
                format!(
                    "Expected alpha char or underscore: found {:?}",
                    value.chars().next().unwrap()
                )
            ));
            return;
        }

        // Check for TypeLiteral
        if value.chars().next().unwrap().is_alphabetic() && (value[1..]).chars().all(|c| c.is_ascii_digit()) {
            let kind = match value.chars().next().unwrap() {
                'f' => Some(TokenKind::FloatTypeLiteral),
                'i' => Some(TokenKind::IntegerTypeLiteral),
                'u' => Some(TokenKind::UnsignedTypeLiteral),
                _ => None,
            };

            if let Some(k) = kind {
                match u32::from_str_radix(&value[1..], 10) {
                    Ok(val) if [8, 16, 32, 64, 128].contains(&val) => {
                        self.tokens.push(Token(self.token_info.len()));
                        self.token_info.push(TokenInfo::new(
                            k,
                            self.lines.len(),
                            self.get_column(start),
                            Span::new(first + start, first + start + value.len()),
                            self.literal_numbers.len(),
                        ));
                        self.literal_numbers.push((value[1..]).to_string());
                    }
                    Ok(val) => {
                        let t = value.chars().next().unwrap();
                        self.errors.push(err!(
                            path=self.file,
                            span=Location::new(
                                self.lines.len(),
                                self.get_column(start),
                                Span::new(first + start, first + start + value.len())
                            ),
                            visual=ErrorCommand::Replace(
                                if val > 128 || 128 - val < 64 {
                                    format!("{}{}", t, 128)
                                } else if val > 64 || 64 - val < 32 {
                                    format!("{}{}", t, 64)
                                } else if val > 32 || 32 - val < 16 {
                                    format!("{}{}", t, 32)
                                } else if val > 16 || 16 - val < 8 {
                                    format!("{}{}", t, 16)
                                } else {
                                    format!("{}{}", t, 8)
                                }
                            ),
                            format!("Unknown type `{}{}`", t, &value[1..]),
                            help=[format!("Try using {t}8, {t}16, {t}32, {t}64, or {t}128 instead", t=t)]
                        ));
                    },
                    Err(e) => {
                        let t = value.chars().next().unwrap();
                        self.errors.push(err!(
                            path=self.file,
                            span=Location::new(
                                self.lines.len(),
                                self.get_column(start),
                                Span::new(first + start, first + start + value.len())
                            ),
                            visual=ErrorCommand::Replace(format!("{}32", t)),
                            format!("Unknown type `{}{}`", t, &value[1..]),
                            help=[format!("Try using {t}8, {t}16, {t}32, {t}64, or {t}128 instead", t=t)]
                        ));
                    }
                }
            }
            return;
        }

        // Keyword or Identifier
        let kind = match TokenKeyword::make(value.as_str()) {
            Some(keyword) => TokenKind::Keyword(keyword),
            None => TokenKind::Identifier,
        };

        let payload = match kind {
            TokenKind::Identifier => match self.ident_map.contains_key(&value) {
                true => self.ident_map.get(&value).unwrap().to_owned(),
                false => {
                    self.ident_map.insert(value.clone(), self.identifiers.len());
                    self.identifiers.push(value.clone());
                    self.identifiers.len() - 1
                }
            },
            _ => 0,
        };

        self.tokens.push(Token(self.token_info.len()));
        self.token_info
            .push(TokenInfo::new(
                kind,
                self.lines.len(),
                self.get_column(start),
                Span::new(first + start, first + start + value.len()),
                payload,
            ));
    }

    fn symbol(&mut self) {
        let mut value = String::new();
        let start = self.column;

        let mut symbol = None;
        // try longest to shortest combinations (Greedy)
        // Slowly build longest symbol possible until failure
        loop {
            let new = match self.peek() {
                Some(c) if !c.is_alphabetic() && !c.is_whitespace() && !c.is_ascii_digit() => {
                    value.push(c);
                    TokenSymbol::make(value.as_str())
                }
                _ => break,
            };

            if new.is_none() {
                value = value[..value.len() - 1].to_string();
                break;
            } else {
                let _ = self.next();
                symbol = new;
            }
        }

        let kind: Option<TokenKind> = symbol.map(TokenKind::Symbol);

        let first = self.lines.last().unwrap().start;
        match kind {
            Some(k) => {
                self.tokens.push(Token(self.token_info.len()));
                self.token_info.push(TokenInfo::new(
                    k,
                    self.lines.len(),
                    self.get_column(start),
                    Span::new(first + start, first + start + value.len()),
                    0, /* Possible open or close linking */
                ))
            }
            None => {
                self.errors.push(err!(
                    path=self.file,
                    span=Location::new(
                        self.lines.len(),
                        self.get_column(start),
                        Span::new(first + start, first + start + value.len())
                    ),
                    format!("Unknown symbol {:?}", value)
                ));
            }
        }
    }

    fn digit(&mut self) {
        let mut value = String::new();
        let start = self.column;

        loop {
            match self.peek() {
                Some(c) if c.is_ascii_digit() || c.is_alphabetic() || "_-+".contains(c) => {
                    value.push(self.next());
                }
                _ => break,
            }
        }

        let first = self.lines.last().unwrap().start;
        if NumberLiteral::verify(&value) {
            self.tokens.push(Token(self.token_info.len()));
            self.token_info.push(TokenInfo::new(
                TokenKind::Literal(TokenLiteral::Number),
                self.lines.len(),
                self.get_column(start),
                Span::new(first + start, first + start + value.len()),
                self.literal_numbers.len(),
            ));
            self.literal_numbers.push(value);
        } else {
            self.tokens.push(Token(self.token_info.len()));
            self.token_info.push(TokenInfo::new(
                TokenKind::Error,
                self.lines.len(),
                self.get_column(start),
                Span::new(first + start, first + start + value.len()),
                self.errors.len(),
            ));
            self.errors.push(err!(
                    path=self.file,
                    span=Location::new(
                        self.lines.len(),
                        self.get_column(start),
                        Span::new(first + start, first + start + value.len())
                    ),
                    "Invalid number format"
                )
            )
        }
    }

    pub fn string(&mut self) {
        let mut value = String::new();
        let start = (self.lines.len(), self.column);
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

        let first = self.lines.last().unwrap().start;
        self.tokens.push(Token(self.token_info.len()));
        self.token_info.push(TokenInfo::new(
            TokenKind::Literal(TokenLiteral::String),
            start.0,
            self.get_column(start.1),
            Span::new(first + start.1, first + start.1 + value.len()),
            self.literal_strings.len(),
        ));
        self.literal_strings.push(value);
    }

    fn char(&mut self) {
        let mut value = String::new();
        let start = self.column;
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
                    let first = self.lines.last().unwrap().start;
                    self.errors.push(err!(
                        path=self.file,
                        span=Location::new(
                            self.lines.len(),
                            self.get_column(start),
                            Span::new(first + start, first + start + value.len())
                        ),
                        "Unterminated character literal",
                        help=["Try adding `'`"]
                    ))
                }
                _ => {
                    value.push(self.next());
                }
            }
        }

        let first = self.lines.last().unwrap().start;
        if value.len() != 1 {
            self.errors.push(err!(
                path=self.file,
                span=Location::new(
                    self.lines.len(),
                    self.get_column(start),
                    Span::new(first + start, first + start + value.len())
                ),
                "Length of character literal must be 1"
            ));
        }

        self.tokens.push(Token(self.token_info.len()));
        self.token_info.push(TokenInfo::new(
            TokenKind::Literal(TokenLiteral::Char),
            self.lines.len(),
            self.get_column(start),
            Span::new(first + start, first + start + value.len()),
            self.literal_chars.len(),
        ));
        self.literal_chars.push(value.chars().next().unwrap());
    }

    pub fn lex(&mut self) {
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
            self.get_column(self.column),
            Span::new(0, self.buffer.len()),
            0,
        ));

        // Deallocate map memory
        self.ident_map = HashMap::new();
    }
}

impl From<&str> for Lexer {
    fn from(source: &str) -> Self {
        Lexer::new(source, None)
    }
}

impl From<String> for Lexer {
    fn from(source: String) -> Self {
        Lexer::new(source.as_str(), None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn empty_lex() {
        let mut lexer = Lexer::from("");
        lexer.lex();
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
        lexer.lex();
        assert_eq!(lexer.lines.len(), 4);
        assert_eq!(lexer.tokens.len(), 19);
        assert_eq!(lexer.token_info.len(), 19);
    }
}
