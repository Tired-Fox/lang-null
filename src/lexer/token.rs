macro_rules! kinds {
    ($name: ident, $($kind: ident : $spelling: literal),* $(,)*) => {
        /// Construct a TokenKind sub enum.
        ///
        /// This will build the enum and set the doc of each entry to the mapped string.
        /// This will also create a `make` method for the enum to get the entry for a given string.
        /// The mapped string is used to match the enum. The result is a Option<$name> where $name
        /// is the enums name. If the string is not an option, the None is returned.
        #[derive(Debug, Eq, PartialEq, Copy, Clone)]
        pub enum $name {
            $(
                #[doc=$spelling]
                $kind,
            )*
        }

        impl $name {
            pub fn make(key: &str) -> Option<$name> {
                match key {
                    $(
                        $spelling => Some(Self::$kind),
                    )*
                    _ => None,
                }
            }

            pub fn compare(&self, other: &str) -> bool {
                match self {
                    $(
                        Self::$kind if $spelling == other => true,
                    )*
                    _ => false,
                }
            }
        }
    };
}
macro_rules! kindn {
    ($name: ident, $($kind: ident <$match: literal>: $doc: literal),* $(,)*) => {
        /// Construct a TokenKind sub enum.
        ///
        /// This will build the enum and set the doc of each entry to the mapped string.
        /// This will also create a `make` method for the enum to get the entry for a given string.
        /// The string is a match if the lowercase string of the entries name matches. The result
        /// is a Option<$name> where $name is the enums name. If the string is not an option,
        /// the None is returned.
        #[derive(Debug, Eq, PartialEq, Copy, Clone)]
        pub enum $name {
            $(
                #[doc=$doc]
                $kind,
            )*
        }

        impl $name {
            pub fn make(key: &str) -> Option<$name> {
                match key {
                    $(
                        $match => Some(Self::$kind),
                    )*
                    _ => None,
                }
            }
        }
    };
}

macro_rules! kind {
    ($name: ident, $($kind: ident: $doc: literal),* $(,)*) => {
        /// Construct a TokenKind sub enum.
        ///
        /// This will build the enum and set the doc of each entry to the mapped string.
        #[derive(Debug, Eq, PartialEq, Copy, Clone)]
        pub enum $name {
            $(
                #[doc=$doc]
                $kind,
            )*
        }
    };
}

kinds! {
    TokenSymbol,
    Amp: "&",
    AmpAmp: "&&",
    AmpEqual: "&=",
    Backslash: "\\",
    Bang: "!",
    BangEqual: "!=",
    Caret: "^",
    CaretEqual: "^=",
    CloseBrace: "}",
    CloseParen: ")",
    CloseSquare: "]",
    Colon: ":",
    ColonColon: "::",
    Comma: ",",
    Dot: ".",
    DotDot: "..",
    DotDotEqual: "..=",
    Ellipse: "...",
    Equal: "=",
    EqualEqual: "==",
    EqualGreater: "=>",
    Greater: ">",
    GreaterEqual: ">=",
    GreaterGreater: ">>",
    GreaterGreaterEqual: ">>=",
    GreaterGreaterGreater: ">>>",
    Less: "<",
    LessEqual: "<=",
    LessEqualGreater: "<=>",
    LessLess: "<<",
    LessLessEqual: "<<=",
    LessLessLess: "<<<",
    Minus: "-",
    MinusEqual: "-=",
    MinusMinus: "--",
    OpenBrace: "{",
    OpenParen: "(",
    OpenSquare: "[",
    Percent: "%",
    PercentEqual: "%=",
    Pipe: "|",
    PipeEqual: "|=",
    PipePipe: "||",
    Plus: "+",
    PlusEqual: "+=",
    Question: "?",
    Semicolon: ";",
    Slash: "/",
    SlashEqual: "/=",
    Star: "*",
    StarEqual: "*=",
    Tilde: "~",
    TildeEqual: "~=",
}

kindn! {
    TokenKeyword,
    And<"and">: "Equivalent to `&&`. Check for both values being truthy",
    Break<"break">: "Break out of context",
    Const<"const">: "A value that cannot have it's data changed",
    Continue<"continue">: "Go to next iteration of a loop",
    Elif<"elif">: "Optional conditional branch",
    Else<"else">: "Default case of a context",
    Enum<"enum">: "Named set of values",
    False<"false">: "Falsy",
    Fn<"fn">: "Defines a procedure",
    For<"for">: "Defines a loop that manages it's own state",
    If<"if">: "Starts a conditional branch",
    Import<"import">: "imports another module",
    In<"in">: "Compares to check if value is in a collection",
    Is<"is">: "Checks the type of the value",
    Let<"let">: "Defines a immutable variable",
    Match<"match">: "Switch statement with pattern matching on a value",
    Not<"not">: "Equivalent to `!`. Negates/Flips the truthiness of a value",
    Or<"or">: "Equivalent to `||`. Check for one of the values to be truthy",
    Pub<"pub">: "Defines a variable or procedure as visible to other modules",
    Return<"return">: "Pass a value back from the current context",
    Struct<"struct">: "Structure of data. Can be initialized with values and have methods attached",
    True<"true">: "Truthy",
    Type<"type">: "A alias for another type or struct",
    While<"while">: "Defines a stateless loop",
}

kind! {
    TokenLiteral,
    Number: r#"All formats support underscores, and all but binary negative sign and decimal point syntax.
and all but binary and hex support scientific notation. Number types are specifies with; `.` for float, `0x` for hexadecimal, `0b` for binary, and `0o` for octal. If non of those are specified then it is an integer. The type of the number may be embedded in the value.

# Example
- Decimal: `-123_23.00e4`
- Hex: `0xFF_A03.F`
- Binary: `0b1001_0101`
- Octal: `0o123_123.74e-3`"#,
    String: "Ex: `\"Hello, world!\"`",
    Char: "A single character"
}

#[derive(Debug, Clone, Copy)]
pub struct Token(pub usize);

#[derive(Debug)]
pub struct TokenInfo {
    /// The kind of token
    pub kind: TokenKind,
    /// The line in the source the token occurs on
    pub line: usize,
    /// The offset in the tokens line
    pub column: usize,
    /// Index into additional info list or length of error in source
    pub payload: usize,
}

impl TokenInfo {
    pub fn new(kind: TokenKind, line: usize, column: usize, payload: usize) -> TokenInfo {
        TokenInfo {
            kind,
            line,
            column,
            payload,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    /// String of reserved non alphabetical characters
    Symbol(TokenSymbol),
    /// Number or String
    Literal(TokenLiteral),
    /// Reserved identifiers that have special meaning to the language
    Keyword(TokenKeyword),
    /// A non symbol, or literal string of characters
    Identifier,

    IntegerTypeLiteral,
    UnsignedTypeLiteral,
    FloatTypeLiteral,

    Error,
    EOF,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_keyword() {
        let keyword = "while";
        assert_eq!(TokenKeyword::make(keyword), Some(TokenKeyword::While));
    }
    #[test]
    fn make_keyword_fail() {
        let keyword = "not-valid-keyword";
        assert_eq!(TokenKeyword::make(keyword), None);
    }

    #[test]
    fn make_symbol() {
        let symbol = "<<=";
        assert_eq!(TokenSymbol::make(symbol), Some(TokenSymbol::LessLessEqual));
    }
    #[test]
    fn make_symbol_fail() {
        let symbol = "n";
        assert_eq!(TokenSymbol::make(symbol), None);
    }
}

#[derive(Debug)]
pub struct ErrorInfo {
    pub size: usize,
    pub message: String,
}
impl ErrorInfo {
    pub fn new(size: usize, message: String) -> ErrorInfo {
        ErrorInfo { size, message }
    }
}

pub struct NumberLiteral;
impl NumberLiteral {
    pub fn verify(value: &String) -> bool {
        let mut chars = value.chars();
        if chars.next().unwrap() == '0' {
            match value.chars().next() {
                Some('x') => {
                    let mut decimal = false;
                    while let Some(c) = chars.next() {
                        match c {
                            'a'..='f' | 'A'..='F' | '0'..='9' | '_' => {}
                            '.' => {
                                if decimal {
                                    return false;
                                }
                                decimal = true;
                            }
                            _ => {
                                return false;
                            }
                        }
                    }
                }
                Some('o') => {
                    let mut decimal = false;
                    let mut exponent = false;
                    let mut plus_minus = false;

                    while let Some(c) = chars.next() {
                        match c {
                            '0'..='7' | '_' => {}
                            '.' => {
                                if decimal {
                                    return false;
                                }
                                decimal = true;
                            }
                            '-' | '+' => {
                                if !exponent || plus_minus {
                                    return false;
                                }
                                plus_minus = true;
                            }
                            'e' => {
                                if exponent {
                                    return false;
                                }
                                exponent = true;
                                decimal = true;
                            }
                            _ => {
                                return false;
                            }
                        }
                    }
                }
                Some('b') => {
                    while let Some(c) = chars.next() {
                        match c {
                            '0'..='1' | '_' => {}
                            _ => {
                                return false;
                            }
                        }
                    }
                }
                Some(_) => {
                    let mut decimal = false;
                    let mut exponent = false;
                    let mut plus_minus = false;

                    while let Some(c) = chars.next() {
                        match c {
                            '0'..='9' | '_' => {}
                            '.' => {
                                if decimal {
                                    return false;
                                }
                                decimal = true;
                            }
                            '-' | '+' => {
                                if !exponent || plus_minus {
                                    return false;
                                }
                                plus_minus = true;
                            }
                            'e' => {
                                if exponent {
                                    return false;
                                }
                                exponent = true;
                                decimal = true;
                            }
                            _ => {
                                return false;
                            }
                        }
                    }
                }
                None => {}
            }
        } else {
            let mut decimal = false;
            let mut exponent = false;
            let mut plus_minus = false;

            while let Some(c) = chars.next() {
                match c {
                    '0'..='9' | '_' => {}
                    '.' => {
                        if decimal {
                            return false;
                        }
                        decimal = true;
                    }
                    '-' | '+' => {
                        if !exponent || plus_minus {
                            return false;
                        }
                        plus_minus = true;
                    }
                    'e' => {
                        if exponent {
                            return false;
                        }
                        exponent = true;
                        decimal = true;
                    }
                    _ => {
                        return false;
                    }
                }
            }
        }
        true
    }
}
