// Args: [... impl Display]
// fmt : "Some string {} with formatting"
//
// String::from(fmt) or fmt if variable
// Args any size
//
// Validate size of array with number of found injections
//
// Injection == {} that are odd numbers, so {{ excape to normal { and }} escapes to }
// Need peekable by 1 byte walk through
use into_param::*;
use std::{any::Any, collections::HashMap, fmt::Display};

pub mod into_param {
    use super::Arg;
    use std::collections::HashMap;

    pub trait IntoParam<T> {
        fn into_param(self) -> T;
    }

    pub type ArgType<'a> = Option<(Option<HashMap<String, Arg<'a>>>, Option<Vec<Arg<'a>>>)>;

    impl<'a> IntoParam<ArgType<'a>> for Vec<Arg<'a>> {
        fn into_param(self) -> ArgType<'a> {
            Some((None, Some(self)))
        }
    }

    impl<'a> IntoParam<ArgType<'a>> for HashMap<String, Arg<'a>> {
        fn into_param(self) -> ArgType<'a> {
            Some((Some(self), None))
        }
    }

    impl<'a, N, P> IntoParam<Option<(N, P)>> for Option<(N, P)>
    where
        N: IntoParam<Option<HashMap<String, Arg<'a>>>>,
        P: IntoParam<Option<Vec<Arg<'a>>>>,
    {
        fn into_param(self) -> Option<(N, P)> {
            self
        }
    }

    impl<'a, N, P> IntoParam<Option<(N, P)>> for (N, P)
    where
        N: IntoParam<Option<HashMap<String, Arg<'a>>>>,
        P: IntoParam<Option<Vec<Arg<'a>>>>,
    {
        fn into_param(self) -> Option<(N, P)> {
            Some(self)
        }
    }

    impl<'a> IntoParam<Option<HashMap<String, Arg<'a>>>> for Option<HashMap<String, Arg<'a>>> {
        fn into_param(self) -> Option<HashMap<String, Arg<'a>>> {
            self
        }
    }

    impl<'a> IntoParam<Option<HashMap<String, Arg<'a>>>> for HashMap<String, Arg<'a>> {
        fn into_param(self) -> Option<HashMap<String, Arg<'a>>> {
            Some(self)
        }
    }

    impl<'a> IntoParam<Option<Vec<Arg<'a>>>> for Option<Vec<Arg<'a>>> {
        fn into_param(self) -> Option<Vec<Arg<'a>>> {
            self
        }
    }

    impl<'a> IntoParam<Option<Vec<Arg<'a>>>> for Vec<Arg<'a>> {
        fn into_param(self) -> Option<Vec<Arg<'a>>> {
            Some(self)
        }
    }
}

pub mod format {

    /// Walk the format string and find the text along with the captures and what
    /// is in the format captures. The returned Vec's are slices into the original
    /// string to save on memory and to keep things "fast".
    pub fn walk<'a>(fmt: &'a str) -> (Vec<&'a str>, Vec<&'a str>) {
        let mut parts: Vec<&str> = Vec::new();
        let mut captures: Vec<&str> = Vec::new();

        let mut chars = fmt.chars().peekable();
        let mut previous = 0;
        let mut current = 0;

        while let Some(ch) = chars.next() {
            match ch {
                '{' if chars.peek() == Some(&'{') => {
                    let _ = chars.next();
                    current += 1
                }
                '{' => {
                    parts.push(&fmt[previous..current]);
                    previous = current;
                    let mut balance = 1;

                    'inner: while let Some(c) = chars.next() {
                        match c {
                            '}' if chars.peek() == Some(&'}') => {
                                let _ = chars.next();
                                current += 2
                            }
                            '}' => {
                                balance -= 1;
                                current += 1;
                                if balance == 0 {
                                    break 'inner;
                                }
                            }
                            _ => {
                                current += 1;
                            }
                        }
                    }
                    captures.push(&fmt[previous + 1..current]);
                    previous = current + 1;
                }
                _ => {}
            }
            current += 1;
        }
        if previous < current {
            parts.push(&fmt[previous..current]);
        }

        (parts, captures)
    }
}

pub mod macros {
    #[macro_export]
    macro_rules! pos {
    ($($val: expr),* $(,)?) => {
        {
            let mut _temp: Vec<$crate::Arg> = Vec::new();
            $(
                _temp.push(Box::new(&$val));
            )*
            _temp
        }
    };
}

    #[macro_export]
    macro_rules! named {
    ($name: ident $(,)? $($rest: tt)*) => {
        {
            let mut _hm: std::collections::HashMap<String, $crate::Arg> = std::collections::HashMap::new();
            _hm.insert(stringify!($name).to_string(), Box::new(&$name));
            named!{
                @hash _hm,
                $($rest)*
            }
            _hm
        }
    };
    ($name: ident: $val: ident $(,)? $($rest: tt)*) => {
        {
            let mut _hm: std::collections::HashMap<String, $crate::Arg> = std::collections::HashMap::new();
            _hm.insert(stringify!($name).to_string(), Box::new(&$val));
            named!{
                @hash _hm,
                $($rest)*
            }
            _hm
        }
    };
    ($name: ident: $val: literal $(,)? $($rest: tt)*) => {
        {
            let mut _hm: std::collections::HashMap<String, $crate::Arg> = std::collections::HashMap::new();
            _hm.insert(stringify!($name).to_string(), Box::new(&$val));
            named!{
                @hash _hm,
                $($rest)*
            }
            _hm
        }
    };
    (@hash $hm: ident, $name: ident, $($rest: tt)*) => {
        $hm.insert(stringify!($name).to_string(), Box::new(&$name));
        named!{
            @hash $hm,
            $($rest)*
        }
    };
    (@hash $hm: ident, $name: ident: $val: expr, $($rest: tt)*) => {
        $hm.insert(stringify!($name).to_string(), Box::new(&$val));
        named!{
            @hash $hm,
            $($rest)*
        }
    };
    (@hash $hm: ident, $name: ident $(,)?) => {
        $hm.insert(stringify!($name).to_string(), Box::new(&$name));
    };
    (@hash $hm: ident, $name: ident: $val: expr $(,)?) => {
        $hm.insert(stringify!($name).to_string(), Box::new(&$val));
    };
    (@hash $hm: ident $(,)?) => {}
}

    #[macro_export]
    macro_rules! args {
    ($val: literal, $($rest: tt)*) => {
        {
            use std::collections::HashMap;
            let mut _hm_: std::collections::HashMap<String, $crate::Arg> = std::collections::HashMap::new();
            let mut _pos_: Vec<$crate::Arg> = Vec::new();
            _pos_.push(Box::new(&$val));
            args!{
                @hash _hm_,
                @pos _pos_,
                $($rest)*
            }
            (_hm_, _pos_)
        }
    };
    ($val: ident, $($rest: tt)*) => {
        {
            let mut _hm_: std::collections::HashMap<String, $crate::Arg> = std::collections::HashMap::new();
            let mut _pos_: Vec<$crate::Arg> = Vec::new();
            _pos_.push(Box::new(&$val));
            args!{
                @hash _hm_,
                @pos _pos_,
                $($rest)*
            }
            (_hm_, _pos_)
        }
    };
    ($name: ident : $val: expr, $($rest: tt)*) => {
        {
            let mut _hm_: std::collections::HashMap<String, Arg> = std::collections::HashMap::new();
            let mut _pos_: Vec<$crate::Arg> = Vec::new();
            _hm_.insert(stringify!($name).to_string(), Box::new(&$val));
            args!{
                @hash _hm_,
                @pos _pos_,
                $($rest)*
            }
            (_hm_, _pos_)
        }
    };
    (@hash $hm: ident, @pos $pos: ident, $val: ident, $($rest: tt)*) => {
        $pos.push(Box::new(&$val));
        args!{
            @hash $hm,
            @pos $pos,
            $($rest)*
        }
    };
    (@hash $hm: ident, @pos $pos: ident, $val: literal, $($rest: tt)*) => {
        $pos.push(Box::new(&$val));
        args!{
            @hash $hm,
            @pos $pos,
            $($rest)*
        }
    };
    (@hash $hm: ident, @pos $pos: ident, $name: ident : $val: expr, $($rest: tt)*) => {
        $hm.insert(stringify!($name).to_string(), Box::new(&$val));
        args!{
            @hash $hm,
            @pos $pos,
            $($rest)*
        }
    };
    (@hash $hm: ident, @pos $pos: ident, $val: ident $(,)?) => {
        $pos.push(Box::new(&$val));
    };
    (@hash $hm: ident, @pos $pos: ident, $val: literal $(,)?) => {
        $pos.push(Box::new(&$val));
    };
    (@hash $hm: ident, @pos $pos: ident, $name: ident : $val: expr $(,)?) => {
        $hm.insert(stringify!($name).to_string(), Box::new(&$val));
    };
    (@hash $hm: ident, @pos $pos: ident $(,)?) => {}
}
}

pub type Arg<'a> = Box<&'a dyn Any>;
pub enum Error<T: ToString> {
    CompatTypeError(T),
    SyntaxError(T),
}

impl<T: ToString> Display for Error<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CompatTypeError(msg) | Self::SyntaxError(msg) => write!(f, "{}", msg.to_string()),
        }
    }
}

pub trait RFmtString {
    fn fmt<'a, A, N, P>(&'a self, args: A) -> String
    where
        A: IntoParam<Option<(N, P)>>,
        N: IntoParam<Option<HashMap<String, Arg<'a>>>>,
        P: IntoParam<Option<Vec<Arg<'a>>>>;
}

impl RFmtString for &str {
    fn fmt<'a, A, N, P>(&'a self, args: A) -> String
    where
        A: IntoParam<Option<(N, P)>>,
        N: IntoParam<Option<HashMap<String, Arg<'a>>>>,
        P: IntoParam<Option<Vec<Arg<'a>>>>,
    {
        let (test, captures) = format::walk(self);
        let args = Arguments::new(captures, args.into_param());
        println!("{:?}\n{:?}", test, args);

        String::new()
    }
}
impl RFmtString for String {
    fn fmt<'a, A, N, P>(&'a self, args: A) -> String
    where
        A: IntoParam<Option<(N, P)>>,
        N: IntoParam<Option<HashMap<String, Arg<'a>>>>,
        P: IntoParam<Option<Vec<Arg<'a>>>>,
    {
        let (test, captures) = format::walk(self);
        let args = Arguments::new(captures, args.into_param());
        println!("{:?}\n{:?}", test, args);

        String::new()
    }
}

#[derive(Debug)]
pub struct Arguments<'a> {
    named: Option<HashMap<String, Arg<'a>>>,
    positional: Option<Vec<Arg<'a>>>,
    fmt_args: Vec<FArg<'a>>,
}

impl<'a> Arguments<'a> {
    fn new<N, P>(captures: Vec<&'a str>, args: Option<(N, P)>) -> Self
    where
        N: IntoParam<Option<HashMap<String, Arg<'a>>>>,
        P: IntoParam<Option<Vec<Arg<'a>>>>,
    {
        let mut args = match args {
            Some(a) => Arguments {
                named: a.0.into_param(),
                positional: a.1.into_param(),
                fmt_args: Vec::new(),
            },
            None => Arguments {
                named: None,
                positional: None,
                fmt_args: Vec::new(),
            },
        };
        for capture in captures {
            match FArg::parse(capture) {
                Ok(arg) => args.fmt_args.push(arg),
                Err(e) => panic!("{}", e),
            }
        }
        args
    }
}

#[derive(Debug, Default)]
pub enum ArgType<'a> {
    #[default]
    Next,
    Positional(usize),
    Named(&'a str),
}

#[derive(Debug, Default)]
pub enum FormatType {
    /// `b` formatting
    Binary,
    /// `?` formatting
    Debug,
    /// Empty format, `{}`
    #[default]
    Display,
    /// `e` formatting
    LowerExp,
    /// `x` formatting
    LowerHex,
    /// `x?` formatting
    LowerHexDebug,
    /// `o` formatting
    Octal,
    /// `p` formatting
    Pointer,
    /// `E` formatting
    UpperExp,
    /// `X` formatting
    UpperHex,
    /// `X?` formatting
    UpperHexDebug,
}

impl From<&str> for FormatType {
    fn from(value: &str) -> Self {
        match value {
            "?" => FormatType::Debug,
            "x?" => FormatType::LowerHexDebug,
            "X?" => FormatType::UpperHexDebug,
            "x" => FormatType::LowerHex,
            "X" => FormatType::UpperHex,
            "o" => FormatType::Octal,
            "b" => FormatType::Binary,
            "e" => FormatType::LowerExp,
            "E" => FormatType::UpperExp,
            "p" => FormatType::Pointer,
            _ => FormatType::Display,
        }
    }
}

#[derive(Debug, Default)]
pub enum Align {
    #[default]
    Left,
    Center,
    Right,
}

#[derive(Debug, Default)]
pub enum Precision<'a> {
    #[default]
    None,
    Count(usize),
    Name(&'a str),
    Next,
}

/// [[fill]
#[derive(Debug)]
pub struct FArg<'a> {
    atype: ArgType<'a>,       // Specific Position, Position, or Named
    ftype: FormatType,        // b|?|``|e|x|o|E|X
    num_fill: bool,           // '0'
    fill: &'a str,            // ' ' or '0'
    width: usize,             // `\d`
    align: Align,             // `<`, `^`, or `>`
    sign: bool,               // `+` and `-` for completness
    alternate: bool,          // `#` is present
    precision: Precision<'a>, // `*`, `\d`, or `ident`
}

#[derive(Debug, Clone, Copy)]
pub enum Step {
    Sign = 1 << 1,
    Alt = 1 << 2,
    NFill = 1 << 3,
    Width = 1 << 4,
    Precision = 1 << 5,
    Type = 1 << 6,
}

impl Step {
    pub fn done(state: u32, flag: u32) -> bool {
        return state & flag == flag;
    }

    pub fn get(flag: u32) -> Step {
        match flag {
            f if f == Step::Sign as u32 => Step::Sign,
            f if f == Step::Alt as u32 => Step::Alt,
            f if f == Step::NFill as u32 => Step::NFill,
            f if f == Step::Width as u32 => Step::Width,
            f if f == Step::Precision as u32 => Step::Precision,
            f if f == Step::Type as u32 => Step::Type,
            f => panic!("Not a Step flag: {}", f),
        }
    }

    pub fn name(step: Step) -> &'static str {
        match step {
            Step::Sign => "sign",
            Step::Alt => "alternate",
            Step::NFill => "number padding",
            Step::Width => "width",
            Step::Precision => "precision",
            Step::Type => "type",
        }
    }

    pub fn next(state: u32, flag: Step) -> Step {
        let mut index = flag as u32;
        loop {
            if Step::done(state, index) {
                return Step::get(index);
            }
            index *= 2;
            if index > Step::Type as u32 {
                break;
            }
        }
        return flag;
    }
}

impl<'a> FArg<'a> {
    pub fn parse(capture: &'a str) -> Result<Self, Error<String>> {
        let argument;
        let spec;
        match capture.contains(":") {
            true => {
                let colon = capture.find(":").unwrap();
                argument = &capture[..colon];
                spec = &capture[colon + 1..];
            }
            _ => {
                argument = capture;
                spec = "";
            }
        };

        let mut farg = FArg::default();

        // Validate argument syntax
        if let Some(c) = argument.chars().nth(0) {
            if c.is_digit(10) {
                match u32::from_str_radix(argument, 10) {
                    Ok(size) => farg.atype = ArgType::Positional(size as usize),
                    Err(_) => {
                        return Err(Error::SyntaxError(
                            "RFMT expected capture argument to be a valid digit.".to_string(),
                        ));
                    }
                }
            } else if !c.is_alphabetic() && c != '_' {
                return Err(Error::SyntaxError(
                    "RFMT expected capture argument to be a valid identifier.".to_string(),
                ));
            } else {
                farg.atype = ArgType::Named(argument)
            }
        }

        // Iterate over spec assigning them to the FArg
        // https://doc.rust-lang.org/std/fmt/index.html#derives
        if spec.len() > 0 {
            let offset = if let Some(i) = spec.find("<") {
                farg.align = Align::Left;
                i
            } else if let Some(i) = spec.find("^") {
                farg.align = Align::Center;
                i
            } else if let Some(i) = spec.find(">") {
                farg.align = Align::Right;
                i
            } else {
                0
            };

            if offset > 0 {
                farg.fill = &spec[..offset];
            }
            let rest = &spec[offset + 1..];

            let error_msg = |sf, st| {
                format!(
                    "RFMT expected {} formatting before {} formatting",
                    Step::name(sf),
                    Step::name(st),
                )
            };

            let mut steps = 0;
            // sign alt 0 width prec type
            if rest.len() > 0 {
                let mut schr = rest.chars().peekable();
                while let Some(c) = schr.next() {
                    match c {
                        '.' => {
                            if steps > Step::Precision as u32 {
                                return Err(Error::SyntaxError(error_msg(
                                    Step::Precision,
                                    Step::next(steps, Step::Precision),
                                )));
                            }
                            /* TODO: precision */
                            steps |= Step::Precision as u32;
                        }
                        '#' => {
                            if steps > Step::Alt as u32 {
                                return Err(Error::SyntaxError(error_msg(
                                    Step::Alt,
                                    Step::next(steps, Step::Alt),
                                )));
                            }
                            farg.alternate = true;
                            steps |= Step::Alt as u32;
                        }
                        '0' => {
                            if steps > Step::NFill as u32 {
                                return Err(Error::SyntaxError(error_msg(
                                    Step::NFill,
                                    Step::next(steps, Step::NFill),
                                )));
                            }
                            farg.num_fill = true;
                            steps |= Step::NFill as u32;
                        }
                        '1'..='9' => {
                            if steps > Step::Width as u32 {
                                return Err(Error::SyntaxError(error_msg(
                                    Step::Width,
                                    Step::next(steps, Step::Width),
                                )));
                            }
                            /* TODO: width */
                            steps |= Step::Width as u32;
                        }
                        '+' | '-' => {
                            if steps > Step::Sign as u32 {
                                return Err(Error::SyntaxError(error_msg(
                                    Step::Sign,
                                    Step::next(steps, Step::Sign),
                                )));
                            }
                            if c == '+' {
                                farg.sign = true;
                            }
                            steps |= Step::Sign as u32;
                        }
                        'b' | 'e' | 'o' | 'x' | 'E' | 'X' | '?' => {
                            /* TODO: type */
                            steps |= Step::Type as u32;
                        }
                        // TODO: Error because unkown syntax
                        _ => break,
                    }
                }
            }
        }

        Ok(farg)
    }
}

impl<'a> Default for FArg<'a> {
    fn default() -> Self {
        FArg {
            atype: ArgType::default(),
            ftype: FormatType::default(),
            fill: " ",
            num_fill: false,
            width: 0,
            align: Align::default(),
            sign: false,
            alternate: false,
            precision: Precision::default(),
        }
    }
}
