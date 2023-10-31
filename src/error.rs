use std::fmt::{Debug, Display};
use lazy_static::lazy_static;

pub(crate) use crate::abort;
use crate::{Location};

lazy_static! {
    static ref VERBOSE: bool = match std::env::var("NULL_VERBOSE") {
        Ok(val) => val == "1",
        Err(_) => true,
    };
}

/// Print error to stderr and exit
#[macro_export]
macro_rules! abort {
    ($($rest: tt)*) => {
        {
            eprintln!("{}", $crate::err!($($rest)*));
            std::process::exit(1);
        }
    };
}

#[macro_export]
macro_rules! err {
    ($($rest: tt)*) => {
       $crate::err_lvl!($($rest)*)
    };
}

#[macro_export]
macro_rules! err_lvl {
    ($($rest: tt)*) => {
        $crate::err_path!(Error, $($rest)*)
    };
    (ERROR: $($rest: tt)*) => {
        $crate::err_path!(Error, $($rest)*)
    };
    (WARN: $($rest: tt)*) => {
        $crate::err_path!(Warning, $($rest)*)
    };
}

#[macro_export]
macro_rules! err_path {
    ($lvl: ident, path=$path: expr, $($rest: tt)*) => {
        $crate::err_loc!($lvl, path=$path, $($rest)*)
    };
    ($lvl: ident, $($rest: tt)*) => {
        $crate::err_loc!($lvl, path=None, $($rest)*)
    };
}

#[macro_export]
macro_rules! err_loc {
    ($lvl: ident, path=$path: expr, span=$loc: expr, $($rest: tt)*) => {
        $crate::err_visual!($lvl, path=None, span=Some($loc), $($rest)*)
    };
    ($lvl: ident, path=$path: expr, $($rest: tt)*) => {
        $crate::err_visual!($lvl, path=None, span=None, $($rest)*)
    };
}

#[macro_export]
macro_rules! err_visual {
    ($lvl: ident, path=$path: expr, span=$loc: expr, visual=$visual: expr, $($rest: tt)*) => {
        $crate::err_help!($lvl, path=None, span=$loc, visual=$visual, $($rest)*)
    };
    ($lvl: ident, path=$path: expr, span=$loc: expr, $($rest: tt)*) => {
        $crate::err_help!($lvl, path=None, span=$loc, visual=$crate::error::ErrorVisualCommand::default(), $($rest)*)
    };
}

/// Print error to stderr and continue
#[macro_export]
macro_rules! err_help {
    ($lvl: ident, path=$path: expr, span=$loc: expr, visual=$visual: expr, $msg: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $loc.clone(),
            $path,
            $visual,
            $msg,
            Vec::new(),
        )
    };
    ($lvl: ident, path=$path: expr, span=$loc: expr, visual=$visual: expr, $msg: expr, help=[$($help: expr),*] $(,)*) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $loc.clone(),
            $path,
            $visual,
            $msg,
            vec![$($help.to_string(),)*],
        )
    };
    ($lvl: ident, path=$path: expr, span=$loc: expr, visual=$visual: expr, $msg: expr, help=$help: ident) => {
        $crate::error::Error::new(
            $crate::error::ErrorLevel::$lvl,
            $loc.clone(),
            $path,
            $visual,
            $msg,
            $help,
        )
    };
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorType {
    SyntaxError,
    ParseError,
    CompilerError,
}

#[derive(Debug, Clone, Default)]
pub enum ErrorVisualCommand {
    #[default]
    Invalid,
    Replace(String),
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\x1b[3m{}\x1b[23m", match self {
            ErrorType::SyntaxError => "Invalid Syntax",
            ErrorType::ParseError => "Parse Error",
            ErrorType::CompilerError => "Compile Error",
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorLevel {
    Error,
    Warning,
}

impl Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            ErrorLevel::Error => "\x1b[31;1merror\x1b[22;39m",
            ErrorLevel::Warning => "\x1b[33;1mwarning\x1b[22;39m",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Error {
    pub level: ErrorLevel,
    pub visual: ErrorVisualCommand,
    pub location: Option<Location>,
    pub message: String,
    pub help: Vec<String>,
    pub file: Option<String>,
}

impl Error {
    pub fn new<T: ToString>(
        level: ErrorLevel,
        location: Option<Location>,
        file: Option<String>,
        visual: ErrorVisualCommand,
        message: T,
        help: Vec<String>,
    ) -> Error {
        Error {
            level,
            location,
            file,
            message: message.to_string(),
            help,
            visual
        }
    }

    pub fn compact(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}{} \x1b[1m{}\x1b[22m",
               self.level,
               match &self.file {
                   Some(path) => format!("{}", path),
                   None => "\x1b[3;38;5;246m%source%\x1b[39m".to_string(),
               },
               match &self.location {
                   Some(location) => format!(":[{}:{}]", location.line, location.column),
                   None => "".to_string(),
               },
               self.message,
        )
    }

    pub fn verbose(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: \x1b[1m{}\x1b[22m\n{}{}\x1b[23m{}",
               self.level,
               self.message,
               match &self.file {
                   Some(path) => format!("  → {}", path),
                   None => "  → \x1b[3;38;5;246m%source%\x1b[39m".to_string(),
               },
               match &self.location {
                   Some(location) => format!(":[{}:{}]", location.line, location.column),
                   None => "".to_string(),
               },
               if self.help.is_empty() {
                   "".to_string()
               } else {
                   format!("\n  {}", self.help.iter().map(|s| format!("\x1b[3;35mhelp\x1b[23;39m: {}", s)).collect::<Vec<String>>().join("\n"))
               }
        )
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *VERBOSE {
            self.verbose(f)
        } else {
            self.compact(f)
        }
    }
}

impl std::error::Error for Error {}

pub struct Errors(pub Vec<Error>);

impl Errors {
    pub fn render(&self, source: &[char]) {
        for error in &self.0 {
            eprint!("{}", error);
            if let Some(location) = error.location.as_ref() {
                eprint!("\n  |\n{} | \x1b[31m{}\x1b[39m\n  |", location.line, source[location.span.start()..location.span.end()].iter().collect::<String>());
            }
            eprintln!();
        }
        eprintln!()
    }
}