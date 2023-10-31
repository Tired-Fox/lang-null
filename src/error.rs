use std::fmt::{Debug, Display};

pub(crate) use crate::abort;
use crate::{Location, Span};

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
        $crate::new_error!(Error, $($rest)*)
    };
    (ERROR: $($rest: tt)*) => {
        $crate::new_error!(Error, $($rest)*)
    };
    (WARN: $($rest: tt)*) => {
        $crate::new_error!(Warning, $($rest)*)
    };
}


/// Print error to stderr and continue
#[macro_export]
macro_rules! new_error {
    ($lvl: ident, $type: ident, $msg: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            None,
            None,
            $msg,
            Vec::new(),
        )
    };
    ($lvl: ident, $type: ident, path=$path: expr, $msg: expr) => {
        $crate::error::Error::new(
            $crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            None,
            Some($path.to_string()),
            $msg,
            Vec::new(),
        )
    };
    ($lvl: ident, $type: ident, $loc: expr, $msg: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            Some($loc.clone()),
            None,
            $msg,
            Vec::new(),
        )
    };
    ($lvl: ident, $type: ident, path=$path: expr, $loc: expr, $msg: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            Some($loc.clone()),
            Some($path.to_string()),
            $msg,
            Vec::new(),
        )
    };
    ($lvl: ident, $type: ident, $msg: expr, $(help=$help: expr),* $(,)*) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            None,
            None,
            $msg,
            vec![$($help.to_string(),)*],
        )
    };
    ($lvl: ident, $type: ident, path=$path: expr, $msg: expr, $(help=$help: expr),* $(,)*) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            None,
            Some($path.to_string()),
            $msg,
            vec![$($help.to_string(),)*],
        )
    };
    ($lvl: ident, $type: ident, $loc: expr, $msg: expr, $(help=$help: expr),* $(,)*) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            Some($loc.clone()),
            None,
            $msg,
            vec![$($help.to_string(),)*],
        )
    };
    ($lvl: ident, $type: ident, path=$path: expr, $loc: expr, $msg: expr, $(help=$help: expr),* $(,)*) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            Some($loc.clone()),
            Some($path.to_string()),
            $msg,
            vec![$($help.to_string(),)*],
        )
    };
    ($lvl: ident, $type: ident, $msg: expr, $help: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            None,
            None,
            $msg,
            $help,
        )
    };
    ($lvl: ident, $type: ident, path=$path: expr, $msg: expr, $help: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            None,
            Some($path.to_string()),
            $msg,
            $help,
        )
    };
    ($lvl: ident, $type: ident, $loc: expr, $msg: expr, $help: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            Some($loc.clone()),
            None,
            $msg,
            $help,
        )
    };
    ($lvl: ident, $type: ident, path=$path: expr, $loc: expr, $msg: expr, $help: expr) => {
        $crate::error::Error::new($crate::error::ErrorLevel::$lvl,
            $crate::error::ErrorType::$type,
            Some($loc.clone()),
            Some($path.to_string()),
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
    pub etype: ErrorType,
    pub level: ErrorLevel,
    pub location: Option<Location>,
    pub message: String,
    pub help: Vec<String>,
    pub file: Option<String>,
}

impl Error {
    pub fn new<T: ToString>(
        level: ErrorLevel,
        etype: ErrorType,
        location: Option<Location>,
        file: Option<String>,
        message: T, help: Vec<String>,
    ) -> Error {
        Error {
            etype,
            level,
            location,
            file,
            message: message.to_string(),
            help,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {} - {}\n\x1b[3;38;5;246m{}\x1b[23;39m{}{}",
               self.level,
               self.etype,
               self.message,
               match &self.file {
                   Some(path) => format!("  {}", path),
                   None => "  {source}".to_string(),
               },
               match &self.location {
                   Some(location) => format!(":\x1b[36m{}\x1b[39m", location),
                   None => "".to_string(),
               },
               if self.help.is_empty() {
                   "".to_string()
               } else {
                   format!("\n  {}", self.help.iter().map(|s| format!("- \x1b[3;35mhelp\x1b[23;39m: {}", s)).collect::<Vec<String>>().join("\n"))
               }
        )
    }
}

impl std::error::Error for Error {}