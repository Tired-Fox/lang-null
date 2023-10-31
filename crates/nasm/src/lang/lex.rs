use std::fmt::Display;

use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro_error::abort;
use quote::ToTokens;
use syn::{braced, bracketed, ext::IdentExt, Ident, Lit, LitInt, LitStr, parenthesized, token::{Brace, Bracket, Paren}, Token};

use super::Expr;

#[derive(Debug, Clone)]
pub enum Symbol {
    Comma(Span),
    Dot(Span),
    Plus(Span),
    Minus(Span),
    Star(Span),
    Slash(Span),
    Percent(Span),
    Dollar(Span),
    Colon(Span),
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Comma(_) => ",",
                Self::Dot(_) => ".",
                Self::Plus(_) => "+",
                Self::Minus(_) => "-",
                Self::Star(_) => "*",
                Self::Slash(_) => "/",
                Self::Percent(_) => "%",
                Self::Dollar(_) => "$",
                Self::Colon(_) => ":",
            }
        )
    }
}

impl Symbol {
    pub fn span(&self) -> Span {
        match self {
            Self::Comma(span)
            | Self::Dot(span)
            | Self::Plus(span)
            | Self::Minus(span)
            | Self::Star(span)
            | Self::Slash(span)
            | Self::Percent(span)
            | Self::Dollar(span)
            | Self::Colon(span) => span.clone(),
        }
    }

    pub fn peek(input: syn::parse::ParseStream) -> bool {
        return input.peek(Token![,])
            || input.peek(Token![.])
            || input.peek(Token![:])
            || input.peek(Token![+])
            || input.peek(Token![-])
            || input.peek(Token![*])
            || input.peek(Token![/])
            || input.peek(Token![%])
            || input.peek(Token![$]);
    }
}

impl syn::parse::Parse for Symbol {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if let Ok(v) = input.parse::<Token![,]>() {
            Ok(Symbol::Comma(v.span))
        } else if let Ok(v) = input.parse::<Token![.]>() {
            Ok(Symbol::Dot(v.span))
        } else if let Ok(v) = input.parse::<Token![+]>() {
            Ok(Symbol::Plus(v.span))
        } else if let Ok(v) = input.parse::<Token![-]>() {
            Ok(Symbol::Minus(v.span))
        } else if let Ok(v) = input.parse::<Token![*]>() {
            Ok(Symbol::Star(v.span))
        } else if let Ok(v) = input.parse::<Token![/]>() {
            Ok(Symbol::Slash(v.span))
        } else if let Ok(v) = input.parse::<Token![%]>() {
            Ok(Symbol::Percent(v.span))
        } else if let Ok(v) = input.parse::<Token![$]>() {
            Ok(Symbol::Dollar(v.span))
        } else if let Ok(v) = input.parse::<Token![:]>() {
            Ok(Symbol::Colon(v.span))
        } else {
            Err(syn::Error::new(input.span(), "Expected symbol token"))
        }
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    Ident(Span, String),
    Keyword(Span, Keyword),

    Symbol(Span, Symbol),
    Number(Span, String),
    String(Span, String),

    List(Span, Vec<Expr>),
    Ref(Span, Expr),
    Injection(Span, TokenStream2),
    Comment(Span, String),
    Unkown,
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Ident(span, _)
            | Token::Symbol(span, _)
            | Token::Number(span, _)
            | Token::String(span, _)
            | Token::List(span, _)
            | Token::Ref(span, _)
            | Token::Keyword(span, _)
            | Token::Injection(span, _) => span.clone(),
            _ => Span::call_site(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Ident(_, val) => val.clone(),
                Self::Injection(_, _) => String::from("{}"),
                Self::String(_, val) => format!("\\\"{}\\\"", val),
                Self::Number(_, val) => val.clone(),
                Self::Symbol(_, sym) => sym.to_string(),
                Self::Keyword(_, kw) => kw.to_string(),
                Self::Comment(_, _) => String::new(),
                Self::Ref(_, expr) => format!("[{}]", expr.to_string()),
                Self::List(_, expressions) => {
                    format!(
                        "({})",
                        expressions
                            .iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<String>>()
                            .join(", ")
                    )
                }
                Self::Unkown => String::new(),
            }
        )
    }
}

impl syn::parse::Parse for Token {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Ident::peek_any) {
            return Ok(match Keyword::parse(input) {
                // Keyword
                Ok(kw) => Token::Keyword(Span::call_site(), kw),
                // Ident
                _ => {
                    let val = input.parse::<Ident>()?;
                    Token::Ident(val.span(), val.to_string())
                }
            });
        } else if input.peek(Lit) {
            if input.peek(LitInt) {
                let num = input.parse::<LitInt>()?;
                return Ok(Token::Number(num.span(), num.to_string()));
            } else if input.peek(LitStr) {
                let str = input.parse::<LitStr>()?;
                return Ok(Token::String(str.span(), str.value()));
            }
            return Err(syn::Error::new(input.span(), "Not a known literal"));
        } else if input.peek(Token![;]) {
            // Parse until the next ; symbol for closing the comment.
            // Example: ;Some comment here;
            let mut tokens = Vec::new();
            let start = input.parse::<Token![;]>()?;
            while !input.is_empty() {
                if input.peek(Token![;]) {
                    let _ = input.parse::<Token![;]>()?;
                    break;
                } else {
                    if Symbol::peek(input) {
                        tokens.push(input.parse::<Symbol>()?.to_string())
                    } else {
                        match input.parse::<syn::Expr>() {
                            Ok(expr) => tokens.push(expr.to_token_stream().to_string()),
                            Err(_) => {
                                return Err(syn::Error::new(input.span(), "Not a valid expression inside a nasm comment"));
                            }
                        }
                    }
                }
            }
            return Ok(Token::Comment(start.span, tokens.join(" ")));
        } else if Symbol::peek(input) {
            let sym = Symbol::parse(input)?;
            return Ok(Token::Symbol(sym.span(), sym));
        } else if input.peek(Brace) {
            let brace;
            braced!(brace in input);
            let val = brace.parse::<TokenStream2>()?;
            return Ok(Token::Injection(brace.span(), val));
        } else if input.peek(Bracket) {
            let bracket;
            bracketed!(bracket in input);

            let mut prm = Vec::new();
            while !bracket.is_empty() {
                let next = Token::parse(&bracket)?;
                prm.push(next);
            }
            return Ok(Token::Ref(bracket.span(), Expr(Span::call_site(), prm)));
        } else if input.peek(Paren) {
            let paren;
            parenthesized!(paren in input);
            // Parse all inner tokens
            let mut params = Vec::new();
            let mut pspan: Option<Span> = None;
            let mut prms = Vec::new();

            while !paren.is_empty() {
                let next = Token::parse(&paren)?;
                if let Token::Symbol(_st, Symbol::Comma(_sc)) = next {
                    //next param to list
                    params.push(Expr(pspan.unwrap_or(Span::call_site()), prms));
                    pspan = None;
                    prms = Vec::new();
                } else {
                    if pspan.is_none() {
                        pspan = Some(next.span())
                    }
                    prms.push(next)
                }
            }

            return Ok(Token::List(paren.span(), params));
        }
        abort!(input.span(), "Unkown token")
    }
}

#[derive(Debug, Clone)]
pub enum Keyword {
    Times(Span),
    Dup(Span),
    Incbin(Span),
    Extern(Span),
    Global(Span),
    Section(Span),
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Times(_) => "times",
                Self::Dup(_) => "dup",
                Self::Incbin(_) => "incbin",
                Self::Extern(_) => "extern",
                Self::Global(_) => "global",
                Self::Section(_) => "section",
            }
        )
    }
}

impl syn::parse::Parse for Keyword {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        match input.cursor().ident() {
            Some((kw, _)) => Ok(match kw.to_string().to_lowercase().as_str() {
                "times" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Times(kw.span())
                }
                "dup" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Dup(kw.span())
                }
                "incbin" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Incbin(kw.span())
                }
                "extern" => {
                    let _ = input.parse::<Token![extern]>();
                    Keyword::Extern(kw.span())
                }
                "global" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Global(kw.span())
                }
                "section" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Section(kw.span())
                }
                _ => return Err(syn::Error::new(kw.span(), "Expected a keyword")),
            }),
            None => Err(syn::Error::new(input.span(), "Expected a keyword")),
        }
    }
}
