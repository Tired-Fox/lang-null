use super::Expr;
use proc_macro2::Span;
use quote::ToTokens;
use syn::{
    braced, bracketed, parenthesized,
    punctuated::Punctuated,
    token::{Brace, Bracket, Paren},
    Ident, Lit, LitInt, LitStr, Token,
};

#[derive(Debug)]
pub enum Symbol {
    Comma(Span),
    Dot(Span),
    Plus(Span),
    Minus(Span),
    Star(Span),
    Slash(Span),
    Percent(Span),
    Dollar(Span),
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
            | Self::Dollar(span) => span.clone(),
        }
    }

    pub fn peek(input: syn::parse::ParseStream) -> bool {
        return input.peek(Token![,])
            || input.peek(Token![.])
            || input.peek(Token![+])
            || input.peek(Token![-])
            || input.peek(Token![*])
            || input.peek(Token![/])
            || input.peek(Token![%])
            || input.peek(Token![$])
            || input.peek(Token![;]);
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
        } else {
            Err(syn::Error::new(input.span(), "Expected symbol token"))
        }
    }
}

#[derive(Debug)]
pub enum Token {
    Ident(Span, String),
    Keyword(Span, Keyword),

    Symbol(Span, Symbol),
    Number(Span, String),
    String(Span, String),

    List(Span, Vec<Expr>),
    Ref(Span, Vec<Token>),
    Injection(Span, String),
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

impl syn::parse::Parse for Token {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) {
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
            } else {
                return Err(syn::Error::new(input.span(), "Not a known literal"));
            }
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
                    let expr = input.parse::<syn::Expr>().map_err(|e| {
                        syn::Error::new(e.span(), "Could not parse expr in comment")
                    })?;
                    tokens.push(expr.to_token_stream().to_string());
                }
            }
            return Ok(Token::Comment(start.span, tokens.join(" ")));
        } else if Symbol::peek(input) {
            let sym = Symbol::parse(input)?;
            return Ok(Token::Symbol(sym.span(), sym));
        } else if input.peek(Brace) {
            let brace;
            braced!(brace in input);
            let val = brace.to_string();
            // Progress the parser allow any expression. Punctuated parse so the dot
            // accessor can be used.
            let parser = Punctuated::<syn::Expr, Token![.]>::parse_terminated;
            let _ = parser(&brace);
            return Ok(Token::Injection(brace.span(), val));
        } else if input.peek(Bracket) {
            let bracket;
            bracketed!(bracket in input);

            let mut prm = Vec::new();
            while !bracket.is_empty() {
                let next = Token::parse(&bracket)?;
                prm.push(next);
            }
            return Ok(Token::Ref(bracket.span(), prm));
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
        Ok(Token::Unkown)
    }
}

#[derive(Debug)]
pub enum Keyword {
    Times,
    Dup,
    Incbin,
    Extern,
    Global,
    Section,
}

impl syn::parse::Parse for Keyword {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        match input.cursor().ident() {
            Some((kw, _)) => Ok(match kw.to_string().to_lowercase().as_str() {
                "times" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Times
                }
                "dup" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Dup
                }
                "incbin" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Incbin
                }
                "extern" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Extern
                }
                "global" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Global
                }
                "section" => {
                    let _ = input.parse::<Ident>();
                    Keyword::Section
                }
                _ => return Err(syn::Error::new(kw.span(), "Expected a keyword")),
            }),
            None => Err(syn::Error::new(input.span(), "Expected a keyword")),
        }
    }
}
