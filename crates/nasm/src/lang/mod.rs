use peekmore::{PeekMore, PeekMoreIterator};
use proc_macro2::Span;
use proc_macro_error::{abort, emit_error};
use std::fmt::Display;
use syn::parse::Parse;

pub mod instruction;
pub mod lex;
pub mod parse;

use lex::*;
use parse::*;

use self::instruction::Instruction;

macro_rules! msg {
    ($msg: expr) => {
        $msg.to_string()
    };
}

#[derive(Debug, Clone)]
pub struct Expr(pub Span, pub Vec<lex::Token>);
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.1
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        )
    }
}

pub struct NASM {
    pub tokens: Vec<lex::Token>,
    pub nodes: Vec<parse::Node>,
    pub errors: Vec<(Span, String)>,
}

impl Display for NASM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut parts = Vec::new();
        let mut prev: &Node = &Node::Section(Span::call_site(), "".to_string());

        let longest_label = self
            .nodes
            .iter()
            .filter_map(|n| match n {
                Node::Label(_, v) => Some(v.len() + 2),
                _ => None,
            })
            .max()
            .unwrap_or(0);

        let mut longest_instruction: usize = self
            .nodes
            .iter()
            .filter_map(|n| match n {
                Node::Instruction(_, v) => Some(v.len()),
                _ => None,
            })
            .max()
            .unwrap_or(4);

        if longest_instruction < 7 {
            longest_instruction = 7;
        }

        let inst_width = |length: usize| {
            (0..longest_instruction - length)
                .map(|_| ' ')
                .collect::<String>()
        };
        let label_width =
            |length: usize| (0..longest_label - length).map(|_| ' ').collect::<String>();

        for node in self.nodes.iter() {
            match node {
                Node::Instruction(inst, prms) => {
                    if let Node::Instruction(_, _) = prev {
                        parts.push(format!(
                            "{}{}{} {}\n",
                            label_width(0),
                            inst,
                            inst_width(inst.to_string().len()),
                            prms.iter()
                                .map(|e| e.to_string())
                                .collect::<Vec<String>>()
                                .join(", ")
                        ));
                    } else {
                        parts.push(format!(
                            "{}{} {}\n",
                            inst,
                            inst_width(inst.to_string().len()),
                            prms.iter()
                                .map(|e| e.to_string())
                                .collect::<Vec<String>>()
                                .join(", ")
                        ));
                    }
                }
                Node::Directive(directive) => {
                    let (name, prm) = match directive {
                        Directive::Global(_, val) => ("global", val),
                        Directive::Extern(_, val) => ("extern", val),
                        Directive::Incbin(_, val) => ("incbin", val),
                    };

                    parts.push(format!(
                        "{}{}{} {}\n",
                        label_width(0),
                        name,
                        inst_width(name.len()),
                        prm
                    ));
                }
                Node::Section(_, name) => {
                    parts.push(format!(
                        "{}section{} .{}\n",
                        label_width(0),
                        inst_width(7),
                        name,
                    ));
                }
                Node::Label(_, val) => {
                    parts.push(format!("{}: {}", val, label_width(val.len() + 2)));
                },
                Node::Injection(_, _) => {
                    parts.push("\n{}\n".to_string());
                }
            }
            prev = node;
        }
        write!(f, "{}", parts.join(""))
    }
}

impl NASM {
    pub fn lex(&mut self, input: syn::parse::ParseStream) {
        while !input.is_empty() {
            self.tokens.push(match Token::parse(input) {
                Ok(t) => t,
                Err(e) => {
                    self.errors.push((e.span(), e.to_string()));
                    lex::Token::Unkown
                }
            })
        }
    }

    pub fn create_keyword<'a, I>(&mut self, kw: &Keyword, tokens: &mut PeekMoreIterator<I>)
    where
        I: Iterator<Item = &'a Token>,
    {
        match kw {
            Keyword::Global(span) => {
                if let Some(Token::Ident(_, _)) = tokens.peek() {
                    self.nodes.push(Node::Directive(Directive::Global(
                        span.clone(),
                        tokens.next().unwrap().to_string(),
                    )))
                } else {
                    self.errors
                        .push((span.clone(), msg!("global keyword expects an identifier")))
                }
            }
            Keyword::Extern(span) => {
                if let Some(Token::Ident(_, _)) = tokens.peek() {
                    self.nodes.push(Node::Directive(Directive::Extern(
                        span.clone(),
                        tokens.next().unwrap().to_string(),
                    )))
                } else {
                    self.errors
                        .push((span.clone(), msg!("extern keyword expects an identifier")))
                }
            }
            Keyword::Incbin(span) => {
                if let Some(Token::String(_, _)) = tokens.peek() {
                    self.nodes.push(Node::Directive(Directive::Incbin(
                        span.clone(),
                        tokens.next().unwrap().to_string(),
                    )))
                } else {
                    self.errors.push((
                        span.clone(),
                        msg!("extern keyword expects a string literal"),
                    ))
                }
            }
            Keyword::Section(span) => {
                // Dot, Ident
                if let Some(Token::Symbol(_, Symbol::Dot(_))) = tokens.peek() {
                    let _ = tokens.next();
                    if let Some(Token::Ident(_, _)) = tokens.peek() {
                        match tokens.next() {
                            Some(Token::Ident(_, v)) => {
                                self.nodes.push(Node::Section(span.clone(), v.clone()));
                                return;
                            }
                            _ => {}
                        }
                    } else {
                        self.errors
                            .push((span.clone(), msg!("section keyword expects a `.<ident>`")));
                    }
                } else {
                    self.errors.push((
                        span.clone(),
                        msg!("section keyword expects an identifier that starts with a `.`"),
                    ));
                }
            }
            _ => {}
        }
    }

    pub fn create_ident<'a, I>(
        &mut self,
        span: &Span,
        ident: &String,
        tokens: &mut PeekMoreIterator<I>,
    ) where
        I: Iterator<Item = &'a Token>,
    {
        if let Some(Token::Symbol(_, Symbol::Colon(_))) = tokens.peek() {
            let _ = tokens.next();
            self.nodes.push(Node::Label(span.clone(), ident.clone()));
        } else if let Some(inst) = Instruction::new(ident.as_str()) {
            // Go until next token is comment, keyword, label, instruction
            let mut params = Vec::new();
            let mut current: Option<Expr> = None;
            loop {
                let next = tokens.peek();
                match next {
                    Some(v) => match v {
                        Token::Comment(_, _) | Token::Keyword(_, _) => break,
                        Token::Ident(_, v) => {
                            if let Some(Token::Symbol(_, Symbol::Colon(_))) = tokens.peek_nth(1) {
                                break;
                            }
                            if let Some(_) = Instruction::new(v.as_str()) {
                                break;
                            }

                            if current.is_none() {
                                current = Some(Expr(span.clone(), Vec::new()))
                            }

                            let next = tokens.next().unwrap().clone();
                            current.as_mut().unwrap().1.push(next);
                        }
                        Token::Symbol(span, s) => match s {
                            Symbol::Comma(_) => {
                                if current.is_some() {
                                    params.push(current.unwrap());
                                }
                                let _ = tokens.next();
                                current = None
                            }
                            Symbol::Dot(span) | Symbol::Colon(span) => {
                                abort!(span, "Invalid expr token");
                            }
                            _ => {
                                if current.is_none() {
                                    current = Some(Expr(span.clone(), Vec::new()))
                                }
                                current
                                    .as_mut()
                                    .unwrap()
                                    .1
                                    .push(tokens.next().unwrap().clone());
                            }
                        },
                        _ => {
                            if current.is_none() {
                                current = Some(Expr(span.clone(), Vec::new()))
                            }
                            current
                                .as_mut()
                                .unwrap()
                                .1
                                .push(tokens.next().unwrap().clone());
                        }
                    },
                    None => break,
                }
            }
            if current.is_some() {
                params.push(current.unwrap())
            }

            if !inst.verify(&params) {
                self.errors.push((
                    span.clone(),
                    format!("{} instruction expected {} argument(s)", inst, inst.count()),
                ))
            }
            self.nodes.push(Node::Instruction(inst, params))
        }
    }

    pub fn combine(&mut self) {
        let tokens = self.tokens.clone();
        let mut tokens = tokens.iter().peekmore();
        while let Some(token) = tokens.next() {
            match token {
                Token::Keyword(_, kw) => self.create_keyword(&kw, &mut tokens),
                Token::Ident(span, ident) => self.create_ident(&span, &ident, &mut tokens),
                Token::Comment(_, _) => {},
                Token::Injection(span, content) => self.nodes.push(Node::Injection(span.clone(), content.clone())),
                token => self.errors.push((token.span(), format!("Invalid syntax: {:?}", token))),
            }
        }
    }
}

impl syn::parse::Parse for NASM {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut nasm = NASM {
            tokens: Vec::new(),
            nodes: Vec::new(),
            errors: Vec::new(),
        };

        nasm.lex(input);
        if nasm.errors.len() > 0 {
            for error in nasm.errors {
                emit_error!(error.0, error.1);
            }
            return Err(syn::Error::new(Span::call_site(), "Lexing errors found"));
        }

        nasm.combine();
        if nasm.errors.len() > 0 {
            for error in nasm.errors {
                emit_error!(error.0, error.1);
            }
            return Err(syn::Error::new(Span::call_site(), "Parsing errors found"));
        }

        Ok(nasm)
    }
}
