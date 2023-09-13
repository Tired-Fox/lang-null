use std::fmt::Display;

use proc_macro2::Span;
use proc_macro_error::abort;
use syn::{
    bracketed, token::Bracket, Ident, Lit, LitBool, LitByte, LitByteStr, LitChar, LitFloat, LitInt,
    LitStr, Token,
};

pub mod register {
    use std::fmt::Display;

    macro_rules! reg_table {
        ($([$name: ident, $byte4: ident, $byte2: ident, $byte1: ident $(,)?]),* $(,)?) => {
            $(
                #[doc="A full 8-byte register. Each enum is a specific sub section of this register."]
                #[derive(Debug, PartialEq, Eq, Clone, Copy)]
                pub enum $name {
                    #[doc="The full register"]
                    $name,
                    #[doc="The first 4-bytes of the register"]
                    $byte4,
                    #[doc="The first 2-bytes of the register"]
                    $byte2,
                    #[doc="The first byte of the register"]
                    $byte1,
                }

                impl Display for $name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(
                            f,
                            "{}",
                            match self {
                                $name::$name => stringify!($name),
                                $name::$byte4 => stringify!($byte4),
                                $name::$byte2 => stringify!($byte2),
                                $name::$byte1 => stringify!($byte1),
                            }
                        )
                    }
                }

                impl $name {
                    pub fn is_part(name: &String) -> bool {
                        match name.to_uppercase().as_str() {
                            stringify!($name) | stringify!($byte4) | stringify!($byte2) | stringify!($byte1) => {
                                true
                            }
                            _ => {
                                false
                            }
                        }
                    }

                    pub fn get_part(name: &String) -> Option<Self> {
                        match name.to_uppercase().as_str() {
                            stringify!($name) => {
                                Some($name::$name)
                            }
                            stringify!($byte4) => {
                                Some($name::$byte4)
                            },
                            stringify!($byte2) => {
                                Some($name::$byte2)
                            },
                            stringify!($byte1) => {
                                Some($name::$byte1)
                            }
                            _ => None
                        }
                    }
                }

                impl From<$name> for Register {
                    fn from(value: $name) -> Register {
                        Register::$name(value)
                    }
                }
            )*

            #[derive(Debug, PartialEq, Eq, Clone, Copy)]
            pub enum Register {
                $(
                    $name($name),
                )*
            }

            impl Register {
                pub fn is_register(name: String) -> bool {
                    $(
                        if $name::is_part(&name) {
                            return true;
                        }
                    )*
                    return false;
                }

                pub fn get_register(name: String) -> Option<Register> {
                    $(
                        if $name::is_part(&name) {
                            return $name::get_part(&name).map(|v| Register::from(v));
                        }
                    )*
                    return None
                }
            }

            impl Display for Register {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    match self {
                        $(
                            Register::$name(v) => {
                                write!(f, "{}", v)
                            }
                        )*
                    }
                }
            }
        };
    }

    reg_table![
        [RAX, EAX, AX, AL],
        [RCX, ECX, CX, CL],
        [RDX, EDX, DX, DL],
        [RBX, EBX, BX, BL],
        [RSI, ESI, SI, SIL],
        [RDI, EDI, DI, DIL],
        [RSP, ESP, SP, SPL],
        [RBP, EBP, BP, BPL],
        [R8, R8D, R8W, R8B],
        [R9, R9D, R9W, R9B],
        [R10, R10D, R10W, R10B],
        [R11, R11D, R11W, R11B],
        [R12, R12D, R12W, R12B],
        [R13, R13D, R13W, R13B],
        [R14, R14D, R14W, R14B],
        [R15, R15D, R15W, R15B],
    ];
}

#[derive(Debug, Clone)]
pub enum Reference {
    RegRef(Span, String),
    MemRef(Span, String),
}

impl Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reference::RegRef(span, name) => {
                write!(f, "{}", name)
            }
            Reference::MemRef(span, name) => {
                write!(f, "{}", name)
            }
        }
    }
}

macro_rules! assert_pl {
    ($name:ident<$min_len: literal>, $span: expr, $message: literal) => {
        if $name.len() != $min_len {
            abort!($span, $message)
        }
    };
}

impl Reference {
    pub fn parse(input: syn::parse::ParseStream) -> Reference {
        let symbol = input.parse::<Ident>();
        match symbol {
            Ok(sym) => {
                if input.peek(Token![+]) || input.peek(Token![-]) {
                    Reference::MemRef(input.span(), format!("{}{}", sym, input))
                } else {
                    Reference::RegRef(input.span(), sym.to_string())
                }
            }
            Err(_) => abort!(input.span(), "Expected a register or memory reference"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constant {
    String(Span, String),
    Bool(Span, bool),
    Number(Span, String),
    Byte(Span, u8),
    ByteStr(Span, Vec<u8>),
}

impl Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::String(span, value) => {
                write!(f, "{}", value)
            }
            Constant::Bool(span, value) => {
                write!(f, "{}", value.clone() as u8)
            }
            Constant::Number(span, value) => {
                write!(f, "{}", value)
            }
            Constant::Byte(span, value) => {
                write!(f, "'{}'", value.clone() as char)
            }
            Constant::ByteStr(span, value) => {
                write!(f, "'{}'", String::from_utf8_lossy(value))
            }
        }
    }
}

impl Constant {
    pub fn parse(input: syn::parse::ParseStream) -> Self {
        if input.peek(LitInt) {
            let next = input.parse::<LitInt>().unwrap();
            Constant::Number(next.span(), next.to_string())
        } else if input.peek(LitFloat) {
            let next = input.parse::<LitFloat>().unwrap();
            Constant::Number(next.span(), next.to_string())
        } else if input.peek(LitBool) {
            let next = input.parse::<LitBool>().unwrap();
            Constant::Bool(next.span(), next.value())
        } else if input.peek(LitStr) {
            let next = input.parse::<LitStr>().unwrap();
            Constant::String(next.span(), next.value())
        } else if input.peek(LitByte) {
            let next = input.parse::<LitByte>().unwrap();
            Constant::Byte(next.span(), next.value())
        } else if input.peek(LitByteStr) {
            let next = input.parse::<LitByteStr>().unwrap();
            Constant::ByteStr(next.span(), next.value())
        } else if input.peek(LitChar) {
            let next = input.parse::<LitChar>().unwrap();
            Constant::String(next.span(), String::from(next.value()))
        } else {
            abort!(input.span(), "Expected constant value")
        }
    }
}

#[derive(Debug, Clone)]
pub enum Values {
    Memory(Span, String),
    Reference(Span, Reference),
    Register(Span, register::Register),
    Constant(Span, Constant),
    Injection,
}

impl Display for Values {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Values::Memory(_, name) => write!(f, "{}", name),
            Values::Reference(_, refer) => write!(f, "{}", refer),
            Values::Register(_, reg) => write!(f, "{}", reg),
            Values::Constant(_, constant) => write!(f, "{}", constant),
            Values::Injection => write!(f, "{{}}"),
        }
    }
}

fn nl_op<T, V>(_: V, arg: T) -> T {
    arg
}

type Source = Values;
type Destination = Values;
type Param = Values;

macro_rules! conditional {
    { ($($type: ident),*), $prefix: ident, $name: ident } => {
        paste::paste! {
            pub enum $name {
                #[doc= $name " to label or *Operand"]
                [<$prefix MP>]($($type,)*),
                #[doc= $name " if equal"]
                [<$prefix E>]($($type,)*),
                #[doc= $name " if zero"]
                [<$prefix Z>]($($type,)*),
                #[doc= $name " if not equal"]
                [<$prefix NE>]($($type,)*),
                #[doc= $name " if not zero"]
                [<$prefix NZ>]($($type,)*),
                #[doc= $name " if negative"]
                [<$prefix S>]($($type,)*),
                #[doc= $name " if non-negative"]
                [<$prefix NS>]($($type,)*),
                #[doc= $name " if greater"]
                [<$prefix G>]($($type,)*),
                #[doc= $name " if not less than equal (signed)"]
                [<$prefix NLE>]($($type,)*),
                #[doc= $name " if greater than equal (signed)"]
                [<$prefix GE>]($($type,)*),
                #[doc= $name " if not less (signed)"]
                [<$prefix NL>]($($type,)*),
                #[doc= $name " if less (signed)"]
                [<$prefix L>]($($type,)*),
                #[doc= $name " if not greater than equal (signed)"]
                [<$prefix NGE>]($($type,)*),
                #[doc= $name " if less than equal"]
                [<$prefix LE>]($($type,)*),
                #[doc= $name " if not greater (unsigned)"]
                [<$prefix NG>]($($type,)*),
                #[doc= $name " if above (unsigned)"]
                [<$prefix A>]($($type,)*),
                #[doc= $name " if not below or equal (unsigned)"]
                [<$prefix NBE>]($($type,)*),
                #[doc= $name " if above or equal (unsigned)"]
                [<$prefix AE>]($($type,)*),
                #[doc= $name " if not below (unsigned)"]
                [<$prefix NB>]($($type,)*),
                #[doc= $name " if below (unsigned)"]
                [<$prefix B>]($($type,)*),
                #[doc= $name " if not above or equal (unsigned)"]
                [<$prefix NAE>]($($type,)*),
                #[doc= $name " if below or equal (unsigned)"]
                [<$prefix BE>]($($type,)*),
                #[doc= $name " if not above (unsigned)"]
                [<$prefix NA>]($($type,)*),
            }

            impl $name {
                pub fn parse(val: &str, mut params: Vec<Values>) -> Self {
                    match val {
                        stringify!([<$prefix MP>]) => {Self::[<$prefix MP>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix E>]) => {Self::[<$prefix E>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix Z>]) => {Self::[<$prefix Z>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NE>]) => {Self::[<$prefix NE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NZ>]) => {Self::[<$prefix NZ>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix S>]) => {Self::[<$prefix S>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NS>]) => {Self::[<$prefix NS>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix G>]) => {Self::[<$prefix G>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NLE>]) => {Self::[<$prefix NLE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix GE>]) => {Self::[<$prefix GE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NL>]) => {Self::[<$prefix NL>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix L>]) => {Self::[<$prefix L>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NGE>]) => {Self::[<$prefix NGE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix LE>]) => {Self::[<$prefix LE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NG>]) => {Self::[<$prefix NG>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix A>]) => {Self::[<$prefix A>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NBE>]) => {Self::[<$prefix NBE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix AE>]) => {Self::[<$prefix AE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NB>]) => {Self::[<$prefix NB>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix B>]) => {Self::[<$prefix B>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NAE>]) => {Self::[<$prefix NAE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix BE>]) => {Self::[<$prefix BE>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        stringify!([<$prefix NA>]) => {Self::[<$prefix NA>]($(nl_op($type::Injection, params.pop().unwrap()),)*)},
                        _ => abort!(Span::call_site(), format!("Unknown {} instruction", stringify!($name))),
                    }
                }
            }
        }
    }
}

conditional! {(Destination), J, Jump}
impl Display for Jump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Jump::JMP(v) => write!(f, "jmp {}", v),
            Jump::JE(v) => write!(f, "je {}", v),
            Jump::JZ(v) => write!(f, "jz {}", v),
            Jump::JNE(v) => write!(f, "jne {}", v),
            Jump::JNZ(v) => write!(f, "jnz {}", v),
            Jump::JS(v) => write!(f, "js {}", v),
            Jump::JNS(v) => write!(f, "jns {}", v),
            Jump::JG(v) => write!(f, "jg {}", v),
            Jump::JNLE(v) => write!(f, "jnle {}", v),
            Jump::JGE(v) => write!(f, "jge {}", v),
            Jump::JNL(v) => write!(f, "jnl {}", v),
            Jump::JL(v) => write!(f, "jl {}", v),
            Jump::JNGE(v) => write!(f, "jnge {}", v),
            Jump::JLE(v) => write!(f, "jle {}", v),
            Jump::JNG(v) => write!(f, "jng {}", v),
            Jump::JA(v) => write!(f, "ja {}", v),
            Jump::JNBE(v) => write!(f, "jnbe {}", v),
            Jump::JAE(v) => write!(f, "jae {}", v),
            Jump::JNB(v) => write!(f, "jnb {}", v),
            Jump::JB(v) => write!(f, "jb {}", v),
            Jump::JNAE(v) => write!(f, "jnae {}", v),
            Jump::JBE(v) => write!(f, "jbe {}", v),
            Jump::JNA(v) => write!(f, "jna {}", v),
        }
    }
}
conditional! {(Destination), SET, Set}
impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Set::SETMP(v) => write!(f, "{}", v),
            Set::SETE(v) => write!(f, "{}", v),
            Set::SETZ(v) => write!(f, "{}", v),
            Set::SETNE(v) => write!(f, "{}", v),
            Set::SETNZ(v) => write!(f, "{}", v),
            Set::SETS(v) => write!(f, "{}", v),
            Set::SETNS(v) => write!(f, "{}", v),
            Set::SETG(v) => write!(f, "{}", v),
            Set::SETNLE(v) => write!(f, "{}", v),
            Set::SETGE(v) => write!(f, "{}", v),
            Set::SETNL(v) => write!(f, "{}", v),
            Set::SETL(v) => write!(f, "{}", v),
            Set::SETNGE(v) => write!(f, "{}", v),
            Set::SETLE(v) => write!(f, "{}", v),
            Set::SETNG(v) => write!(f, "{}", v),
            Set::SETA(v) => write!(f, "{}", v),
            Set::SETNBE(v) => write!(f, "{}", v),
            Set::SETAE(v) => write!(f, "{}", v),
            Set::SETNB(v) => write!(f, "{}", v),
            Set::SETB(v) => write!(f, "{}", v),
            Set::SETNAE(v) => write!(f, "{}", v),
            Set::SETBE(v) => write!(f, "{}", v),
            Set::SETNA(v) => write!(f, "{}", v),
        }
    }
}
conditional! {(Source, Destination), CMOV, CMov}
impl Display for CMov {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CMov::CMOVMP(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVZ(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNZ(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVS(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNS(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVG(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNLE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVGE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNL(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVL(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNGE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVLE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNG(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVA(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNBE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVAE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNB(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVB(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNAE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVBE(s, d) => write!(f, "{}, {}", s, d),
            CMov::CMOVNA(s, d) => write!(f, "{}, {}", s, d),
        }
    }
}

impl From<Ident> for Values {
    fn from(value: Ident) -> Self {
        if register::Register::is_register(value.to_string()) {
            Values::Register(
                value.span(),
                register::Register::get_register(value.to_string()).unwrap(),
            )
        } else {
            Values::Memory(value.span(), value.to_string())
        }
    }
}
impl Values {
    fn to_i32(&self) -> i32 {
        match self {
            Param::Constant(span, val) => match val {
                Constant::Number(span, num) => match i32::from_str_radix(num, 10) {
                    Ok(num) => num,
                    Err(_) => abort!(span, "Failed to parse number constant"),
                },
                _ => abort!(span, "Expected a number"),
            },
            Param::Memory(span, _) | Param::Register(span, _) | Param::Reference(span, _) => {
                abort!(span, "Expected a constant")
            }
            _ => abort!(Span::call_site(), "Expected a constant"),
        }
    }

    pub fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) {
            let reg = input.parse::<Ident>()?;
            if register::Register::is_register(reg.to_string()) {
                Ok(Param::Register(
                    reg.span(),
                    register::Register::get_register(reg.to_string()).unwrap(),
                ))
            } else {
                Ok(Param::Memory(reg.span(), reg.to_string()))
            }
        } else if input.peek(Bracket) {
            let refer;
            bracketed!(refer in input);
            Ok(Param::Reference(refer.span(), Reference::parse(&refer)))
        } else if input.peek(Lit) {
            if input.peek(LitByte) || input.peek(LitByteStr) {
                return Err(syn::Error::new(
                    input.span(),
                    "Parameter doesn't support byte or byte str",
                ));
            }
            Ok(Param::Constant(input.span(), Constant::parse(input)))
        } else {
            Err(syn::Error::new(input.span(), "Unkown parameter type"))
        }
    }
}

pub enum Instruction {
    /// Move source to destination
    MOV(Span, Destination, Source),
    /// Push source onto stack
    PUSH(Span, Source),
    /// Pop top of stack into destination
    POP(Span, Destination),
    /// Convert word in AX to dword in EAX (sign-extended)
    CWTL(Span),
    /// Convert dword in EAX to qword in RAX (sign-extended)
    CLTQ(Span),
    /// Convert qword in RAX to oword in RDX:RAX
    CQTO(Span),
    /// Increment by 1
    INC(Span, Destination),
    /// Decrement by 1
    DEC(Span, Destination),
    /// Arithmetic negation
    NEG(Span, Destination),
    /// Bitwise complement
    NOT(Span, Destination),
    /// Load effective address of source into destination
    LEAQ(Span, Destination, Source),
    /// Add source to destination
    ADD(Span, Destination, Source),
    /// Subtract source from destination
    SUB(Span, Destination, Source),
    /// Multiply destination by source
    IMUL(Span, Destination, Source),
    /// Bitwise XOR destination by source
    XOR(Span, Destination, Source),
    /// Bitwise OR destination by source
    OR(Span, Destination, Source),
    /// Bitwise AND destination by source
    AND(Span, Destination, Source),
    /// Left shift destination by k bits
    SAL(Span, Destination, i32),
    /// Left shift destination by k bits
    SHL(Span, Destination, i32),
    /// Arithmetic right shift destination by k bits
    SAR(Span, Destination, i32),
    /// Logical right shift destination by k bits
    SHR(Span, Destination, i32),
    /// Signed full multiply of RAS by source. Result stored in RDX:RAX
    IMULQ(Span, Source),
    /// Unsigned full multiply of RAX by source
    /// Result stored in RDX:RAX
    MULQ(Span, Source),
    /// Signed divide RDX:RAX by source
    /// Quotient stored in RAX
    /// Remainder stored in RDX
    IDIVQ(Span, Source),
    /// Unsigned divide RDX:RAX by S
    /// Quotient stored in RAX
    /// Remainder stored in RDX
    DIVQ(Span, Source),
    /// Set condition codes according to S1-S2
    CMP(Span, Source, Source),
    /// Set condition codes according to S1 & S2
    TEST(Span, Source, Source),
    /// Push return address and jump to label or *Operand (location)
    CALL(Span, Destination),
    /// Set RSP to RBP then pop top of stack into RBP
    LEAVE(Span),
    /// Pop return address from stack and jump there
    RET(Span),

    /// Conditionally set destination value
    SET(Span, Set),
    /// Conditionally jump to a label or *Operand (location)
    J(Span, Jump),
    /// Conditionally move a value from source to destination
    CMOVE(Span, CMov),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MOV(_, dest, src) => {
                write!(f, "mov {}, {}", dest, src)
            }
            Self::PUSH(_, src) => {
                write!(f, "push {}", src)
            }
            Self::POP(_, dest) => {
                write!(f, "pop {}", dest)
            }
            Self::CWTL(_) => {
                write!(f, "cwtd")
            }
            Self::CLTQ(_) => {
                write!(f, "cltd")
            }
            Self::CQTO(_) => {
                write!(f, "cqto")
            }
            Self::INC(_, dest) => {
                write!(f, "inc {}", dest)
            }
            Self::DEC(_, dest) => {
                write!(f, "dec {}", dest)
            }
            Self::NEG(_, dest) => {
                write!(f, "neg {}", dest)
            }
            Self::NOT(_, dest) => {
                write!(f, "not {}", dest)
            }
            Self::LEAQ(_, dest, src) => {
                write!(f, "leaq {}, {}", dest, src)
            }
            Self::ADD(_, dest, src) => {
                write!(f, "add {}, {}", dest, src)
            }
            Self::SUB(_, dest, src) => {
                write!(f, "sub {}, {}", dest, src)
            }
            Self::IMUL(_, dest, src) => {
                write!(f, "imul {}, {}", dest, src)
            }
            Self::XOR(_, dest, src) => {
                write!(f, "xor {}, {}", dest, src)
            }
            Self::OR(_, dest, src) => {
                write!(f, "or {}, {}", dest, src)
            }
            Self::AND(_, dest, src) => {
                write!(f, "and {}, {}", dest, src)
            }
            Self::SAL(_, dest, count) => {
                write!(f, "sal {}, {}", dest, count)
            }
            Self::SHL(_, dest, count) => {
                write!(f, "shl {}, {}", dest, count)
            }
            Self::SAR(_, dest, count) => {
                write!(f, "sar {}, {}", dest, count)
            }
            Self::SHR(_, dest, count) => {
                write!(f, "shr {}, {}", dest, count)
            }
            Self::IMULQ(_, src) => {
                write!(f, "imulq {}", src)
            }
            Self::MULQ(_, src) => {
                write!(f, "mulq {}", src)
            }
            Self::IDIVQ(_, src) => {
                write!(f, "idivq {}", src)
            }
            Self::DIVQ(_, src) => {
                write!(f, "divq {}", src)
            }
            Self::CMP(_, src1, src2) => {
                write!(f, "cmp {}, {}", src1, src2)
            }
            Self::TEST(_, src1, src2) => {
                write!(f, "test {}, {}", src1, src2)
            }
            Self::CALL(_, dest) => {
                write!(f, "call {}", dest)
            }
            Self::LEAVE(_) => {
                write!(f, "leave")
            }
            Self::RET(_) => {
                write!(f, "ret")
            }

            Self::SET(_, set) => {
                write!(f, "{}", set)
            }
            Self::J(_, jmp) => {
                write!(f, "{}", jmp)
            }
            Self::CMOVE(_, cmov) => {
                write!(f, "{}", cmov)
            }
        }
    }
}

impl Instruction {
    pub fn is_instruction(value: &String) -> bool {
        match value.to_uppercase().as_str() {
            "MOV" | "PUSH" | "POP" | "CWTL" | "CLTQ" | "CQTO" | "INC" | "DEC" | "NEG" | "NOT"
            | "LEAQ" | "ADD" | "SUB" | "IMUL" | "XOR" | "OR" | "AND" | "SAL" | "SHL" | "SAR"
            | "SHR" | "IMULQ" | "MULQ" | "IDIVQ" | "DIVQ" | "CMP" | "TST" | "CALL" | "LEAVE"
            | "RET," => true,
            val if val.starts_with("SET") || val.starts_with("J") || val.starts_with("CMOVE") => {
                true
            }
            _ => false,
        }
    }

    pub fn parse(inst: Ident, params: Vec<Param>) -> Self {
        match inst.to_string().to_uppercase().as_str() {
            "MOV" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "MOV must have two arguments: destination, source"
                );
                Instruction::MOV(inst.span(), params[0].clone(), params[1].clone())
            }
            "PUSH" => {
                assert_pl!(params<1>, inst, "PUSH must have one arguments: source");
                Instruction::PUSH(inst.span(), params[0].clone())
            }
            "POP" => {
                assert_pl!(params<1>, inst, "POP must have one arguments: destination");
                Instruction::POP(inst.span(), params[0].clone())
            }
            "CWTL" => {
                assert_pl!(params<0>, inst, "CWTL takes no arguments");
                Instruction::CWTL(inst.span())
            }
            "CLTQ" => {
                assert_pl!(params<0>, inst, "CLTQ takes no arguments");
                Instruction::CLTQ(inst.span())
            }
            "CQTO" => {
                assert_pl!(params<0>, inst, "CQTO takes no arguments");
                Instruction::CQTO(inst.span())
            }
            "INC" => {
                assert_pl!(params<1>, inst, "INC must have one argument: destination");
                Instruction::INC(inst.span(), params[0].clone())
            }
            "DEC" => {
                assert_pl!(params<1>, inst, "DEC must have one argument: destination");
                Instruction::DEC(inst.span(), params[0].clone())
            }
            "NEG" => {
                assert_pl!(params<1>, inst, "NEG must have one argument: destination");
                Instruction::NEG(inst.span(), params[0].clone())
            }
            "NOT" => {
                assert_pl!(params<1>, inst, "NOT must have one argument: destination");
                Instruction::NOT(inst.span(), params[0].clone())
            }
            "LEAQ" => {
                assert_pl!(params<2>, inst, "LEAQ must have one argument: destination");
                Instruction::LEAQ(inst.span(), params[0].clone(), params[1].clone())
            }
            "ADD" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "ADD must have one argument: destination and source"
                );
                Instruction::ADD(inst.span(), params[0].clone(), params[1].clone())
            }
            "SUB" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "SUB must have one argument: destination and source"
                );
                Instruction::SUB(inst.span(), params[0].clone(), params[1].clone())
            }
            "IMUL" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "IMUL must have one argument: destination and source"
                );
                Instruction::IMUL(inst.span(), params[0].clone(), params[1].clone())
            }
            "XOR" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "XOR must have one argument: destination and source"
                );
                Instruction::XOR(inst.span(), params[0].clone(), params[1].clone())
            }
            "OR" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "OR must have one argument: destination and source"
                );
                Instruction::OR(inst.span(), params[0].clone(), params[1].clone())
            }
            "AND" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "AND must have one argument: destination and source"
                );
                Instruction::AND(inst.span(), params[0].clone(), params[1].clone())
            }
            "SAL" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "SAL must have two arguments: destination and count"
                );
                Instruction::SAL(inst.span(), params[0].clone(), params[1].to_i32())
            }
            "SHL" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "SHL must have two arguments: destination and count"
                );
                Instruction::SHL(inst.span(), params[0].clone(), params[1].to_i32())
            }
            "SAR" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "SAR must have two arguments: destination and count"
                );
                Instruction::SAR(inst.span(), params[0].clone(), params[1].to_i32())
            }
            "SHR" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "SHR must have two arguments: destination and count"
                );
                Instruction::SHR(inst.span(), params[0].clone(), params[1].to_i32())
            }
            "IMULQ" => {
                assert_pl!(params<1>, inst, "IMULQ must have one argument: source");
                Instruction::IMULQ(inst.span(), params[0].clone())
            }
            "MULQ" => {
                assert_pl!(params<1>, inst, "MULQ must have one argument: source");
                Instruction::MULQ(inst.span(), params[0].clone())
            }
            "IDIVQ" => {
                assert_pl!(params<1>, inst, "IDIVQ must have one argument: source");
                Instruction::IDIVQ(inst.span(), params[0].clone())
            }
            "DIVQ" => {
                assert_pl!(params<1>, inst, "DIVQ must have one argument: source");
                Instruction::DIVQ(inst.span(), params[0].clone())
            }
            "CMP" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "CMP must have two arguments: source and source"
                );
                Instruction::CMP(inst.span(), params[1].clone(), params[0].clone())
            }
            "TEST" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "TEST must have two arguments: source and source"
                );
                Instruction::TEST(inst.span(), params[1].clone(), params[0].clone())
            }
            "CALL" => {
                assert_pl!(params<1>, inst, "CALL must have one argument: fn");
                println!("{:?}", params);
                Instruction::CALL(inst.span(), params[0].clone())
            }
            "LEAVE" => {
                assert_pl!(params<0>, inst, "LEAVE takes no arguments");
                Instruction::LEAVE(inst.span())
            }
            "RET," => {
                assert_pl!(params<0>, inst, "RET takes no arguments");
                Instruction::LEAVE(inst.span())
            }

            c if c.starts_with("SET") => {
                assert_pl!(
                    params<1>,
                    inst,
                    "SET must have one argument: destination and value"
                );
                Instruction::SET(inst.span(), Set::parse(c, params))
            }
            c if c.starts_with("J") => {
                assert_pl!(params<1>, inst, "JMP must have one argument: destination");
                Instruction::J(inst.span(), Jump::parse(c, params))
            }
            c if c.starts_with("CMOV") => {
                assert_pl!(
                    params<2>,
                    inst,
                    "CMOV must have two arguments: source and destination"
                );
                Instruction::CMOVE(inst.span(), CMov::parse(c, params))
            }
            _ => abort!(inst.span(), "Invalid instruction: {}", inst),
        }
    }
}

#[cfg(test)]
mod text {
    use super::*;

    #[test]
    fn registers() {
        use register::*;
        assert!(RDI::is_part(&"edi".to_string()));
        assert_eq!(RDI::get_part(&"edi".to_string()), Some(RDI::EDI));
        assert!(Register::is_register("R15".to_string()));
        assert_eq!(
            Register::get_register(String::from("rax")),
            Some(Register::RAX(RAX::RAX))
        );
        assert_eq!(
            Register::get_register(String::from("edi")),
            Some(Register::RDI(RDI::EDI))
        );
    }
}
