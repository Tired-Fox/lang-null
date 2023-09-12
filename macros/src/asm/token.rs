use proc_macro2::Span;
use proc_macro_error::abort;
use syn::{
    bracketed, token::Bracket, Ident, Lit, LitBool, LitByte, LitByteStr, LitChar, LitFloat, LitInt,
    LitStr, Token,
};

pub mod register {
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

pub enum Reference {
    RegRef(Span, String),
    MemRef(Span, String),
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

pub enum Constant {
    String(Span, String),
    Bool(Span, bool),
    Number(Span, String),
    Byte(Span, u8),
    ByteStr(Span, Vec<u8>),
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

pub enum Source {
    Constant(Span, Constant),
    Reference(Span, Reference),
    Register(Span, register::Register),
    Memory(Span, String),
}

impl From<Param> for Source {
    fn from(value: Param) -> Self {
        match value {
            Param::Constant(span, val) => Source::Constant(span, val),
            Param::Reference(span, val) => Source::Reference(span, val),
            Param::Register(span, val) => Source::Register(span, val),
            Param::Memory(span, val) => Source::Memory(span, val),
        }
    }
}

pub enum Destination {
    Memory(Span, String),
    Reference(Span, Reference),
    Register(Span, register::Register),
}

impl From<Param> for Destination {
    fn from(value: Param) -> Self {
        match value {
            Param::Memory(span, mem) => Destination::Memory(span, mem),
            Param::Register(span, reg) => Destination::Register(span, reg),
            Param::Reference(span, refer) => Destination::Reference(span, refer),
            Param::Constant(span, _) => abort!(span, "Destination cannot be a constant"),
        }
    }
}

macro_rules! conditional {
    { ($($store: expr),*), $prefix: ident, $name: ident } => {
        paste::paste! {
            pub enum $name {
                #[doc= $name " to label or *Operand"]
                [<$prefix MP>](String),
                #[doc= $name " if equal"]
                [<$prefix E>](String),
                #[doc= $name " if zero"]
                [<$prefix Z>](String),
                #[doc= $name " if not equal"]
                [<$prefix NE>](String),
                #[doc= $name " if not zero"]
                [<$prefix NZ>](String),
                #[doc= $name " if negative"]
                [<$prefix S>](String),
                #[doc= $name " if non-negative"]
                [<$prefix NS>](String),
                #[doc= $name " if greater"]
                [<$prefix G>](String),
                #[doc= $name " if not less than equal (signed)"]
                [<$prefix NLE>](String),
                #[doc= $name " if greater than equal (signed)"]
                [<$prefix GE>](String),
                #[doc= $name " if not less (signed)"]
                [<$prefix NL>](String),
                #[doc= $name " if less (signed)"]
                [<$prefix L>](String),
                #[doc= $name " if not greater than equal (signed)"]
                [<$prefix NGE>](String),
                #[doc= $name " if less than equal"]
                [<$prefix LE>](String),
                #[doc= $name " if not greater (unsigned)"]
                [<$prefix NG>](String),
                #[doc= $name " if above (unsigned)"]
                [<$prefix A>](String),
                #[doc= $name " if not below or equal (unsigned)"]
                [<$prefix NBE>](String),
                #[doc= $name " if above or equal (unsigned)"]
                [<$prefix AE>](String),
                #[doc= $name " if not below (unsigned)"]
                [<$prefix NB>](String),
                #[doc= $name " if below (unsigned)"]
                [<$prefix B>](String),
                #[doc= $name " if not above or equal (unsigned)"]
                [<$prefix NAE>](String),
                #[doc= $name " if below or equal (unsigned)"]
                [<$prefix BE>](String),
                #[doc= $name " if not above (unsigned)"]
                [<$prefix NA>](String),
            }
        }
    }
}

conditional! {(String), J, Jump}
conditional! {(Destination), SET, Set}
conditional! {(Source, Destination), CMOV, CMov}

pub enum Param {
    Reference(Span, Reference),
    Register(Span, register::Register),
    Constant(Span, Constant),
    Memory(Span, String),
}

impl Param {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
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
    TST(Span, Source, Source),
    /// Push return address and jump to label or *Operand (location)
    CALL(Span, String),
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

    fn parse(inst: Ident, params: Vec<Param>) -> Self {
        match inst.to_string().to_uppercase().as_str() {
            "MOV" => {
                assert_pl!(
                    params<2>,
                    inst,
                    "MOV requires two arguments: destination, source"
                );
                Instruction::MOV(
                    inst.span(),
                    Destination::from(params[0]),
                    Source::from(params[1]),
                )
            }
            "PUSH" => {
                assert_pl!(params<1>, inst, "PUSH requires one arguments: source");
                Instruction::PUSH(inst.span(), Source::from(params[0]))
            }
            "POP" => {
                assert_pl!(params<1>, inst, "POP requires one arguments: destination");
                Instruction::POP(inst.span(), Destination::from(params[0]))
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
                assert_pl!(params<1>, inst, "INC requires one argument: destination");
                Instruction::INC(inst.span(), Destination::from(params[0]))
            }
            "DEC" => {
                assert_pl!(params<1>, inst, "DEC requires one argument: destination");
                Instruction::DEC(inst.span(), Destination::from(params[0]))
            }
            "NEG" => {
                assert_pl!(params<1>, inst, "NEG requires one argument: destination");
                Instruction::NEG(inst.span(), Destination::from(params[0]))
            }
            "NOT" => {
                assert_pl!(params<1>, inst, "NOT requires one argument: destination");
                Instruction::NOT(inst.span(), Destination::from(params[0]))
            }
            "LEAQ" => {
                assert_pl!(params<2>, inst, "LEAQ requires one argument: destination");
                Instruction::LEAQ(
                    inst.span(),
                    Destination::from(params[0]),
                    Source::from(params[1]),
                )
            }
            "ADD" => {}
            "SUB" => {}
            "IMUL" => {}
            "XOR" => {}
            "OR" => {}
            "AND" => {}
            "SAL" => {}
            "SHL" => {}
            "SAR" => {}
            "SHR" => {}
            "IMULQ" => {}
            "MULQ" => {}
            "IDIVQ" => {}
            "DIVQ" => {}
            "CMP" => {}
            "TST" => {}
            "CALL" => {}
            "LEAVE" => {}
            "RET," => {}

            "SET" => {}
            "J" => {}
            "CMOVE" => {}
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
