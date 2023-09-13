// {https://www.felixcloutier.com/x86/}[Reference]
// Unfinished

macro_rules! cond_check {
    ($($name: ident: $doc: literal),* $(,)?) => {
        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
        pub enum CC {
            $(
                #[doc=$doc]
                $name,
            )*
        }

        impl CC {
            pub fn new(name: &str) -> Option<CC> {
                match name.to_uppercase().as_str() {
                    $(
                        stringify!($name) => Some(CC::$name),
                    )*
                    _ => None
                }
            }
        }

        impl std::fmt::Display for CC {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(
                    f,
                    "{}",
                    match self {
                        $(
                            CC::$name => stringify!($name),
                        )*
                    }
                )
            }
        }
    };
}

cond_check! {
    A: "Above",
    AE: "Above or equal",
    B: "Below",
    BE: "Below or equal",
    C: "Carry",
    E: "Equal",
    G: "Greater",
    GE: "Greater than equal",
    L: "Less",
    LE: "Less than equal",
    NA: "Not above",
    NAE: "Not above or equal",
    NB: "Not below",
    NBE: "Not below or equal",
    NC: "Not carry",
    NE: "Not equal",
    NG: "Not greater",
    NGE: "Not greater than equal",
    NL: "Not less",
    NLE: "Not less than equal",
    NO: "Not overflow",
    NP: "Not parity",
    NS: "Not sign",
    NZ: "Not zero",
    O: "Overflow",
    P: "Parity",
    PE: "Parity even",
    PO: "Parity odd",
    S: "Sign",
    Z: "Zero",
}

macro_rules! instructions {
    (
        cc: {$($cc: ident[$cs: literal]: $dc: literal),* $(,)?},
        decl: {$($val: ident: $dn: literal),* $(,)?},
        $($name: ident[$size: literal]: $doc: literal),* $(,)?
    ) => {
        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
        pub enum Instruction {
            $(
                #[doc=$dc]
                $cc(CC),
            )*
            $(
                #[doc=$dn]
                $val,
            )*
            $(
                #[doc=$doc]
                $name,
            )*
        }

        impl Instruction {
            pub fn count<T>(&self, list: Vec<T>) -> bool {
                match self {
                    $(
                        Instruction::$cc(_) => {
                            return list.len() == $cs;
                        }
                    )*
                    $(
                        Instruction::$val => return list.len() == 1,
                    )*
                    $(
                        Instruction::$name => {
                            return list.len() == $size;
                        }
                    )*
                }
            }

            pub fn new(name: &str) -> Option<Instruction> {
                match name.to_uppercase().as_str() {
                    $(
                        stringify!($name) => Some(Instruction::$name),
                    )*
                    $(
                        stringify!($val) => Some(Instruction::$val),
                    )*
                    $(
                        i if i.starts_with(stringify!($cc)) => {
                            if let Some(cc) = CC::new(&i[stringify!($cc).len()..]) {
                                Some(Instruction::$cc(cc))
                            } else {
                                None
                            }
                        }
                    )*
                    _ => None
                }
            }
        }

        impl std::fmt::Display for Instruction {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Instruction::$cc(cc) => {
                            write!(f, "{}{}", stringify!($cc), cc)
                        }
                    )*
                    $(
                        Instruction::$val => write!(f, stringify!($val)),
                    )*
                    $(
                        Instruction::$name => write!(f, stringify!($name)),
                    )*
                }
            }
        }
    };
}

instructions! {
    cc: {
        CMOV[2]: "Conditionally mov",
        J[1]: "Conditionally jump",
        SET[1]: "Conditionally set"
    },

    decl: {
        DB: "Declare initialized byte data",
        DW: "Declare initialized word data",
        DD: "Declare initialized dword data",
        DQ: "Declare initialized qword data",
        DT: "Declare initialized tword data",
        DO: "Declare initialized oword data",
        DY: "Declare initialized yword data",
        DZ: "Declare initialized zword data",

        RESB: "Declare uninitialized byte data",
        RESW: "Declare uninitialized word data",
        RESD: "Declare uninitialized dword data",
        RESQ: "Declare uninitialized qword data",
        REST: "Declare uninitialized tword data",
        RESO: "Declare uninitialized oword data",
        RESY: "Declare uninitialized yword data",
        RESZ: "Declare uninitialized zword data",
    },

    AAA[0]: "ascii adjust after addition",
    AAD[0]: "ascii adjust ax before division",
    AAM[0]: "ascii adjust ax after multiply",
    AAS[0]: "ascii adjust al after subtraction",

    ADC[2]: "add with carry",
    ADCX[2]: "unsigned integer addition of two operands with carry flag",
    ADD[2]: "add source to destination",
    ADOX[2]: "unsigned integer addition of source to destination ith overflow flag",

    AND[2]: "logical and",
    ANDN[2]: "logical and not",

    BLSI[2]: "extract lowest set isolated bit",
    BLSMSK[2]: "get mask up to lowest set bit",
    BLSR[2]: "reset lowest set bit",

    BSF[2]: "bit scan forward",
    BSR[2]: "bit scan reverse",
    BSWAP[1]: "byte swap",
    BT[2]: "bit test",
    BTC[2]: "bit test and complement",
    BTR[2]: "bit test and reset",
    BTS[2]: "bit test and set",
    BZHI[2]: "zero high bits starting with specified bit position",

    CALL[1]: "call procedure",

    CBW[0]: "convert byte to word/convert word to doubleword/convert doubleword toQuadword",
    CDQ[0]: "convert word to doubleword/convert doubleword to quadword",
    CDQE[0]: "convert byte to word/convert word to doubleword/convert doubleword toQuadword",

    CLAC[0]: "clear ac flag in eflags register",
    CLC[0]: "clear carry flag",
    CLD[0]: "clear direction flag",
    CLI[0]: "clear interrupt flag",
    CLRSSBSY[0]: "clear busy flag in a supervisor shadow stack token",
    CLTS[0]: "clear task-switched flag in cr0",
    CLUI[0]: "clear user interrupt flag",

    CMC[0]: "complement carry flag",

    CMP[2]: "compare two operands",

    CQO[0]: "convert word to doubleword/convert doubleword to quadword",
    CWD[0]: "convert word to doubleword/convert doubleword to quadword",
    CWDE[0]: "convert byte to word/convert word to doubleword/convert doubleword toQuadword",

    DEC[1]: "decrement by 1",
    DIV[1]: "unsigned divide",

    IDIV[1]: "signed divide",
    IMUL[1]: "signed multiply",

    INC[1]: "increment by 1",
    INT[1]: "n	call to interrupt procedure",
    INTO[1]: "call to interrupt procedure if overflow",

    JMP[1]: "jump",

    LDS[1]: "load far pointer",
    LES[1]: "load far pointer",
    LFS[1]: "load far pointer",
    LEA[2]: "load effective address",

    MOV[2]: "move",

    MOVD[2]: "move doubleword/move quadword",
    MOVQ[2]: "move doubleword/move quadword",
    MOVSX[2]: "move with sign-extension",
    MOVZX[2]: "move with zero-extend",

    MUL[1]: "unsigned multiply",
    MULX[1]: "unsigned multiply without affecting flags",
    NEG[1]: "two's complement negation",
    NOP[0]: "no operation",
    NOT[1]: "one's complement negation",
    OR[2]: "logical inclusive or",

    PUSH[1]: "push word, doubleword, or quadword onto the stack",
    RCL[1]: "rotate left with carry flag",
    RCR[1]: "rotate right with carry flag",

    RET[0]: "return from procedure",

    ROL[1]: "rotate left",
    ROR[1]: "rotate right",
    RORX[1]: "rotate right logical without affecting flags",

    SAL[1]: "shift left",
    SAR[1]: "shift right",
    SARX[1]: "shift without affecting flags",

    SHL[1]: "shift left",
    SHLX[1]: "shift without affecting flags",

    SHR[1]: "shift right",
    SHRX[1]: "shift without affecting flags",

    STC[0]: "set carry flag",
    STD[0]: "set direction flag",
    STI[0]: "set interrupt flag",

    SUB[2]: "subtract",
    SYSCALL[1]: "fast system call",

    TEST[2]: "logical compare",
    XOR[2]: "logical exclusive or",
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn count() {
        assert!(Instruction::ADD.count(vec!["0", "1"]));
    }

    #[test]
    fn normal() {
        assert!(Instruction::new("CALL").is_some());
        assert_eq!(Instruction::CALL.to_string(), "CALL".to_string());
    }

    #[test]
    fn conditional() {
        assert!(Instruction::new("JE").is_some());
        assert_eq!(Instruction::new("JE"), Some(Instruction::J(CC::E)));
        assert_eq!(Instruction::J(CC::E).to_string(), "JE".to_string());
    }
}
