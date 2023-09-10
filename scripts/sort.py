lns = """\
    LessLessLess: "`<<<`",
    GreaterGreaterGreater: "`>>>`",
    LessLess: "`<<`",
    GreaterGreater: "`>>`",
    Less: "`<`",
    Greater: "`>`",
    Equal: "`=`",
    LessEqual: "`<=`",
    GreaterEqual: "`>=`",
    EqualEqual: "`==`",
    BangEqual: "`!=`",
    Bang: "`!`",
    AmpAmp: "`&&`",
    PipePipe: "`||`",
    Amp: "`&`",
    Pipe: "`|`",
    Caret: "`^`",
    CaretEqual: "`^=`",
    PipeEqual: "`|=`",
    AmpEqual: "`&=`",
    LessEqualGreater: "`<=>`",
    Tilde: "`~`",
    TildeEqual: "`~=`",
    LessLessEqual: "`<<=`",
    GreaterGreaterEqual: "`>>=`",
    ColonEqual: "`:=`",
    Plus: "`+`",
    Minus: "`-`",
    PlusEqual: "`+=`",
    MinusEqual: "`-=`",
    Slash: "`/`",
    SlashEqual: "`/=`",
    Star: "`*`",
    StarEqual: "`*=`",
    Percent: "`%`",
    PercentEqual: "`%=`",
    At: "`@`",
    Colon: "`:`",
    Comma: "`,`",
    Dot: "`.`",
    Question: "`?`",
    Backslash: "`\\`",
    Semicolon: "`;`",
    MinusMinus: "`--`",
    OpenSquare: "`[`",
    CloseSquare: "`]`",
    OpenParen: "`(`",
    CloseParen: "`)`",
    OpenBrace: "`{`",
    CloseBrace: "`}`",
"""

def sort_lines(lines: str, end_on: str = "") -> str:
    cmp = lambda x, y: x < y
    if end_on != "":
        cmp = lambda x, y: x[:x.index(end_on)] < y[:y.index(end_on)]
    lines = lines.split('\n')
    lines = sorted(lines)
    return f"\n".join(lines)

if __name__ == "__main__":
    print(sort_lines(lns, ":"))

