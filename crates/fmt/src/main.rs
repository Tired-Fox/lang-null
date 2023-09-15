extern crate rfmt;
use rfmt::{args, named, pos, RFmtString};

// format_string := text [ maybe_format text ] *
// maybe_format := { { | } } | format
// format := { [ arg ] [ : fromat_spec ] [ ws ] * }
// arg := int | ident

// format_spec := [[fill]align][sign]['#']['0'][width]['.' precision]type
// fill := character
// align := < ^ >
// sign := + | -
// width := count
// precision := count | *
// type := '' | ? | x? | X? | ident
// count := parameter | integer
// parameter := argument '$'

fn main() {
    let rformat = "1: {} hello: {hello}, world: {1}! {world:<b.prec$} {{}}";
    let format = String::from(rformat);

    let exit_code = 69;

    rformat.fmt(args![exit_code, hello: "Hello", "world"]);
    // format.fmt(pos![exit_code]);
    // rformat.fmt(named![exit_code]);
    println!("{:1<10.0}", 10.32)
}
