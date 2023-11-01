#[macro_export]
macro_rules! platform {
    ($name: literal, $asm: expr, [$($externs: ident),* $(,)?]) => {
        #[cfg(target_os=$name)]
        return ($asm, Some(vec![$($externs,)*]))
    };
    ($name: literal, $asm: expr $(,)?) => {
        #[cfg(target_os=$name)]
        return ($asm, None)
    };
}

use nasm_to_string::nasm;
use crate::compiler::external::Extern;
use crate::parser::token::{Block, Declaration};
pub(crate) use crate::platform;

pub trait ToAssembly {
    fn to_asm(&self, path: &str) -> (String, Option<Vec<Extern>>);
}

impl ToAssembly for Declaration {
    fn to_asm(&self, path: &str) -> (String, Option<Vec<Extern>>) {
        let path = if path.is_empty() {String::new()} else {format!("{}.", path)};
        let mut externals = None;
        let asm = match self {
            Declaration::Function{name, args, body, ..} => {
                let (block, exts) = match body{
                    Some(body) => body.to_asm(format!("{}__{}", path, name.to_string()).as_str()),
                    None => (String::new(), None),
                };
                externals = exts;
                nasm! {
                   {format!("{}__{}:", path, name.to_string())}
                       {block}
                       RET
                }
            },
            Declaration::Variable{name, value, ..} => {
                nasm![{format!("{}__{}:", path, name.to_string())} db {0}]
            }
        };
        (asm, externals)
    }
}

impl ToAssembly for Block {
    fn to_asm(&self, path: &str) -> (String, Option<Vec<Extern>>) {
        // let mut externals = Vec::new();
        // let asm = Vec::new();
        (String::new(), None)
    }
}
