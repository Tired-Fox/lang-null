mod syscall;
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
use crate::abort;
use crate::compiler::assembly::syscall::syscall_nasm;
use crate::compiler::external::Extern;
use crate::compiler::Scope;
use crate::parser::token::{Block, Call, Declaration, Token};
pub(crate) use crate::platform;

pub trait ToAssembly {
    fn to_asm(&self, path: &str, scope: &Scope) -> (String, Option<Vec<Extern>>);
}

impl ToAssembly for Token {
    fn to_asm(&self, _path: &str, _scope: &Scope) -> (String, Option<Vec<Extern>>) {
        match self {
            Token::Call(call) => call.to_asm(_path, _scope),
            // Token::Ident(ident) => ident.to_asm(_path, _scope),
            // Token::Literal(literal) => literal.to_asm(_path, _scope),
            Token::Decleration(declaration) => declaration.to_asm(_path, _scope),
            Token::Block(block) => block.to_asm(_path, _scope),
            _ => (String::new(), None),
        }
    }
}

impl ToAssembly for Declaration {
    fn to_asm(&self, path: &str, scope: &Scope) -> (String, Option<Vec<Extern>>) {
        let path = if path.is_empty() {String::new()} else {format!("{}.", path)};
        let mut externals = None;
        let asm = match self {
            Declaration::Function{name, args, body, ..} => {
                let (block, exts) = match body{
                    Some(block) => {
                        block.to_asm(format!("{}__{}", path, name.to_string()).as_str(), scope)
                    },
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

impl ToAssembly for Call {
    fn to_asm(&self, path: &str, scope: &Scope) -> (String, Option<Vec<Extern>>) {
        match self {
            Call::Call{name, params, ..} => {
                if scope.method(name.to_string().as_str()).is_none() {
                    match syscall_nasm(name.to_string(), path, &params) {
                        Ok(result) => {
                            result
                        },
                        Err((message, help)) => {
                            abort!(message, help=help);
                        }
                    }
                } else {
                    (String::new(), None)
                }
            },
            Call::Syscall{name, params, ..} => {
                match syscall_nasm(name.to_string(), path, &params) {
                    Ok(result) => {
                        result
                    },
                    Err((message, help)) => {
                        abort!(message, help=help);
                    }
                }
            }
        }
    }
}

impl ToAssembly for Block {
    fn to_asm(&self, path: &str, scope: &Scope) -> (String, Option<Vec<Extern>>) {
        let mut block_scope = Scope::from(&self.0);
        block_scope.inherit(scope);

        let mut result = String::new();
        let mut externals = Vec::new();

        for token in self.0.iter() {
            let (nasm, ext) = token.to_asm(path, &block_scope);
            result.push_str(nasm.as_str());
            if let Some(ext) = ext {
                externals.extend(ext);
            }
        }

        (result, if externals.is_empty() { None } else {Some(externals)})
    }
}
