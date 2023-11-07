use crate::compiler::external::Extern;
use crate::compiler::syscall;
use crate::parser::token::{Literal, Parameter, Punctuated};

pub fn syscall_nasm(name: String, path: &str, args: &Punctuated<Parameter, ','>) -> Result<(String, Option<Vec<Extern>>), (String, Vec<String>)> {
    match name.to_string().as_str() {
        "exit" => {
            if args.len() != 1 {
                return Err((
                    "Invalid number of arguments for syscall".to_string(),
                    vec!["Expected 1 argument".to_string()]
                ));
            }
            let code = match args.iter().last().unwrap() {
                Parameter::Literal(Literal::Number { value, .. }) => {
                    value.parse::<i32>().unwrap()
                },
                _ => {
                    return Err((
                        "Invalid argument for syscall".to_string(),
                        vec!["Expected literal".to_string()]
                    ));
                }
            };
            Ok(syscall::exit(code))
        },
        val => {
            Err((format!("Syscall does not have assembly: {}", val), Vec::new()))
        }
    }
}