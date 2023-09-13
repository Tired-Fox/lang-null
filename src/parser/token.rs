use std::process::Command;

use crate::compiler::syscall::*;

pub struct Function {
    name: &'static str,
    args: Vec<&'static str>,
    types: Vec<&'static str>,
    body: Option<String>,
}

pub struct Builtin(pub String);

macro_rules! function {
    [$name: ident ($($arg: ident: $type: expr),*)] => {
        Function {
            name: stringify!($name),
            args: vec![$(stringify!($arg),)*],
            types: vec![$(stringify!($type),)*],
            body: None
        }
    }
}

pub fn tester() {
    use std::fs;

    let (result, externs) = exit(69);
    let file = format!(
        r#"global start
{}
section .text
start:
{}
"#,
        match externs.as_ref() {
            Some(many) => {
                many.iter()
                    .map(|e| format!("extern {}", e.name))
                    .collect::<Vec<String>>()
                    .join("\n")
            }
            None => String::new(),
        },
        result
    );

    let _ = fs::write("dist/test.asm", file);
    let _ = Command::new("toolchain/nasm/nasm.exe")
        .args(["-fwin64", "dist/test.asm", "dist/test.obj"])
        .spawn();

    let mut args = vec![
        "/console",
        "dist/test.obj",
        "/fo",
        "dist/test.exe",
        "dist/test.obj",
    ];
    if let Some(imports) = externs.as_ref() {
        for import in imports.iter() {
            args.push(import.link);
        }
    }
    let _ = Command::new("toolchain/GoLink/GoLink.exe")
        .args(args)
        .spawn();

    let status = Command::new("./dist/test.exe").status();
    println!("Exit Code: {:?}", status);
}
