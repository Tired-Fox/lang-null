use std::collections::HashMap;
use std::fmt::Display;
use std::fs;
use std::path::PathBuf;
use std::process::Command;

use nasm_to_string::nasm;
use crate::abort;

use crate::parser::{Parser, token::{Declaration, Token}};
use crate::parser::token::{Call, Literal, Parameter};

pub mod external;
pub mod syscall;

struct Target;

impl Target {
    fn root(path: &str) -> PathBuf {
        PathBuf::from("dist").join(path)
    }

    fn exe_with_root(path: &PathBuf, exe: &str) -> String {
        if path.display().to_string().is_empty() {
            exe.to_string()
        } else {
            path.join(exe.to_string() + Target::executable()).display().to_string()
        }
    }

    fn assembly() -> &'static str {
        #[cfg(target_os = "windows")]
        return "-fwin64";
        #[cfg(target_os = "linux")]
        return "-felf64";
        #[cfg(target_os = "macos")]
        return "-fmacho64";
    }

    fn executable() -> &'static str {
        #[cfg(target_os = "windows")]
        return ".exe";
        #[cfg(not(target_os = "windows"))]
        return "";
    }
}

#[derive(Default)]
pub struct Builder {
    nasm: PathBuf,
    link: PathBuf,
}

impl Builder {
    pub fn nasm<P: AsRef<std::path::Path>>(mut self, path: P) -> Self {
        self.nasm = path.as_ref().to_path_buf();
        self
    }

    pub fn go_link<P: AsRef<std::path::Path>>(mut self, path: P) -> Self {
        self.link = path.as_ref().to_path_buf();
        self
    }

    pub fn path<P: AsRef<std::path::Path>>(self, path: P) {
        let name = path.as_ref().to_path_buf().file_stem().unwrap().to_str().unwrap().to_string();
        Compiler {
            _nasm_root: self.nasm,
            _link_root: self.link,

            parser: Parser::path(path),
            externals: HashMap::new(),

            chunks: Vec::new(),
            methods: Vec::new(),
            data: Vec::new(),
        }.compile(name.as_str());
    }

    pub fn source(self, name: &str, source: &str) {
        Compiler {
            _nasm_root: self.nasm,
            _link_root: self.link,

            parser: Parser::source(source),
            externals: HashMap::new(),

            chunks: Vec::new(),
            methods: Vec::new(),
            data: Vec::new(),
        }.compile(name);
    }
}

pub struct Compiler {
    _nasm_root: PathBuf,
    _link_root: PathBuf,

    parser: Parser,
    chunks: Vec<String>,

    methods: Vec<String>,
    data: Vec<String>,

    // External name to external link
    externals: HashMap<String, String>,
}

impl Compiler {
    pub fn builder() -> Builder {
        Builder::default()
    }

    pub fn path<P: AsRef<std::path::Path>>(path: P) -> Self {
        Compiler::new(Parser::path(path))
    }

    pub fn source(source: &str) -> Compiler {
        Compiler::new(Parser::source(source))
    }

    fn new(parser: Parser) -> Compiler {
        Compiler {
            parser,
            chunks: Vec::new(),
            methods: Vec::new(),
            data: Vec::new(),

            externals: HashMap::new(),

            _nasm_root: PathBuf::new(),
            _link_root: PathBuf::new(),
        }
    }

    fn assemble(&mut self, name: &str) {
        // First parse the source
        self.parser.parse();

        let global = self.parser.global();
        if global.get_function("main").is_none() {
            panic!("Main function is missing");
        }

        for token in self.parser.tokens.iter() {
            match token {
                Token::Decleration(decl) => {
                    match decl {
                        Declaration::Function(_name, _args, _body) => {
                            todo!("Implement function declaration")
                        }
                        Declaration::Variable(_name, _value) => {
                            todo!("Implement variable declaration")
                        }
                    }
                }
                Token::Call(call) => {
                    match call {
                        Call::Call(_name, _args) => {
                            todo!("Implement function call")
                        }
                        Call::Syscall(name, args) => {
                            match name.to_string().as_str() {
                                "exit" => {
                                    match args.len() {
                                        0 => {
                                            let (nasm, ext) = syscall::exit(0);
                                            self.chunks.push(nasm);
                                            if let Some(externals) = ext {
                                                for external in externals {
                                                    if !self.externals.contains_key(external.name) {
                                                        self.externals.insert(external.name.to_string(), external.link.to_string());
                                                    }
                                                }
                                            }
                                        }
                                        _ => {
                                            let exit_code = if let Parameter::Literal(num) = &args[0] {
                                                if let Literal::Number(num) = num {
                                                    i32::from_str_radix(num, 10).unwrap()
                                                } else {
                                                    panic!("Invalid parameter type to syscall exit(); expected `i32`")
                                                }
                                            } else {
                                                panic!("Invalid parameter type to syscall exit(); expected Literal")
                                            };

                                            let (nasm, ext) = syscall::exit(exit_code);
                                            self.chunks.push(nasm);
                                            if let Some(externals) = ext {
                                                for external in externals {
                                                    if !self.externals.contains_key(external.name) {
                                                        self.externals.insert(external.name.to_string(), external.link.to_string());
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                _ => abort!("Only function declarations and function calls are allowed in the global scope")
            }
        }

        // To out file
        fs::write(Target::root("nasm").join(name.to_string() + ".asm"), self.to_string()).expect("Failed to write asm file");
    }

    fn object(&self, name: &str) {
        println!("{} {} {} -o {}",
            Target::exe_with_root(&self._nasm_root, "nasm"),
            Target::assembly(),
            Target::root("nasm").join(name.to_string() + ".asm").display().to_string().as_str(),
            Target::root("obj").join(name.to_string() + ".obj").display().to_string().as_str(),
        );
        Command::new(Target::exe_with_root(&self._nasm_root, "nasm"))
            .args([
                Target::assembly(),
                Target::root("nasm").join(name.to_string() + ".asm").display().to_string().as_str(),
                "-o",
                Target::root("obj").join(name.to_string() + ".obj").display().to_string().as_str(),
            ])
            .output()
            .expect("Nasm command failure");
    }

    fn link(&self, name: &str) {
        println!("{} /console /entry __null_main {} {} /fo {}",
            Target::exe_with_root(&self._link_root, "GoLink"),
            Target::root("obj").join(name.to_string() + ".obj").display().to_string().as_str(),
            self.externals
                .iter()
                .map(|(_name, link)| link.to_string())
                .collect::<Vec<String>>().join(" "),
            Target::root("build").join(name.to_string() + Target::executable()).display().to_string().as_str(),
        );
        Command::new(Target::exe_with_root(&self._link_root, "GoLink"))
            .args([
                "/console",
                "/entry",
                "start", /* TODO: Make dynamic unique */
                Target::root("obj").join(name.to_string() + ".obj").display().to_string().as_str(),
            ])
            .args(self.externals
                .iter()
                .map(|(_name, link)| link.to_string())
                .collect::<Vec<String>>()
                .as_slice())
            .args([
                "/fo",
                Target::root("build").join(name.to_string() + Target::executable()).display().to_string().as_str(),
            ])
            .output()
            .expect("GoLink command failure");
    }

    /// Creates a new target dir with obj, nasm, and build sub dirs only if they are missing.
    pub fn init() {
        fs::create_dir_all(Target::root("obj")).unwrap();
        fs::create_dir_all(Target::root("nasm")).unwrap();
        fs::create_dir_all(Target::root("build")).unwrap();
    }

    /// Clean up the obj and nasm dirs. Used for after compilation.
    pub fn cleanse() {
        if Target::root("").exists() {
            fs::remove_dir_all(Target::root("obj")).unwrap();
            fs::remove_dir_all(Target::root("nasm")).unwrap();
            fs::remove_dir_all(Target::root("build")).unwrap();
        }
    }

    /// Assembles, links, and compiles `null` source code from the compiler instance.
    pub fn compile(&mut self, name: &str) {
        self.assemble(name);
        self.object(name);
        self.link(name);
    }
}

impl Display for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let assembly = nasm! {
            global __null_main
            {
                self.externals.keys().map(|name| {
                    format!("extern {}", name)
                }).collect::<Vec<_>>().join("\n")
            }

            section .text
            {self.methods.join("\n")}

            __null_main:
                {self.chunks.join("\n\n")}

            section .data
            {self.data.join("\n")}
        };

        write!(
            f,
            "{}",
            assembly
        )
    }
}