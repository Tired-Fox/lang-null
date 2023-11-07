use std::collections::HashMap;
use std::fmt::Display;
use std::fs;
use std::io::Read;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use nasm_to_string::nasm;

use crate::err;
use crate::compiler::assembly::ToAssembly;
use crate::compiler::external::Extern;
use crate::error::{Error, Errors};
use crate::parser::{Parser, token::{Declaration, Token}};
use crate::parser::token::{Block, Call, Literal, Parameter, Punctuated};

pub mod external;
pub mod assembly;
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

            parser: Parser::with_path(path),
            externals: HashMap::new(),
            errors: Vec::new(),

            chunks: Vec::new(),
            data: Vec::new(),
        }.compile(name.as_str());
    }

    pub fn source(self, name: &str, source: &str) {
        Compiler {
            _nasm_root: self.nasm,
            _link_root: self.link,

            parser: Parser::with_source(source),
            externals: HashMap::new(),
            errors: Vec::new(),

            chunks: Vec::new(),
            data: Vec::new(),
        }.compile(name);
    }
}

struct Scope<'scope> {
   methods: HashMap<String, &'scope Declaration>,
   variables: HashMap<String, &'scope Declaration>,
}

impl<'scope> Scope<'scope> {
    fn new() -> Self {
        Scope {
            methods: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn method(&self, name: &str) -> Option<&'scope Declaration> {
        self.methods.get(name).copied()
    }

    fn variable(&self, name: &str) -> Option<&'scope Declaration> {
        self.variables.get(name).copied()
    }

    fn add_method(&mut self, name: String, decl: &'scope Declaration) {
        self.methods.insert(name, decl);
    }

    fn add_variable(&mut self, name: String, decl: &'scope Declaration) {
        self.variables.insert(name, decl);
    }
}

impl<'scope> From<&'scope Vec<Token>> for Scope<'scope> {
    fn from(tokens: &'scope Vec<Token>) -> Self {
        let mut scope = Scope::new();
        for token in tokens.iter() {
            if let Token::Decleration(decl) = token {
                match decl {
                    Declaration::Function{name, ..} => {
                        scope.add_method(name.to_string(), decl);
                    },
                    Declaration::Variable{name, ..} => {
                        scope.add_variable(name.to_string(), decl);
                    }
                }
            }
        }
        scope
    }
}

pub struct Compiler {
    _nasm_root: PathBuf,
    _link_root: PathBuf,

    parser: Parser,
    errors: Vec<Error>,
    chunks: Vec<String>,

    data: Vec<String>,

    // External name to external link
    externals: HashMap<String, String>,
}

impl Compiler {
    pub fn builder() -> Builder {
        Builder::default()
    }

    pub fn with_path<P: AsRef<std::path::Path>>(path: P) -> Self {
        Compiler::new(Parser::with_path(path))
    }

    pub fn with_source(source: &str) -> Compiler {
        Compiler::new(Parser::with_source(source))
    }

    fn new(parser: Parser) -> Compiler {
        Compiler {
            parser,
            chunks: Vec::new(),
            data: Vec::new(),

            externals: HashMap::new(),
            errors: Vec::new(),

            _nasm_root: PathBuf::new(),
            _link_root: PathBuf::new(),
        }
    }

    fn process_scope(&self, tokens: &Vec<Token>) -> (String, Vec<Extern>) {
        let scope = Scope::from(tokens);
        let mut errors = Vec::new();
        let mut externals = Vec::new();
        let mut result = String::new();

        for token in tokens.iter() {
            match token {
                Token::Call(call) => {
                    if let Call::Call { name, params, .. } = call {
                        if let Some(decl) = scope.method(name.to_string().as_str()) {
                            todo!("Not yet implemented");
                            // for param in params {
                            //
                            // }
                        } else if syscall::SYSCALLS.contains(&name.to_string().as_str()) {
                            match name.to_string().as_str() {
                                "exit" => {
                                    if params.len() != 1 {
                                        errors.push(err!(
                                            path=self.parser.file(),
                                            span=token.location(),
                                            "Invalid number of arguments for syscall",
                                            help=["Expected 1 argument"]
                                        ));
                                        break;
                                    }
                                    let code = match params.iter().last().unwrap() {
                                        Parameter::Literal(Literal::Number { value, .. }) => {
                                            value.parse::<i32>().unwrap()
                                        },
                                        _ => {
                                            errors.push(err!(
                                                path=self.parser.file(),
                                                span=token.location(),
                                                "Invalid argument for syscall",
                                                help=["Expected literal"]
                                            ));
                                            break
                                        }
                                    };
                                    let (nasm, links) = syscall::exit(code);
                                    if let Some(externs) = links {
                                        for external in externs {
                                            externals.push(external);
                                        }
                                    }
                                    result.push_str(nasm.as_str());
                                }
                                _ => { todo!("Syscall not yet implemented {}", name.to_string()) }
                            }
                        }
                    }
                },
                Token::Decleration(decl) => {
                    match decl {
                        Declaration::Function{name, args, body, ..} => {
                            if let Some(Block(tokens, _)) = body {
                                let (nasm, links) = self.process_scope(tokens);
                                externals.extend(links);
                                result.push_str(format!("__{}:\n", name.to_string()).as_str());
                                result.push_str(nasm.as_str());
                                result.push_str("\nRET");
                            }
                        },
                        Declaration::Variable{name, value, ..} => {
                            todo!("Not yet implemented");
                        }
                    }
                }
                _ => errors.push(err!(
                    path=self.parser.file(),
                    span=token.location(),
                    "Only declarations are allowed in the global scope"
                ))
            }
        }

        if !errors.is_empty() {
            Errors(&self.errors).render(self.parser.source());
            std::process::exit(1);
        }
        (result, externals)
    }

    fn assemble(&mut self, name: &str) {
        // First parse the source
        self.parser.parse();

        let (_, externals) = syscall::exit(0);
        if let Some(externals) = externals {
            for external in externals {
                if !self.externals.contains_key(external.name) {
                    self.externals.insert(external.name.to_string(), external.link.to_string());
                }
            }
        }

        let global: Scope = Scope::from(&self.parser.tokens);
        if global.method("main").is_none() {
            panic!("Main function is missing");
        }

        let (result, externals) = self.process_scope(&self.parser.tokens);
        for external in externals {
            self.externals.insert(external.name.to_string(), external.link.to_string());
        }
        self.chunks.push(result);

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
        let mut result = Command::new(Target::exe_with_root(&self._nasm_root, "nasm"))
            .args([
                Target::assembly(),
                Target::root("nasm").join(name.to_string() + ".asm").display().to_string().as_str(),
                "-o",
                Target::root("obj").join(name.to_string() + ".obj").display().to_string().as_str(),
            ])
            .stdout(Stdio::piped())
            .spawn()
            .expect("Nasm command failure");

        let exit_code = result.wait().unwrap();
        let output = result.wait_with_output().unwrap();

        if !exit_code.success() {
            eprintln!("{}", String::from_utf8(output.stdout).unwrap());
            std::process::exit(exit_code.code().unwrap())
        }
    }

    fn link(&self, name: &str) {
        println!("{} /console /entry __null {} {} /fo {}",
                 Target::exe_with_root(&self._link_root, "GoLink"),
                 Target::root("obj").join(name.to_string() + ".obj").display().to_string().as_str(),
                 self.externals
                     .iter()
                     .map(|(_name, link)| link.to_string())
                     .collect::<Vec<String>>().join(" "),
                 Target::root("build").join(name.to_string() + Target::executable()).display().to_string().as_str(),
        );
        let mut result= Command::new(Target::exe_with_root(&self._link_root, "GoLink"))
            .args([
                "/console",
                "/entry",
                "__null", /* TODO: Make dynamic unique */
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
            .stdout(Stdio::piped())
            .spawn()
            .expect("GoLink command failure");

        let output = result.wait_with_output().unwrap();
        let output = String::from_utf8(output.stdout).unwrap();

        if output.contains("Warning!") || output.contains("Error!") {
            eprintln!("{}", output);
            std::process::exit(1)
        }
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
            global __null
            {
                self.externals.keys().map(|name| {
                    format!("extern {}", name)
                }).collect::<Vec<_>>().join("\n")
            }

            section .text
            {self.chunks.join("\n")}

            __null:
                ; "TODO: Setup for argv to pass to main" ;
                call __main

                {syscall::exit(0).0}

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