use std::fmt::Display;

#[derive(Debug)]
pub struct Extern {
    pub name: &'static str,
    pub link: &'static str,
}

impl Display for Extern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.link)
    }
}

macro_rules! externs {
    {$($name: ident: $link: literal),* $(,)*} => {
        $(
            #[allow(non_upper_case_globals)]
            pub const $name: Extern = Extern { name: stringify!($name), link: $link  };
        )*
    };
}

externs! {
    ExitProcess: "kernel32.dll"
}
