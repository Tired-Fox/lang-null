/// exit :: () {
///     sys_call::exit(value);
/// }
#[macro_export]
macro_rules! built_in {
    ($name:ident :: ($($arg: ident: $($arg_type: tt)*),* $(,)?) {$($body: tt)*}) => {
        $crate::parser::token::Function::Decleration(
            $crate::parser::token::Ident(stringify!($name).to_string()),
            $crate::parser::token::Punctuated {
                values: vec![
                    $crate::parser::token::Argument{
                        name: $crate::parser::token::Ident(stringify!($($arg)*)),
                        r#type: $crate::parser::token::Ident(stringify!($($arg_type)*)),
                    }
                ]
            },
            $crate::parser::token::Block(Vec::new())
        )
    };
    (sys $name:ident :: ($($arg: ident: $($arg_type: tt)*),* $(,)?)) => {
         $crate::parser::token::Call::Syscall(
             $crate::parser::token::Ident(stringify!($name).to_string()),
             $crate::parser::token::Punctuated(vec![
                 $($crate::parser::token::Parameter::Ident($crate::parser::token::Ident(stringify!($arg).to_string())),)*
             ])
         )
    }
}