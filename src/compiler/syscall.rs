use std::fmt::Display;

use super::external::*;
use nasm_to_string::nasm;
use super::assembly::platform;

pub type SyscallRet = (String, Option<Vec<Extern>>);

pub struct Syscall(&'static str);
impl Display for Syscall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        return write!(f, "{}", self.0);
    }
}

macro_rules! syscalls {
    ($($name: ident: [$linux: literal, $macos: literal, $windows: literal]),* $(,)?) => {
        $(
            paste::paste!{
                #[cfg(target_os="linux")]
                const [<SYSCALL_ $name:upper>]: Syscall = Syscall($linux);
                #[cfg(target_os="macos")]
                const [<SYSCALL_ $name:upper>]: Syscall = Syscall($macos);
                #[cfg(target_os="windows")]
                const [<SYSCALL_ $name:upper>]: Syscall = Syscall($windows);
            }
        )*
        pub const SYSCALLS: &[&'static str] = &[
            $(stringify!($name),)*
        ];
    };
}

syscalls! {
    exit: ["60", "0x02000001", "ExitProcess"],
    print: ["1", "0x02000004", "printf"]
}

pub fn exit(exit_code: i32) -> SyscallRet {
    platform!(
        "windows",
        nasm![
            mov rcx, {exit_code}
            call {SYSCALL_EXIT}
        ],
        [ExitProcess]
    );

    platform!(
        "unix",
        nasm![
            mov ebx, {exit_code}
            mov eax, {SYSCALL_EXIT}
            syscall
        ]
    )
}

pub fn print(location: String, _length: usize) -> SyscallRet {
    platform!(
        "windows",
        nasm![
            mov rcx, {location}
            call {SYSCALL_PRINT}
        ],
        [PrintF]
    );

    platform!(
        "unix",
        nasm![
            mov  rdi, 1          ; STDOUT ;
            move rsi, {location}
            move rdx, {length}
            mov  rax, {SYSCALL_PRINT}
            syscall
        ]
    )
}
