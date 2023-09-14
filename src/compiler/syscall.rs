use std::fmt::Display;

use super::external::*;
use null_macros::nasm;

pub type SyscallRet = (String, Option<Vec<Extern>>);

struct Syscall(&'static str, &'static str, &'static str);
impl Display for Syscall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(target_os = "linux")]
        return write!(f, "{}", self.0);
        #[cfg(target_os = "macos")]
        return write!(f, "{}", self.1);
        #[cfg(target_os = "windows")]
        return write!(f, "{}", self.2);
    }
}

macro_rules! syscalls {
    ($($name: ident: [$linux: literal, $macos: literal, $windows: literal]),* $(,)?) => {
        $(
            paste::paste!{
                const [<SYSCALL_ $name:upper>]: Syscall = Syscall($linux, $macos, $windows);
            }
        )*
    };
}

syscalls! {
    exit: ["60", "0x02000001", "ExitProcess"]
}

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

pub fn exit(exit_code: i32) -> SyscallRet {
    platform!(
        "windows",
        nasm![
            mov exc, {exit_code}
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
