use std::fmt::Display;

use super::external::*;
use null_macros::asm;

pub type SyscallRet = (String, Option<Vec<Extern>>);

struct Syscall(&'static str, &'static str);
impl Display for Syscall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[cfg(target_os = "linux")]
        return self.0;
        #[cfg(target_os = "macos")]
        return self.1;
        #[cfg(not(target_os = "unix"))]
        panic!("The current OS and architecture doesn't support the current syscall")
    }
}

macro_rules! syscalls {
    ($($name: ident: [$linux: literal, $macos: literal]),* $(,)?) => {
        cfg_if::cfg_if! {
            if #[cfg(unix)] {
                $(
                    paste::paste!{
                        const [<SYSCALL_ $name:upper>]: Syscall = Syscall($linux, $macos);
                    }
                )*
            }
        }
    };
}

syscalls! {
    exit: ["60", "0x02000001"]
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
        asm![
            mov exc, {exit_code}
            call {ExitProcess.name}
        ],
        [ExitProcess]
    );

    platform!(
        "unix",
        asm![
            mov ebx, {exit_code}
            mov eax, 0x2000001
            syscall
        ]
    )
}
