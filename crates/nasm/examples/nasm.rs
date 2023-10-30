fn main() {
    let result = nasm_to_string::nasm!{
                                                ; Link: kernel32.dll, msvcrt.dll ;
                                                ; Flag: /console ;
                 global  start
                 extern  ExitProcess            ; windows::kernel32.dll ;
                 extern  printf                 ; windows::msvcrt.dll ;
                 section .data
        message: db      "Hello, world!",0xa,0
                 section .text
        start:
                 mov     rcx, message           ; Set fmt ;
                 call    printf                 ; printf() ;
                 xor     ecx, ecx               ; Set exit code ;
                 call    ExitProcess            ; exit() ;
    };

    println!("{}", result);}