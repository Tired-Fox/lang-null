; Stack offset of 16 byte with 32 byte shadow space

; x86 Call Conventions:
; https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_Calling_Conventions
; Linux Syscall table:
; https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#calling-conventions
;
; Stack offset of 16 bytes
; 32 byte shadow space on stack
; == sub rsp, 40
;
; Register:
;       RCX, RDX, R8, R9 (ints)
;       XMM0, XMM1, XMM2, XMM3 (floats)
;       Rest of params are on stack
global start

extern ExitProcess              ; kernel32.dll
extern printf                   ; msvcrt.dll

section .data                   ; Initialized variables and consts
fmt db "%s",0
message db "Hello, world!",0xa, 0x00

section .bss                    ; Unintialized variables
numwritten resd 1

section .text                   ; Actual code of the program/file
start:
        sub rsp, 40             ; Offset and Alignment for windows

        ; Parameter setup
        ; Call printf(fmt, [...args], numargs)
        mov rcx, message
        call printf

        xor ecx, ecx            ; Zero out the ecx register
        call ExitProcess
