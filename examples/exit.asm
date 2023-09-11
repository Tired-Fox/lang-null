; Stack offset of 16 byte with 32 byte shadow space

; x86 Call Conventions:
; https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_Calling_Conventions
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

extern ExitProcess              ; (win32) kernel32.dll

section .data                   ; Initialized variables and consts
section .bss                    ; Unintialized variables

section .text                   ; Actual code of the program/file
start:
        mov rcx, 52             ; Set exit code parameter to 52
        call ExitProcess
