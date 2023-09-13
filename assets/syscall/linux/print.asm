; Sample linux x86_64 assembly for printing `Hello, world!`
global _start

section .data:
    message db "Hello, world!", 0xa, 0

section .text
_start:
   mov  rdi, 1         ; stdout
   move rsi, message   ; Add the message to second param
   move rdx, 14        ; Size of the output
   mov  rax, 1         ; sys_write()
   syscall

   xor  ebx, ebx       ; exit code (0)
   mov  eax, 60        ; exit()
   syscall
