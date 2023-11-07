; Sample linux x86_64 _assembly for printing `Hello, world!`
global _start

section .data:
    message db "Hello, world!", 0xa, 0

section .text
_start:
   mov  ebx, 69       ; exit code (0)
   mov  eax, 60        ; exit()
   syscall
