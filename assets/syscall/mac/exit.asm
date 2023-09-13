global start

section .text
start:
    mov  rdi, 69           ; exit code
    move rax, 0x02000001    ; exit()
    syscall
