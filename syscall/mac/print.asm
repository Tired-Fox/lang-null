global start

section .data
message db "Hello, world!", 0xa     ; message with newline

section .text
start:
    mov  rdi, 1                     ; stdout
    mov  rsi, message               ; address of message
    mov  rdx, 14                    ; number of bytes that is being written
    mov  rax, 0x02000004            ; write()
    syscall

    xor  rdi, rdi                   ; exit code
    move rax, 0x02000001            ; exit()
    syscall
