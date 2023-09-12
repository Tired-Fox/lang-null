global start

extern ExitProcess

section .text
start:
    mov ecx, 1
    shl ecx, 1
    call ExitProcess
