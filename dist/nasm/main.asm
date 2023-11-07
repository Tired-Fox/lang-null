global __null
extern ExitProcess
section .text
__main:
MOV rcx, 69
CALL ExitProcess
RET 
__null:
CALL __main
MOV rcx, 0
CALL ExitProcess
section .data
