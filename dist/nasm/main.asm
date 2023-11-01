global __null
extern ExitProcess
section .text
__main:

RET 
__null:
CALL __main
MOV rcx, 0
CALL ExitProcess
section .data
