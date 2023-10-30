       global  start

extern ExitProcess
       section .text
start: 
MOV     rcx, 50
CALL    ExitProcess

