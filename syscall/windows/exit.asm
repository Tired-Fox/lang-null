; Link kernel32.dll
; Flag: /console
global start

extern ExitProcess              ; windows::kernel32.dll

section .text                   
start:
        mov ecx, 69             ; Set exit code
        call ExitProcess        ; exit()
