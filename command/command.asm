; currently it's just hello world
; for now the goal is to just execute it
org 0x100

_start:
    lea dx, [msg]
    mov ah, 0x9
    int 0x21

    retf

msg: 
db "Now COMMAND.COM is only simple binary for testing", 0xD, 0xA
db "If you see this message, that means kernel (BOOT.SYS) has executed COMMAND.COM properly", 0xD, 0xA
db "Be happy!", 0xD, 0xA
db "COMMAND.COM is gonna quit, so you'll see a error message coming from kernel", 0xA, 0xA, 0xD, '$'

times 1200 db 69