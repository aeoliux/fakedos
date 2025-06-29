; currently it's just hello world
; for now the goal is to just execute it
org 0x100

_start:
    mov dx, hello
    mov ah, 0x9
    int 0x21

    hlt
    jmp $

hello: db "Hello world!", 0xD, 0xA, '$'