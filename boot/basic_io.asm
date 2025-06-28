; ah = 0x9, dx = string to print
display_string:
    mov si, dx
    mov ah, 0xe
    .loop:
        lodsb
        cmp al, byte '$'
        je .end
        int 0x10
        jmp .loop
    .end:
        retf