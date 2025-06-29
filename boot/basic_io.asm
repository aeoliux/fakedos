; ah = 0x9, (ds):dx = string to print
display_string:
    push si
    push ax

    mov si, dx
    mov ah, 0xe
    .loop:
        lodsb
        cmp al, byte '$'
        je .end
        int 0x10
        jmp .loop
    .end:
        pop ax
        pop si

        retf

display_string_nulled:
    push si
    push ax

    mov si, dx
    mov ah, 0xe
    .loop:
        lodsb
        or al, al
        jz .end
        int 0x10
        jmp .loop
    .end:
        pop ax
        pop si

        ret