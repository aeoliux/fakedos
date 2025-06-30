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

; ds:dx -> string
; cx    -> counter
; es:di <- uppercased string
string_to_uppercase:
    push si
    push ax
    push bx

    ; si = dx
    mov si, dx
    xor bx, bx
    .loop:
        cmp bx, cx
        jae .all

        lodsb ; al = ds:si, si++

        ; al < 0x61 = already uppercased or not a char
        cmp al, 0x61
        jb .already_upper

        ; al > 0x7a = not a char
        cmp al, 0x7a
        ja .already_upper

        ; al contains lowercased char
        ; al - 0x20 = al (uppercased)
        sub al, 0x20

        .already_upper:
            mov [es:di + bx], al
            inc bx
            jmp .loop

    .all:
        pop bx
        pop ax
        pop si
        ret