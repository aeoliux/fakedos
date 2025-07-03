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

; ah        -> 0x2
; dl <-     character
print_char:
    push ax

    mov ah, 0xe
    mov al, dl
    int 0x10

    pop ax
    retf

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

; ah        -> 0x7 | 0x8
; al <-     character
read_char_no_echo:
    push cx
    mov cx, 0x1
    jmp read_char.continue

; ah        -> 0x1
; al <-     character
read_char:
    push cx
    mov cx, 0x0
    .continue:

    ; by BIOS
    mov ah, 0x0
    int 0x16

    cmp cx, 0x0
    jne .return

    mov ah, 0xe
    int 0x10

    .return:
    xor bx, bx
    pop cx
    retf

; ah            -> 0xa
; ds:dx         -> keyboard buffer, ds:dx = max chars, ds:dx + 2,3,4...,[ds:dx] = input
; ds:dx + 1 <-  actual bytes read
; ds:dx + 2.<-  input
keyboard_input:
    push si
    push cx

    ; ds:si = keyboard buffer
    mov si, dx

    ; bx = iterator, skip first two positions, cl = max chars
    mov bx, 0x2
    mov cl, [ds:si]

    .read:
        push bx
        call far [gs:a0x1] ; interrupt 0x1
        pop bx

        cmp al, 0x8 ; backspace
        jne .next
        dec bx
        jmp .read

        .next:
        ; al = contains read character
        mov [ds:si + bx], al ; store it
        
        inc bl ; increment bl
        cmp al, 0xD ; termination character
        je .all
        
        cmp bl, cl ; we got limit
        je .all

        jmp .read

    .all:
    sub bl, 0x2
    mov [ds:si + 1], bl ; save amount of characters read
    xor bx, bx

    pop cx
    pop si
    retf