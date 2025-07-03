; ds:dx     -> buffer
; cx        -> length
; al <-     return code
execute_cmd:
    mov si, dx                  ; ds:si = buffer
    xor bx, bx                  ; bx = iterator
    .split:
        mov al, [si + bx]       ; check for terminations
        cmp al, ' '             ; spacebar means there are some cmdline args
        je .parse_args

        cmp al, 0xD
        je .found_executable

        inc bx
        cmp bx, cx
        jnae .split

    .parse_args:
    push bx
    push si

    inc bx
    push bx     ; save start of cmdline
    .find_end:  ; find termination character (0xD)
        mov al, [si + bx]
        cmp al, 0xD
        je .found_end
        inc bx
        jmp .find_end
    .found_end:
    mov cx, bx  ; save it in cx
    pop bx      ; bx = start of cmdline
    sub cx, bx  ; cx = length of cmdline - 1
    inc cx

    mov ax, cs
    mov [cmdline + 0x1], cl         ; save length
    mov [binary_ebp + 0x4], ax      ; save current segment

    lea di, [cmdline + 0x2]         ; es:di = destination of cmdline
    add si, bx                      ; ds:si = cmdline from keyboard input
    cld
    rep movsb                       ; copy cmdline

    pop si
    pop bx

    .found_executable:
    mov al, 0x0
    mov [si + bx], al

    mov ah, 0x4b
    mov al, 0x0
    lea bx, [binary_ebp]
    int 0x21

    ret

binary_ebp: dw 0x0      ; env
            dw cmdline  ; cmdline offset
            dw 0x0      ; cmdline segment
            dw 0x0, 0x0 ; unused fcb
cmdline:    db 0x40, 0x0
            times 0x40 db 0x0