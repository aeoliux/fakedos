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
    mov [si + bx], al               ; put nullbyte at the end of the executable's path

    ; save memory usage
    push ax
    mov ah, 0x6d
    int 0x21
    mov [memory_used], ax
    pop ax

    ; execute command
    mov ah, 0x4b
    mov al, 0x0
    lea bx, [binary_ebp]
    int 0x21
    pushf ; save error indicator (CF)

    ; get current memory usage and check if it is different than before execution
    push ax
    push bx
    mov ah, 0x6d
    int 0x21
    mov bx, [memory_used]
    cmp ax, bx              ; if memory usage now is different, it means we got a memleak or program freed too much memory
                            ; but it might also be issue with kernel
    ja .memleak_detected
    .here_back:
    pop bx
    pop ax

    popf ; restore flags
    ret

    .memleak_detected:
    ; print message about memory leak
    mov ah, 0x9
    lea dx, [memleak_msg]
    int 0x21

    clc
    jmp .here_back

    memory_used: dw 0x0

memleak_msg:    db 0xA, 0xD
                db "Memory usage before execution does not match memory usage after execution!", 0xA, 0xD
                db "It can be either a memory leak or program freed too much memory.", 0xA, 0xD
                db "You can continue using your computer, but it is recommended to reboot.", 0xA, 0xD
                db "This can prevent any memory corruption during this session.", 0xA, 0xD, '$'

execution_error: db "Failed to execute program", 0xA, 0xD, '$'

binary_ebp: dw 0x0      ; env
            dw cmdline  ; cmdline offset
            dw 0x0      ; cmdline segment
            dw 0x0, 0x0 ; unused fcb
cmdline:    db 0x40, 0x0
            times 0x40 db 0x0