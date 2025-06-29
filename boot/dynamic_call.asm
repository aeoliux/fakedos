; this file contains powerfull stuff which allow us to make injections into the kernel
; functions are mapped in special table which contain offset and segment
; every function callable by dynamic call !!!MUST BE FAR CALLED AND FAR RETURNED!!!

; ax -> entry
; bx -> table offset
; es -> table segment
dynamic_call:
    mov cx, 0x4
    mul cx
    ; ax = table offset to entry
    add ax, bx
    mov bx, ax
    ; ax = segment offset to entry
    mov dx, [es:bx]                 ; this gives us offset to function
    cmp dx, 0x0
    je .stc
    mov word [gs:.caller], dx       ; we store it in .caller
    add bx, 0x2                     ; we add 0x2 so we get pointer to segment of that function
    mov dx, [es:bx]                 ; get that segment
    mov word [gs:.caller + 2], dx   ; store that segment in .caller

    ; now you'll be able to just
    ; jmp far [dynamic_call.caller]

    ret

    .stc:
        stc
        ret

    .caller: dw 0x0, 0x0
