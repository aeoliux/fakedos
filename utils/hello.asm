; simple test "Hello world!" binary
; but also it will display command line arguments if you provided them

org 0x100

main:
    mov ah, 0x9
    lea dx, [hello]
    int 0x21

    mov al, [0x81]  ; 0x80 is offset to cmdline arguments set in PSP, 0x80 - limit, 0x81 - actual bytes count, 0x82... - cmdline
    cmp al, 0x0
    je .no_args

    mov ah, 0x9
    lea dx, [args]
    int 0x21

    xor ch, ch
    mov cl, [0x81] ; cmdline length
    xor bx, bx
    .print_loop:
        mov ah, 0x2
        mov dl, [0x82 + bx]
        int 0x21

        inc bx
        cmp bx, cx
        jb .print_loop
    
    mov ah, 0x9
    lea dx, [newline]
    int 0x21

    .no_args:
    ; this will exit
    ; proof that stack is secure and you can push everything without popping
    push ax
    push ax
    push ax

    mov al, 0x0
    int 0x20

hello: db "Hello world!", 0xA, 0xD, '$'
args: db "You passed some command line arguments: $"
newline: db 0xA, 0xD, '$'