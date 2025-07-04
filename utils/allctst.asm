org 0x100

main:
    mov ah, 0x9
    lea dx, [intro]
    int 0x21

    mov ah, 0x6d
    int 0x21
    push ax

    mov ah, 0x48
    mov bx, 0x1
    int 0x21
    push ax

    mov ah, 0x48
    mov bx, 0x1
    int 0x21
    mov [.temp], ax

    pop ax
    push es
    mov es, ax
    mov ah, 0x49
    int 0x21
    pop es

    push es
    mov ax, [.temp]
    mov es, ax
    mov ah, 0x49
    int 0x21
    pop es

    mov ah, 0x6d
    int 0x21
    pop bx

    cmp ax, bx
    je .dealloc_ok
    mov al, 0x1                     ; exit with error code
    push ax
    lea dx, [deallocator_error]
    jmp .print_result

    .dealloc_ok:
    mov al, 0x0                     ; exit with no error code
    push ax
    lea dx, [deallocator_succe]

    .print_result:
    mov ah, 0x9
    int 0x21

    mov al, [0x81]                  ; check if command line arguments were passed
    cmp al, 0x0
    ja .leak

    mov ah, 0x9
    lea dx, [advice]
    int 0x21

    jmp .back_to_kernel

    .leak:
    mov ah, 0x9
    lea dx, [last_message]
    int 0x21

    mov ah, 0x48
    mov bx, 0x1
    int 0x21

    .back_to_kernel:
    pop ax
    int 0x20

    .temp: dw 0x0

intro:              db "ALLCTST.COM: FakeDOS (de)allocator tester", 0xD, 0xA
                    db "             Checking memory usage and allocating 2 blocks of memory", 0xD, 0xA, 0xA, '$'
deallocator_error:  db "ALLCTST.COM: Deallocation failed! (memory usage grew up)", 0xD, 0xA, '$'
deallocator_succe:  db "ALLCTST.COM: Deallocation has succeded! (memory usage did not grow up)", 0xD, 0xA, '$'
advice:             db "ALLCTST.COM: Pass a random command line argument to leak memory!", 0xD, 0xA, '$'
last_message:       db "ALLCTST.COM: Now let's create memory leak and let's see how DOS can handle it!", 0xD, 0xA, '$'