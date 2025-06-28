; set up 0x21 DOS interrupt
ivt_setup:
    ; save some registers
    push ax
    push bx
    push es

    ;
    xor ax, ax              ;
    mov es, ax              ; es will be our register for accessing first memory segment, so it must be 0x0
    mov ax, cs              ; ax contains current code segment (that's were ivt_handler should be stored)
    mov bx, ivt_handler     ; bx contains offset to ivt_handler

    ; 0x0:0x21*4 is where we need to put address to ivt_handler
    ; IVT is at 0x0:0x0 and each entry is 4 bytes long
    ; addressing interrupts is pretty easy, entry is after entry
    ; just multiply interrupt number by 4 and you have your offset where you should put address to your handler
    ; entry looks like: <2 bytes offset><2 bytes segment>
    mov word [es:0x0 + (0x21 * 4) + 0x0],   bx ; 0x0 + (0x21 * 4) + 0x0 = offset to ivt_handler
    mov word [es:0x0 + (0x21 * 4) + 0x2],   ax ; 0x0 + (0x21 * 4) + 0x1 = segment of ivt_handler

    ; restore them and return
    pop es
    pop bx
    pop ax
    ret

; interrupt handler, ah -> interrupt
ivt_handler:
    ; save registers
    pusha
    push es
    push ds
    push gs
    push fs
    
    ; calculate offset to function address
    ; interrupt * 4 = offset, interrupt * 4 + 2 = segment
    mov al, ah  ; al = ah (interrupt code)
    xor ah, ah  ; clear ah -> ax = ah:al = 0:al, this makes an interrupt code a 16-bit value instead of 8-bit
                ; handlers table is built just like IVT: <2 bytes offset><2 bytes segment>, so total length of an entry is 4 bytes
    mov cx, 0x4 ; so to get offset to offset :)))), we need to multiply interrupt code by 4
    mul cx      ; now ax contains offset to an offset of interrupt handler

                            ; now let's get the memory address to an offset of handler
    mov cx, ivt_handlers    ; everything is stored at ivt_handlers, so let's use this address
    add ax, cx              ; now let's sum pointer to ivt_handlers and offset to an offset of a handler
    mov si, ax              ; and store result in si
                            ; now si contains an memory address which offset and segment of a handler is stored at

    ; let's uncover address of the handler
    mov ax, cs      ; ivt handlers table should be stored at main interrupt handler
    mov ds, ax      ; so now ds contains our current code segment
    mov dx, [ds:si] ; ds:si = currentSegment:handlerInfoOffset = dx, now dx contains offset to our desired handler
    add si, 0x2     ; by adding 0x2 to handler info offset we get info about handler's segment
    mov cx, [ds:si] ; ds:si = currentSegment:(handlerInfoOffset + 2) = cx, now cx contains segment of out desired handler
                    ; the final address to our interrupt handler is: cx:dx, however cx:dx is not callable, so we need to transform it a bit into other registers

    ; some handlers might be not implemented and there is only a placeholder for them as a nullword
    ; let's filter them and if the interrupt being called is not implemented, just ignore it
    cmp dx, 0x0
    je .deinit
    cmp cx, 0x0
    je .deinit

    ; now let's prepare to jump to the handler. It might be in another segment, so it won't be that easy
    ; we need to do a far call, so we need to store address of an handler in a specific way (just as handler's addresses are stored: <2 bytes offset>:<2 bytes segment>
    mov word [ds:temp_far_ptr], dx      ; first 2 bytes will be offset
    mov word [ds:temp_far_ptr + 2], cx  ; second 2 bytes will be segment
    ; now let's restore all registers, interrupt handler should know their original values
    pop fs
    pop gs
    pop ds
    pop es
    popa
    ; and save them again because we need to restore before returning from interrupt
    pusha
    push es
    push ds
    push gs
    push fs
    ; now let's farcall this handler
    call far [temp_far_ptr]

    .deinit:
    ; now let's do cleanup: restore previous state of registers before returning to the program or another kernel procedure
    pop fs
    pop gs
    pop ds
    pop es
    popa
    iret    ; go back

temp_far_ptr dd 0

ivt_handlers:
a00: dw 0, 0
a01: dw 0, 0
a02: dw 0, 0
a03: dw 0, 0
a04: dw 0, 0
a05: dw 0, 0
a06: dw 0, 0
a07: dw 0, 0
a08: dw 0, 0
a09: dw display_string, 0x1000
; to be filled later