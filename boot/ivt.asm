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
    push gs
    pusha
    push es
    push ds
    push fs

    mov bx, cs
    mov fs, bx
    mov es, bx
    mov gs, bx ; gs is segment register which refers to segment where ivt_handler is stored
    
    ; ; calculate offset to function address
    ; ; interrupt * 4 = offset, interrupt * 4 + 2 = segment
    ; mov al, ah  ; al = ah (interrupt code)
    ; xor ah, ah  ; clear ah -> ax = ah:al = 0:al, this makes an interrupt code a 16-bit value instead of 8-bit
    ;             ; handlers table is built just like IVT: <2 bytes offset><2 bytes segment>, so total length of an entry is 4 bytes
    ; mov cx, 0x4 ; so to get offset to offset :)))), we need to multiply interrupt code by 4
    ; mul cx      ; now ax contains offset to an offset of interrupt handler

    ;                         ; now let's get the memory address to an offset of handler
    ; mov cx, ivt_handlers    ; everything is stored at ivt_handlers, so let's use this address
    ; add ax, cx              ; now let's sum pointer to ivt_handlers and offset to an offset of a handler
    ; mov si, ax              ; and store result in si
    ;                         ; now si contains an memory address which offset and segment of a handler is stored at

    ; ; let's uncover address of the handler
    ; mov ax, cs      ; ivt handlers table should be stored at main interrupt handler
    ; mov ds, ax      ; so now ds contains our current code segment
    ; mov dx, [ds:si] ; ds:si = currentSegment:handlerInfoOffset = dx, now dx contains offset to our desired handler
    ; add si, 0x2     ; by adding 0x2 to handler info offset we get info about handler's segment
    ; mov cx, [ds:si] ; ds:si = currentSegment:(handlerInfoOffset + 2) = cx, now cx contains segment of out desired handler
    ;                 ; the final address to our interrupt handler is: cx:dx, however cx:dx is not callable, so we need to transform it a bit into other registers

    ; ; some handlers might be not implemented and there is only a placeholder for them as a nullword
    ; ; let's filter them and if the interrupt being called is not implemented, just ignore it
    ; cmp dx, 0x0
    ; je .deinit

    ; ; now let's prepare to jump to the handler. It might be in another segment, so it won't be that easy
    ; ; we need to do a far call, so we need to store address of an handler in a specific way (just as handler's addresses are stored: <2 bytes offset>:<2 bytes segment>
    ; mov word [ds:temp_far_ptr], dx      ; first 2 bytes will be offset
    ; mov word [ds:temp_far_ptr + 2], cx  ; second 2 bytes will be segment

    mov al, ah
    xor ah, ah
    mov bx, ivt_handlers
    call dynamic_call
    jc .deinit

    ; now let's restore all registers, interrupt handler should know their original values
    pop fs
    pop ds
    pop es
    popa

    ; now let's farcall this handler
    call far [gs:dynamic_call.caller]

    pop gs

    ; check for carry flag, flags are not stored across far jump, so we use bx register for that
    push bx
    .carry_check:
    cmp bx, 0x0
    je .no_carry
    stc
    jmp .done

    .no_carry:
    clc

    .done:
    push bp
    pushf
    pop bx

    mov bp, sp
    mov [bp + 4 + 2 + 2], bx

    pop bp
    pop bx
    iret

    .deinit:
    ; now let's do cleanup: restore previous state of registers before returning to the program or another kernel procedure
    pop fs
    pop ds
    pop es
    popa
    pop gs
    push bx
    mov bx, 0x1
    jmp .carry_check

ivt_handlers:
a000:	dw		0x0,				0x0
a0x1:	dw		0x0,				0x0
a0x2:	dw		0x0,				0x0
a0x3:	dw		0x0,				0x0
a0x4:	dw		0x0,				0x0
a0x5:	dw		0x0,				0x0
a0x6:	dw		0x0,				0x0
a0x7:	dw		0x0,				0x0
a0x8:	dw		0x0,				0x0
a0x9:	dw		display_string,		0x1000
a0xa:	dw		0x0,				0x0
a0xb:	dw		0x0,				0x0
a0xc:	dw		0x0,				0x0
a0xd:	dw		0x0,				0x0
a0xe:	dw		0x0,				0x0
a0xf:	dw		0x0,			    0x0
a0x10:	dw		0x0,    			0x0
a0x11:	dw		0x0,				0x0
a0x12:	dw		0x0,				0x0
a0x13:	dw		0x0,				0x0
a0x14:	dw		0x0,				0x0
a0x15:	dw		0x0,				0x0
a0x16:	dw		0x0,				0x0
a0x17:	dw		0x0,				0x0
a0x18:	dw		0x0,				0x0
a0x19:	dw		0x0,				0x0
a0x1a:	dw		0x0,				0x0
a0x1b:	dw		0x0,				0x0
a0x1c:	dw		0x0,				0x0
a0x1d:	dw		0x0,				0x0
a0x1e:	dw		0x0,				0x0
a0x1f:	dw		0x0,				0x0
a0x20:	dw		0x0,				0x0
a0x21:	dw		0x0,				0x0
a0x22:	dw		0x0,				0x0
a0x23:	dw		0x0,				0x0
a0x24:	dw		0x0,				0x0
a0x25:	dw		0x0,				0x0
a0x26:	dw		0x0,				0x0
a0x27:	dw		0x0,				0x0
a0x28:	dw		0x0,				0x0
a0x29:	dw		0x0,				0x0
a0x2a:	dw		0x0,				0x0
a0x2b:	dw		0x0,				0x0
a0x2c:	dw		0x0,				0x0
a0x2d:	dw		0x0,				0x0
a0x2e:	dw		0x0,				0x0
a0x2f:	dw		0x0,				0x0
a0x30:	dw		0x0,				0x0
a0x31:	dw		0x0,				0x0
a0x32:	dw		0x0,				0x0
a0x33:	dw		0x0,				0x0
a0x34:	dw		0x0,				0x0
a0x35:	dw		0x0,				0x0
a0x36:	dw		0x0,				0x0
a0x37:	dw		0x0,				0x0
a0x38:	dw		0x0,				0x0
a0x39:	dw		0x0,				0x0
a0x3a:	dw		0x0,				0x0
a0x3b:	dw		0x0,				0x0
a0x3c:	dw		0x0,				0x0
a0x3d:	dw		open_file,			0x1000
a0x3e:	dw		close_file,			0x1000
a0x3f:	dw		read_file,			0x1000
a0x40:	dw		0x0,				0x0
a0x41:	dw		0x0,				0x0
a0x42:	dw		0x0,				0x0
a0x43:	dw		0x0,				0x0
a0x44:	dw		0x0,				0x0
a0x45:	dw		0x0,				0x0
a0x46:	dw		0x0,				0x0
a0x47:	dw		0x0,				0x0
a0x48:	dw		0x0,				0x0
a0x49:	dw		0x0,				0x0
a0x4a:	dw		0x0,				0x0
a0x4b:	dw		0x0,				0x0
a0x4c:	dw		0x0,				0x0
a0x4d:	dw		0x0,				0x0
a0x4e:	dw		0x0,				0x0
a0x4f:	dw		0x0,				0x0
a0x50:	dw		0x0,				0x0
a0x51:	dw		0x0,				0x0
a0x52:	dw		0x0,				0x0
a0x53:	dw		0x0,				0x0
a0x54:	dw		0x0,				0x0
a0x55:	dw		0x0,				0x0
a0x56:	dw		0x0,				0x0
a0x57:	dw		0x0,				0x0
a0x58:	dw		0x0,				0x0
a0x59:	dw		0x0,				0x0
a0x5a:	dw		0x0,				0x0
a0x5b:	dw		0x0,				0x0
a0x5c:	dw		0x0,				0x0
a0x5d:	dw		0x0,				0x0
a0x5e:	dw		0x0,				0x0
a0x5f:	dw		0x0,				0x0
a0x60:	dw		0x0,				0x0
a0x61:	dw		0x0,				0x0
a0x62:	dw		0x0,				0x0
a0x63:	dw		0x0,				0x0
a0x64:	dw		0x0,				0x0
a0x65:	dw		0x0,				0x0
a0x66:	dw		0x0,				0x0
a0x67:	dw		0x0,				0x0
a0x68:	dw		0x0,				0x0
a0x69:	dw		0x0,				0x0
a0x6a:	dw		0x0,				0x0
a0x6b:	dw		0x0,				0x0
a0x6c:	dw		0x0,				0x0
; to be filled later
