; PROGRAM STRUCTURE
; ------------------------------
;
;       DOS PSP 
;       + .COM padding
;       16 paragraphs (256 bytes)
;
; -------------------------------
;
;   Program's code (up to 64KB - 256B, whole segment)
;
; -------------------------------
; 
;
;   allocated memory as stack
;
;
;           ...

; ah        -> 0x4b
; al        -> 00h
; ds:dx     -> path
; es:bx     -> execution parameter block
; al <-     return code
execute:
    mov [.temp], bx

    ; open file
    mov ah, 0x3d
    mov al, 0x0
    int 0x21
    jc .error
    push ax     ; save fd for next operations
    push di

    mov bx, ax  ; store fd in bx

    push cx
    ; get file size
    push fs
    push si
    call calculate_offset_to_fd_entry
    jc .file_size_error
    mov cx, [fs:FDTable.fileSize + si]  ; here we have file size
    pop si
    pop fs

    ; we need to know how many blocks we need to allocate for that program
    ; formula: file size / block size (+ 1 if needed)
    mov ax, cx      ; ax = file size
    push bx         ; save fd
    push dx         
    xor dx, dx      
    mov bx, 0x10    ; dx:bx = 0:bx = block size
    div bx          ; ax = file size / block size
    cmp dx, 0x0     ; check if this is whole executable, if there is a division carry, it means
                    ; that whole executable won't fit into ax blocks. we need one more block
    je .no_inc
    inc ax          ; if we are here, that means we need one more block
    .no_inc:
    pop dx

    ; ax contains amount of blocks required to load that executable
    push cx
    mov bx, ax                      ; now bx contains it
    add bx, 0x10                    ; COMs are org'd to be executed at 0x100 and we need space for PSP
                                    ; so we need 16 more blocks for that
    mov cx, 0xfffe  ; allocation type = executable
    call internal_allocate
    pop cx
    pop bx ; restore fd to bx
    jc .allocate_error

    ; copy command line args + TODO env
    push bx
    push cx
    push ds
    push es
    push si
    push di

    ; [es:di + 2] = far pointer to cmdline
    ; ax:0x80 = destination cmdline

    mov di, [.temp]
    lds si, [es:di + 2]
    ; ds:si = far pointer to cmdline
    mov bx, ds
    cmp bx, 0x0
    je .no_cmdline      ; each program is loaded at specific segment, so segment 0x0 means there is no cmdline
    xor ch, ch
    mov cl, [ds:si + 1] ; [ds:si + 1] = length of cmdline
    cmp cl, 0x0
    je .no_cmdline      ; length 0 means there is no cmdline
    add cx, 0x2         ; copy with length and max length too

    ; set up es:di as programSegment:0x80
    mov es, ax
    mov di, 0x80

    ; cx contains length, so clear direction flag and copy bytes
    cld
    rep movsb

    .no_cmdline:
    pop di
    pop si
    pop es
    pop ds
    pop cx
    pop bx

    ; ax = block segment
    ; bx = fd
    ; cx = file size
    ; time to read
    push ax
    push ds
    push dx

    mov ds, ax      ;
    mov dx, 0x100   ; load to blockSegment:0x100
    mov ah, 0x3f
    int 0x21

    pop dx
    pop ds
    pop ax
    pop cx
    pop di
    pop bx
    jc .error

    ; close file
    push ax
    mov ah, 0x3e
    int 0x21

    pop ax ; ax = segment offset
    pusha
    pushf
    push es
    push ds
    push gs
    push fs

    ; set up new stack
    mov [gs:.temp], ax
    call load_new_stack
    jc .stk_alloc_error
    mov ax, [gs:.temp]

    ; set up segments
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax

    ; save current SS:SP in PSP at offset 0x2e
    mov [es:0x2e], sp
    mov [es:0x30], ss

    ; set up call address
    ; we call program by farreturning....
    ; retf = pop ip -> pop cs -> continue at cs:ip
    ; so first we push segment and offset of this code
    ; and then we push segment and offset of program's code
    ; so once we do retf, cpu will execute program's code
    ; and when program does retf it will go back here
    push es                 ; stack = prog seg
    mov bx, 0x100
    push bx                 ; stack = prog seg, prog off (0x100)

    ; clear registers before execution
    xor ax, ax
    xor bx, bx
    xor dx, dx
    xor cx, cx
    xor di, di
    xor si, si
    retf                    ; ip = pop = prog off -> cs = pop = prog seg => jumps to programs code
                            ; now: once program exits or retf will execute again
                            ; ip = pop = this off -> cs = pop = this seg => goes back to this code

    .here_back:
    call restore_previous_stack

    pop fs
    pop gs
    mov [gs:.temp], al
    pop ds
    pop es
    popf
    popa

    ; now deallocate block, ax = block (gonna be added later)

    xor bx, bx
    xor ah, ah
    mov al, [gs:.temp]
    jmp .return

    .stk_alloc_error:
        pop fs
        pop gs
        pop ds
        pop es
        popf
        popa
        jmp .error

    .file_size_error:
        pop si
        pop fs
        pop cx
        pop di
        pop ax
        jmp .error
    
    .allocate_error:
        pop ax
        pop bx
        jmp .error

    .error:
        mov bx, 0x1
    .return:
        retf
    
    .temp: dw 0x0

; ah        -> 0x4c
; al        -> exit code
exit:
    pop bx  ; ip
    pop es  ; cs
    popf    ; eflags

    ; es = program's cs
    ; restore stack to its initial position
    lss sp, [es:0x2e] ; ss = es:0x2e + 2, sp = es:0x2e

            ; assuming stack is the same (we don't know but we just want it to exit, all checks and stuff like that will be added later)
    push cs                 ; set up return address
    mov bx, execute.here_back
    push bx
    retf                    ; return to program executor