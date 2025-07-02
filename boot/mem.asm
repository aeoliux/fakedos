SegmentLimit:               dw 0x0
OptimalStackSize:           dw 0x0
OptimalStackSizeInBytes:    dw 0x0

PreviousAllocatedBlock:         dw 0x0                  ; previous allocated block
FirstFreeSegment:               dw FIRST_FREE_BLOCK     ; last used memory segment

MemBlock            equ 0x0
MemBlock.allocated  equ MemBlock                    ; 2 bytes, 0xffff = yes, 0xfffe = new executable, else = no
MemBlock.blockSize  equ MemBlock.allocated + 0x2    ; 2 bytes
MemBlock.previous   equ MemBlock.blockSize + 0x2    ; 2 bytes (previous block segment)

; block scheme
; segment - 1   =       Block header (4 bytes padded to be 16 bytes)
; segment       =       Block available to the program

get_ram_size:
    int 0x12
    ; ax = conventional memory size in KB

    ; formula to memory size in segments
    ; conv mem * 1024 / 16 = conv mem * 64
    mov cx, 64
    mul cx
    ; ax = memory size in segments
    mov [cs:SegmentLimit], ax ; store it

    ; calculate optimal stack size per program
    ; at this points it's constant 4KB
    mov ax, 0x100
    mov [cs:OptimalStackSize], ax
    mov bx, 0x10    ; also save optimal stack size in bytes for offsetting
    mul bx
    mov [cs:OptimalStackSizeInBytes], ax

    ret

; function to set up stack per program, stack is allocated block
; ss:sp         -> current stack
; ss:sp <-      new stack
load_new_stack:
    pop dx  ; save return address
    
    ; save current stack address
    mov bx, sp
    mov [gs:.temp], bx
    mov bx, ss
    mov [gs:.temp + 2], bx

    ; allocate new stack
    mov bx, [gs:OptimalStackSize]
    mov cx, 0xffff
    call internal_allocate
    jc .return  ; allocation error
    
    mov ss, ax  ; SET UP NEW STACK SEGMENT

    mov bx, [gs:OptimalStackSizeInBytes]    ; bx = optimal stack size in bytes
    mov sp, bx  ; SET UP STACK OFFSET
    mov bp, sp

    push ax                 ; save base stack offset onto new stack
    mov bx, [gs:.temp + 2]
    push bx                 ; save previous stack segment
    mov bx, [gs:.temp]
    push bx                 ; save previous stack offset
    
    .return: 
        push dx                 ; restore return address onto new stack
        ret

    ; new stack after return
    ; 
    ; 1.    initial stack segment (used later for deallocation of this stack)
    ; 2.    previous stack segment (used later for restoring)
    ; 3.    previous stack offset (used later for restoring)

    .temp: dw 0x0, 0x0

; function to restore stack of previous program
; ss:sp         -> new stack
; ss:sp <-      previous stack
restore_previous_stack:
    pop dx  ; save return address

    pop cx  ; previous stack offset
    pop bx  ; previous stack segment
    pop ax  ; this stack segment

    ; now new stack is empty, deallocate it (TODO!)

    ; restore previous stack
    mov sp, cx
    mov bp, sp
    mov ss, bx

    push dx ; set return address
    ret

; ah = 0x48
; bx        -> n of segments (segment = 16 bytes)
; ax <-     segment
allocate_blocks:
    push cx

    mov cx, 0xffff ; 0xffff = allocate
    call internal_allocate

    pop cx
    retf

; bx        -> n of segments (segment = 16 bytes)
; cx        -> allocation type
; ax <-     segment
internal_allocate:
    mov ax, [gs:FirstFreeSegment]   ; first free block
    add ax, bx                      ; first free block + n of blocks program wants to allocate
    inc ax                          ; first free block + n of blocks program wants to allocate + 1 block for block header
    ; ax = memory used after allocation

    ; check if we have enough memory for that
    push dx
    mov dx, [gs:SegmentLimit]   ; dx = segment limit
    cmp ax, dx                  ; compare
    pop dx
    jae .no_available_mem       ; if memory used after allocation > segment limit
                                ; we don't have enough, so return an error

    push fs
    push si

    ; fs:si points to new block's header
    mov si, [gs:FirstFreeSegment]
    mov fs, si
    xor si, si ; fs:0x0 = new block header, fs+0x1:0x0 = new block

    mov [fs:si + MemBlock.allocated], cx    ; set allocation type
    mov [fs:si + MemBlock.blockSize], bx    ; set block size
    mov ax, [gs:PreviousAllocatedBlock]     ; every block contains information about previous allocated block
    mov [fs:si + MemBlock.previous], ax     ; it is a chain

    pop si
    pop fs

    ; get block segment
    mov ax, [gs:FirstFreeSegment]           ; ax = block header segment
    inc ax                                  ; ax = block header segment + 1 = block segment
    mov [gs:PreviousAllocatedBlock], ax     ; save this block as last allocated

    ; set new first free block
    push cx
    mov cx, ax  ; cx = block segment
    add cx, bx  ; cx = block segment + block size = first free block segment
    mov [gs:FirstFreeSegment], cx
    pop cx

    jmp .return

    .no_available_mem:
    stc
    mov bx, 0x1     ; i know it's bad. It will be fixed in the future
    mov ax, 0x0

    .return:

    ret