; ah        -> 0x3D
; al        -> 0x0 read, 0x1 write, 0x2 read-write
; ds:dx     -> filename pointer
; ax <-     fd
open_file:
    push fs
    push si
    push ax
    push dx

    ; fs is segment to fds table
    xor bx, bx
    mov fs, bx

    ; find free slot
    xor si, si  ; si as iterator
    .loop:
        ; access information if entry is free
        mov ax, [fs:FDTable.allocated + si]
        ; first byte if zero => free
        cmp ax, 0x0
        je .found_empty

        ; add to our iterator
        add si, FDTableEntrySize

        ; if exceeded max amount of files => fail
        cmp si, FDTableEntrySize * FDMaxFiles
        jge .no

        ; loop it
        jmp .loop
    
    .no:
        ; if we are here, it failed
        pop dx
        pop ax
        pop si
        pop fs
        
        mov bx, 0x1 ; carry flag
        retf

    .found_empty:
        ; we got an empty slot, let's get their number by dividing iterator by table entry size
        xor dx, dx                  ;
        mov ax, si                  ; dx:ax = entry * size of entry
        mov bx, FDTableEntrySize    ; bx = size of entry
        div bx                      ; ax = dx:ax / bx = (entry * sizeof entry) / size of entry = entry
        ; entry number is file descriptor

        ; ax contains parameters to file opener so we need to save fd in other register
        mov bx, ax                  ; bx = fd

        ; restore registers containing important infos about the file to be read
        pop dx
        pop ax
        push bx

        ; call filesystem function
        ; !!! in the future there will be dynamic caller to handle multiple filesystems which support is loaded as a driver !!!
        call _fat12_open

        ; restore registers and farreturn to interrupt handler
        pop ax
        pop si
        pop fs

        retf

; bx -> fd
; si <- fd entry offset
; fs <- segment to the table
calculate_offset_to_fd_entry:
    cmp bx, FDMaxFiles
    jl .ok
    stc
    ret

    .ok:
    push ax
    xor ax, ax
    mov fs, ax

    mov ax, bx
    push cx
    push dx

    mov cx, FDTableEntrySize
    mul cx
    mov si, ax

    pop dx
    pop cx
    pop ax
    ret

; ah -> 0x3e
; bx -> fd
close_file:
    push fs
    push si
    ; get offset to file table entry
    call calculate_offset_to_fd_entry
    jc .return

    ; set it as unallocated (the most dumb way, but do you need more?)
    mov [fs:FDTable.allocated + si], al

    ; no error here
    xor bx, bx
    jmp .return

    .error: mov bx, 0x1
    .return:
        pop si
        pop fs
        retf

; ah        -> 0x3f
; bx        -> fd
; cx        -> n of bytes to be read
; ds:dx     -> destination
; cx <-     n of bytes read
read_file: ; currently placeholder
    push fs
    push si

    call calculate_offset_to_fd_entry
    jc .return
    ; now fs:FDTable.allocated + si points to file table entry

    call _fat12_read
    jc .return

    xor bx, bx
    jmp .return

    .error: mov bx, 0x1
    .return:
        pop si
        pop fs
        retf

; FDTable address:  0x0:0x0500
;                   fs: 0x0500
FDTable                     equ 0x0500
    FDTable.allocated       equ FDTable +               0x0 ; 1 byte
    FDTable.pointer         equ FDTable.allocated +     0x1 ; 2 bytes
    FDTable.mode            equ FDTable.pointer +       0x2 ; 1 byte
    FDTable.filesystem      equ FDTable.mode +          0x1 ; 1 byte
    FDTable.fsReserved      equ FDTable.filesystem +    0x1 ; 8 bytes
    FDTable.end             equ FDTable.fsReserved +    0x8 ; end

FDTableEntrySize    equ FDTable.end - FDTable
FDMaxFiles          equ 255

FilesystemData      equ (FDTableEntrySize * FDMaxFiles) + FDTable
FilesystemDataSize  equ 0x800
FilesystemDataEnd   equ FilesystemData + FilesystemDataSize

%include 'boot/fat12.asm'