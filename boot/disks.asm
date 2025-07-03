DriveInfo               equ 0x0
DriveInfo.letter        equ DriveInfo                   ; db 0x0
DriveInfo.dl            equ DriveInfo.letter    + 0x1   ; db 0x0
DriveInfo.cylinders     equ DriveInfo.dl        + 0x1   ; dw 0x0
DriveInfo.heads         equ DriveInfo.cylinders + 0x2   ; dw 0x0
DriveInfo.spt           equ DriveInfo.heads     + 0x2   ; dw 0x0
DriveInfo.plbastart     equ DriveInfo.spt       + 0x2   ; dw 0x0, 0 for floppies
DriveInfo.psize         equ DriveInfo.plbastart + 0x2   ; dw 0x0, 0 for floppies
DriveInfo.end           equ DriveInfo.psize     + 0x2

CurrentDrive:   dw 0x0 ; segment to current drive info
DetectedDrives: db 0x0
DrivesToScan:   db 0x00, 0x01, 0x80, 0x81, 0x82, 0x83, 0xff ; 0xff == end

; scan drives
scan_drives:
    xor dh, dh
    mov [CurrentDrive], dx ; save the drive we booted from

    ; bx as iterator over DrivesToScan
    xor bx, bx
    xor cx, cx
    .scan_drive:
        mov dl, [DrivesToScan + bx] ; get next drive number
        cmp dl, 0xff
        je .all

        ; get drive geometry from BIOS
        push bx
        push es
        mov ah, 0x8
        int 0x13
        pop es
        jc .no_drive            ; if an error is returned, it means that this drive is inaccessible or not present
        call split_ch_cl        ; convert BIOS CHS to 16-bit values
        inc cx
        mov [.temp], cx
        inc dh
        mov [.temp + 2], dh
        mov [.temp + 3], ax ; store it temporarly
        pop bx
        push bx

        ; check if drive is floppy disk
        mov dl, [DrivesToScan + bx]
        cmp dl, 0x01
        jbe .floppy

        ; currently no disk support
        ; push es
        ; push bx
        ; xor si, si
        ; mov es, si
        ; mov fs, si
        ; mov si, FilesystemData

        ; mov ax, 0x1 ; first sector
        ; mov bl, 0x1 ; one sector
        ; mov cx, 0x0
        ; mov dh, 0x0
        ; call read_disk
        ; pop bx
        ; pop es
        ; jc .no_drive

        ; add si, 0x1be ; offset to first MBR entry
        ; .scan_mbr:
        ;     mov al, [fs:si + 0x4]
        ;     xor ah, ah
        ;     cmp al, 0x0
        ;     je .next_entry

        ;     push bx
        ;     mov bx, 0x1
        ;     mov cx, 0xffff
        ;     call internal_allocate
        ;     pop bx
        ;     jc .next_entry

        ;     ; we save drive + partition information at ax
        ;     mov es, ax

        ;     .next_entry:
        ;     add si, 0x40    ; get next MBR entry
        ;     cmp si, 0x1fd   ; last was checked
        ;     jae .no_drive
        ;     jmp .scan_mbr
        jmp .no_drive

        .floppy:
            ; allocate block for drive information
            push cx
            push dx
            mov bx, 0x1
            mov cx, 0xffff
            call internal_allocate
            pop dx
            pop cx
            jc .no_drive
            
            pop bx
            push bx

            ; set up es as segment to drive's data block
            push es
            mov es, ax

            ; save all disk information to the allocated block
            mov cx, bx
            add cl, 0x41    ;   make it drive letter
            mov [.message], cl
            call .drive_detected
            mov [es:DriveInfo.letter], cl
            mov [es:DriveInfo.dl], dl
            mov ax, [.temp]
            mov [es:DriveInfo.cylinders], ax
            xor ah, ah
            mov al, [.temp + 2]
            mov [es:DriveInfo.heads], ax
            mov ax, [.temp + 3]
            mov [es:DriveInfo.spt], ax

            ; floppies dont have partitions
            xor ax, ax
            mov [es:DriveInfo.plbastart], ax
            mov [es:DriveInfo.psize], ax

            ; if this is drive we booted from
            mov ax, [CurrentDrive]
            xor dh, dh
            cmp al, dl
            jne .not_current
            mov cx, es
            mov [CurrentDrive], cx

            .not_current:
            pop es
        .no_drive:
        pop bx
        inc bx
        jmp .scan_drive

    .all:
    ret

    .drive_detected:
    push ax
    push dx

    mov ah, 0x9
    lea dx, [.message]
    int 0x21

    pop dx
    pop ax
    ret
    .message: db "X: drive found!", 0xA, 0xD, '$'
    .temp: db 0x0, 0x0, 0x0, 0x0, 0x0 ; 0 = cylinder, 2 = head, 3 = sector


; ah        -> 0x19
; al <-     drive letter
get_drive_int:
    push es

    mov ax, [gs:CurrentDrive]
    mov es, ax

    mov al, [es:DriveInfo.letter]

    pop es
    xor bx, bx
    retf

; dl <-     current drive
; es <-     drive info segment
get_current_drives_dl:
    push ax

    mov ax, [gs:CurrentDrive]
    mov es, ax
    mov dl, [es:DriveInfo.dl]

    pop ax
    ret

; set current drive
; dl = drive number
; set_drive:
;     ; ask BIOS for information about the drive specified in dl
;     mov [bp + 2], dl
;     mov ah, 0x8
;     int 0x13
;     jc .ret
; 
    ; ; ch = lower 8 bits of max cylinder
    ; ; cl = 2 high bits of max cylinder + 6 bits of sectors per track
    ; ; dh = 8 bit max head
    ; call split_ch_cl
    ; ; cx = max cylinder
    ; ; ax(al) = sectors per track (max sector)
    ; ; dh = max head
;
;     ; store information about the current drive
;     mov dl, [bp + 2]                    
;     mov [Drive.dl], dl          ; save drive dl
;     inc cx                      ; increment number of cylinders (BIOS returns the MAX cylinder value)
;     mov [Drive.cylinders], cx   ; store it
;     mov dl, dh                  ; dx = dh:dh = dh:dl
;     xor dh, dh                  ; dx = 0:dh = 0:dl
;     inc dx                      ; now this gives us total amount of heads
;     mov [Drive.heads], dx       ; store it
;     mov [Drive.spt], ax         ; and store amount of sectors per track

;     .ret:
;     ret

; split cylinder and sector
; ch, cl -> bios shit
; cx <- cylinder
; ax <- sector
split_ch_cl:
    ; 8C 7C 6C 5C 4C 3C 2C 1C   |   10C 9C 6S 5S 4S 3S 2S 1S
    ;           ch              |               cl
    push cx
    and cl, 11000000b   ; cl = 10C 9C 00 00 00 00  00 00
    shr cl, 6           ; cl =  00 00 00 00 00 00 10C 9C
    push cx
    mov cl, ch          ; cl = 8C 7C 6C 5C 4C 3C  2C 1C
    pop ax              ;
    mov ch, al          ; ch = 00 00 00 00 00 00 10C 9C
    ; cx = ch:cl = 00 00 00 00 00 00 10C 9C 8C 7C 6C 5C 4C 3C 2C 1C
    ; cx contains cylinder now

    pop ax
    and al, 00111111b   ; al =  00 00 6S 5S 4S 3S 2S 1S
    xor ah, ah
    ; ax = ah:al = 0:al = 00 00 00 00 00 00 00 00 00 00 6S 5S 4S 3S 2S 1S
    ; ax now contain sector

    ; finish
    ret

; merge cylinder and sector
; cx -> cylinder
; ax -> sector
; ch, cl <- bios shit
merge_ch_cl:
    push dx

    mov dx, cx
    ; ch = 00 00 00 00 00 00 10C 9C
    ; cl = 8C 7C 6C 5C 4C 3C  2C 1C
    shl ch, 6   ; ch = 10C 9C 00 00 00 00 00 00
    or al, ch   ; al = 10C 9C 6S 5S 4S 3S 2S 1S
    mov cl, al
    mov ch, dl

    pop dx
    ret

; ax        -> sector
; bl        -> amount of sectors to be read
; cx        -> cylinder
; dh        -> head
; es:si     -> destination
read_disk:
    push es
    call get_current_drives_dl
    pop es

    call merge_ch_cl

    push es
    push bx
    mov al, bl
    mov bx, si

    mov ah, 0x2
    int 0x13
    pop bx
    pop es
    ret

    ; dx -> lba
    ; bl -> amount of sectors to be read
    ; es:si -> destination
    .lba:
        call lba_to_chs
        jmp read_disk

; dx -> lba
; ax <- sector
; cx <- cylinder
; dh <- head
lba_to_chs:
    push es
    push bx
    push dx
    
    ; get current drives info
    call get_current_drives_dl

    ; C, temp = lba / (n of heads * n of sectors per track)
	; xor dx, dx			;
	mov ax, [es:DriveInfo.heads]	    ; dx:ax = 0:n of heads
	mov bx, [es:DriveInfo.spt]    ; bx = n of sectors per track

	mul bx	; dx:ax = n of heads * n of sectors per track
	mov bx, ax	;
	xor dx, dx	; 0:bx = 0:ax
	pop ax ; ax = lba
	div bx ; ax (C), dx (temp) = lba / (n of heads * n of sectors per track)
	push ax ; save ax (C)

	; H, S - 1 = temp / n of sectors per track
	mov ax, dx	;
	xor dx, dx	; dx:ax = temp
	mov bx, [es:DriveInfo.spt] ; bx = n of sectors per track
	div bx ; ax (H), dx (S - 1) = temp / n of sectors per track

	; S = S - 1 + 1
	inc dx

	; C = bp, H = ax, S = dx

    pop cx      ; C = cx
    push dx     ; S = bp
    mov dh, al  ; H = al = dh
    pop ax      ; S = bp = ax
    ; C = cx, H = dh, S = ax

    pop bx
    pop es
    ret

%include 'boot/filesystem.asm'