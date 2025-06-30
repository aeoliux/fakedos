DrivesIds:          db 'A',     'B'
DrivesDls:          db 0x0,     0x1

Drive:
    .dl:            db 0x0
    .cylinders:     dw 0x0
    .heads:         dw 0x0
    .spt:           dw 0x0
    .fsId:          db 0x0 ; 0 = FAT12 and 1, 2, 3 by external drivers
EndDrive:

DriveInfoSize   equ (EndDrive - Drive)

; set current drive
; dl = drive number
set_drive:
    ; ask BIOS for information about the drive specified in dl
    mov [bp + 2], dl
    mov ah, 0x8
    int 0x13
    jc .ret

    ; ch = lower 8 bits of max cylinder
    ; cl = 2 high bits of max cylinder + 6 bits of sectors per track
    ; dh = 8 bit max head
    call split_ch_cl
    ; cx = max cylinder
    ; ax(al) = sectors per track (max sector)
    ; dh = max head

    ; store information about the current drive
    mov dl, [bp + 2]                    
    mov [Drive.dl], dl          ; save drive dl
    inc cx                      ; increment number of cylinders (BIOS returns the MAX cylinder value)
    mov [Drive.cylinders], cx   ; store it
    mov dl, dh                  ; dx = dh:dh = dh:dl
    xor dh, dh                  ; dx = 0:dh = 0:dl
    inc dx                      ; now this gives us total amount of heads
    mov [Drive.heads], dx       ; store it
    mov [Drive.spt], ax         ; and store amount of sectors per track

    .ret:
    ret

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

; dl -> drive ID
; dl <- drive dl (BIOS ID)
driveId_to_dl:
    cmp dl, 0x61 ; >= 0x61 -> lowercase
    jae .lowercase

    cmp dl, 0x41 ; >= 0x41
    jae .uppercase

    jmp .unknown

    .uppercase:
        cmp dl, 0x5b ; >= 0x5b -> unknown
        jae .unknown

        cmp dl, 0x42 ; A or B
        jbe .uppercase_floppy
        add dl, 0x19
        ret
        .uppercase_floppy:
            sub dl, 0x41
            ret

    .lowercase:
        cmp dl, 0x7b ; >= 0x7a -> unknown
        jae .unknown

        cmp dl, 0x62
        jbe .lowercase_floppy
        add dl, 0x1d
        ret

        .lowercase_floppy:
            sub dl, 0x61
            ret

    .unknown:
        stc
        ret

; ax        -> sector
; bl        -> amount of sectors to be read
; cx        -> cylinder
; dh        -> head
; es:si     -> destination
read_disk:
    mov dl, [gs:Drive.dl]
    call merge_ch_cl

    push es
    push bx
    mov al, bl
    mov bx, si

    mov ah, 0x2
    int 0x13

    pop es
    pop bx
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
    push bx
    push dx

    ; C, temp = lba / (n of heads * n of sectors per track)
	xor dx, dx			;
	mov ax, [gs:Drive.heads]	; dx:ax = 0:n of heads
	mov bx, [gs:Drive.spt]	; bx = n of sectors per track
	mul bx	; dx:ax = n of heads * n of sectors per track
	mov bx, ax	;
	xor dx, dx	; 0:bx = 0:ax
	pop ax ; ax = lba
	div bx ; ax (C), dx (temp) = lba / (n of heads * n of sectors per track)
	push ax ; save ax (C)

	; H, S - 1 = temp / n of sectors per track
	mov ax, dx	;
	xor dx, dx	; dx:ax = temp
	mov bx, [gs:Drive.spt] ; bx = n of sectors per track
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
    ret

%include 'boot/filesystem.asm'