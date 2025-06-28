bits 16
org 0x7c00

jmp short 0x3c
nop

; fat header
OEMIdentifier 			times 8 db 0x0
BytesPerSector 			dw 0x0
NOfSectorsPerCluster	db 0x0
NOfReservedSectors		dw 0x0
NOfFATs					db 0x0
NOfRootDirEntries		dw 0x0
TotalSectorCount		dw 0x0
MediaDescriptorType		db 0x0
NOfSectorsPerFAT		dw 0x0
NOfSectorsPerTrack		dw 0x0
NOfHeads				dw 0x0
NOfHiddenSectors		dd 0x0
LargeSectorCount		dd 0x0

DriveNumber			db 0x0
FlagsNTOrReserved	db 0x0
Signature			db 0x0
VolumeSerialNumber	dd 0x0
VolumeLabelString	times 11 db 0x0
SystemIdentifier	times 8 db 0x0

Drive 					equ 0x6000
RootDirOffsetPtr 		equ 0x6010
FirstDataSegmentSector	equ 0x6020

_start:
	; clear direction flag and set up stack
	cld
	xor ax, ax
	mov ss, ax
	mov sp, 0x7000
	mov bp, sp
	mov ds, ax
	mov ax, 0x1000
	mov fs, ax

	; save drive number
	mov [Drive], dl

	; fat size (n of sectors per FAT * n of FATs)
	mov ax, [NOfSectorsPerFAT]
	xor cx, cx
	mov cl, [NOfFATs]
	mul cx ; ax = fat size
	push ax	; save fat size in sectors
	mov cx, [BytesPerSector] ; get root dir offset
	mul cx
	mov [RootDirOffsetPtr], ax ; save it
	; root dir size in sectors (n of root dir entries * 32 / sector size)
	mov ax, [NOfRootDirEntries]
	mov cx, 32
	mul cx
	mov cx, [BytesPerSector]
	xor dx, dx
	div cx ; ax = root dir size
	pop cx
	add ax, cx 	; ax = first data sector = fat size (in sectors) + root dir (in sectors) + reserved sectors
	push ax		; fat table + root directory
	mov cx, [NOfReservedSectors]
	add ax, cx
	mov [FirstDataSegmentSector], ax

	; read fat table + root directory entries
	mov dx, [NOfReservedSectors]
	pop ax
	mov bx, fs
	mov es, bx
	xor bx, bx
	call read_disk

	; find file
	mov si, filename
	call find_file

	; read file based on found cluster
	mov bx, 0x2000
	mov es, bx
	xor bx, bx
	call read_file

	; set up registers and jump to kernel
	mov bx, 0x2000
	mov es, bx
	mov ds, bx
	mov fs, bx
	mov gs, bx
	mov fs, bx
	xor bx, bx
	mov dl, [Drive]
	jmp 0x2000:0x0

error:
	mov ah, 0xe
	mov al, '-'
	int 0x10
halt:
	hlt
	jmp halt

; ds:si 	-> filename
; ax <-		first cluster number
find_file:
	; es:di = fs:RootDirectoryEntriesOffset
	mov bx, fs 	;
	mov es, bx	; es:di = fs:0
	mov dx, [RootDirOffsetPtr]	;
	mov di, dx					; es:di = fs:RootDirOffset\

	; dx = root dir entries counter
	xor dx, dx

	; start loop
	.find:
		push si	; save string pointer
		push di	; save root dir entry pointer

		mov cx, 11 	; 11 bytes to compare
		cld			; clear direction flag
		rep cmpsb	; compare es:di (dir entry filename) with ds:si (searched file's filename)
		pop di		; restore dir entry pointer
		pop si		; restore filename pointer
		jz .done	; if equals = we got it, di contains pointer to our desired directory entry

		inc dx		; increment root dir entries counter
		add di, 32	; add to root dir entries pointer an offset to next root dir entry (32 bytes)

		; check if root dir entries counter checked all entries. If yes, panic :(
		mov cx, [NOfRootDirEntries]
		cmp dx, cx
		jae error

		; loop!!! (check another entry)
		jmp .find

	.done:
		; cluster number is at offset 26
		add di, 26
		mov ax, [fs:di] ; deref it and store at ax
		ret

; ax 		-> first cluster
; es:bx 	-> dst
read_file:
	push ax ; save cluster number

	; physical cluster = logical - 2 (first two are reserved)
	sub ax, 2
	; get physical sector of that cluster
	; lba = physical cluster * n of sectors per cluster + first data sector
	xor cx, cx							; 
	mov cl, [NOfSectorsPerCluster]		; 
	mul cx								; ax = cluster * NSC
	mov cx, [FirstDataSegmentSector]	;
	add ax, cx							; ax = cluster * NSC + DS = lba
	
	; read cluster
	mov dx, ax
	xor ax, ax
	mov al, [NOfSectorsPerCluster]
	call read_disk

	; adjust destination memory address
	xor ah, ah
	mov al, [NOfSectorsPerCluster]
	mov cx, [BytesPerSector]
	mul cx
	add bx, ax

	pop ax	; again save and restore
	push ax

	; fat table offset = cluster * 1.5 ?= cluster + (cluster / 2)
	xor dx, dx
	mov cx, 0x2	; cx = 2
	div cx		; ax (C) / cx (2) = cluster / 2 = ax
	mov cx, ax	; cluster / 2 = cx
	pop ax		; ax = cluster
	push ax		; save active cluster number
	add ax, cx	; ax = cluster (ax) + (cluster / 2 from cx)
	mov si, ax	; si = pointer to new cluster info

	; get info about new cluster
	mov ax, [fs:si]

	; cluster data is 12 bit so we need to extract it
	; ax now contains mixed 12 bit value which has info about new cluster
	; but it's not extracted
	pop cx		; let's get active cluster to cx
	and cx, 0x1	; check if active cluster is even	
	; even: 	2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1
	; odd:		2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1
	jz .even
	; if odd: shift 4 bits from the right side
	; =			0 0 0 0 2 2 2 2 2 2 2 2 2 2 2 2
	; and it's 12 bit value
	shr ax, 0x4
	jmp .done

	.even:
	; if even: extract only first 12 bits
	; =			0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1
	and ax, 0000111111111111b

	.done:

	; cluster was marked as bad
	cmp ax, 0xff7
	je error

	; if >= to 0xff8, file has been read. Otherwise ax is new cluster
	cmp ax, 0xff8
	jl read_file

	ret

; dx 		-> lba
; al/ax 	-> sectors to read (max. 127)
; es:bx 	-> dst
read_disk:
	; save lba
	mov ah, 0x2
	push ax
	push dx

	; C, temp = lba / (n of heads * n of sectors per track)
	xor dx, dx			;
	mov ax, [NOfHeads]	; dx:ax = 0:n of heads
	mov bx, [NOfSectorsPerTrack]	; bx = n of sectors per track
	mul bx	; dx:ax = n of heads * n of sectors per track
	mov bx, ax	;
	xor dx, dx	; 0:bx = 0:ax
	pop ax ; ax = lba
	div bx ; ax (C), dx (temp) = lba / (n of heads * n of sectors per track)
	push ax ; save ax (C)

	; H, S - 1 = temp / n of sectors per track
	mov ax, dx	;
	xor dx, dx	; dx:ax = temp
	mov bx, [NOfSectorsPerTrack] ; bx = n of sectors per track
	div bx ; ax (H), dx (S - 1) = temp / n of sectors per track

	; S = S - 1 + 1
	inc dx

	; C = bp, H = ax, S = dx

	; convert to BIOS int 0x13 parameters
	; cx = 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1
	pop cx		; cx = C
	push cx		; let's make another copy of C
	shr cx, 0x2
	; cx = 0 0 16 15 14 13 12 11 (10 9) 8 7 6 5 4 3
	and cx, 0000000011000000b
	; cx = 0 0 0 0 0 0 0 0 10 9 0 0 0 0 0 0
	pop bx
	mov ch, bl
	and dx, 00111111b
	or cl, dl

	mov dh, al
	mov dl, [Drive]
	
	pop ax ; restore parameters

	int 0x13
	jc error

	ret

times 498 - ($ - $$) db 0x0 ; at the position of this semicolon should be file extension
filename		db "BOOT    SYS"

times 510 - ($ - $$) db 0x0
dw 0xaa55