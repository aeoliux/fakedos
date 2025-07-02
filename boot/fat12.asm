; in file table entry we save current cluster number which refers to file pointer

; al        -> mode
; fs        -> segment to filesystem data space
; si        -> offset to file table entry
; ds:dx     -> filename
; bx <-     0 = success, 1 = error
_fat12_open:
    mov [gs:.al], al

    mov bx, 0x1
    call _fat12_initialize
    jc .ret
    call _fat12_search
    jc .ret

    ; fs:ax is now a pointer to FAT directory entry
    push cx

    ; set up file table entry
    mov cl, 0x1
    mov [fs:FDTable.allocated + si], cl
    xor cx, cx
    mov [fs:FDTable.pointer + si], cx
    mov cl, [gs:.al]
    mov [fs:FDTable.mode + si], cl
    xor cl, cl
    mov [fs:FDTable.filesystem + si], cl
    
    ; get first cluster of that file and save it to file table entry reserved for fs driver
    push di
    mov di, ax
    mov cx, [fs:FAT12_DirEntry.Cluster + di]
    mov [fs:FDTable.fsReserved + si], cx

    ; save file size
    mov cx, [fs:FAT12_DirEntry.SizeInBytes + di] ; low 2 bytes, support for 64KB files gonna be added later
    mov [fs:FDTable.fileSize + si], cx
    pop di

    pop cx
    xor bx, bx

    .ret:
        ret

    .al: db 0x0

_fat12_filename_buffer: times 11 db 0x0, '$'
; ds:dx                         -> source filename
; gs:_fat12_filename_buffer <-  parsed filename
_fat12_parse_filename:
    push es
    push di
    push cx
    push si

    ; es:di = destination buffer
    mov cx, gs
    mov es, cx
    mov di, _fat12_filename_buffer
    
    ; 11 bytes to copy and uppercase
    mov cx, 0xb

    ; copy and uppercase
    call string_to_uppercase

    ; source for lodsb
    mov cx, es
    mov ds, cx
    mov si, _fat12_filename_buffer
    .find_dot:
        lodsb
        cmp al, '.'
        je .dot_found
        cmp si, _fat12_filename_buffer + 0xb
        jae .not_found
        jmp .find_dot

    .dot_found:
        ; now filename looks like `BOOT.SYS` and we need it to be `BOOT    SYS`
        ; we need to copy `SYS` down to position 8 and add spaces starting at dot's position
        dec si  ; si - 1 = position of the dot
        push si ; save dot position on the stack
        inc si  ; si = position of the file extension

        add si, 0x2                             ; position file extensions' last character
        mov di, _fat12_filename_buffer + 0xa    ; final desired position of file extension
        .loop:
            mov al, [gs:si] ; get first byte
            mov [gs:di], al ; save it at desired position
            dec si
            dec di

            ; check if we copied 3 bytes
            cmp di, _fat12_filename_buffer + 0x8
            jae .loop

        pop si

        ; pad rest of filename with spaces
        ; si = dot position
        .loop2:
            mov al, 0x20    ; 0x20 = space char
            mov [gs:si], al
            inc si
            ; check if we copied all bytes until file extension
            cmp si, _fat12_filename_buffer + 0x8
            jl .loop2

    .not_found:
        mov bx, 0x1
    .ret:
        pop si
        pop cx
        pop di
        pop es
        ret

; fs        -> segment to filesystem data space
; ds:dx     -> filename
; ax <-     pointer to directory entry
_fat12_search:
    push ds
    push es
    push si
    push di
    push dx

    call _fat12_parse_filename
    mov bx, gs
    mov ds, bx
    mov dx, _fat12_filename_buffer

    ; we start at first sector of root dir entries
    mov ax, [fs:FAT12_FirstRootDirLba] ; ax = first root dir sector
    
    .read_rootdir_sector:
        push es
        push si
        push ax
        push dx

        ; we load root dir sector into fs:FAT12_Buffer
        mov si, fs
        mov es, si
        mov si, FAT12_Buffer

        ; lba is root dir sector
        mov dx, ax

        ; load 1 sector
        xor bh, bh
        mov bl, 0x1

        ; read
        call read_disk.lba
        jc .read_error
        
        pop dx
        pop ax
        pop si
        pop es

        ; fs:FAT12_Buffer contains directory entries
        push di
        mov di, FAT12_DirEntry
        .search:
            push ax
            push ds
            push es
            push di
            push si
            push cx

            ; ds:dx = ds:si = filename
            mov si, dx

            ; es:di = root dir entry filename
            mov bx, fs
            mov es, bx

            ; 11 bytes to compare
            mov cx, 11

            ; compare
            cld
            rep cmpsb

            pop cx
            pop si
            pop di
            pop es
            pop ds

            ; bx = pointer to directory entry
            jz .found_file

            ; get pointer to another directory entry
            add di, FAT12_DirEntrySize

            ; if all entries are checked, get next sector
            mov ax, [fs:FAT12_SizeOfSector]
            add ax, FAT12_DirEntry
            cmp di, ax
            pop ax
            jge .next_sector

            ; check for next entry
            jmp .search

        .next_sector:
            pop di
            ; we need next dir entries sector
            inc ax
            mov bx, [fs:FAT12_FirstDataLba]
            ; if our lba counter will reach the data sector, we dont have that file. Finding failed
            cmp ax, bx
            jge .no_more
            jmp .read_rootdir_sector
    
    .found_file:
        ; bx contains pointer to directory entry
        pop ax
        mov ax, di
        sub ax, FAT12_Buffer ; ax needs to have only offset, not whole pointer
        xor bx, bx
        pop di

        jmp .ret
    .read_error:
        mov bx, 0x1
        pop dx
        pop ax
        pop si
        pop es
        
        stc
        jmp .ret
    .no_more:
        stc
        mov bx, 0x1
        jmp .ret

    .ret:
        pop dx
        pop di
        pop si
        pop es
        pop ds
        ret

; fs:FDTable.allocated + si     -> file table entry
; cx                            -> n of bytes to read
; ds:dx                         -> destination
; ds:dx                         <- data read
_fat12_read:
    push dx
    push cx
    push es
    push si

    ; initialize FAT data
    mov bx, 0x1
    call _fat12_initialize
    jc .ret

    ; get cluster number
    mov ax, [fs:FDTable.fsReserved + si]

    ; start reading loop
    .read_cluster:
        push ax ; save cluster number for future calculations
        push dx ; save memory offset
        push cx ; save bytes count

        ; physical cluster = logical - 2
        sub ax, 0x2
        ; data sector = S per cluster * physical cluster + first data sector
        mov bx, [fs:FAT12_SectorsPerCluster]
        mul bx
        ; ax = S per cluster * physicall cluster
        mov bx, [fs:FAT12_FirstDataLba]
        add ax, bx

        ; we load cluster data into fat buffer
        mov si, fs
        mov es, si
        mov si, FAT12_Buffer
        mov dx, ax ; lba = data sector
        mov bx, [fs:FAT12_SectorsPerCluster] ; bl = sectors per cluster
        call read_disk.lba ; read!
        pop cx
        jc .read_error

        ; offset memory: size of sector * sectors per cluster
        mov ax, [fs:FAT12_SizeOfSector]
        mov bx, [fs:FAT12_SectorsPerCluster]
        mul bx

        ; if counter < size of sector * sectors per cluster, we don't read more clusters
        pop dx
        cmp cx, ax
        jbe .all_read

        ; we loaded whole cluster so we're gonna copy it, ax = sectors per cluster * size of sector
        
        push cx

        ; copy (sectors per cluster * size of sector) bytes to the destination
        mov cx, ax
        call .copy_fat_buffer

        pop cx

        ; counter = counter - (size of sector * sectors per cluster)
        sub cx, ax

        pop ax                          ; get active cluster
        call _fat12_get_next_cluster    ; get next cluster for that file
        jc .ret

        jmp .read_cluster
    .all_read:
        call .copy_fat_buffer

        pop dx
        xor bx, bx
        jmp .ret

    .read_error:
        pop dx
        pop ax
        mov bx, 0x1
        mov ax, 0x1e
        jmp .ret
    .ret:
        pop si
        pop es
        pop cx
        pop dx
        ret

    ; ds:dx     -> destination
    ; cx        -> bytes count
    ; dx <-     ds:dx + cx
    .copy_fat_buffer:
        push si
        push di
        push ds

        ; ds:dx = es:di
        mov di, ds
        mov es, di
        mov di, dx

        ; ds:si = fs:FAT12_Buffer
        mov si, fs
        mov ds, si
        mov si, FAT12_Buffer

        push cx
        cld
        rep movsb   ; copy cx bytes
        pop cx

        pop ds
        pop di
        pop si

        add dx, cx

        ret

; fs:FDTable.allocated + si     -> file table entry
; cx                            -> n
; _fat12_move_pointer:
;     push ax
;     push cx

;     mov ax, [fs:FDTable.fsReserved + si]
;     call _fat12_seek
;     pop cx
;     jc .nope

;     push dx
;     mov dx, [fs:FDTable.pointer + si]
;     add dx, cx
;     mov [fs:FDTable.pointer + si], dx
;     pop dx

;     xor bx, bx
;     .nope:
;     pop ax
;     ret

; todo!
; ax        -> first cluster
; cx        -> file pointer
; ax <-     cluster
; cx <-     offset
; _fat12_seek:
;     push si
;     push dx
;     push dx
;     push ax
;     push cx ; file pointer

;     ; get size of cluster in bytes: sectors per cluster * sector size
;     mov ax, [fs:FAT12_SectorsPerCluster]
;     mov cx, [fs:FAT12_SizeOfSector]
;     mul cx      
;     xor dx, dx  ; cx = size of cluster in bytes
;     push ax

;     ; we need to know how many clusters we need to skip
;     ; file pointer / cluster size = clusters to skip (ax), offset in cluster (dx)
;     mov cx, ax  ; cx = cluster size
;     pop bx
;     pop ax      ; ax = file pointer
;     div cx      ; ax = clusters to skip, dx = offset in cluster

;     mov cx, ax  ; cx = cluster counter
;     pop ax      ; ax = current cluster
;     push dx

;     push bx

;     xor bx, bx
;     .skip_clusters:
;         cmp cx, 0x0 ; nothing to skip
;         jbe .skipped

;         push ax
;         call _fat12_get_next_cluster ; ax = next cluster
;         pop dx ; previous cluster
;         jc .skipped
        
;         dec cx
;         jmp .skip_clusters

;     .skipped:
;         pop si  ; size of cluster
;         pop cx  ; offset

;         pop bx  ; first cluster
;         cmp ax, 0x0 ; that means eof
;         jne .no_eof

;     .no_eof:
;         pop dx
;         pop si

;         ret

; ax    -> current cluster
; ax    <- next cluster
_fat12_get_next_cluster:
    push dx
    push cx
    push ax

    ; (active cluster / 2) + active cluster = fat offset
    xor dx, dx
    mov bx, 0x2
    div bx          ; ax / bx = active cluster / 2
    pop bx          ; bx = active cluster
    push bx         ; save active cluster again
    add ax, bx      ; active cluster / 2 + active cluster
    ; ax = fat offset

    ; fat sector = first fat sector + (fat offset / sector size)
    ; dx:ax / sector size = fat offset / sector size
    mov bx, [fs:FAT12_SizeOfSector]
    xor dx, dx
    div bx      ; ax = fat offset / sector size, dx = fat offset % sector size
    ; dx is entry offset which we'll use to access cluster information
    push dx     ; so we save it
    mov bx, [fs:FAT12_FirstFatLba]
    add ax, bx  ; ax = fat offset / sector size + first fat sector = fat sector

    ; read FAT now
    mov dx, ax  ; dx = fat sector
    mov bl, 0x1
    push es
    push si
    mov si, fs
    mov es, si
    mov si, FAT12_Buffer
    call read_disk.lba
    pop si
    pop es
    pop bx ; get entry offset
    pop cx ; restore info about active cluster
    jc .read_error

    ; get cluster information
    mov ax, [fs:FAT12_Buffer + bx] ; ax contains cluster info

    ; it is 12-bit value splitted, we need to extract it and make it 16-bit
    test cx, 0x1
    jz .even

    ; odd cluster needs to have 4 shifted
    shr ax, 0x4
    jmp .check

    .even: ; even cluster only needs to have first 4 bits zeroed
    and ax, 0xfff
    jmp .check

    .read_error:
        mov ax, 0x1e
        mov bx, 0x1
        stc
        jmp .ret
    
    .eof:
        mov ax, 0x0
        xor bx, bx
        stc
        jmp .ret

    .check:
        ; check cluster status
        cmp ax, 0xff8   ; eof
        jae .eof

        cmp ax, 0xff7   ; bad
        je .read_error

        xor bx, bx
    .ret:
        pop cx
        pop dx
        ret


_fat12_initialize:
    push ax
    push cx
    push dx
    push es
    push si

    ; we load FAT12 BPB to fs:FAT12_BPB (0x0:0x500)
    mov si, fs
    mov es, si
    mov si, FAT12_BPB
    
    ; currently partitions are not supported, so first sector of hard drive/floppy is used as BPB
    mov dx, 0x0
    ; read 1 sector
    mov bl, 0x1
    call read_disk.lba

    jc .error

    ; n of reserved sectors = first fat LBA
    mov ax, [fs:FAT12_BPB.NOfReservedSectors]
    mov [fs:FAT12_FirstFatLba], ax

    ; first root dir lba = n of fats * sectors per fat + first fat lba
    xor ah, ah
    mov al, [fs:FAT12_BPB.NOfFATs]
    mov cx, [fs:FAT12_BPB.NOfSectorsPerFAT]
    mul cx                                  ; ax = n of fats * sectors per fat
    mov dx, [fs:FAT12_FirstFatLba]
    add ax, dx                              ; ax = ax + first fat lba = first root dir lba
    mov [fs:FAT12_FirstRootDirLba], ax

    ; first data lba = root dir entries * size of root dir entry (32) / sector size + first root dir lba
    mov ax, [fs:FAT12_BPB.NOfRootDirEntries]
    mov [fs:FAT12_RootDirEntries], ax
    mov cx, FAT12_DirEntrySize
    mul cx                                  ; dx:ax = root dir entries * size of root dir entry (32)
    mov cx, [fs:FAT12_BPB.BytesPerSector]
    cmp cx, 0x0
    je .error
    mov [fs:FAT12_SizeOfSector], cx
    div cx                                  ; ax = dx:ax / sector size = root dir entries * size of root dir entry (32) / sector size
    mov dx, [fs:FAT12_FirstRootDirLba]
    add ax, dx
    mov [fs:FAT12_FirstDataLba], ax

    xor ah, ah
    mov al, [fs:FAT12_BPB.NOfSectorsPerCluster]
    mov [fs:FAT12_SectorsPerCluster], ax

    mov ax, [fs:FAT12_BPB.NOfSectorsPerFAT]
    mov [fs:FAT12_SectorsPerFAT], ax

    .return:
    pop si
    pop es
    pop dx
    pop cx
    pop ax
    
    xor bx, bx
    clc
    ret 

    .error:
    pop si
    pop es
    pop dx
    pop cx
    pop ax

    stc
    mov bx, 0x1
    ret

; currently LBA is 16-bit (support for larger drives will be added later)
; here it saves some data required for FAT12, because we don't want to have BPB forever loaded in memory
; why? memory saving...
FAT12_SizeOfSector                  equ FilesystemData
FAT12_SectorsPerCluster             equ FAT12_SizeOfSector      + 0x2
FAT12_SectorsPerFAT                 equ FAT12_SectorsPerCluster + 0x2
FAT12_FirstFatLba                   equ FAT12_SectorsPerFAT     + 0x2
FAT12_FirstRootDirLba               equ FAT12_FirstFatLba       + 0x2
FAT12_RootDirEntries                equ FAT12_FirstRootDirLba   + 0x2
FAT12_FirstDataLba                  equ FAT12_RootDirEntries    + 0x2

FAT12_Buffer                        equ FAT12_FirstDataLba              + 0x2
FAT12_BPB                           equ FAT12_Buffer
FAT12_BPB.OEMIdentifier 			equ FAT12_BPB                       + 0x3   ;times 8 db 0x0
FAT12_BPB.BytesPerSector 			equ FAT12_BPB.OEMIdentifier         + 0x8   ;dw 0x0
FAT12_BPB.NOfSectorsPerCluster	    equ FAT12_BPB.BytesPerSector        + 0x2   ;db 0x0
FAT12_BPB.NOfReservedSectors		equ FAT12_BPB.NOfSectorsPerCluster  + 0x1   ;dw 0x0
FAT12_BPB.NOfFATs					equ FAT12_BPB.NOfReservedSectors    + 0x2   ;db 0x0
FAT12_BPB.NOfRootDirEntries		    equ FAT12_BPB.NOfFATs               + 0x1   ;dw 0x0
FAT12_BPB.TotalSectorCount		    equ FAT12_BPB.NOfRootDirEntries     + 0x2   ;dw 0x0
FAT12_BPB.MediaDescriptorType		equ FAT12_BPB.TotalSectorCount      + 0x2   ;db 0x0
FAT12_BPB.NOfSectorsPerFAT		    equ FAT12_BPB.MediaDescriptorType   + 0x1   ;dw 0x0
FAT12_BPB.NOfSectorsPerTrack		equ FAT12_BPB.NOfSectorsPerFAT      + 0x2   ;dw 0x0
FAT12_BPB.NOfHeads				    equ FAT12_BPB.NOfSectorsPerTrack    + 0x2   ;dw 0x0
FAT12_BPB.NOfHiddenSectors		    equ FAT12_BPB.NOfHeads              + 0x2   ;dd 0x0
FAT12_BPB.LargeSectorCount		    equ FAT12_BPB.NOfHiddenSectors      + 0x4   ;dd 0x0

FAT12_BPB.DriveNumber			    equ FAT12_BPB.LargeSectorCount      + 0x4   ;db 0x0
FAT12_BPB.FlagsNTOrReserved	        equ FAT12_BPB.DriveNumber           + 0x1   ;db 0x0
FAT12_BPB.Signature			        equ FAT12_BPB.FlagsNTOrReserved     + 0x1   ;db 0x0
FAT12_BPB.VolumeSerialNumber	    equ FAT12_BPB.Signature             + 0x1   ;dd 0x0
FAT12_BPB.VolumeLabelString	        equ FAT12_BPB.VolumeSerialNumber    + 0x4   ;times 11 db 0x0
FAT12_BPB.SystemIdentifier	        equ FAT12_BPB.VolumeLabelString     + 0xb   ;times 8 db 0x0
FAT12_BPB.End                       equ FAT12_BPB.SystemIdentifier      + 0x8

FAT12_DirEntry                      equ FAT12_Buffer
FAT12_DirEntry.Filename             equ FAT12_DirEntry                          ; times 11 db 0
FAT12_DirEntry.Attrs                equ FAT12_DirEntry.Filename         + 0xB   ; db 0
FAT12_DirEntry.Reserved             equ FAT12_DirEntry.Attrs            + 0x1   ; db 0
FAT12_DirEntry.CreationHSeconds     equ FAT12_DirEntry.Reserved         + 0x1   ; db 0
FAT12_DirEntry.CreationTime         equ FAT12_DirEntry.CreationHSeconds + 0x1   ; dw 0
FAT12_DirEntry.CreationDate         equ FAT12_DirEntry.CreationTime     + 0x2   ; dw 0
FAT12_DirEntry.LastAccessDate       equ FAT12_DirEntry.CreationDate     + 0x2   ; dw0
FAT12_DirEntry.ClusterExtended      equ FAT12_DirEntry.LastAccessDate   + 0x2   ; dw 0
FAT12_DirEntry.LastModifiedTime     equ FAT12_DirEntry.ClusterExtended  + 0x2   ; dw 0
FAT12_DirEntry.LastModifiedDate     equ FAT12_DirEntry.LastModifiedTime + 0x2   ; dw 0
FAT12_DirEntry.Cluster              equ FAT12_DirEntry.LastModifiedDate + 0x2   ; dw 0
FAT12_DirEntry.SizeInBytes          equ FAT12_DirEntry.Cluster          + 0x2   ; dd 0
FAT12_DirEntry.End                  equ FAT12_DirEntry.SizeInBytes      + 0x4   ; end

FAT12_DirEntrySize                  equ FAT12_DirEntry.End - FAT12_DirEntry