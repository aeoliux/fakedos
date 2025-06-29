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
    call _fat12_find
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
    pop di

    pop cx
    xor bx, bx

    .ret:
        ret

    .al: db 0x0

; TODO!
_fat12_read:
    call _fat12_initialize
    ret

; fs        -> segment to filesystem data space
; ds:dx     -> filename
; ax <-     pointer to directory entry
_fat12_find:
    push es
    push si
    push di
    push dx

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
    mov [fs:FAT12_SizeOfSector], cx
    div cx                                  ; ax = dx:ax / sector size = root dir entries * size of root dir entry (32) / sector size
    mov dx, [fs:FAT12_FirstRootDirLba]
    add ax, dx
    mov [fs:FAT12_FirstDataLba], ax

    .return:
    pop si
    pop es
    pop dx
    pop cx
    pop ax
    xor bx, bx
    ret

    .error:
    pop si
    pop es
    pop dx
    pop cx
    pop ax
    stc
    mov bx, 0x1
    jmp .return

; currently LBA is 16-bit (support for larger drives will be added later)
; here it saves some data required for FAT12, because we don't want to have BPB forever loaded in memory
; why? memory saving...
FAT12_SizeOfSector                  equ FilesystemData
FAT12_FirstFatLba                   equ FAT12_SizeOfSector      + 0x2
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