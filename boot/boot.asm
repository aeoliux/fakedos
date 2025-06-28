bits 16
org 0x0

; we are currently at 0x2000:0x0 and we want more RAM, so we are going deeper. Down to 0x1000:0x0
_start:
    ; we are going to copy ourselves to 0x1000:0x0 using rep movsb
    ; so let's set up registers
    mov ax, 0x1000  ;
    mov es, ax      ; es = destination segment
    xor ax, ax      ;
    mov di, ax      ; di = destination offset
    ; destination address = es:di

    mov ax, 0x2000  ;
    mov ds, ax      ; ds = source segment
    xor ax, ax      ;
    mov si, ax      ; si = source offset
    ; source address = ds:si

    mov cx, 0x1000  ; we want to copy whole memory segment (4096/0x1000 bytes)
    cld             ; clear direction flag
    rep movsb       ; MAGIC!!!!

    ; let's set up segment registers
    mov bx, 0x1000
    mov es, bx      ;
    mov ds, bx      ;
    mov gs, bx      ;
    mov fs, bx      ; everything except stack pointer goes to 0x1000 (destination code segment)

    xor bx, bx      ; 
    mov ss, bx      ; for stack we will use 0x0000 segment (only for kernel, userspace is gonna have a different stack)
    mov sp, 0x7000  ; let's go 0x7000 as stack offset
    mov bp, sp      ; bp = sp, ofc

    ; we are ready so we are jumping to... the same code, BUT we are switching segments. The final goal is to save as much RAM as we can
    jmp 0x1000:0x50 ; 0x1000 (destination segment) x 0x10 + 0x50 (kernel main code offset)
halt:
    hlt
    jmp halt

times 0x50 - ($ - $$) db 0

; here is the main kernel code
_boot:
    ; first we are going to set up interrupts
    call ivt_setup

    ; let's print some random message to check if display_string interrupt work (the same usage as in regular DOS)
    mov ah, 0x9
    mov dx, message
    int 0x21

    ; halt everything if something failed
    jmp halt

; DOS-like boot message
message: db 0xA, "Starting...", 0xD, 0xA, 0xA, '$'

%include 'boot/ivt.asm'
%include 'boot/basic_io.asm'