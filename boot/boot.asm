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

    mov cx, KERNEL_SIZE  ; we want to copy whole memory segment (64KB)
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
    mov sp, 0x8500  ; let's go 0x8500 as stack offset
    mov bp, sp      ; bp = sp, ofc

    ; we are ready so we are farjumping to... the same code, BUT we are switching segments. The final goal is to save as much RAM as we can
    mov bx, 0x1000
    push bx
    mov bx, _boot
    push bx
    retf
halt:
    hlt
    jmp halt

times 0x50 - ($ - $$) db 0

; here is the main kernel code
_boot:
    ; save drive number
    push dx

    ; get size of available RAM in paragraphs (16 bytes blocks)
    call get_ram_size

    ; first we are going to set up interrupts
    call ivt_setup
 
    ; let's print some random message to check if display_string interrupt work (the same usage as in regular DOS)
    mov ah, 0x9
    lea dx, [startup_message]
    int 0x21

    ; boot procedure explained:
    ; 1. set default drive
    ; 2. load drivers
    ; 3. load COMMAND.COM
    ; 4. execute COMMAND.COM

    ; 1. get information about the drive we booted from and set it as default
    pop dx          ; restore drive number
    call set_drive
    jc _os_error

    ; 2. load drivers (TODO!)

    ; 3. execute COMMAND.COM
    mov ah, 0x4b
    mov al, 0x0
    lea dx, [command_com]
    int 0x21
    jc .command_com_load_error

    ; COMMAND.COM crash message
    mov dx, command_com_crashed_message
    jmp .print

    ; COMMAND.COM missing message
    .command_com_missing:
    mov dx, command_com_missing_message
    jmp .print

    ; COMMAND.COM load error message
    .command_com_load_error:
    mov dx, command_com_load_error_message

    ; print
    .print:
    mov bx, cs
    mov es, bx
    mov ds, bx
    xor bx, bx

    ; print selected message
    mov ah, 0x9
    int 0x21

    ; halt everything if something failed
    jmp halt

_os_error:
    lea dx, [error_message]
    jmp _boot.print

; DOS-like boot message
startup_message: db 0xA, "Starting...", 0xD, 0xA, 0xA, '$'
error_message: db "BOOT.SYS: System encountered an critical error and it cannot continue. Sorry...", 0xD, 0xA, '$'
command_com_missing_message: db "BOOT.SYS: COMMAND.COM is not present on the disk or disk is not accessible. System halted!", 0xD, 0xA, '$'
command_com_crashed_message: db "BOOT.SYS: COMMAND.COM unexpectedly exited. System halted!", 0xD, 0xA, '$'
command_com_load_error_message: db "BOOT.SYS: COMMAND.COM was not loadable. System halted!", 0xD, 0xA, '$'
command_com: db "command.com", 0x0, '$'

command_com_addr: dw 0x100, 0x2000

%include 'boot/ivt.asm'
%include 'boot/dynamic_call.asm'
%include 'boot/mem.asm'
%include 'boot/basic_io.asm'
%include 'boot/disks.asm'
%include 'boot/exec.asm'

KERNEL_SIZE         equ ($ - $$)
FIRST_FREE_BLOCK    equ (KERNEL_SIZE / 0x10) + 0x1 + 0x1000