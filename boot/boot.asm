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
    push dx
    call ivt_setup
 
    ; let's print some random message to check if display_string interrupt work (the same usage as in regular DOS)
    mov ah, 0x9
    lea dx, [startup_message]
    int 0x21
    pop dx

    ; boot procedure explained:
    ; 1. set default drive
    ; 2. load drivers
    ; 3. load COMMAND.COM
    ; 4. execute COMMAND.COM

    ; 1. get information about the drive we booted from and set it as default
    call set_drive
    jc _os_error

    ; 2. load drivers (TODO!)

    ; 3.    load COMMAND.COM
    ; 3.1.  open COMMAND.COM
    mov ah, 0x3d
    mov al, 0x0
    lea dx, [command_com]
    int 0x21
    jc .command_com_missing
    push ax ; save file descriptor

    ; 3.2.  read COMMAND.COM (TODO!)

    ; 3.3. close COMMAND.COM
    pop bx
    mov ah, 0x3e
    int 0x21
    jc _os_error

    ; 4. execute COMMAND.COM (TODO!)
    jmp .command_com_crash

    ; COMMAND.COM crash message
    .command_com_crash:
    lea dx, [command_com_crashed_message]
    jmp .print

    ; COMMAND.COM missing message
    .command_com_missing:
    lea dx, [command_com_missing_message]
    jmp .print

    ; COMMAND.COM load error message
    .command_com_load_error:
    lea dx, [command_com_load_error_message]

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
error_message: db "System encountered an critical error and it cannot continue. Sorry...", 0xD, 0xA, '$'
command_com_missing_message: db "COMMAND.COM is not present on the disk or disk is not accessible. System halted!", 0xD, 0xA, '$'
command_com_crashed_message: db "COMMAND.COM unexpectedly exited. System halted!", 0xD, 0xA, '$'
command_com_load_error_message: db "COMMAND.COM was not loadable. System halted!", 0xD, 0xA, '$'
command_com: db "COMMAND COM", 0x0

%include 'boot/ivt.asm'
%include 'boot/dynamic_call.asm'
%include 'boot/basic_io.asm'
%include 'boot/disks.asm'