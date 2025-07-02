; currently it's just hello world
; for now the goal is to just execute it
org 0x100

_start:
    ; display message
    lea dx, [msg]
    mov ah, 0x9
    int 0x21

    ; execute COMMAND.COM in loop until memory ends
    mov ah, 0x4b
    mov al, 0x0
    lea dx, [command_com]
    int 0x21
    jnc .no_error

    ; last message
    mov ah, 0x9
    lea dx, [not_executed_more_times]
    int 0x21

    ; exit with funny number code
    .no_error:
    mov ah, 0x4c
    mov al, 69
    int 0x21

msg: db "This is COMMAND.COM, now I'm gonna execute myself again!", 0xA, 0xA, 0xD, '$'
not_executed_more_times: db "Next execution failed, so it's likely that memory is full :(", 0xA, 0xD
db "Let's quit and go back to kernel!", 0xA, 0xA, 0xD, '$'
command_com: db "command.com", 0x0

; fill with some random byte just to check if kernel loads whole file
times 1200 db 69