; currently it's just hello world
; for now the goal is to just execute it
org 0x100

_start:
    .prompt_loop:
        ; get current selected drive
        mov ah, 0x19
        int 0x21
        mov [prompt], al        ; save it to prompt message

        ; print prompt
        mov ah, 0x9
        lea dx, [prompt]
        int 0x21

        ; read from keyboard to cmdbuffer (max 0x40 bytes)
        mov ah, 0xa
        lea dx, [cmdbuffer]
        int 0x21

        ; print newline
        push dx
        mov ah, 0x9
        lea dx, [newline]
        int 0x21
        pop dx

        ; execute command
        xor ch, ch
        mov cl, [cmdbuffer + 1] ; cmdbuffer + 1 = length of input
                                ; we store it at cx, because it's parameter to execute_cmd function
        add dx, 0x2
        call execute_cmd
        jnc .prompt_loop        ; if no error, just go back to prompt

        mov ah, 0x9
        lea dx, [bad_command_or_filename]
        int 0x21
        jmp .prompt_loop

        ; yes, of course we can jump back to _start and have one label,
        ; but there's going to be more code at the beginning, so let it be like it is now.

%include 'command/parse.asm'

prompt: db 0x0, ":\> $"
newline: db 0xA, 0xD, '$'
cmdbuffer:  db 0x40, 0x0
            times 0x48 db 0x0 ; add some additional space
            
char_read_error: db "COMMAND.COM: Failed to read input. Quiting...", 0xD, 0xA, '$'
bad_command_or_filename: db "Bad command or filename", 0xD, 0xA, '$'