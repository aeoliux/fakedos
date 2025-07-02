# FakeDOS - kernel
WIP DOS-like kernel `BOOT.SYS`.

## What it does?
1. Copies itself from `0x2000:0x0000` to `0x1000:0x0000`.
2. Sets up segment registers to `0x1000:0x0000`.
3. Sets up base stack to `0x0000:0x8500`.
4. Makes far jump into `0x1000:0x0050` (`0x50` is offset to kernel's main code).
5. Gets RAM size.
6. Sets up basic interrupts: `0x21`.
7. Prints startup message.
8. Sets default drive to that kernel is booted from.
9. Loads drivers (TODO!).
10. Executes `COMMAND.COM` from FAT12 filesystem.
11. If `COMMAND.COM` quits, kernel displays message about system crash.
12. System is halted.

## How it works?
### Program execution
#### Binary format
- FakeDOS currently supports raw binary format similar to `.COM` files, but without PSP (Program Segment Prefix).
- Binary needs to be offsetted to `0x100` (just like in DOS).
- Every program must have exit directive at the end of the execution. It can be
```asm
mov ah, 0x4c
mov al, <exit code>
int 0x21

; OR

mov al, <exit code>
retf ; yeah, regular far return works
```
#### How to execute?
Command line arguments and environment variables are not supported (but they will be).
```asm
; .....

mov ah, 0x4b
mov al, 0x0
lea dx, [program_path]
int 0x21
jc .execution_error

; .....

program_path: db "command.com", 0x0

```
#### How kernel executes program?
1. Opens program's file as read-only.
2. Gets its size.
3. Allocates memory block (each block is built from paragraphs, 1 paragraph = 16 bytes). Blocks size is set to hold whole executable + 4KB stack (in the future stack size will be configurable in `CONFIG.SYS`).
4. Reads whole executable (limit is 64KB - 256B) into newly allocated memory block.
5. Pushes whole CPU state (registers and flags) onto the stack.
6. Sets segment registers.
7. Sets new stack and stores in it an address of the old stack. This causes stacks to be connected like a chain.
8. Performs a far call by using `retf` (it pushes return's and program's addresses onto new stack).
9. **Program is being executed**
10. Once program exits, code control is returned to the kernel.
11. Restores old stack (its address is saved in the new stack).
12. Restores previous CPU state (registers and flags).
13. Deallocates memory used by that program (TODO!).
13. Gives code control back to interrupt caller.
#### How exiting works?
1. Making interrupt causes flags, return address to be pushed onto the stack.
2. `exit` interrupt pops all of those values from the stack, so on the stack now there are only program executor's return address and address of previous stack.
3. Now `exit` interrupt just calls `retf` and code is returned to program executor at point no. 10.