# FakeDOS - kernel
WIP DOS-like kernel `BOOT.SYS`.

## Be careful!
Every program must deallocate manually each memory block it allocates at the runtime. If it does not, this can cause stack and program's code not to deallocate properly. Memory model used in this kernel is very primitive: memory blocks are stacked on top of each other and they are connected in chain, so to properly free blocks before an allocated block. This allocated block must be freed too, however kernel has freeing queue implemented, so you don't need to free memory in specific order (but it is recommended). **All you need to do is just to free all blocks you have allocated.**

## What it does?
1. Copies itself from `0x2000:0x0000` to `0x1000:0x0000`.
2. Sets up segment registers to `0x1000:0x0000`.
3. Sets up base stack to `0x0000:0x8500`.
4. Makes far jump into `0x1000:0x0050` (`0x50` is offset to kernel's main code).
5. Gets RAM size.
6. Sets up basic interrupts: `0x21`.
7. Prints startup message.
8. Detects floppy disks and sets default drive to that kernel is booted from.
9. Loads drivers (TODO!).
10. Executes `COMMAND.COM` from FAT12 filesystem.
11. If `COMMAND.COM` quits, kernel displays message about system crash.
12. System is halted.

## How it works?
### Program execution
#### Binary format
- FakeDOS currently supports raw binary format similar to `.COM` files, but with very simple PSP (only the most necessary kernel's data).
- Binary needs to be offsetted to `0x100` (just like in DOS).
- Every program must have exit directive at the end of the execution. It can be
```asm
mov ah, 0x4c
mov al, <exit code>
int 0x21

OR

mov al, 0x0
int 0x20
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
3. Allocates memory block (each block is built from paragraphs, 1 paragraph = 16 bytes). Blocks size is set to hold PSP (256B) + whole executable + 4KB stack (in the future stack size will be configurable in `CONFIG.SYS`).
4. Reads whole executable (limit is 64KB - 256B) into newly allocated memory block.
5. Pushes whole CPU state (registers and flags) onto the stack.
6. Sets segment registers.
7. Sets new stack and stores in it an address of the old stack. This causes stacks to be connected like a chain.
8. Sets program's initial stack address in the PSP.
9. Performs a far call by using `retf` (it pushes program's addresses onto new stack).
10. **Program is being executed**
11. Once program exits, code control is returned to the kernel.
12. Restores old stack (its address is saved in the new stack).
13. Deallocates old stack.
14. Restores previous CPU state (registers and flags).
15. Deallocates memory block containing program's code (deallocating blocks allocated by that program has not been implemented yet).
16. Gives code control back to interrupt caller.
#### How exiting works?
1. Making interrupt causes flags, return address to be pushed onto the stack.
2. To determine which program has called `exit`, it pops return address from the stack. Return address is segment+offset, so kernel knows which program performed `exit` operation. Obtained segment points to program's PSP.
3. Now program's initial stack is restored based on the information from PSP (that's how stack safety works).
4. Kernel sets return address to program executor at point no. 11 and `retf` is executed. Code control is returned to program executor.