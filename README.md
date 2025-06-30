# FakeDOS
An attempt to make a DOS-like OS which utilizes x86 real mode

## Bootsector
Bootsector supports FAT12 floppies. It checks for file `boot.sys`, but only at root directory.
`boot.sys` is loaded to `0x2000:0x0` and executed as raw binary. Max filesize is 64KB (autosegmentation of
memory has not been added yet to bootsector).

## Kernel
Now? Displays `Starting...` message, loads COMMAND.COM from root directory of FAT12 floppy and executes it
in some basic way (no PSP and real execution of program, only `call far` now)

### Next steps
```
- implementation of interrupts (now only printing string via BIOS API works)
- basic COMMAND.COM
```
