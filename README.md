# FakeDOS
An attempt to make an DOS-like OS which utilizes x86 real mode

## Bootsector
Bootsector supports FAT12. It checks for file `boot.sys`, but only at root directory.
`boot.sys` is loaded to `0x2000:0x0` and executed as raw binary. Max filesize is 64KB (autosegmentation of
memory has not been added yet to bootsector).

## Kernel
Now? Only displays `Starting...`, but via its own interrupt. Interrupt handler for `int 0x21` is done.

### Next steps
```
- implementation of interrupts (now only printing string via BIOS API works)
- read-only FAT12 support (later maybe read-write)
- basic COMMAND.COM
```