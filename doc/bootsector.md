# FakeDOS - bootsector
Bootsector reads small FAT12 partitions by BIOS `ah=0x2 int 0x13`.

## Issues
Most of those issues are going to be fixed in the future. Bootsector is now being written to be able to boot, but not properly. Those issues might be fixed in the future once kernel is finished.
- It loads at once both FAT12 table and all root directory entries. This is not RAM efficient! It is not recommended to use this bootsector with drives bigger than floppy disk, because it might do some memory overflow.
- FAT12 and root directory entries are loaded into `0x1000:0x0`, so again it is not memory efficient and not capable of booting from large drives.
- It loads `BOOT.SYS` kernel into address `0x2000:0x0`, so PCs with less memory than 256KB + BIOS RAM might get memory overflow.
- No support for partitions (now it is only Volume Boot Record).
- Does not boot from hard drives (unknown reason).
- It takes disk geometry from FAT12 header, so if BIOS decides it is not correct geometry, booting will fail (this won't get fixed).

## What is does?
1. Loads `BOOT.SYS` into `0x2000:0x0000` from FAT12 formatted floppy.
2. Sets segment registers to `0x2000`.
4. Makes far jump to `0x2000:0x0000`.

## How it works?
There is too much to be changed in this bootsector (there is a plan for complete rewrite), but there are a lot of comments in its code `vbr/vbr.asm` which are explaining a lot.