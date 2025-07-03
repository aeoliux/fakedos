# FakeDOS
An attempt to make a DOS-like OS which utilizes x86 real mode. Check [documentation](doc/main.md).

## Boot in QEMU
i386 or x86_64 QEMU is required:
```bash
make clean qemu # AS ROOT, volume needs to be mounted in order to copy all files to it
```
Once `COMMAND.COM` shows prompt, you can type something, but only works:
```
command.com
hello.com
hello.com <with some command line arguments>
```