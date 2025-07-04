# FakeDOS
An attempt to make a DOS-like OS which utilizes x86 real mode. Check [documentation](doc/main.md).

## Boot in QEMU
i386 or x86_64 QEMU is required:
```bash
make clean qemu # AS ROOT, volume needs to be mounted in order to copy all files to it
```
Once `COMMAND.COM` shows prompt, you can type something, but only works:
```
command.com                                     # default command interpreter
hello.com                                       # "Hello world!"
hello.com <with some command line arguments>    # "Hello world!" + displays provided cmdline args
allctst.com                                     # checks if memory allocation and deallocation works
allctst.com <random cmdline>                    # checks if memory allocation and deallocation works
                                                # + leaks memory on purpose to show how the OS handles it
```