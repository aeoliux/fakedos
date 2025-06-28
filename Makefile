all: vbr/vbr.bin boot/boot.sys

%.bin: %.asm
	nasm -f bin -o $@ $^

%.sys: %.asm
	nasm -f bin -o $@ $^

disk.img: all
	dd if=/dev/zero of=disk.img bs=512 count=2880
	mkfs.fat -F12 -n DISK disk.img
	dd if=vbr/vbr.bin of=disk.img skip=62c seek=62c bs=1c count=450 conv=notrunc
	mount -o loop disk.img /mnt
	find . -name '*.sys' | xargs -I \{} cp \{} /mnt
	umount /mnt

qemu: disk.img
	qemu-system-i386 -fda disk.img ${QEMU_ARGS}

clean:
	find . -name '*.bin' -delete
	find . -name '*.sys' -delete
	find . -name '*.img' -delete
