compile:
	nasm -g -F dwarf -f elf64 build/out.asm -o build/out.o
	ld build/out.o -o build/out
