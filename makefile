compile:
	nasm -g -F dwarf -f elf64 build/out.asm -o build/out.o
	ld -m elf_x86_64 build/out.o -o build/out

run:
	cargo run $(TARGET)
	make compile
	./build/out
