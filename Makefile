

objects := nodes.o pass.o compile.o 
macros  := class-syntax.scm

all: scheme asm-test

scheme: $(objects)
	csc -o $@ $^

%.o: %.scm $(macros)
	csc -c $<

asm-test: asm-test.o asm-test.c
	gcc -o asm-test -o $@ $^

asm-test.o: asm-test.nasm
	nasm -f elf64 -g -o $@ $^ 

clean:
	-rm -rf *.o scheme asm-test