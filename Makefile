

objects := nodes.o pass.o compile.o munch.o arch.o liveness.o utils.o
macros  := class-syntax.scm munch-syntax.scm arch-syntax.scm

all: scc asm-test

scc: $(objects)
	csc -o $@ $^

%.o: %.scm $(macros)
	csc -c $<

asm-test: asm-test.o asm-test.c
	gcc -o asm-test -o $@ $^

asm-test.o: asm-test.nasm
	nasm -f elf64 -g -o $@ $^ 

clean:
	-rm -rf *.o scheme asm-test
