
objects := nodes.o pass.o main.o munch.o arch.o liveness.o utils.o ssa.o ssa-const.o ssa-types.o ssa-transforms.o

all: scc tests

scc: $(objects)
	csc -o $@ $^

# extra dependencies

nodes.o: struct-syntax.scm
main.o: struct-syntax.scm
pass.o: struct-syntax.scm

arch.o:  x86-64.scm arch-syntax.scm

ssa.o: struct-syntax.scm

munch.o: patterns.scm munch-syntax.scm fast-match-syntax.scm

# default rule

%.o: %.scm
	csc -c $<

tests: tests/test-fast-match

tests/test-fast-match: tests/test-fast-match.scm fast-match-syntax.scm
	csc -o $@ $<

asm-test: asm-test.o asm-test.c
	gcc -o asm-test -o $@ $^

asm-test.o: asm-test.nasm
	nasm -f elf64 -g -o $@ $^ 

clean:
	-rm -rf *.o scc asm-test
