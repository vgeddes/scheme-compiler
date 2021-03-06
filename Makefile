
PACKAGE := scc
VERSION := 0.1

objects := nodes.o pass.o machine.o liveness.o utils.o tree.o arch.o

objects_x86_64 = arch/x86-64/arch-x86-64.o arch/x86-64/spec-x86-64.o arch/x86-64/rules-x86-64.o

tests_bin = tests/test-fast-match

all: scc tests

scc: main.o $(objects) $(objects_x86_64)
	csc -o $@ $^

# extra dependencies

nodes.o:   struct-syntax.scm
main.o:    struct-syntax.scm
pass.o:    struct-syntax.scm
tree.o:    struct-syntax.scm
arch/x86-64/arch-x86-64.o: arch-syntax.scm
arch/x86-64/spec-x86-64.o: arch-syntax.scm
arch/x86-64/rules-x86-64.o: arch-syntax.scm munch-syntax.scm
arch.o:    nodes.scm
tests/test-spill.o: arch-syntax.scm


# default rule

%.o: %.scm
	csc -c $<

tests: tests/test-spill

tests/test-fast-match: tests/test-fast-match.scm fast-match-syntax.scm
	csc -o $@ $<

tests/test-spill: tests/test-spill.o $(objects) $(objects_x86_64)
	csc -o $@ $^

asm-test: asm-test.o asm-test.c
	gcc -o asm-test -o $@ $^

asm-test.o: asm-test.nasm
	nasm -f elf64 -g -o $@ $^

dist: tarball

dist-prep:
	rm -rf $(PACKAGE)-$(VERSION)
	mkdir $(PACKAGE)-$(VERSION)
	cp -a *.scm Makefile README docs tests $(PACKAGE)-$(VERSION)

tarball: dist-prep
	tar -czf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	rm -rf $(PACKAGE)-$(VERSION)

tags:
	etags *.scm

.PHONY : clean
clean:
	rm -rf $(PACKAGE)-$(VERSION) $(PACKAGE)-$(VERSION).tar.gz
	rm -rf $(objects) $(objects_x86_64) scc
