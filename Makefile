
PACKAGE := scc
VERSION := 0.1

objects := globals.o pass.o machine.o liveness.o  helpers.o tree.o arch.o

objects_x86_64 = arch/x86-64/arch-x86-64.o arch/x86-64/spec-x86-64.o arch/x86-64/rules-x86-64.o

tests_bin = tests/test-fast-match

all: scc tests

scc: main.o $(objects) $(objects_x86_64)
	csc -d2 -o $@ $^

# extra dependencies

liveness.o: globals.o machine.o
main.o:    pass.o liveness.o tree.o machine.o
pass.o:    hil-syntax.scm tree.o machine.o arch/x86-64/arch-x86-64.o
tree.o:     helpers.o machine.o
machine.o: globals.o helpers.o arch.o
arch/x86-64/arch-x86-64.o: arch-syntax.o arch/x86-64/spec-x86-64.o arch/x86-64/rules-x86-64.o
arch/x86-64/spec-x86-64.o: arch-syntax.o machine.o
arch/x86-64/rules-x86-64.o: arch-syntax.o tree.o machine.o munch-syntax.o
arch.o:    globals.o
tests/test-spill.o: arch-syntax.scm

# default rule

%.o: %.scm
	csc -d2 -c -J $<

tests:

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
