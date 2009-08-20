

objects := class.o nodes.o converter.o test.o 
macros  :=  class-syntax.scm

all: test

test: $(objects)
	csc  -o $@ $^

%.o: %.scm $(macros)
	csc  -c $<

clean:
	-rm -rf *.o test