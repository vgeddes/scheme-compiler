

objects := class.o nodes.o pass.o compile.o 
macros  := class-syntax.scm

all: scheme

scheme: $(objects)
	csc  -o $@ $^

%.o: %.scm $(macros)
	csc  -c $<

clean:
	-rm -rf *.o scheme