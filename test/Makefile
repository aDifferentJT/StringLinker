test: test-gcc test-g++ test-clang test-clang++
.PHONY: test

.PHONY: test-gcc test-g++ test-clang test-clang++
test-gcc: main.c string.h string.o
	gcc main.c string.o -o gcc.out && \
	./gcc.out | diff string.txt -
test-g++: main.cpp string.h string.o
	g++ main.cpp string.o -o g++.out && \
	./g++.out | diff string.txt -
test-clang: main.c string.h string.o
	clang main.c string.o -o clang.out && \
	./clang.out | diff string.txt -
test-clang++: main.cpp string.h string.o
	clang++ main.cpp string.o -o clang++.out && \
	./clang++.out | diff string.txt -

string.o string.h: ../stringLinker string.txt
	../stringLinker -i string.txt

.PHONY: clean
clean:
	rm string.asm string.h string.o main.o *.out

