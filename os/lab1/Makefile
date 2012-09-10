CC=g++
CPPFLAGS=-Werror

all: lab1
	rm -f *.o

lab1: main ash
	$(CC) main.o ash.o -o ash
	
ash: ash.cpp ash.hpp
	$(CC) $(CPPFLAGS) -c ash.cpp
	
main: main.cpp
	$(CC) $(CPPFLAGS) -c main.cpp
	
clean:
	rm -f *.o ash
	
default: lab1
