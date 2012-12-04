all: format

format: formatc
	g++ -Werror -g3 format.o -o ../format

formatc:
	g++ -Werror -g3 -c format.cpp -o format.o

clean:
	rm -f *.o format format.exe
