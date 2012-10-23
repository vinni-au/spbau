all: init format

init: initc
	g++ -Werror -g3 init.o -o init

initc:
	g++ -Werror -g3 -c init.cpp -o init.o

format: formatc
	g++ -Werror -g3 format.o -o format

formatc:
	g++ -Werror -g3 -c format.cpp -o format.o

clean:
	rm -f *.o init format
