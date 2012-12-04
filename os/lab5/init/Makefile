all: init

init: initc
	g++ -Werror -g3 init.o -o ../init

initc:
	g++ -Werror -g3 -c init.cpp -o init.o

clean:
	rm -f *.o init init.exe
