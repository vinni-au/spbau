all: mkdr main stringvector	
	@g++ obj/main.o obj/stringvector.o -o hw2
	@echo "Compile successfull."

mkdr:
	@if [ ! -d obj ]; then mkdir obj; fi

main:
	@g++ -c -g3 main.cpp -o obj/main.o

stringvector:
	@g++ -c -g3 stringvector.cpp -o obj/stringvector.o

clean:
	@rm -f obj/*.o hw2
