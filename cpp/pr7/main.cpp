#include "SFile.hpp"

int main()
{
	SFile f1("f1.txt");
	f1.write("test1\n");
	SFile f2("f1.txt");
	f2.write("test2\n");
	return 0;
}
