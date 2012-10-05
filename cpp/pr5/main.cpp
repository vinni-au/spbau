#include <iostream>
#include <cstdio>

struct A 
{
	int a1;
	int a2;
	
	void print() {
		std::cout << a1 << " " << a2 << std::endl;
	}
};

void print (char* mem, int size)
{
	for (int i = 0; i < size; ++i)
		printf("%d ", (int)mem[i]);
	std::cout << std::endl;
}

int main()
{
	char mem[1024];
	print(mem, 32);
	A &b = (A&)mem;
	b.a1 = 1;
	b.a2 = 2;
	b.print();
	print(mem, 32);
	return 0;
}
