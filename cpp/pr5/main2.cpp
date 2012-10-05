#include <iostream>
#include <cstdio>

#pragma pack(push,1)

struct A
{
	A() {
		a1 = 1;
		a2 = 2;
		a3 = 3;
		a4 = 4;
		a5 = 5;
	}
private:
	int a1, a2;
	char a3;
	bool a4;
	int a5;
};

#pragma pop
int main()
{
	A a;
	char *ptr = (char*)&a;
	for (int i = 0; i < 32; ++i)
		printf("%02x", (unsigned char)ptr[i]);
	std::cout << std::endl;
	return 0;
}

