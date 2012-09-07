#include <iostream>
#include "fib.h"

int main()
{
	for (int i = 1; i < 25; ++i)
		std::cout << fib(i) << " ";
	return 0;
}
