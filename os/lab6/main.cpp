#include <iostream>
#include "memoryallocator.hpp"

int main()
{
	MemoryAllocator* ma = new MemoryAllocator;
	std::cout << *ma;
	return 0;
}
