#include <iostream>
#include "memoryallocator.hpp"

int main()
{
    MemoryAllocator ma;
    std::cout << "Creating MemoryAllocator" << std::endl;
    std::cout << ma;
    std::cout << "Allocating 768 bytes" << std::endl;
    void* ololo = ma.alloc(768);
    std::cout << ma;
    std::cout << "Allocating 10 bytes" << std::endl;
    void* olo = ma.alloc(10);
    std::cout << ma;
    std::cout << "Deallocating 768 bytes" << std::endl;
    ma.free(ololo);
    std::cout << ma;
    std::cout << "Allocating 30 bytes" << std::endl;
    ololo = ma.alloc(30);
    std::cout << ma;
	return 0;
}
