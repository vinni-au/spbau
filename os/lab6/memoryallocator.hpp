#ifndef _MEMORYALLOCATOR_HPP_
#define _MEMORYALLOCATOR_HPP_

#include <iostream>
#include <list>

struct MCB {
    MCB* next;
    size_t blocksize;
    void* block;

    MCB() : next(0), blocksize(0), block(0)
    {	}
};


struct MemoryAllocator
{
	explicit MemoryAllocator(size_t size = 1024);

	void* alloc(size_t size);
	void free(void* ptr);
	
	friend std::ostream& operator<<(std::ostream& os, const MemoryAllocator& ma);

private:
	size_t m_size;	
	char* m_buffer;
	size_t m_mcbsize;
};

#endif //_MEMORYALLOCATOR_HPP_

