#include "memoryallocator.hpp"
#include <cstring>

MemoryAllocator::MemoryAllocator(size_t size /* = 1024 */)
{
    if (size < 1024)
        size = 1024;
    m_size = size;
    m_mcbsize = sizeof(MCB);
    m_buffer = new char[size];
    MCB* root = new MCB;
    memcpy(m_buffer, root, m_mcbsize);
    m_freemap.push_back(std::pair<char*, size_t>(m_buffer+m_mcbsize, size-m_mcbsize));
}

void* MemoryAllocator::alloc(size_t size)
{
    
}

void MemoryAllocator::free(void* ptr)
{
    MCB* root = (MCB*)m_buffer;
    while (root->block != ptr) {
        root = root->next;
        if (root == 0)
            break;
    }
    if (root) {
        
    }
}

std::ostream& operator<<(std::ostream& os, const MemoryAllocator& ma)
{
    os << "Memory map for allocator" << std::endl;
    os << "# - MCB byte" << std::endl;
    os << "0 - free byte" << std::endl;
    os << "1 - allocated byte" << std::endl;
    int curbyte = 0;
    for (; curbyte < ma.m_mcbsize; ++curbyte)
        os << "#";
    os << std::endl;
    return os;
}
