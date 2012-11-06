#include "memoryallocator.hpp"
#include <cstring>

MemoryAllocator::MemoryAllocator(size_t size /* = 1024 */)
{
    if (size < 1024)
        size = 1024;
    m_size = size;
    m_mcbsize = sizeof(MCB);
    m_buffer = new char[size];
    new (m_buffer) MCB;
}

void* MemoryAllocator::alloc(size_t size)
{
    MCB* root = (MCB*)m_buffer;
    while (root->next != 0) {
        size_t freesize = (char*)root->next - ((char*)root + m_mcbsize + root->blocksize);
        if (freesize >= size + m_mcbsize) {
            MCB *mcb = new ((char*)root+root->blocksize+m_mcbsize) MCB;
            mcb->blocksize = size;
            mcb->next = root->next;
            root->next = (MCB*)((char*)root+root->blocksize+m_mcbsize);
            root->next->block = (char*)root->next + m_mcbsize;
            return root->next->block;
        }
        root = root->next;
    }
    if (root->next == 0) {
        size_t freesize = m_size - (((char*)root + m_mcbsize + root->blocksize) - m_buffer);
        if (freesize < size + m_mcbsize)
            return 0;
        MCB* mcb = new ((char*)root+root->blocksize+m_mcbsize) MCB;
        mcb->blocksize = size;
        mcb->next = 0;
        root->next = (MCB*)((char*)root+root->blocksize+m_mcbsize);
        root->next->block = (char*)root->next + m_mcbsize;
        return root->next->block;
    }
    return 0;
}

void MemoryAllocator::free(void* ptr)
{
    MCB* root = (MCB*)m_buffer;
    MCB* cur = root->next;
    while (cur != 0) {
        if (cur->block == ptr) {
            root->next = cur->next;
            memset(cur, 0, m_mcbsize);
        }
        root = cur;
        cur = cur->next;
    }
}

std::ostream& operator<<(std::ostream& os, const MemoryAllocator& ma)
{
    os << "Memory map for allocator" << std::endl;
    os << "# - MCB byte" << std::endl;
    os << "0 - free byte" << std::endl;
    os << "1 - allocated byte" << std::endl;
    size_t curbyte = 0;
    for (; curbyte < ma.m_mcbsize; ++curbyte)
        os << "#";
    int curstate = 0;
    int block = 0;
    MCB* next = ((MCB*)ma.m_buffer)->next;
    while (curbyte < ma.m_size) {
        if (((void*)next) == (void*)(ma.m_buffer + curbyte)) {
            curstate = 2;
            block = ma.m_mcbsize;
        }
        if (curstate == 0) {
            os << "0";
            curbyte++;
        }
        if (curstate == 2 && block == 0) {
            curstate = 1;
            block = next->blocksize;
        }
        if (curstate == 1 && block == 0) {
            curstate = 0;
            next = next->next;
        }
        if (curstate == 2) {
            os << "#";
            block--;
            curbyte++;
        }
        if (curstate == 1) {
            os << "1";
            block--;
            curbyte++;
        }
        if (curbyte%64 == 0)
            os << std::endl;
    }
    os << std::endl;
    return os;
}
