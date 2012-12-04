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

struct smart_ptr {
    ~smart_ptr() {
        ptrs.remove(this);
    }

    void* operator->() const
    {   return ptr; }

    void* get() const
    {   return ptr; }

    int id() const
    {   return m_id;  }

    static smart_ptr* ptr_by_id(int id) {
        std::list<smart_ptr*>::iterator it = ptrs.begin();
        for (; it != ptrs.end(); ++it) {
            if ((*it)->id() == id)
                return *it;
        }
        return 0;
    }

    static smart_ptr* ptr_by_ptr(void* ptr) {
        std::list<smart_ptr*>::iterator it = ptrs.begin();
        for (; it != ptrs.end(); ++it) {
            if ((*it)->get() == ptr)
                return *it;
        }
        return 0;
    }

private:
    explicit smart_ptr(void* ptr)
        : ptr(ptr)
    {
        static int nextid = 0;
        m_id = ++nextid;
        ptrs.push_back(this);
    }

private:
    void* ptr;
    int m_id;

    static std::list<smart_ptr*> ptrs;

    friend struct MemoryAllocator;
};

struct MemoryAllocatorInfo {
    int allocated;
    int allocatedToUser;
    int freeRaw;
    int free;

    MemoryAllocatorInfo() :
        allocated(0),
        allocatedToUser(0),
        freeRaw(0),
        free(0)
    {   }

    MemoryAllocatorInfo(int n1, int n2, int n3, int n4) :
        allocated(n1),
        allocatedToUser(n2),
        freeRaw(n3),
        free(n4)
    {   }
};

struct MemoryAllocator
{
    explicit MemoryAllocator(size_t size = 1024);

    smart_ptr *alloc(size_t size);
    bool free(smart_ptr *ptr);

    short r_ptr(void* ptr) const {
        return (short)(((char*)ptr) - m_buffer);
    }

    void* ptr(short r_ptr) const {
        return (void*)(m_buffer + r_ptr);
    }

    MemoryAllocatorInfo info(std::ostream& os = std::cout, bool out = false);

    void defragment();

    friend std::ostream& operator<<(std::ostream& os, const MemoryAllocator& ma);

private:
    size_t m_size;
    char* m_buffer;
    size_t m_mcbsize;
};

#endif //_MEMORYALLOCATOR_HPP_

