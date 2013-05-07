#include <memory>

  template< class T, 
          class A = std::allocator<T> >

  class vector { 

    T* v;     // Start of allocation
    T* space; // End of elements, start of space allocated for possible expansion 
    T* last;  // End of allocated space

    A alloc;  // Allocator

   public:

    explicit vector(size_t n, const T &val = T(), const A& a = A()) 
      : alloc(a)                                  // #1
    {
      v = alloc.allocate(n);                      // #2
      try {
        std::uninitialized_fill(v, v + n, val);   // #3
        space = last = v + n;
      } catch(...) {
        alloc.deallocate(v, n);
        throw;
      }
    }; 

    ~vector() { // Destructor
      for (size_t i = 0; i < size(); ++i)
        alloc.destroy(v+i);
      alloc.deallocate(v, capacity());
    }

    vector(const vector& a) {
      size_t count = 0;
      try {
        int asize = a.size();
        if (capacity() < asize) {
          for (size_t i = 0; i < size(); ++i) 
            alloc.destroy(v+i);
          alloc.deallocate(v, capacity());
          v = 0;
          v = alloc.allocate(asize);
          space = last = v + asize;
        }
        for (; count < a.size(); ++count)
          alloc.construct(v+count,a.v[count]);
      } catch(...) {
        if (v != 0) {
          for (size_t i = 0; i < count; ++i)
            alloc.destroy(v+i);
          alloc.deallocate(v, capacity());
          space = last = v;
          throw;
        }
      }
    }

    void swap(vector& a) {
      std::swap(v, a.v);
      std::swap(space, a.space);
      std::swap(last, a.last);
    }

    vector& operator=(const vector& a) { // #BASIC
      vector tmp(a);
      swap(a);
      return *this;
    }

    size_t size() const { return space - v; } 
    size_t capacity() const { return last - v; }

    void push_back(const T& value) { // #STRONG           
      if (space == last) {
        realloc(size()*2);
      }
      
      try {
        alloc.construct(v+size(), value);
        space++;
      } catch (...) {
        alloc.destroy(v+size());
        throw;
      }
    }

    void realloc(size_t newsize) {
      if (newsize <= size())
        return;

      T* newv = 0;
      size_t i = 0;
      size_t size = this->size();
      try {
        newv = alloc.allocate(size*2);
        for (; i < size; ++i) {
          alloc.construct(newv+i, v[i]);
        }
        for (int j = 0; j < size; ++j)
          alloc.destroy(v+j);
        alloc.deallocate(v, size);
        space = newv + size;
        last = newv + size*2;
        v = newv;
      } catch (...) {
        for (int j = 0; j < i; ++j)
          alloc.destroy(newv+j);
        alloc.deallocate(newv, size*2);
        throw;
      }
    }

};


