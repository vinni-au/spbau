#include <iostream>

#include "vector.hh"

struct A {

  A()           { std::cout << "A::A()" << std::endl; }
  A(const A &a) { std::cout << "A::A(const A&)" << std::endl; }
  ~A()          { std::cout << "A::~A()" << std::endl; }
  
};

//
// TEST #1: Assures vector's STRONG exception-safety policy
//

template<size_t _S, size_t _N>
  struct _TooBig : _TooBig<_S * _N, _N - 1> {};

template<size_t _S>
  struct _TooBig<_S, 0> {
    char _[_S];
  };

template<size_t _UL> 
  struct __ {
  static const unsigned long value = _UL;
};


void first() {

  using std::shared_ptr;
  using std::move;

  using std::cout;
  using std::endl;

  typedef _TooBig<1, 12> faulty_t;

  vector<shared_ptr<faulty_t> > v(1, std::shared_ptr<faulty_t>(new faulty_t));
  try {

    for (;;) {
      v.push_back(
        move(shared_ptr<faulty_t>(new faulty_t))
      );
      cout << v.size() << endl;
    }

  } catch (...) {
    cout << v.size() << endl;
  }

}


//
// TEST #2: Assures vector's STRONG exception-safety policy
//

struct _DoNotCopyMeMoreThanTwice {

  _DoNotCopyMeMoreThanTwice() 
    : copies_(0) {}  

  _DoNotCopyMeMoreThanTwice(const _DoNotCopyMeMoreThanTwice& o) 
    : copies_(o.copies_ + 1) 
    {
      if (copies_ > 1)
        throw std::exception();
    } 

  size_t copies_;

};

void second() {

  using std::cout;
  using std::endl;

  typedef _DoNotCopyMeMoreThanTwice faulty_t;

  vector<faulty_t> v(16);

  try {

    for (;;) {
      v.push_back(
        faulty_t()
      );
      cout << v.size() << endl;
    }

  } catch (...) {
    cout << v.size() << endl;
  }

}


int main(int ARGC, char *ARGV[]) {

  first();
  second();
  
  return 0;

}
