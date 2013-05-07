#include <exception>
#include <iostream>

class Foo {
 public:
  Foo() : sbuf(new int[1 << 15]) {};
  ~Foo() { delete [] sbuf; }

  int *sbuf;
};

class Bar {
 public:
  Bar() : lbuf(new int[1l << 45]) { /* OOPS! TOO MUCH? */ } 
  ~Bar() { delete [] lbuf; }

  int *lbuf;
};

class Baz {
 public:
  Baz() : foo(new Foo), bar(new Bar)  { } 

  ~Baz() { delete foo;  delete bar; }

  Foo *foo;
  Bar *bar;
};


int main(int ARGC, char *ARGV[]) {
  try {
    Baz baz;
  } catch (std::bad_alloc ba) {
    std::cout << "CATCHA!" << std::endl;
  }

  return 0;
}
