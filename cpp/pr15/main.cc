#include <iostream>

namespace N {
  struct T{};
  void foo (T x) {}

};

void foo (N::T x) {}

int main() {
  N::T x;
  (foo)(x);

  int * p = new int[10];
  p[10] = 0;
  return 0;
}
