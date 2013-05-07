#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <vector>

#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>

using namespace boost::lambda;

template<typename T>
struct placeholder_t : std::ostream {
    placeholder_t(T& t) {
        inner = t;
    }

    void operator()() {
        *this << inner;
    }

private:
    T inner;
};

template<typename T>
placeholder_t<T> operator<< (std::ostream& s, placeholder_t<T>& p) {
    return p;
}

placeholder_t __1;

int main(int ARGC, char *ARGV[]) {

  using std::cout;
  using std::endl;
  using std::for_each;
  using std::transform;
  using std::vector;

  vector<int> v(32);

  // Generate

  for_each(v.begin(), v.end(), _1 = 0xDEADBEEF);
  for_each(v.begin(), v.end(), cout << std::hex << __1 << "\n");

  cout << endl;
  // Transform

  transform(v.begin(), v.end(), v.begin(), bind(std::rand));
  for_each(v.begin(), v.end(), cout << std::hex << _1 << "\n");

  cout << endl;
  // Sort

  sort(v.begin(), v.end(), _1 > _2);
  for_each(v.begin(), v.end(), cout << std::hex << _1 << "\n");

  return 0;

}
