#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

void _first() {

  std::vector<std::string>  bart_pray = { "I", "will", "not", "aim", "for", "the", "head" };
  std::vector<size_t>       lengths;

  std::cout << "mem_fn way" << std::endl;
  std::transform(bart_pray.begin(), bart_pray.end(), std::back_inserter(lengths), std::mem_fn(&std::string::length));
  std::for_each(lengths.begin(), lengths.end(), [](size_t a) -> void { std::cout << a << " "; });
  std::cout << std::endl;
}

struct _int {

      _int(int i = 0) : i(i) {}

      _int mul(int b) const {
        return _int(i * b);
      }

      _int add(int c) const {
        return _int(i + c);
      }

      int i;
    };

    
void _second() {

  std::vector<_int> integers = { _int(1), _int(2), _int(3), _int(4) };
  std::vector<_int> scaled;

  using namespace std::placeholders;

  // POPULATE ``scaled'' with the integers from ``integers'' scaled by the factor of 2 and shifted by 5
  std::cout << std::endl << "Temp array way" << std::endl;
  std::vector<_int> tmp;
  tmp.resize(integers.size());
  scaled.resize(integers.size());

  std::transform(integers.begin(), integers.end(), tmp.begin(), std::bind(&_int::mul, _1, 2));
  std::transform(tmp.begin(), tmp.end(), scaled.begin(), std::bind(&_int::add, _1, 5));

  std::for_each(scaled.begin(), scaled.end(),
                [](_int const& a) -> void {std::cout << a.i << " ";} );
  std::cout << std::endl;

  std::cout << "Nested bind way" << std::endl;
  scaled.clear();

  //i.mul(2).add(5);
  
  // OOPS!

  std::transform(integers.begin(), integers.end(), std::back_inserter(scaled),
                 std::bind(&_int::add,
                           std::bind(&_int::mul, _1, 2),
                           5));

  std::for_each(scaled.begin(), scaled.end(),
                [](_int const& a) -> void {std::cout << a.i << " ";} );
  std::cout << std::endl;
}


int main(int ARGC, char *ARGV[]) {

  //_first();
  _second();

  return 0;

}


/*#include <iostream>
#include <functional>
#include <vector>
#include <memory>
#include <algorithm>

struct Animal {
    Animal(double weight) : weight(weight) {}
    virtual void bark(int count) = 0;
    double weight;
};

struct Duck : Animal {
    Duck() : Animal(3.0) {}
    void bark(int count) {
        for (size_t i = 0; i < count; ++i)
            std::cout << "I'm duck! I can't bark" << std::endl;
    }
};

struct Dog : Animal {
    Dog() : Animal(10.0) {}
    void bark(int count) {
        for (size_t i = 0; i < count; ++i)
            std::cout << "Bow-wow!" << std::endl;
    }
};

int main()
{
    std::vector<std::unique_ptr<Animal>> zoo;
    zoo.push_back(std::unique_ptr<Animal>(new Dog()));
    zoo.push_back(std::unique_ptr<Animal>(new Duck()));

    /*for (auto it = zoo.begin(); it != zoo.end(); ++it) {
        (*it)->bark();
    }*/
/*    std::for_each(zoo.begin(), zoo.end(), std::mem_fn(&Animal::bark));
    std::for_each(zoo.begin(),
                  zoo.end(),
                  [](const std::unique_ptr<Animal>& a) -> void {
                    std::cout << std::mem_fn(&Animal::weight)(a) << std::endl;
                  }
    );
    std::for_each(zoo.begin(), zoo.end(),
                  std::bind(&Animal::bark, std::placeholders::_1, 5));
    return 0;
}
*/
