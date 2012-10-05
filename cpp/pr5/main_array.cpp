#include "array.hpp"

int main()
{
	Array a;
	a.add(1);
	a.add(2);
	a.add(3);
	Array b(a);
	std::cout << "Size: " << a.size() << std::endl;
	b.set(4, 1);
	for (int i = 0; i < a.size(); ++i)
		std::cout << a.get(i) << " ";
	std::cout << std::endl;
	std::cout << "Size: " << b.size() << std::endl;
	for (int i = 0; i < b.size(); ++i)
		std::cout << b.get(i) << " ";
	std::cout << std::endl;
	Array c;
	c = a;
	std::cout << "Size: " << c.size() << std::endl;
	for (int i = 0; i < c.size(); ++i)
		std::cout << c.get(i) << " ";
	std::cout << std::endl;
	return 0;
}
