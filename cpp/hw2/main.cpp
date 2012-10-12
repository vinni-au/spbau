#include <iostream>
#include "stringvector.hpp"

int main()
{
	StringVector v(0);
	v.add("ololo");
	v.add("oh, what's this");
	v.add("what a hell");
	cout << "size is " << v.size() << endl << v;
	cout << "resizing to 2" << endl;
	v.resize(2);
	cout << v;
	cout << "resizing back to 3" << endl;
	v.resize(3);
	cout << v;
	v.reserve(16);
	cout << "reserving 16" << endl;
	cout << v;
	return 0;
}
