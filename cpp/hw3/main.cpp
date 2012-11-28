#include <iostream>
#include "string.hpp"

int main()
{
    String a = "ololo";
    String b("");
    b = a;
    b.at(0) = 'c';
	return 0;
}
