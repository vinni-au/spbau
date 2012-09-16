#include "foo.h"

int foo()
{
	static unsigned seed = 0;
	seed = (13*seed + 23423)%13452234;
	return seed;
}
