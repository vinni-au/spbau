#include "fib.h"

long long fib(unsigned n)
{
	if (n == 1)
		return 0;
	if (n == 2)
		return 1;

	long long f1 = 0;
	long long f2 = 1;
	long long result;
	for (int i = 2; i < n; ++i) {
		result = f1 + f2;
		f1 = f2;
		f2 = result;
	}
	return result;
}
