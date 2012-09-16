#include <stdio.h>
#include "foo.h"

int main()
{
	int i;
	for (i = 0; i < 10; ++i)
		printf("%d ", foo());
	int *t = 0x0;
	*t = 12;
	return 0;
}
