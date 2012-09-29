#include <iostream>
#include <cstdlib>

int main()
{
	int *p = (int*)malloc(sizeof(int));
	//it's gonna crash ;-)
	free(p+1);
	return 0;
}
