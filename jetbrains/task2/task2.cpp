#include <iostream>

int a[] = {
	0x00, 0x01, 0x02, 0x03,
	0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0A, 0x0B,
	0x0C, 0x0D, 0x0E, 0x0F
};

void print()
{
	for (int i = 0; i < sizeof(a)/sizeof(int); ++i)
		std::cout << a[i] << " ";
	std::cout << std::endl;
}

inline void swap(int& a, int& b)
{
	int temp = a;
	a = b;
	b = temp;
}

void f(int l, int r)
{
	if (l == r) {
		a[l] *= 2;
		return;
	}
	int m = l + (r-l)/2;
	f(m+1, r);
	f(  l, m);
	for (int i = l; i <= m; ++i)
		swap(a[i], a[i+m-l+1]);
}

int main()
{
	print();
	f(0, sizeof(a)/sizeof(int) - 1);
	print();
	return 0;
}
