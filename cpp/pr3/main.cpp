#include <iostream>

#define DATA_LEN 1024

int data[DATA_LEN];
unsigned reallen = 0;

void read()
{
	int i = 0;
	while (!std::cin.eof()) {
		std::cin >> *(data+i);
		++i;
	}
	reallen = i-1;
}

void print()
{
	std::cout << "Array: " << std::endl;
	int i = 0;	
	while (i < reallen) {
		std::cout << *(data+i++) << " ";
	}
	std::cout << std::endl;
}

void merge(int* a, int l, int m, int r)
{
	int buf[DATA_LEN];
	int i1 = l;
	int i2 = m;
	int i = 0;
	while (i < r-l) {
		if (i1 == m) {
			(*(buf+i)) = (*(a+i2));
			++i2;
		} else if (i2 == r) {
			(*(buf+i)) = (*(a+i1));
			++i1;
		} else {
			if ((*(a+i1)) < (*(a+i2))) {
				(*(buf+i)) = (*(a+i1));
				++i1;
			} else {
				(*(buf+i)) = (*(a+i2));
				++i2;
			}
		}
		++i;	
	}	
	i = 0;
	while (i < r-l) {
		(*(a+i+l)) = (*(buf+i));
		++i;
	}
}

void sort(int* a, int l, int r)
{
	int mid = (r-l)/2;
	if ((r-l) <= 1) 
		return;
	sort(a, l, l+mid);
	sort(a, l+mid, r);
	merge(a, l, l+mid, r);
}

int main()
{
	read();
	print();
	sort(data, 0, reallen);
	print();
}
