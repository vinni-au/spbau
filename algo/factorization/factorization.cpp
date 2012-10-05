/*
 * Simple factorization
 * Storozhev Anton
 * antonstorozhev@gmail.com
 */
#include <iostream>
#include <vector>

std::vector<int> primes;

int main()
{	
	int k = 1;
	int number;
	int i;
	int count;
	bool prime;
	primes.push_back(2);
	int num = 48;
	std::cin >> num;

	std::cout << "Number: " << num << std::endl;

	while (num != 1) {
		int c = primes.back();
		int k1 = 0;
		while (0 == (num % c)) {
			num /= c;
			k1++;
		}
		if (k1)
			std::cout << c << "^" << k1 << " ";
		prime = true;
		number = 2*k++ + 1;
		i = primes.size();
		for (int j = 0; j < i; ++j)
			if (0 == number % primes[j]) {
				prime = false;
				break;
			}
		if (prime)
			primes.push_back(number);
		prime = false;
		if (i == count-1)
			break;
	}
	std::cout << std::endl;
}
