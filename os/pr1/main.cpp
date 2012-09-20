#include <pthread.h>
#include <unistd.h>
#include <iostream>
#include <cmath>


double sum = 0.0f;
const unsigned N = 20;
const double step = 0.00001;
const double A = 0.0, B = 2.0;

inline double f(double x)
{
	return sin(x);
}

void* sum_thread(void* args)
{
	unsigned num = (unsigned)args;
	double size = (B-A)/N;
	double left = size*num;
	double right = left + size;
	double tsum = 0.0;
	std::cout << "Started thread N " << num << " with left = " << left
		<< ", right = "<< right << ", size = " << size << std::endl;
	for (double x = left; x < right; x += step) {
		tsum += f(x);
	}
	sum += tsum;
	std::cout << "Finished thread N " << num << " with tsum = " << tsum << std::endl;
	return (void*)0;
}

int main()
{
	pthread_t* threads = new pthread_t[N];

	for (int i = 0; i < N; ++i)
		if (0 != pthread_create(&threads[i], NULL,
				sum_thread, (void*)i))
			return -1;
	for (int i = 0; i < N; ++i)
		if (0 != pthread_join(threads[i], NULL))
			return -1;

	std::cout << "\n\nInt = " << sum*step << std::endl;

	return 0;
}
