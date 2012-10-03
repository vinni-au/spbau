/**********************************
 * Thread collaboration - Library
 * Readers-writers problem
 * Anton Storozhev, Academic Univ.
 * antonstorozhev@gmail.com
 **********************************/
#include <iostream>
#include <list>
#include <pthread.h>

//bookplaces count
int N;
//writers count
int W;
//readers count
int R;

//writers threads
pthread_t* wth;
//readers threads
pthread_t* rth;

void* reader_thread(void*)
{
	return (void*)0;
}

void* writer_thread(void*)
{
	std::list<int> myBooks;
	return (void*)0;
}

int main()
{
	std::cin >> N >> W >> R;
	wth = new pthread_t[W];
	rth = new pthread_t[R];
	for (int i = 0; i < W; ++i)
		if (0 != pthread_create(wth+i, NULL, writer_thread, NULL)) {
			std::cerr << "Error! Can't create writer thread" << std::endl;
			return -1;
		}
	for (int i = 0; i < R; ++i)
		if (0 != pthread_create(rth+i, NULL, reader_thread, NULL)) {
			std::cerr << "Error! Can't create reader thread" << std::endl;
			return -1;
		}
	return 0;
}
