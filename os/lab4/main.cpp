/**********************************
 * Thread collaboration - Library
 * Readers-writers problem
 * Anton Storozhev, Academic Univ.
 * antonstorozhev@gmail.com
 **********************************/
#include <iostream>
#include <list>
#include <vector>
#include <pthread.h>
#include <cstring>
#include <cstdlib>
#include <string>
#include <linux/futex.h>
#include <sys/time.h>
#include <unistd.h>

//bookplaces count
int N;
//writers count
int W;
//readers count
int R;

//logger thread
pthread_t logth;
//writers threads
pthread_t* wth;
//readers threads
pthread_t* rth;

//bookplaces array
// -1 there's no book
//  0 book is readed or edited
//  1 book is ready to be readed or edited
int* bookplaces;

std::list<std::string> log;

static inline useconds_t randsleep()
{
	return (rand()%450 + 50);
}

void* reader_thread(void* arg)
{
	int i = (int)arg;
#ifdef DEBUG
	log.push_back(std::string("Reader started: "));
#endif
	std::vector<int> books;
	while (true) {
		books.clear();
		for (int i = 0; i < N; ++i)
			if (bookplaces[i] != -1)
				books.push_back(i);
		int sel = rand()%books.size();
		sel = books[sel];
		if (bookplaces[sel] == 0) {
			log.push_back(std::string("WAIT_READ"));//sel
			pause();
		}
		log.push_back(std::string("START_READ"));//sel
		bookplaces[sel] = 0;
		usleep(randsleep());
		bookplaces[sel] = 1;
		log.push_back(std::string("END_READ"));//sel
		//TODO: wake up others if there's any
	}
	return (void*)0;
}

void* writer_thread(void* arg)
{
	std::vector<int> myBooks;
	int i = (int)arg;
#ifdef DEBUG
	log.push_back(std::string("Writer started: "));
#endif
	std::vector<int> freeplaces;
	//0 - nothind to do
	//1 - edit book
	//2 - new book
	char action = 0;
	while (true) {
		freeplaces.clear();
		for (int i = 0; i < N; ++i)
			if (bookplaces[i] == -1)
				freeplaces.push_back(i);
		if (freeplaces.size() && myBooks.size())
			action = rand()%2 + 1;
		if (freeplaces.size() && !myBooks.size())
			action = 2;
		if (!freeplaces.size() && myBooks.size())
			action = 1;
		if (action == 0)
			pause();
		if (action == 2) {
			int sel = rand()%freeplaces.size();
			sel = freeplaces[sel];
			bookplaces[sel] = 1;
			myBooks.push_back(sel);
			log.push_back(std::string("PUBLISH ")); //i sel
		} else if (action == 1) {
			int sel = rand()%myBooks.size();
			sel = myBooks[sel];
			if (bookplaces[sel] == 0) {
				log.push_back(std::string("WAIT_EDIT"));//i sel
				pause();
			}
			log.push_back(std::string("START_EDIT"));//i sel
			bookplaces[sel] = 0;
			usleep(randsleep());
			bookplaces[sel] = 1;
			log.push_back(std::string("END_EDIT"));//i sel
			//TODO:wake up
		}
	}
	return (void*)0;
}

void* logger_thread(void*)
{
	while (true) {
		if (log.size()) {
			std::cout << *log.begin() << std::endl;
			log.pop_front();
		}		
		usleep(50);
	}
}

int main()
{
	std::cin >> N >> W >> R;
	bookplaces = new int[N];
	wth = new pthread_t[W];
	rth = new pthread_t[R];
	memset(bookplaces, -1, N);

	//start logger
	if (0 != pthread_create(&logth, NULL, logger_thread, NULL)) {
		std::cerr << "Error! Can't start logger" << std::endl;
		return -1;
	}

	//start writers
	for (int i = 0; i < W; ++i)
		if (0 != pthread_create(wth+i, NULL, writer_thread, (void*)i)) {
			std::cerr << "Error! Can't create writer thread" << std::endl;
			return -1;
		}
			
	//start readers
	for (int i = 0; i < R; ++i)
		if (0 != pthread_create(rth+i, NULL, reader_thread, (void*)i)) {
			std::cerr << "Error! Can't create reader thread" << std::endl;
			return -1;
		}

	for (int i = 0; i < W; ++i)
		if (0 != pthread_join(wth[i], NULL))
			return -1;

	for (int i = 0; i < R; ++i)
		if (0 != pthread_join(rth[i], NULL))
			return -1;

	if (0 != pthread_join(logth, NULL))
		return -1;

	return 0;
}
