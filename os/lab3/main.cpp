#include <iostream>
#include <vector>
#include <cctype>
#include <cstdlib>
#include <cstdio>
#include "data.hpp"
#include "process.hpp"
#include "scheduler.hpp"

bool canbeeof = true;

static void parseBursts(std::istream& s, Task* task)
{
#ifdef PARSE
	std::cout << "    Trying to parse Bursts " << std::endl;
#endif
	char ch = static_cast<char>(s.get());
	while (ch != '(' && !isalnum(ch)) {
		ch = static_cast<char>(s.get());
		if (s.eof())
			return;
	}
	while (!isalnum(ch)) {
		canbeeof = false;
		if (ch == '(') {
#ifdef PARSE
	std::cout << "     Parsing burst" << std::endl;
#endif
			std::vector<int> b;
			for (int i = 0; i < 2; ++i) {
				while (!isdigit(ch = static_cast<char>(s.get()))) ;
				std::string tonum(1, ch);
				while (isdigit(ch)) {
					ch = static_cast<char>(s.get());
					tonum.push_back(ch);
				}
				s.unget();
				int a = atoi(tonum.c_str());
				if (a <= 0)
					throw new std::istream::failure("Parse error");
				if (i == 0)
					b.push_back(a);
                else b.push_back(a+b[0]);
#ifdef PARSE
	std::cout << "     parsed number: " << a << std::endl;
#endif

				if (i == 0)
					while ((ch = static_cast<char>(s.get())) != ',') ;
				else {
					while ((ch = static_cast<char>(s.get())) != ')') ;
					if (s.eof())
						return;
					task->ioBursts.push_back(b);
					canbeeof = true;
					while (!isalnum(ch) && ch != '(') {
						ch = static_cast<char>(s.get());
						if (s.eof())
							return;
					}
				}
			}
		}
	}
	s.unget();
#ifdef PARSE
	std::cout << "    Ended parse bursts" << std::endl << std::endl;
#endif
}

static Data* parse(std::istream& s)
{
#ifdef PARSE
	std::cout << "Entering parse" << std::endl;
#endif
	s.exceptions(std::ios_base::failbit | std::ios_base::badbit);
	Data* data = new Data;
	try {
        s >> data->q1;
        s >> data->q2;
		while (!s.eof()) {
#ifdef PARSE
	std::cout << "    Trying to parse task" << std::endl;
#endif
			Task* task = new Task;
			data->tasks.push_back(task);
			canbeeof = false;
			s >> task->id;
			s >> task->startTime;
			s >> task->workTime;
			canbeeof = true;
#ifdef PARSE
	std::cout << "     Parsed: id = " << task->id <<
		", workTime = " << task->workTime <<
		", startTime = " << task->startTime << std::endl;
#endif
			parseBursts(s, task);
		}
	}
	catch (std::istream::failure& e) {
		if (s.eof() && canbeeof)
			return data;
		delete data;
		return 0;
	}
	return data;
}


int main()
{
#ifdef DEBUG
	freopen("test.txt","r", stdin);
	freopen("out.txt","w", stdout);
#endif
	Data* data = parse(std::cin);
	if (0 != data) {
#ifdef DEBUG
		std::cout << "Data readed. Task count: " << data->taskCount() << std::endl;
		std::cout << *data;
#endif
        if (data->validate()) {
            Scheduler* sh = new Scheduler(data);
            sh->step();
            while (!sh->isStopped()) {
                sh->step();
                std::cout.flush();
            }
        } else {
            std::cout << "WRONG_DATA" << std::endl;
            return -2;
        }
	} else {
		std::cout << "WRONG_FORMAT" << std::endl;
		return -1;
	}
	return 0;
}

