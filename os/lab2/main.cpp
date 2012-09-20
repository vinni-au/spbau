#include <iostream>
#include <vector>
#include <cctype>
#include <cstdlib>

#define DEBUG

bool canbeeof = true;

struct Task {
	std::string id;
	unsigned startTime;
	unsigned workTime;
	std::vector<std::vector<int> > ioBursts;

	Task() {}
};

struct Data {
	int q;
	std::vector<Task> tasks;
};

class Scheduler {
public:
	explicit Scheduler(Data* data);
	std::string step();
	std::vector<std::string> run();

	void setData(Data* data)
	{ m_data = data; }
	Data* data() const
	{ return m_data; }

	bool isStopped() const
	{ return m_stopped; }
private:
	Data* m_data;
	bool m_stopped;

	void start();
	void reset();
};

Scheduler::Scheduler(Data* data) :
	m_stopped(true)
{
	m_data = data;
}

void Scheduler::reset()
{
	m_stopped = true;
}

void Scheduler::start()
{
	reset();
	m_stopped = false;
}

std::string Scheduler::step()
{
	if (m_stopped)
		start();
}

std::vector<std::string> Scheduler::run()
{
	std::vector<std::string> res;
	start();
	while (!m_stopped)
		res.push_back(step());
	return res;
}

static void parseBursts(std::istream& s, Task& task)
{
#ifdef DEBUG
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
#ifdef DEBUG
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
				b.push_back(a);
#ifdef DEBUG
	std::cout << "     parsed number: " << a << std::endl;
#endif

				if (i == 0)
					while ((ch = static_cast<char>(s.get())) != ',') ;
				else {
					while ((ch = static_cast<char>(s.get())) != ')') ;
					if (s.eof())
						return;
					task.ioBursts.push_back(b);
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
#ifdef DEBUG
	std::cout << "    Ended parse bursts" << std::endl << std::endl;
#endif
}

static Data* parse(std::istream& s)
{
#ifdef DEBUG
	std::cout << "Entering parse" << std::endl;
#endif
	s.exceptions(std::ios_base::failbit);
	Data* data = new Data();
	std::string str;
	try {
		unsigned q;
		s >> q;
		data->q = q;
		while (!s.eof()) {
#ifdef DEBUG
	std::cout << "    Trying to parse task" << std::endl;
#endif
			Task task;
			canbeeof = false;
			s >> task.id;
			s >> task.workTime;
			s >> task.startTime;
			canbeeof = true;
#ifdef DEBUG
	std::cout << "     Parsed: id = " << task.id <<
		", workTime = " << task.workTime <<
		", startTime = " << task.startTime << std::endl;
#endif
			parseBursts(s, task);
		}
	}
	catch (std::istream::failure) {
		if (s.eof() && canbeeof)
			return data;
		delete data;
		return 0;
	}
	return data;
}


int main()
{
	std::string s;
	Data* data = parse(std::cin);
	if (0 != data) {

	} else {
		std::cout << "WRONG_FORMAT" << std::endl;
		return -1;
	}
	return 0;
}
