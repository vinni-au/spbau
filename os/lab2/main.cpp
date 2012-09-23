#include <iostream>
#include <vector>
#include <list>
#include <cctype>
#include <cstdlib>

#define DEBUG
//#define PARSE

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
	std::vector<Task*> tasks;

	~Data();

	int taskCount() const
	{ return tasks.size(); }
	
	friend std::ostream& operator<<(std::ostream&, Data&);
};

std::ostream& operator<<(std::ostream& os, Data& data)
{
	os << "Quantum = " << data.q << std::endl;
	std::vector<Task*>::iterator it = data.tasks.begin();
	for (; it != data.tasks.end(); it++) {
		os << " Task: " << (*it)->id << std::endl;
	}
}

Data::~Data() 
{
	std::vector<Task*>::iterator it = tasks.begin();
	for (; it != tasks.end(); it++)
		delete *it;
}

class Process {
public:
	Process(Task *task);
	~Process();

	unsigned timeToEnd() const
	{ return (m_task->workTime - m_time); }

	unsigned timeToIO() const;
	
	bool done() const
	{ return (m_time > m_task->workTime); }

private:
	Task* m_task;
	unsigned m_time;
};

Process::Process(Task* task) :
	m_time(0)
{
	m_task = new Task;
	m_task->id = task->id;
	m_task->startTime = task->startTime;
	m_task->workTime = task->workTime;
	m_task->ioBursts = task->ioBursts;
}

Process::~Process()
{
	delete m_task;
}

unsigned Process::timeToIO() const
{
	size_t size = m_task->ioBursts.size();
	if (m_time >= m_task->ioBursts[size-1][1])
		return 0;
	unsigned cur;
	unsigned min = static_cast<unsigned>(-1);
	std::vector<std::vector<int> >::iterator it = m_task->ioBursts.begin();	
	for (; it != m_task->ioBursts.end(); it++) {
		if (m_time <= (*it)[1] && m_time >= (*it)[0])
			return 0;
		cur = (*it)[0] - m_time;
		if (cur < min)
			min = cur;
	}
	return min;
}

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

	unsigned m_time;
	unsigned m_step;

	std::vector<Process*> m_stoppedList;
	std::vector<Process*> m_pausedList;
	Process* m_activeCPU;
	Process* m_activeIO;

	void createProcesses();
};

Scheduler::Scheduler(Data* data) :
	m_stopped(true), m_time(0), m_step(data->q)
{
	m_data = data;
	createProcesses();
}

void Scheduler::createProcesses()
{
	std::vector<Task*>::iterator it = m_data->tasks.begin();
	for (; it != m_data->tasks.end(); it++)
		m_stoppedList.push_back(new Process(*it));
}

void Scheduler::reset()
{
	m_stopped = true;
	m_time = -1;
	m_activeCPU = 0;
	m_activeIO = 0;
	m_stoppedList.clear();
	m_pausedList.clear();
	createProcesses();
}

void Scheduler::start()
{
	reset();
	m_stopped = false;
#ifdef DEBUG
	std::cout << "Simulation started" << std::endl;
#endif
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
				b.push_back(a);
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
	std::string str;
	try {
		unsigned q;
		s >> q;
		data->q = q;
		while (!s.eof()) {
#ifdef PARSE
	std::cout << "    Trying to parse task" << std::endl;
#endif
			Task* task = new Task;
			data->tasks.push_back(task);
			canbeeof = false;
			s >> task->id;
			s >> task->workTime;
			s >> task->startTime;
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
	std::string s;
	Data* data = parse(std::cin);
	if (0 != data) {
#ifdef DEBUG
		std::cout << "Data readed. Task count: " << data->taskCount() << std::endl;
		std::cout << *data;
#endif
		Scheduler* sh = new Scheduler(data);
		while (!sh->isStopped())
			std::cout << sh->step();						
	} else {
		std::cout << "WRONG_FORMAT" << std::endl;
		return -1;
	}
	return 0;
}

