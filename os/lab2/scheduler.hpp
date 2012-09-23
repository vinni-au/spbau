#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include <string>
#include <vector>
#include <iostream>
#include "process.hpp"

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
	std::vector<Process*> m_waitingList;
	Process* m_activeCPU;
	Process* m_activeIO;

	void pickNext();
};

#endif
