#ifndef _PROCESS_H_
#define _PROCESS_H_

#include "data.hpp"

class Process {
public:
	Process(Task *task);
	~Process();

	unsigned timeToEnd() const
	{ return (m_task->workTime - m_time)-1; }

	unsigned timeToIO() const;
	
	bool done() const
	{ return (m_time >= m_task->workTime); }

	void next()
	{ ++m_time; }

private:
	Task* m_task;
	unsigned m_time;

	friend class Scheduler;
};

#endif

