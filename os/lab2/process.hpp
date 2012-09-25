#ifndef _PROCESS_H_
#define _PROCESS_H_

#include "data.hpp"

class Process {
public:
	Process(Task *task);
	~Process();

	unsigned timeToEnd() const
	{
        return (m_task->workTime - m_time);
    }

	unsigned timeToIO() const;
	
	bool done() const
    { return (timeToEnd() <= 0); }

    void next(bool cpu = true)
    {
        ++m_time;
        if (cpu)
            ++m_ptime;
    }

private:
	Task* m_task;
	unsigned m_time;
    unsigned m_ptime;

	friend class Scheduler;
};

#endif

