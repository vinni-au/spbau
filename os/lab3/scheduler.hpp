#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include <string>
#include <vector>
#include <list>
#include <iostream>
#include "process.hpp"

class Scheduler {
public:
	explicit Scheduler(Data* data);
	void step();
	void run();

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

	//! Current time (tick)
	unsigned m_time;
	//! Current step (quantum)
    unsigned m_step;
    unsigned m_step1;
    unsigned m_step2;
	//! Current time in quantum
	unsigned m_qtime;
	//! Done processes count
	unsigned m_pdone;

    bool m_place1;

    std::list<Process*> m_waitingCpuList1;
    std::list<Process*> m_waitingCpuList2;
    std::list<Process*> m_waitingIoList;

	Process* m_activeCPU;
	Process* m_activeIO;

    void plan();
	void advanceCPU();
	void advanceIO();
    Process* pickFromQueue(std::list<Process*>& list);
    void printCandidates(std::list<Process*> &list);
};

#endif
