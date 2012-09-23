#include "scheduler.hpp"
#include <sstream>

Scheduler::Scheduler(Data* data) :
	m_stopped(true), m_time(-1), m_step(data->q)
{
	m_data = data;
}

void Scheduler::reset()
{
	m_stopped = true;
	m_time = -1;
	m_activeCPU = 0;
	m_activeIO = 0;
	//FIXME: it causes memory leak
	//destroy all elements of the lists
	m_stoppedList.clear();
	m_waitingList.clear();
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
	std::string result;
	static unsigned qtime = 0;
	static unsigned pdone = 0;
	static Process* wasio = 0;

	if (m_stopped)
		start();

	++m_time;

#ifdef DEBUG
//	std::cout << "Performing step at time: " << m_time << std::endl;
#endif

	//create processes from tasks are coming now
	std::vector<Task*>::iterator it = m_data->tasks.begin();
	for (; it != m_data->tasks.end(); it++)
		if ((*it)->startTime == m_time)
			m_waitingList.push_back(new Process(*it));

	if (m_activeIO) {
		m_activeIO->next();
		if (m_activeIO->timeToIO()) {
			m_waitingList.push_back(m_activeIO);
			wasio = m_activeIO;
			m_activeIO = 0;
		}	
	}
	if (!m_activeCPU) {
		if (m_waitingList.size()) {
			if (qtime == 0 || qtime == m_data->q)
				pickNext();
		} else {
			std::stringstream ss;
			ss << m_time << " IDLE" << std::endl;
			std::getline(ss, result);
			qtime = 0;
			std::cout << result << std::endl;
		}
	} 
	if (m_activeCPU) {
		++qtime;
		if (m_activeCPU != wasio)
			m_activeCPU->next();
		else wasio = 0;
		std::stringstream ss;
		ss << m_time << " " << m_activeCPU->m_task->id << std::endl;
		std::getline(ss, result);
		std::cout << result << std::endl;
		if (m_activeCPU->done()) {
			delete m_activeCPU;
			m_activeCPU = 0;
			qtime = 0;
			++pdone;
			pickNext();
		} else {
			if (m_activeCPU->timeToIO() == 0) {
				if (m_activeIO) {
					m_waitingList.push_back(m_activeCPU);
					pickNext();
				} else {
					m_activeIO = m_activeCPU;
					m_activeCPU = 0;
					qtime = 0;
					pickNext();
				}
			} else if (qtime == 0 || qtime == m_data->q) { 
				pickNext();	
				qtime = 0;
			}
		}
	} 
	if (pdone >= m_data->tasks.size()) 
		m_stopped = true;
	return result;
}

void Scheduler::pickNext() 
{
	//pick up process from waiting list & put it on cpu
	//process with shotest time to end or io
	unsigned stimeio = -1;
	unsigned stimew = -1;
	if (m_activeCPU)
		m_waitingList.push_back(m_activeCPU);
	if (m_waitingList.size() == 0)
		return;
	std::vector<Process*>::iterator it = m_waitingList.begin();
	std::vector<Process*>::iterator cand;
	for (; it != m_waitingList.end(); it++) {
			if ((*it)->timeToEnd() < stimew)
				cand = it;
			if ((*it)->timeToIO() < stimeio)
				cand = it;
	}
	m_activeCPU = (*cand);
	m_waitingList.erase(cand);
}

std::vector<std::string> Scheduler::run()
{
	std::vector<std::string> res;
	start();
	while (!m_stopped)
		res.push_back(step());
	return res;
}


