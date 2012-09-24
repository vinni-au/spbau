#include "scheduler.hpp"
#include <sstream>

Scheduler::Scheduler(Data* data) :
	m_stopped(true), 
	m_time(0), 
	m_step(data->q), 
	m_qtime(0), 
	m_pdone(0),
	m_data(data)
{	}

void Scheduler::reset()
{
	m_stopped 	= true;
	m_time 		= 0;
	m_activeCPU = 0;
	m_activeIO 	= 0;
	m_pdone 	= 0;
	m_qtime 	= 0;
	//FIXME: it causes memory leak
	//destroy all elements of the lists
	m_waitingIoList.clear();
	m_waitingCpuList.clear();
}

void Scheduler::start()
{
	reset();
	m_stopped = false;
#ifdef DEBUG
	std::cout << "Simulation started" << std::endl;
#endif
}

void Scheduler::plan()
{
	if (m_activeCPU) {
		if (m_activeCPU->timeToIO()) 
			m_waitingCpuList.push_back(m_activeCPU);
		else 
			m_activeIO = m_activeCPU;
		m_activeCPU = 0;
	}
	if (m_waitingCpuList.size()) {
		unsigned stimew = -1;
		unsigned stimeio = -1;
		std::vector<Process*>::iterator it = m_waitingCpuList.begin();
		std::vector<Process*>::iterator cand;
		for (; it != m_waitingCpuList.end(); it++) {
				if ((*it)->timeToIO() < stimeio)
					cand = it;
				if ((*it)->timeToEnd() < stimew)
					cand = it;
		}
		m_activeCPU = *cand;
		m_waitingCpuList.erase(cand);
	} else m_activeCPU = 0;
}

void Scheduler::advanceCPU()
{
	m_activeCPU->next();
	++m_qtime;
	std::cout << m_time << " " << m_activeCPU->m_task->id << std::endl;
	std::cout.flush();
	if (m_activeCPU->done() || m_activeCPU->timeToIO() == 0)
		m_qtime = 0;
	if (m_activeCPU->done())
		++m_pdone;
}

void Scheduler::advanceIO()
{
	m_activeIO->next();
	if (m_activeIO->timeToIO()) {//io ended
		m_waitingCpuList.push_back(m_activeIO);
		m_activeIO = 0;
		if (m_waitingIoList.size()) {
			std::vector<Process*>::iterator it=m_waitingIoList.begin();
			m_activeIO = *it;
			m_waitingIoList.erase(it);
		}
	}
}

void Scheduler::step()
{
	if (m_stopped)
		start();

	//create processes from tasks are coming now
	std::vector<Task*>::iterator it = m_data->tasks.begin();
	for (; it != m_data->tasks.end(); it++)
		if ((*it)->startTime == m_time)
			m_waitingCpuList.push_back(new Process(*it));

	//perform planning if CPU not active or quantum is run out
	if (m_activeCPU == 0 || m_qtime == 0)
		plan();

	if (m_activeCPU)
		advanceCPU();
	else {
		std::cout << m_time << " IDLE" << std::endl;
		std::cout.flush();
	}

	if (m_activeIO)
		advanceIO();

	++m_time;
	if (m_qtime >= m_step)
		m_qtime = 0;
}

/*
std::string Scheduler::step()
{
	std::string result;

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
			m_waitingCpuList.push_back(new Process(*it));

	if (m_activeIO) {
		m_activeIO->next();
		if (m_activeIO->timeToIO()) {
			m_waitingList.push_back(m_activeIO);
			wasio = m_activeIO;
			m_activeIO = 0;
//-------------------testing
			pickNext();
			qtime = 0;
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
					m_activeCPU = 0;
					pickNext();
				} else {
					m_activeIO = m_activeCPU;
					m_activeCPU = 0;
					qtime = 0;
					pickNext();
				}
			} else {
				if (m_activeIO)
					if (m_activeIO->timeToIO()) {
						m_waitingList.push_back(m_activeIO);
						m_activeIO = 0;
						qtime = 0;
					}
				if (qtime == 0 || qtime == m_data->q) { 
					if (m_activeCPU->timeToIO() == 0 && m_activeIO == 0) {
							m_activeIO = m_activeCPU;
							m_activeCPU = 0;
					}					
					pickNext();	
					qtime = 0;
				}
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
*/

void Scheduler::run()
{
	start();
	while (!m_stopped)
		step();
}


