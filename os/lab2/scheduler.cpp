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
#ifdef DEBUG
    std::cout << "Planning" << std::endl;
#endif
	if (m_activeCPU) {
		if (m_activeCPU->timeToIO()) 
			m_waitingCpuList.push_back(m_activeCPU);
		else 
			m_activeIO = m_activeCPU;
		m_activeCPU = 0;
	}
	if (m_waitingCpuList.size()) {
#ifdef DEBUG
        std::cout << " Candidates are: " << std::endl;
        std::vector<Process*>::iterator i = m_waitingCpuList.begin();
        for (; i != m_waitingCpuList.end(); i++)
            std::cout << "    " << (*i)->m_task->id
                      << " time " << (*i)->m_time
                      << " time to end " << (*i)->timeToEnd()
                      << " time to io " << (*i)->timeToIO() << std::endl;
        std::cout.flush();
#endif
        unsigned min = -1;
		std::vector<Process*>::iterator it = m_waitingCpuList.begin();
		std::vector<Process*>::iterator cand;
        std::vector<Process*> candids;
        std::vector<Process*> candidates;
        if (m_waitingCpuList.size() == 1)
            cand = m_waitingCpuList.begin();
        else {
            for (; it != m_waitingCpuList.end(); it++) {
                if ((*it)->timeToIO() < m_step) {
                    if ((*it)->timeToIO() < min) {
                        cand = it;
                        min = (*it)->timeToIO();
                        candids.clear();
                        candids.push_back(*it);
                    } else if ((*it)->timeToIO() == min) {
                        candids.push_back(*it);
                    }
                }
                if ((*it)->timeToEnd() < m_step) {
                    if ((*it)->timeToEnd() < min) {
                        cand = it;
                        min = (*it)->timeToEnd();
                        candids.clear();
                        candids.push_back(*it);
                    } else if ((*it)->timeToEnd() == min) {
                        candids.push_back(*it);
                    }
                }
            }
            if (!candids.size()) {
                min = -1;
                it = m_waitingCpuList.begin();
                for (; it != m_waitingCpuList.end(); it++) {
                        if ((*it)->timeToEnd() < min) {
                            cand = it;
                            min = (*it)->timeToEnd();
                            candidates.clear();
                            candidates.push_back(*it);
                        } else if ((*it)->timeToEnd() == min)  {
                            candidates.push_back(*it);
                        }
                }
            } else if (candids.size() == 1)
                cand = candids.begin();
            else candidates = candids;
            if (candidates.size() > 1) {
#ifdef DEBUG
                std::cout << " Distinguish from " << std::endl;
                i = candidates.begin();
                for (; i != candidates.end(); i++)
                    std::cout << "    " << (*i)->m_task->id
                              << " time " << (*i)->m_time
                              << " time to end " << (*i)->timeToEnd()
                              << " time to io " << (*i)->timeToIO() << std::endl;
                std::cout.flush();
#endif
                unsigned max = 0;
                std::vector<Process*>::iterator it1 = candidates.begin();
                for (; it1 != candidates.end(); it1++) {
                    if ((*it1)->m_ptime > max) {
                        cand = it1;
                        max = (*it1)->m_ptime;
                    }
                }
            }
        }
        m_activeCPU = *cand;
        it = m_waitingCpuList.begin();
        for (; it != m_waitingCpuList.end(); it++)
            if ((*it) == m_activeCPU) {
                m_waitingCpuList.erase(it);
                break;
            }
	} else m_activeCPU = 0;
#ifdef DEBUG
    if (m_activeCPU)
        std::cout << "Planned " << m_activeCPU->m_task->id << std::endl;
    else std::cout << "Planned none" << std::endl;
#endif
}

void Scheduler::advanceCPU()
{
#ifdef DEBUG
    std::cout << "Advance CPU for " << m_activeCPU->m_task->id
              << " lenght " << m_activeCPU->m_task->workTime
              << " time " << m_activeCPU->m_time << std::endl;
#endif
	m_activeCPU->next();
	++m_qtime;
#ifdef DEBUG
    std::cout << " Time to end = " << m_activeCPU->timeToEnd() << std::endl;
    std::cout << " Time to io = " << m_activeCPU->timeToIO() << std::endl;
#endif
    std::cout << m_time << " " << m_activeCPU->m_task->id << std::endl;
	std::cout.flush();
	if (m_activeCPU->done() || m_activeCPU->timeToIO() == 0)
		m_qtime = 0;
    if (m_activeCPU->done()) {
		++m_pdone;
        delete m_activeCPU;
        m_activeCPU = 0;
        if (m_pdone == m_data->tasks.size()) {
            m_stopped = true;
            std::cout << m_time + 1 << " IDLE" << std::endl;
        }
    }
}

void Scheduler::advanceIO()
{
    m_activeIO->next(false);
#ifdef DEBUG
    std::cout << "Advance IO for " << m_activeIO->m_task->id
              << " time " << m_activeIO->m_time << std::endl;
    std::cout << " Time to io = " << m_activeIO->timeToIO() << std::endl;
#endif
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

#ifdef DEBUG
    std::cout << std::endl << "----time = " << m_time << std::endl;
#endif

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
#ifdef DEBUG
    if (m_time > 50)
        m_stopped = true;
#endif
}

void Scheduler::run()
{
	start();
	while (!m_stopped)
		step();
}

