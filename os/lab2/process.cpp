#include "data.hpp"
#include "process.hpp"

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
	if (!size)
		return -1;
	if (m_time >= m_task->ioBursts[size-1][1])
		return -1;
	unsigned cur;
	unsigned min = static_cast<unsigned>(-1);
	std::vector<std::vector<int> >::iterator it = m_task->ioBursts.begin();	
	for (; it != m_task->ioBursts.end(); it++) {
		if (m_time == (*it)[0])
			return 0;
		if (m_time <= (*it)[1] && m_time > (*it)[0])
			return 0;
		cur = (*it)[0] - m_time;
		if (cur < min)
			min = cur;
	}
	return min;
}

