#include "data.hpp"

std::ostream& operator<<(std::ostream& os, Data& data)
{
    os << "Quantum 1 = " << data.q1 << std::endl;
    os << "Quantum 2 = " << data.q2 << std::endl;
    std::vector<Task*>::iterator it = data.tasks.begin();
	for (; it != data.tasks.end(); it++) {
		os << " Task: " << (*it)->id << std::endl;
	}
}

bool Data::validate()
{
    std::vector<Task*>::iterator it = tasks.begin();
    if (q1 >= q2)
        return false;
    for (; it != tasks.end(); it++) {
        Task* current = *it;
        if (current) {
            if (current->ioBursts.size())
                if (current->ioBursts[current->ioBursts.size()-1][1] >= current->workTime)
                    return false;
            if (current->ioBursts.size() >= 2) {
                for (int i = 1; i < current->ioBursts.size(); ++i) {
                    if (current->ioBursts[i][0] < current->ioBursts[i-1][1])
                        return false;
                }
            }
        }
    }
    return true;
}

Data::~Data() 
{
	std::vector<Task*>::iterator it = tasks.begin();
	for (; it != tasks.end(); it++)
		delete *it;
}
