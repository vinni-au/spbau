#include "data.hpp"

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
