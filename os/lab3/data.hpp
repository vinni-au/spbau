#ifndef _DATA_H_
#define _DATA_H_

#include <vector>
#include <string>
#include <iostream>

struct Task {
	std::string id;
	unsigned startTime;
	unsigned workTime;
	std::vector<std::vector<int> > ioBursts;

	Task() {}
};

struct Data {
    unsigned q1;
    unsigned q2;
	std::vector<Task*> tasks;

	~Data();

	int taskCount() const
	{ return tasks.size(); }

    bool validate();
	
	friend std::ostream& operator<<(std::ostream&, Data&);
};

#endif
