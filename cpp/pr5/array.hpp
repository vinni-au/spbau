#ifndef ARRAY_H
#define ARRAY_H

#include <iostream>

struct Array {
	int get(size_t i);
	void set(int val, size_t i);
	void add(int val);
	size_t size();
	Array();
	Array(Array& src);
	Array& operator=(Array& src);
	~Array();

private:
	size_t m_realsize;
	size_t m_size;
	int* m_data;
	void extend();
};

#endif
