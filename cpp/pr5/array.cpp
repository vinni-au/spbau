#include "array.hpp"
#include <cstring>

Array::Array()
{
	m_realsize = 1;
	m_size = 0;
	m_data = new int[m_realsize];
}

Array::~Array()
{
	delete[] m_data;
}

Array::Array(Array& src)
{
	m_realsize = src.m_realsize;
	m_size = src.m_size;
	m_data = new int[m_realsize];
	memcpy(m_data, src.m_data, m_realsize*sizeof(int));	
}

size_t Array::size()
{
	return m_size;
}

int Array::get(size_t i)
{
	if (i < m_size)
		return m_data[i];
	return 0;
}

void Array::set(int val, size_t i)
{
	if (i >= m_realsize) {
	//extend or throw error
	}
	m_data[i] = val;
	if (i >= m_size)
		m_size = i + 1;
}

void Array::add(int val)
{
	if (m_size >= m_realsize)
		extend();
	m_size++;
	m_data[m_size - 1] = val;
}

void Array::extend()
{
	int* tmp = new int[m_realsize*2];
	memcpy(tmp, m_data, m_realsize*sizeof(int));
	delete[] m_data;
	m_data = tmp;
	m_realsize *= 2;
}

Array& Array::operator=(Array& src)
{
	if (m_realsize < src.m_realsize) {
		delete[] m_data;
		m_data = new int[src.m_realsize];
		m_realsize = src.m_realsize;		
	}
	m_size = src.m_size;
	memcpy(m_data, src.m_data, m_size*sizeof(int));
}
