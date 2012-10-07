#include "stringvector.hpp"
#include <cstring>

StringVector::StringVector(size_t size) :
	m_size(size),
	m_capacity(size*2)
{
	m_data = new char[m_capacity*sizeof(string)];
}

StringVector::StringVector(const StringVector& other) :
	m_size(other.m_size),
	m_capacity(other.m_capacity)
{
	m_data = new char[m_capacity*sizeof(string)];
	memcpy(m_data, other.m_data, m_size*sizeof(string));
}

StringVector::~StringVector()
{
	delete[] m_data;
}

string* StringVector::at(size_t i)
{
	return reinterpret_cast<string*>(m_data+i*sizeof(string));
}

size_t StringVector::size() const
{
	return m_size;
}

size_t StringVector::capacity() const
{
	return m_capacity;
}

void StringVector::resize(size_t newsize)
{
}

void StringVector::reserve(size_t newcapacity)
{
}

void StringVector::add(string s)
{
}

StringVector& StringVector::operator=(const StringVector& other)
{
}
