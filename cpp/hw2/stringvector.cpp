#include <cstring>
#include "stringvector.hpp"
#include <iostream>

StringVector::StringVector(size_t size) :
	m_size(size),
	m_capacity(size <= 0 ? 1 : 2*size),
	m_data(0)
{
	m_data = new char[m_capacity*sizeof(string)];
	for (int i = 0; i < size; ++i)
		new (m_data + i*sizeof(string)) string;
}

StringVector::StringVector(StringVector& other) :
	m_size(other.m_size),
	m_capacity(other.m_capacity),
	m_data(0)
{
	m_data = new char[m_capacity*sizeof(string)];
	memcpy(m_data, other.m_data, m_size*sizeof(string));
	for (int i = 0; i < m_size; ++i)
		at(i) = other.at(i);
}

StringVector::~StringVector()
{
	delete[] m_data;
}

string& StringVector::at(size_t i)
{
	return *reinterpret_cast<string*>(m_data+i*sizeof(string));
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
	while (newsize >= m_capacity) 
		extend();
	if (newsize < m_size)
		for (int i = m_size - 1; i >= newsize; --i)
			at(i).~string();
	else 
		for (int i = m_size; i < newsize; ++i)
			add(string());
	m_size = newsize;
}

void StringVector::reserve(size_t newcapacity)
{
	if (newcapacity > m_capacity) {
		char* tmp = new char[newcapacity*sizeof(string)];
		memcpy(tmp, m_data, m_size*sizeof(string));
		delete[] m_data;
		m_data = tmp;
		m_capacity = newcapacity;
	}
}

void StringVector::add(string s)
{
	if (m_size == m_capacity)
		extend();

	new (m_data + m_size++*sizeof(string)) string(s);
}

StringVector& StringVector::operator=(StringVector& other)
{
	delete[] m_data;
	m_data = new char[other.m_capacity*sizeof(string)];
	m_capacity = other.m_capacity;
	for (int i = 0; i < other.m_size; ++i)
		add(other.at(i));
}

void StringVector::extend()
{
	char* tmp = new char[2*m_capacity*sizeof(string)];
	memcpy(tmp, m_data, m_size*sizeof(string));
	delete m_data;
	m_data = tmp;
	m_capacity *= 2;
}

ostream& operator<<(ostream& os, StringVector& v)
{
	size_t size = v.size();
	for (size_t i = 0; i < size; ++i)
		os << v.at(i) << endl;
}
