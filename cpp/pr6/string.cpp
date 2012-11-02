#include "string.hpp"
#include <cstring>

String::String(const char* ch) :
	m_buffer(0),
	m_size(0)
{
	m_size = strlen(ch);
	m_buffer = new char[m_size];
	strncpy(m_buffer, ch, m_size);
}

String::String(const String& other) :
	m_buffer(0),
	m_size(0)
{
	m_size = other.m_size;
	m_buffer = new char[m_size];
	strncpy(m_buffer, other.m_buffer, m_size);	
}

String::~String()
{
	delete[] m_buffer;
}

size_t String::size() const
{
	return m_size;
}

char String::at(size_t i) const
{
	if (i >= m_size)//there should be exc throw
		return -1;
	return m_buffer[i];
}

char& String::at(size_t i)
{
	if (i >= m_size) //there should be exc throw
		;
	return (char&)m_buffer[i];
}

String& String::operator=(const String& other)
{
	m_size = other.m_size;
	delete[] m_buffer;
	m_buffer = new char[m_size];
	strncpy(m_buffer, other.m_buffer, m_size);
	return *this;
}

const char* String::c_str() const
{
	return m_buffer;
}

String& String::append(const String& other)
{
	char* buf = new char[m_size + other.m_size];
	strcpy(buf, m_buffer);
	strncat(buf, other.m_buffer, other.m_size);
	delete[] m_buffer;
	m_buffer = buf;
	return *this;
}

std::ostream& operator<<(std::ostream& os, String str)
{
	return os;
}
