#include "string.hpp"
#include <cstring>

String::String(const char* ch) :
	m_buffer(0),
	m_size(0)
{
    m_refcount = new int;
    (*m_refcount) = 0;
	m_size = strlen(ch);
	m_buffer = new char[m_size];
	strncpy(m_buffer, ch, m_size);
}

String::String(const String& other) :
	m_buffer(0),
	m_size(0)
{
    attach(&other);
}

String::~String()
{
    (*m_refcount)--;
    if ((*m_refcount) < 0)
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
    detach();
	if (i >= m_size) //there should be exc throw
		;
	return (char&)m_buffer[i];
}

String& String::operator=(const String& other)
{
    delete[] m_buffer;
    //do attach
    m_refcount = other.m_refcount;
    (*m_refcount)++;
    m_buffer = other.m_buffer;
	m_size = other.m_size;
	return *this;
}

String* String::substring(int left, int right) //[left, right]
{
    if (right < left)
        return new String("");

    char* buf = new char[right - left + 2];
    strncpy(buf, m_buffer + left, right - left + 1);
    buf[right - left + 1] = '\0';
    String* res = new String(buf);
    delete[] buf;
    return res;
}

const char* String::c_str() const
{
	return m_buffer;
}

String& String::append(const String& other)
{
    detach();
    char* buf = new char[m_size + other.m_size];
    strncpy(buf, m_buffer, m_size);
    strncpy(buf + m_size, other.m_buffer, other.m_size);
    delete[] m_buffer;
	m_buffer = buf;
	return *this;
}

void String::detach()
{
    if ((*m_refcount) > 0) {
        (*m_refcount--);
        char* buffer = new char[m_size];
        strncpy(buffer, m_buffer, m_size);
        m_buffer = buffer;
    }
}

void String::attach(const String *other)
{
    m_buffer = other->m_buffer;
    m_refcount = other->m_refcount;
    (*m_refcount)++;
    m_size = other->m_size;
}

std::ostream& operator<<(std::ostream& os, String str)
{
	return os;
}
