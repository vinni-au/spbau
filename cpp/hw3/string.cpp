#include "string.hpp"
#include <cstring>
#include <assert.h>

String::String(const char* ch) :
    m_buffer(0),
    m_size(0),
    m_refcount(new int(0))
{
#ifdef DEBUG
    static int count = 0;
    cout << "ctor const char* #" << count++ << endl;
#endif
    m_size = strlen(ch);
    m_buffer = new char[m_size];
    strncpy(m_buffer, ch, m_size);
}

String::String(const String& other) :
    m_buffer(0),
    m_size(0)
{
#ifdef DEBUG
    static int count = 0;
    cout << "ctor copy #" << count++ << endl;
#endif
    attach(&other);
}

String::~String()
{
#ifdef DEBUG
    static int count = 0;
    cout << "dtor #" << count++ << endl;
#endif
    (*m_refcount)--;
    if ((*m_refcount) <= 0)
#ifdef DEBUG
    {
        cout << "deleting buffer \"" << (*this) << "\" #" <<  count - 1 << endl;
#endif
        delete[] m_buffer;
#ifdef DEBUG
    }
#endif
}

void String::swap(String &b)
{
#ifdef DEBUG
    static int count = 0;
    cout << "swap #" << count++ << endl;
#endif
    std::swap(m_buffer, b.m_buffer);
    std::swap(m_refcount, b.m_refcount);
    std::swap(m_size, b.m_size);
}

size_t String::size() const
{
    return m_size;
}

char String::at(size_t i) const
{
    assert(i < m_size); //there should be exc throw
    return m_buffer[i];
}

char& String::at(size_t i)
{
    assert(i < m_size); //there should be exc throw
    detach();
    return (char&)m_buffer[i];
}

String& String::operator=(const String& other)
{
    if (this != &other)
        String(other).swap(*this);
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
#ifdef DEBUG
    cout << "detach \"" << (*this) << "\"" << endl;
#endif
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

std::ostream& operator<<(std::ostream& os, const String& str)
{
    for (size_t i = 0; i < str.size(); ++i)
        os.put(str.m_buffer[i]);
    return os;
}
