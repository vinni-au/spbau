#ifndef STRINGVECTOR_H
#define STRINGVECTOR_H

#include <string>
using namespace std;	

//! Vector of std::string
struct StringVector
{
	explicit StringVector(size_t size);
	StringVector(StringVector& other);
	~StringVector();

	string& at(size_t i);
	size_t size() const;
	size_t capacity() const;

	void resize(size_t newsize);
	void reserve(size_t newcapacity);
	
	void add(string s);

	StringVector& operator=(StringVector& other);
	friend ostream& operator<<(ostream& os, StringVector& v);

private:
	char* m_data;
	size_t m_size;
	size_t m_capacity;

	void extend();
};


#endif //STRINGVECTOR_H
