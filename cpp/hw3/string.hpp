#include <iostream>

struct String {
	String(const char*);
	String(const String& other);
	~String();

	String& append(const String& other);

	char at(size_t) const;
	char& at(size_t);

	size_t size() const;

    String& operator=(const String& other);

    String* substring(int left, int right);

	const char* c_str() const;

	friend std::ostream& operator<<(std::ostream& os, String str);
private:
    char* m_buffer;
    int* m_refcount;
	size_t m_size;
	void swap(String&);

    void detach();
    void attach(const String* other);
};
