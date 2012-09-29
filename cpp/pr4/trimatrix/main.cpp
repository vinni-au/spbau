#include <iostream>


class TriMatrix {
public:
	TriMatrix(int size = 1, int value = 0);
	~TriMatrix();

	void print(std::ostream& s);
	TriMatrix* merge(TriMatrix b);
private:
	int **m_data;
	int *m_sizes;
	int m_rowcount;
};

TriMatrix::TriMatrix(int size, int value)
{
	m_sizes = new int[size];
	m_data = new int*[size];
	m_rowcount = size;
	for (int i = 0; i < size; ++i) {
		m_sizes[i] = size-i;
		m_data[i] = new int[size-i];
		for (int j = 0; j < size-i; ++j)
			*(*(m_data+i)+j) = value;
	}
}

TriMatrix::~TriMatrix()
{
	for (int i = 0; i < m_rowcount; ++i)
		delete[] m_data[i];
	delete[] m_data;
	delete[] m_sizes;
}


void TriMatrix::print(std::ostream& s)
{
	for (int i = 0; i < m_rowcount; ++i) {
		for (int j = 0; j < m_sizes[i]; ++j)
			s << *(*(m_data+i)+j) << " ";
		s << std::endl;
	}
	s << std::endl;
}

TriMatrix* TriMatrix::merge(TriMatrix b)
{
	if (m_rowcount == b.m_rowcount) {
		int i, j;
		int **data = new int*[m_rowcount];
		for (i = 0; i < m_rowcount; ++i) {
			data[i] = new int[m_sizes[i]*2];
			for (j = 0; j < m_sizes[i]; ++j)
				*(*(data+i)+j) = *(*(m_data+i)+j);
			for (j = m_sizes[i]; j < m_sizes[i]*2; ++j)
				*(*(data+i)+j) = *(*(b.m_data+i)+j-m_sizes[i]);		
		}
		for (i = 0; i < m_rowcount; ++i)
			delete[] m_data[i];
		delete[] m_data;
		m_data = data;
		for (i = 0; i < m_rowcount; ++i)
			m_sizes[i] *= 2;
	}
	return this;
}


int main()
{
	TriMatrix* a = new TriMatrix(5, 0);
	TriMatrix* b = new TriMatrix(5, 4);
	a->print(std::cout);
	b->print(std::cout);
	TriMatrix* c = a->merge(*b);
	c->print(std::cout);
	return 0;
}
