#include "SFile.hpp"

int SFile::m_pools = 0;
SFile* SFile::m_pool[1024] = {0};

SFile::SFile(std::string s) :
	m_handle(0),
	m_refc(0),
	m_name(s),
	m_index(-1)
{
	int index = find(s);
	if (index >= 0) {
		m_handle = m_pool[index]->m_handle;
		m_pool[index]->m_refc++;
		m_index = index;
	} else {
		m_handle = fopen(s.c_str(), "w");
		m_index = m_pools;
		m_pool[m_pools++] = this;		
	}
}

SFile::~SFile()
{
	m_pool[m_index]->m_refc--;
	if (m_pool[m_index]->m_refc < 0) {
		fclose(m_handle);
		m_pool[m_index] = 0;
		for (int i = m_index; i < m_pools - 1; ++i) {
			m_pool[i] = m_pool[i+1];
		}
		m_pools--;
	}
}

int SFile::find(std::string s)
{
	for (int i = 0; i < SFile::m_pools; ++i)
 		if (SFile::m_pool[i]->m_name == s)
			return i;
	
	return -1;
}

