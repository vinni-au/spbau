#ifndef _SFILE_HPP
#define _SFILE_HPP

#include <cstdio>
#include <string>

struct SFile {
	SFile(std::string filename);
	~SFile();

	bool write(std::string s) {
		return fprintf(m_handle, "%s", s.c_str());
	}

private:
	FILE* m_handle;
	std::string m_name;
	int m_refc;
	static SFile* m_pool[1024];
	static int m_pools;
	int m_index;

	int find(std::string s);
};


#endif //_SFILE_HPP
