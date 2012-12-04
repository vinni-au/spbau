#ifndef GLOBAL_H
#define GLOBAL_H

#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <cstring>

typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned char ubyte;

std::string fromNum(int num)
{
	std::stringstream ss;
	ss << num;
	std::string res;
	ss >> res;
	return res;
}

struct FSInfo 
{
	int bcount;
	int bsize;

	FSInfo(int bcount, int bsize) :
		bcount(bcount),
		bsize(bsize)
	{}

	static FSInfo* readFromFile(std::string dirname) {
		std::ifstream f;
		dirname.append("/");
		f.open((dirname + "status").c_str());
		if (f.is_open()) {
			int N, size;
			f >> N >> size;
			f.close();
			if (N <= 0 || size < 1024)
				return 0;

			return new FSInfo(N, size);
		} else return 0;
	}
};

struct sblock
{
	//number of blocks total
	ushort bcount;
	ubyte* map;
	//size of block
	uint size;

	sblock(int num, uint size) : size(size), bcount(num) {
		int bytescount = num%8 ? num/8 + 1 : num/8;
		if (bytescount < size)
			bytescount = size;
		map = new ubyte[bytescount];
		memset(map, 0, bytescount);
	}

	void alloc(ushort block) {
		ushort wordnum = block / 8;
		ubyte mask = 1 << block % 8;
		map[wordnum] = map[wordnum] | mask;
	}

	void free(ushort block) {
		ushort wordnum = block / 8;
		ubyte mask = 1 << block % 8;
		map[wordnum] = map[wordnum] & (~mask);
	}

	static sblock* loadFromFile(std::string dirname) {
		FSInfo* fsi = FSInfo::readFromFile(dirname);
		if (fsi) {
			sblock* result = new sblock(fsi->bcount, fsi->bsize);
			ushort sbc = result->sbcount();
			for (int i = 0; i < sbc; ++i) {
				std::ifstream file;
				file.open((dirname + "/" + fromNum(i)).c_str(), std::ios::in | std::ios::binary);
				if (file.is_open()) {
					char* buf = new char[fsi->bsize];
					file.read(buf, fsi->bsize);
					memcpy(result->map + i*fsi->bsize, buf, fsi->bsize);
					file.close();
					delete[] buf;
				} 
        else {
					delete result;
					return 0;
				}
			}
		} else 
			return 0;
	}

	inline ushort numbytes() const {
		ushort num = bcount%8 ? bcount/8 + 1 : bcount/8;
		return num;
	}

	inline ushort sbcount() const {
		ushort sbcount = numbytes()%size ? numbytes()/size + 1 : numbytes()/size;
		return sbcount;
		
	}

	std::vector<ubyte*> data() {
		std::vector<ubyte*> res;
		int offset = 0;
		ushort sbc = sbcount();
		for (ushort i = 0; i < sbc; i++) {
			ubyte* cur = new ubyte[size];
			memset(cur, 0, size);
			if (i == sbc - 1)
				memcpy(cur, map+offset, numbytes() - i*size);
			else memcpy(cur, map+offset, size);
			offset += size;
			res.push_back(cur);
		}
		return res;
	}
};

struct dblock {
	ubyte name[10];
	ushort next;
	ushort filecount;
	ubyte* d;

	dblock (std::string name, uint size) : next(0), filecount(0) {
		d = new ubyte[size];
		memset(d, 0, size);
	}

	ubyte* data() {
		return d;
	}
};

#endif //GLOBAL_H
