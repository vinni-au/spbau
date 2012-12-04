#include <iostream>
#include <fstream>
#include <cstring>
#include "../global.h"

int main(int argc, char* argv[])
{
	if (argc != 2) 
		return -1;

	std::string dirname(argv[1]);

	FSInfo* info = FSInfo::readFromFile(dirname);
	if (info) {
		ubyte* buffer = new ubyte[info->bsize];
		memset(buffer, 0, info->bsize);

		for (int i = 0; i < info->bcount; ++i) {
			std::ofstream file;
			file.open((dirname + "/" + fromNum(i)).c_str(), std::ios::out | std::ios::binary);
			if (file.is_open()) {
				file.write((const char*)buffer, info->bsize);
			} else return -1;
			file.close();
		}
	} else return -1;
	return 0;
}
