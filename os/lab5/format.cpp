#include <iostream>

#include "global.h"

int main(int argc, char* argv[])
{
	if (argc != 2)
		return -1;

	std::string dirname = argv[1];

	FSInfo* info = FSInfo::readFromFile(dirname);
	if (info) {
		sblock sb(info->bcount, info->bsize);
		ushort rootblock = sb.sbcount();
		for (ushort i = 0; i < rootblock; ++i)
			sb.alloc(i);

		std::vector<ubyte*> data = sb.data();
		size_t size = data.size();
		for (int i = 0; i < size; ++i) {
			std::ofstream file;
			file.open((dirname + "/" + fromNum(i)).c_str(), std::ios::out | std::ios::binary);
			if (file.is_open()) {
				file.write((char*)data[i], info->bsize);
				delete data[i];
			} else return -1;
		}
	} else return -1;

	return 0;
}
