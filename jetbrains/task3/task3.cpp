#include <iostream>
#include <fstream>

int main()
{
	std::fstream file("input.txt", std::fstream::in);

	int s = 0;
	int a = 0;
	while (!file.eof()) {
		file >> a;
		if (file.eof())
			break;
		s ^= a;		
	}

	std::cout << "Number is " << s << std::endl;
	
	file.close();
	return 0;
}
