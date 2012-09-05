/**************************
 * Operating systems
 * Lab. No 1. Command Shell
 * Ash - Anton's shell
 *
 * (c) Anton Storozhev, 2012
 * antonstorozhev@gmail.com
 ***************************/
#include "ash.hpp"
#include <iostream>
 
Ash::Ash()
{
}
 
void Ash::run()
{
	std::cout << "Ash command shell v0.1" << std::endl << std::endl;
	std::cout << "Possible commands: " << std::endl;
	std::cout << "<filename of executable>" << std::endl;
	std::cout << "ls" << std::endl;
	std::cout << "pwd" << std::endl;
	std::cout << "ps" << std::endl;
	std::cout << "kill <PID>" << std::endl;
	std::cout << "exit" << std::endl << std::endl;


	std::string userCommand;
	for (;;) {
		std::cout << "> ";
		std::cin >> userCommand;
		if (userCommand == "exit")
			return;
	}
}