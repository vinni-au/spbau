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
#include <stdlib.h>
 
Ash::Ash()
{	}
 
void Ash::run(bool showinfo /* = true */)
{
	if (showinfo) {
		std::cout << "Ash command shell v0.1" << std::endl << std::endl;
		showHelp();
	}	

	std::string userCommand;
	for (;;) {
		std::cout << "> ";
		std::cin >> userCommand;
		if (userCommand == "exit")
			return;
		if (userCommand == "help")
			showHelp();
		if (userCommand == "ls")
			system("ls");
		if (userCommand == "pwd")
			system("pwd");
		if (userCommand == "ps")
			system("ps");
		if (userCommand == "kill") {
			std::cin >> userCommand;
			
		}
		system(userCommand.c_str());
	}
}

void Ash::showHelp()
{
	std::cout << "Possible commands: " << std::endl;
	std::cout << "help" << std::endl;
	std::cout << "<filename of executable>" << std::endl;
	std::cout << "ls" << std::endl;
	std::cout << "pwd" << std::endl;
	std::cout << "ps" << std::endl;
	std::cout << "kill <PID>" << std::endl;
	std::cout << "exit" << std::endl << std::endl;
}
