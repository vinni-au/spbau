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
#include <sstream>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
 
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
		else if (userCommand == "ls")
			runCmd("/bin/ls");
		else if (userCommand == "pwd")
			runCmd("/bin/pwd");
		else if (userCommand == "ps")
			runCmd("/bin/ps");
		else if (userCommand == "kill") {
			std::cin >> userCommand;
			int pid = atoi(userCommand.c_str());
			std::cin >> userCommand;
			int sig = atoi(userCommand.c_str());			
			if (0 != pid) {
				if(-1 == kill(pid, sig))
					std::cout << "Error: can't send signal " << sig << " to process " << pid << std::endl;
			} else std::cout << "Error: Invalid PID. Must be a positive number" << std::endl;
		} else runCmd(userCommand.c_str());
	}
}

void Ash::showHelp()
{
	std::cout << "Possible commands: " << std::endl;
	std::cout << "help" << std::endl;
	std::cout << "<path to executable>" << std::endl;
	std::cout << "ls" << std::endl;
	std::cout << "pwd" << std::endl;
	std::cout << "ps" << std::endl;
	std::cout << "kill <PID> <SIG>" << std::endl;
	std::cout << "exit" << std::endl << std::endl;
}

void Ash::runCmd(const char* cmd, const char* args)
{
	int code = fork();
	if (0 == code) {
			if (-1 == execl(cmd, args, (char*)NULL))
				std::cout << "Error: unable to execute command - exec error" << std::endl;
			exit(0);
	} if (-1 == code) {
		std::cout << "Error: unable to execute command - can't fork" << std::endl;
	} else {
		waitpid(code, NULL, 0);	
	}
}
