#include <iostream>
#include <fstream>
#include "stackmachine.hpp"

using namespace std;

int main(int argc, char* argv[])
{
    std::ios_base::sync_with_stdio(false);
    StackMachine* sm;

    if (argc == 2) {
        std::ifstream is(argv[1]);
        sm = StackMachine::parse(is);
        is.close();
    } else sm = StackMachine::parse();

    do {
        sm->step();
    } while (sm->is_running());
    delete sm;
    return 0;
}

