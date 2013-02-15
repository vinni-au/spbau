#include <iostream>
#include "stackmachine.hpp"

using namespace std;

int main(int argc, char* argv[])
{
    std::ios_base::sync_with_stdio(false);
    StackMachine* sm = StackMachine::parse();
    do {
        sm->step();
    } while (sm->is_running());
    delete sm;
    return 0;
}

