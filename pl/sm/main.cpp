#include <iostream>
#include "stackmachine.hpp"

using namespace std;

int main(int argc, char* argv[])
{
    std::ios_base::sync_with_stdio(false);
    StackMachine sm(std::cin);
    do {
        sm.step();
    } while (sm.is_running());
    return 0;
}

