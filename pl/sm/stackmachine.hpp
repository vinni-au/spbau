#ifndef STACKMACHINE_HPP
#define STACKMACHINE_HPP

#include <vector>
#include <stack>
#include <string>
#include <map>
#include <istream>
#include <ostream>
#include "global.h"
#include "sminstruction.hpp"

class StackMachine
{
public:
    StackMachine();
private:
    std::stack<int_t> m_stack;
    std::vector<int_t> m_variables;
    std::vector<size_t> m_labels;
    std::map<std::string, size_t> m_ident_ind;
    std::map<std::string, size_t> m_label_ind;
    std::vector<SMInstruction> m_program;

    size_t m_ip;
    bool m_running;

    void step(std::istream& in, std::ostream& out);
 };

#endif // STACKMACHINE_HPP
