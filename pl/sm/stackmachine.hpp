#ifndef STACKMACHINE_HPP
#define STACKMACHINE_HPP

#include <vector>
#include <stack>
#include <string>
#include <map>
#include <istream>
#include <ostream>
#include <iostream>
#include "global.h"
#include "sminstruction.hpp"

class StackMachine
{
public:
    StackMachine(std::istream& in = std::cin, std::ostream& err = std::cerr);

    bool is_running() const {
        return m_running;
    }

    void step(std::istream& in = std::cin, std::ostream& out = std::cout, std::ostream& err = std::cerr);
private:
    std::stack<int_t> m_stack;
    std::vector<int_t> m_variables;
    std::vector<size_t> m_labels;
    std::map<std::string, size_t> m_ident_ind;
    std::map<std::string, size_t> m_label_ind;
    std::vector<SMInstruction> m_program;

    size_t m_ip;
    bool m_running;

    int_t pop(std::ostream& err = std::cerr);
 };

#endif // STACKMACHINE_HPP
