#include "stackmachine.hpp"

StackMachine::StackMachine() :
    m_running(false),
    m_ip(0)
{
}

void StackMachine::step(std::istream &in, std::ostream &out) {
    SMInstruction& current = m_program.at(m_ip);

    switch (current.op) {
    case SMInstruction::E:
        m_running = false;
        break;
    case SMInstruction::C:
        m_stack.push(current.arg);
        break;
    case SMInstruction::L: {
        int_t value = m_stack.top();
        m_stack.pop();
        m_variables[m_ident_ind[current.ident]] = value;
        break;
    }
    case SMInstruction::S: {
        int_t value = m_variables[m_ident_ind[current.ident]];
        m_stack.push(value);
        break;
    }
    case SMInstruction::R: {
        int_t value;
        in >> value;
        m_stack.push(value);
        break;
    }
    case SMInstruction::W: {
        int_t value = m_stack.top();
        m_stack.pop();
        out << value;
        break;
    }
    }

    if (current.op & 0b10000000) {
        int_t value2 = m_stack.top();
        m_stack.pop();
        int_t value1 = m_stack.top();
        m_stack.pop();
        int_t result = 0;
        switch(current.op) {
        case SMInstruction::Plus:
            result = value1 + value2;
            break;
        case SMInstruction::Minus:
            result = value1 - value2;
            break;
        case SMInstruction::Mult:
            result = value1 * value2;
        case SMInstruction::Div:
            result = value1 / value2;
            break;
        case SMInstruction::Mod:
            result = value1 % value2;
            break;
        case SMInstruction::EqEq:
            result = value1 == value2 ? 1 : 0;
            break;
        case SMInstruction::NotEq:
            result = value1 != value2 ? 1 : 0;
            break;
        case SMInstruction::Lt:
            result = value1 < value2 ? 1 : 0;
            break;
        case SMInstruction::LtEq:
            result = value1 <= value2 ? 1 : 0;
            break;
        case SMInstruction::Gt:
            result = value1 > value2 ? 1 : 0;
            break;
        case SMInstruction::GtEq:
            result = value1 >= value2 ? 1 : 0;
            break;
        case SMInstruction::Or:
            result = value1 || value2 ? 1 : 0;
            break;
        case SMInstruction::And:
            result = value1 && value2 ? 1 : 0;
            break;
        }
        m_stack.push(result);
    }

    ++m_ip;

    switch(current.op) {
    case SMInstruction::J:
        m_ip = current.arg;
        break;
    case SMInstruction::JT: {
        int_t value = m_stack.top();
        m_stack.pop();
        if (value != 0)
            m_ip = current.arg;
        break;
    }
    case SMInstruction::JF: {
        int_t value = m_stack.top();
        m_stack.pop();
        if (value == 0)
            m_ip = current.arg;
        break;
    }
    }
}
