#include "stackmachine.hpp"

StackMachine::StackMachine(std::istream &in) :
    m_running(false),
    m_ip(0)
{
    do {
        std::string curstr;
        std::getline(in, curstr);
        if (curstr.size() > 0) {
            SMInstruction instr = SMInstruction::parse(curstr);
            m_program.push_back(instr);
            if (instr.op == SMInstruction::E)
                break;
            if (instr.op == SMInstruction::Label) {
                m_labels.push_back(m_program.size() - 1);
                m_label_ind[instr.ident] = m_labels.size() - 1;
            }
            if (instr.op == SMInstruction::L || instr.op == SMInstruction::S) {
                m_variables.push_back(0);
                m_ident_ind[instr.ident] = m_variables.size() - 1;
            }
        }
    } while ((in.rdstate() & std::ios_base::eofbit) == 0);
    m_running = true;
}

void StackMachine::step(std::istream &in, std::ostream &out, std::ostream &err) {
    SMInstruction& current = m_program.at(m_ip);

    switch (current.op) {
    case SMInstruction::E:
        m_running = false;
        break;
    case SMInstruction::C:
        m_stack.push(current.arg);
        break;
    case SMInstruction::L: {
        int_t value = pop(err);
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
        int_t value = pop(err);
        out << value << std::endl;
        break;
    }
    }

    if (current.op & 0b10000000) {
        int_t value2 = pop(err);
        int_t value1 = pop(err);
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
            break;
        case SMInstruction::Div:
            if (value2 == 0) {
                m_running = false;
                err << "ERROR! Division by zero!" << std::endl;
            } else
                result = value1 / value2;
            break;
        case SMInstruction::Mod:
            if (value2 == 0) {
                m_running = false;
                err << "ERROR! Division by zero!" << std::endl;
            } else
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
        int_t value = pop(err);
        if (value != 0)
            m_ip = current.arg;
        break;
    }
    case SMInstruction::JF: {
        int_t value = pop(err);
        if (value == 0)
            m_ip = current.arg;
        break;
    }
    }
}

inline int_t StackMachine::pop(std::ostream &err) {
    if (m_stack.size() == 0) {
        m_running = false;
        err << "ERROR: cannot pop()" << std::endl;
    }
    int_t result = m_stack.top();
    m_stack.pop();
    return result;
}
