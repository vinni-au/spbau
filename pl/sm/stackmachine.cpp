#include "stackmachine.hpp"

StackMachine::StackMachine() :
    m_running(false),
    m_ip(0)
{
}

StackMachine* StackMachine::parse(std::istream &in, std::ostream &err) {
    StackMachine* result = new StackMachine;
    do {
        std::string curstr;
        std::getline(in, curstr);
        if (curstr.size() > 0) {
            SMInstruction instr = SMInstruction::parse(curstr);
            result->m_program.push_back(instr);
            if (instr.op == SMInstruction::E)
                break;
            if (instr.op == SMInstruction::Label) {
                if (result->m_label_ind.find(instr.ident) == result->m_label_ind.end()) {
                    result->m_labels.push_back(result->m_program.size() - 1);
                    result->m_label_ind[instr.ident] = result->m_labels.size() - 1;
                } else {
                    std::cerr << "ERROR: label already defined" << std::endl;
                }
            }
            if (instr.op == SMInstruction::L || instr.op == SMInstruction::S) {
                if (result->m_ident_ind.find(instr.ident) == result->m_ident_ind.end()) {
                    result->m_variables.push_back(0);
                    result->m_ident_ind[instr.ident] = result->m_variables.size() - 1;
                }
            }
        }
    } while ((in.rdstate() & std::ios_base::eofbit) == 0);
    result->m_running = true;
    return result;
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
        m_ip = ip_from_labels(current);
        break;
    case SMInstruction::JT: {
        int_t value = pop(err);
        if (value != 0)
            m_ip = ip_from_labels(current);
        break;
    }
    case SMInstruction::JF: {
        int_t value = pop(err);
        if (value == 0)
            m_ip = ip_from_labels(current);
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

inline size_t StackMachine::ip_from_labels(const SMInstruction &instr) {
    if (instr.arg >= 0) {
        return instr.arg;
    } else {
        return m_labels[m_label_ind[instr.ident]];
    }
}
