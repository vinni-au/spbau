#ifndef CONTEXT_H
#define CONTEXT_H

#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include "global.h"

struct VariableImpl;

struct Context {
    Context(std::istream& in = std::cin, std::ostream& out = std::cout);

    VariableImpl* makeVariableImpl(std::string ident);
    bool isVariableDefined(std::string const &ident) const;

    std::istream& in;
    std::ostream& out;

private:
    typedef std::unordered_map<std::string, VariableImpl*> map_ident_varimp_t;
    map_ident_varimp_t m_ident_varimp;
};

struct VariableImpl {
    int_t get_value() {
        return m_value;
    }

    bool set_value(int_t value) {
        m_value = value;
    }

private:
    VariableImpl(std::string ident) : ident(ident), m_value(0) {
    }

    int_t m_value;
    std::string ident;

    friend struct Context;
};

#endif // CONTEXT_H
