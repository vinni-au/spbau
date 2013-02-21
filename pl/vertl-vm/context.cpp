#include "context.h"

Context::Context(std::istream &in, std::ostream &out): in(in), out(out){
}

VariableImpl* Context::makeVariableImpl(std::string ident) {
    VariableImpl* result;

    map_ident_varimp_t::iterator it = m_ident_varimp.find(ident);
    if (it == m_ident_varimp.end()) {
        result = new VariableImpl(ident);
        m_ident_varimp[ident] = result;
    } else {
        result = it->second;
    }

    return result;
}

bool Context::isVariableDefined(std::string const &ident) const{
    return (m_ident_varimp.find(ident) != m_ident_varimp.end());
}
