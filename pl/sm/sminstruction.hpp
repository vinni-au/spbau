#ifndef SMINSTRUCTION_HPP
#define SMINSTRUCTION_HPP

#include <string>
#include <sstream>
#include "global.h"

struct SMInstruction
{
    SMInstruction() :
        op(Unknown), arg(0) {
    }

    enum {
        Unknown = 0,
        C,
        L,
        S,
        R,
        W,
        J,
        JT,
        JF,
        E,
        Plus = 0b10000000,
        Minus,
        Mult,
        Div,
        Mod,
        EqEq,
        NotEq,
        Gt,
        Lt,
        GtEq,
        LtEq,
        Or,
        And
    };

    short op;
    int_t arg;
    std::string ident;

    SMInstruction static parse(std::string str);
};

#endif // SMINSTRUCTION_HPP
