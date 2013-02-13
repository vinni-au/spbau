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
        Label,
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

    SMInstruction static parse(std::string str) {
        size_t pos = str.find(':');
        if (pos != std::string::npos) {
            str.erase(str.begin() + pos, str.end());
            SMInstruction result;
            result.op = Label;
            result.ident = str;
            return result;
        }
        std::stringstream ss;
        ss << str;
        char ch = 0;
        SMInstruction result;
        if (ss >> ch) {
            switch (ch) {
            case 'C':
                result.op = C;
                break;
            case 'L':
                result.op = L;
                break;
            case 'S':
                result.op = S;
                break;
            case 'R':
                result.op = R;
                break;
            case 'W':
                result.op = W;
                break;
            case 'J':
                result.op = J;
                break;
            case 'E':
                result.op = E;
                break;
            case '+':
                result.op = Plus;
                break;
            case '-':
                result.op = Minus;
                break;
            case '/':
                result.op = Div;
                break;
            case '%':
                result.op = Mod;
                break;
            case '<':
                result.op = Lt;
                if (ss.get() == '=')
                    result.op = LtEq;
                else ss.unget();
                break;
            case '>':
                result.op = Gt;
                if (ss.get() == '=')
                    result.op = GtEq;
                else ss.unget();
                break;
            case '&':
                if (ss >> ch)
                    if (ch == '&')
                        result.op = And;
                break;
            case '|':
                if (ss >> ch)
                    if (ch == '|')
                        result.op = Or;
                break;
            }
        }
        if (result.op != E && result.op != R && result.op != W) {
            if (result.op == J || result.op == JF || result.op == JT) {
                std::string s;
                ss >> s;
                if (s.size() > 0) {
                    if (std::isdigit(s.at(0))) {
                        std::stringstream(s) >> result.arg;
                    } else {
                        result.ident = s;
                        result.arg = -1;
                    }
                }

            } else
            if (result.op == L || result.op == S)
                ss >> result.ident;
            else ss >> result.arg;
        }
        return result;
    }
};

#endif // SMINSTRUCTION_HPP
