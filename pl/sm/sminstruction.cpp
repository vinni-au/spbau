#include "sminstruction.hpp"

SMInstruction SMInstruction::parse(std::string str) {
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
            if (ss >> ch) {
                if (ch == 'F')
                    result.op = JF;
                else if (ch == 'T')
                    result.op = JT;
                else
                    result.op = J;
            }
            break;
        case 'E':
            result.op = E;
            break;
        case 'B':
            if (ss >> ch) {
                if (ch == ' ')
                    ss >> ch;
                switch (ch) {
                case '*':
                    result.op = Mult;
                    break;
                case '!':
                    ss >> ch;
                    result.op = NotEq;
                    break;
                case '=':
                    ss >> ch;
                    result.op = EqEq;
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
                    int pos = s.find("$");
                    if (pos != std::string::npos) {
                        s.erase(pos, 1);
                    }
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

