#ifndef GLOBAL_H
#define GLOBAL_H

typedef int int_t;

#include <string>

inline void trimch(std::string& str, char ch = ' ') {
    std::string::size_type pos = str.find_last_not_of(ch);
    if (pos != std::string::npos) {
        str.erase(pos + 1);
        pos = str.find_first_not_of(ch);
        if (pos != std::string::npos)
            str.erase(0, pos);
    }
    else str.erase(str.begin(), str.end());
}

inline void trim(std::string& str) {
    trimch(str, ' ');
    trimch(str, '\t');
    trimch(str, '\r');
    trimch(str, '\n');
    trimch(str, ':');
}

#endif // GLOBAL_H
