#include <iostream>
#include <fstream>
#include "parser.h"
#include "astnode.h"

int main(int argc, char* argv[])
{
    std::ios_base::sync_with_stdio(false);
    std::istream& in = std::cin;
    std::ifstream fin;
    Parser* parser = 0;
    if (argc == 2) {
        fin.open(argv[1]);
        if (fin.is_open())
            parser = new Parser(fin);
    }
    if (parser == 0)
        parser = new Parser;
    ASTNode* program = parser->parse();
    program->interpret(new Context);
    return 0;
}

