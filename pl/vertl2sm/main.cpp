#include <iostream>
#include <fstream>
#include "global.h"
#include "astnode.h"
#include "parser.h"
#include "visitorsm.h"

int main(int argc, char* argv[])
{
    if (argc <= 1) {
        std::cout << "Using: vertl2sm <input_file> [<output_file>]" << std::endl;
        return 0;
    }
    std::ifstream fin(argv[1]);
    if (!fin.is_open()) {
        std::cerr << "Couldn't open input file" << std::endl;
        return -1;
    }
    Parser p(fin);
    ASTNode* program = p.parse();
    VisitorSM* v;
    std::ofstream fout;
    if (argc >= 3) {
        fout.open(argv[2]);
        if (!fout.is_open()) {
            std::cerr << "Couldn't open output file" << std::endl;
            return -1;
        }
        v = new VisitorSM(fout);
    } else v = new VisitorSM;

    program->visit(v);
    v->makeE();
    fin.close();
    fout.close();
    return 0;
}

