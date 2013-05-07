#ifndef PARSER_H
#define PARSER_H

#include <iostream>
#include <unordered_map>
#include "global.h"
#include "astnode_fwd.h"

struct Parser {
    Parser(std::istream& in = std::cin, std::ostream& err = std::cerr);
    ASTNode* parse();

private:
    std::string next();

    OperatorASTNode* operator_p();
    AssignmentASTNode* assignment_p();
    WriteASTNode* write_p();
    ReadASTNode* read_p();
    SemicolonASTNode* semicolon_p();
    IfASTNode* if_p();
    WhileASTNode* while_p();
    ExpressionASTNode* expression_p();


    bool m_end;

    std::istream& cin;
    std::ostream& cerr;

    static std::unordered_map<std::string, int> m_ops;
};

#endif // PARSER_H
