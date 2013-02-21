#include "parser.h"
#include "astnode.h"
#include <sstream>

std::unordered_map<std::string, int> Parser::m_ops = {
    {"+",   OperationASTNode::Plus},
    {"-",   OperationASTNode::Minus},
    {"*",   OperationASTNode::Star},
    {"/",   OperationASTNode::Div},
    {"%",   OperationASTNode::Mod},
    {"==",  OperationASTNode::EqEq},
    {"!=",  OperationASTNode::NotEq},
    {">",   OperationASTNode::Gt},
    {">=",  OperationASTNode::GtEq},
    {"<",   OperationASTNode::Lt},
    {"<=",  OperationASTNode::LtEq},
    {"&&",  OperationASTNode::And},
    {"||",  OperationASTNode::Or}
};

Parser::Parser(std::istream &in, std::ostream &err) : cin(in), cerr(err), m_end(false){
}

ASTNode* Parser::parse() {
    ASTNode* program = operator_p();
    return program;
}

OperatorASTNode* Parser::operator_p() {
    std::string str = next();
    if (str == "s")
        return new SkipASTNode();
    if (str == ";") {
        OperatorASTNode* left = operator_p();
        OperatorASTNode* right = operator_p();
        return new SemicolonASTNode(left, right);
    }
    if (str == "=")
        return assignment_p();
    if (str == "w")
        return write_p();
    if (str == "r")
        return read_p();
    if (str == ":")
        return semicolon_p();
    if (str == "i")
        return if_p();
    if (str == "l")
        return while_p();

    return 0;
}

AssignmentASTNode* Parser::assignment_p() {
    std::string var = next();
    VariableASTNode* varnode = new VariableASTNode(var);
    ExpressionASTNode* exprnode = expression_p();
    return new AssignmentASTNode(varnode, exprnode);
}

WriteASTNode* Parser::write_p() {
    ExpressionASTNode* exprnode = expression_p();
    return new WriteASTNode(exprnode);
}

ReadASTNode* Parser::read_p() {
    std::string var = next();
    VariableASTNode* varnode = new VariableASTNode(var);
    return new ReadASTNode(varnode);
}

SemicolonASTNode* Parser::semicolon_p() {
    OperatorASTNode* firstop = operator_p();
    OperatorASTNode* secondop = operator_p();
    return new SemicolonASTNode(firstop, secondop);
}

IfASTNode* Parser::if_p() {
    ExpressionASTNode* condition = expression_p();
    OperatorASTNode* thenbr = operator_p();
    OperatorASTNode* elsebr = operator_p();
    return new IfASTNode(condition, thenbr, elsebr);
}

WhileASTNode* Parser::while_p() {
    ExpressionASTNode* condition = expression_p();
    OperatorASTNode* op = operator_p();
    return new WhileASTNode(condition, op);
}

ExpressionASTNode* Parser::expression_p() {
    std::string str = next();
    if (str == "!") {
        std::string nums = next();
        std::stringstream ss(nums);
        int_t value = 0;
        if (ss >> value)
            return new NumberASTNode(value);
    }

    if (str == "x") {
        std::string ident = next();
        return new VariableASTNode(ident);
    }

    if (str == "@") {
        std::string op = next();
        ExpressionASTNode* left = expression_p();
        ExpressionASTNode* right = expression_p();
        int opcode = m_ops[op];
        return new OperationASTNode(opcode, left, right);
    }

    return 0;
}

std::string Parser::next() {
    std::string result;
    std::getline(cin, result);
    trim(result);
    return result;
}
