#include "visitorsm.h"

std::unordered_map<int, std::string> VisitorSM::m_op = {
    {OperationASTNode::Plus,    "+"},
    {OperationASTNode::Minus,   "-"},
    {OperationASTNode::Star,    "*"},
    {OperationASTNode::Div,     "/"},
    {OperationASTNode::Mod,     "%"},
    {OperationASTNode::EqEq,    "=="},
    {OperationASTNode::NotEq,   "!="},
    {OperationASTNode::Gt,      ">"},
    {OperationASTNode::GtEq,    ">="},
    {OperationASTNode::Lt,      "<"},
    {OperationASTNode::LtEq,    "<="},
    {OperationASTNode::And,     "&&"},
    {OperationASTNode::Or,      "||"}
};

std::string VisitorSM::generateLabel() {
    std::stringstream ss;
    ss << "label" << curlabeln++;
    return ss.str();
}

void VisitorSM::visitRead(ReadASTNode *node) {
    makeR();
    makeS(node->variable->name);
}

void VisitorSM::visitNumber(NumberASTNode *node) {
    makeC(node->result);
}

void VisitorSM::visitVariable(VariableASTNode *node) {
    makeL(node->name);
}

void VisitorSM::visitOperation(OperationASTNode *node) {
    makeOp(VisitorSM::m_op[node->op]);
}

void VisitorSM::visitAssignment(AssignmentASTNode *node) {
    makeS(node->variable->name);
}

void VisitorSM::visitWrite(WriteASTNode *node) {
    makeW();
}
