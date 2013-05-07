#include "astnode.h"
#include <assert.h>

void ReadASTNode::visit(VisitorSM *visitor) {
    visitor->visitRead(this);
}

void NumberASTNode::visit(VisitorSM *visitor) {
    visitor->visitNumber(this);
}

void VariableASTNode::visit(VisitorSM *visitor) {
    visitor->visitVariable(this);
}

void OperationASTNode::visit(VisitorSM *visitor) {
    this->left->visit(visitor);
    this->right->visit(visitor);
    visitor->visitOperation(this);
}

void WriteASTNode::visit(VisitorSM *visitor) {
    this->expression->visit(visitor);
    visitor->visitWrite(this);
}

void AssignmentASTNode::visit(VisitorSM *visitor) {
    this->expression->visit(visitor);
    visitor->visitAssignment(this);
}

void SemicolonASTNode::visit(VisitorSM *visitor) {
    this->first->visit(visitor);
    this->second->visit(visitor);
}

void IfASTNode::visit(VisitorSM *visitor) {
    std::string elselbl = visitor->generateLabel();
    std::string endlbl = visitor->generateLabel();
    this->condition->visit(visitor);
    visitor->makeJumpF(elselbl);
    this->thenbr->visit(visitor);
    visitor->makeJump(endlbl);
    visitor->makeLabel(elselbl);
    this->elsebr->visit(visitor);
    visitor->makeLabel(endlbl);
}

void WhileASTNode::visit(VisitorSM *visitor) {
    std::string beginlbl = visitor->generateLabel();
    std::string endlbl = visitor->generateLabel();
    visitor->makeLabel(beginlbl);
    this->condition->visit(visitor);
    visitor->makeJumpF(endlbl);
    this->op->visit(visitor);
    visitor->makeJump(beginlbl);
    visitor->makeLabel(endlbl);
}
