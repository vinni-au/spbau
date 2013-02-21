#include "astnode.h"
#include <assert.h>

void OperationASTNode::interpret(Context *context) {
    left->interpret(context);
    right->interpret(context);
    switch(op) {
    case Plus:
        result = left->result + right->result;
        break;
    case Minus:
        result = left->result - right->result;
        break;
    case Div:
        assert(right->result != 0);
        result = left->result / right->result;
        break;
    case Mod:
        assert(right->result != 0);
        result = left->result % right->result;
        break;
    case Star:
        result = left->result * right->result;
        break;
    case EqEq:
        result = (left->result == right->result) ? 1 : 0;
        break;
    case Gt:
        result = (left->result > right->result) ? 1 : 0;
        break;
    case GtEq:
        result = (left->result >= right->result) ? 1 : 0;
        break;
    case Lt:
        result = (left->result < right->result) ? 1 : 0;
        break;
    case LtEq:
        result = (left->result <= right->result) ? 1 : 0;
        break;
    case And:
        result = (left->result && right->result) ? 1 : 0;
        break;
    case Or:
        result = (left->result || right->result) ? 1 : 0;
        break;
    case NotEq:
        result = (left->result != right->result) ? 1 : 0;
        break;
    }
}

void ReadASTNode::interpret(Context *context) {
    int_t value;
    context->in >> value;
    VariableImpl* var = context->makeVariableImpl(variable->name);
    var->set_value(value);
}

void WriteASTNode::interpret(Context *context) {
    expression->interpret(context);
    context->out << expression->result << std::endl;
}

void AssignmentASTNode::interpret(Context *context) {
    expression->interpret(context);
    VariableImpl* var = context->makeVariableImpl(variable->name);
    var->set_value(expression->result);
}

void SemicolonASTNode::interpret(Context *context) {
    first->interpret(context);
    second->interpret(context);
}

void IfASTNode::interpret(Context *context) {
    condition->interpret(context);
    if (condition->result == 0)
        elsebr->interpret(context);
    else thenbr->interpret(context);
}

void WhileASTNode::interpret(Context *context) {
    condition->interpret(context);
    while (condition->result != 0) {
        op->interpret(context);
        condition->interpret(context);
    }
}

void VariableASTNode::interpret(Context *context) {
    VariableImpl* var = context->makeVariableImpl(name);
    result = var->get_value();
}
