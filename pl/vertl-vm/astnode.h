#ifndef ASTNODE_H
#define ASTNODE_H

#include <string>
#include <assert.h>
#include "global.h"
#include "context.h"

struct ASTNode
{
    enum {
        Node,
        Operator,
        Expression,
        Variable,
        Number,
        Skip,
        Read,
        Write,
        Operation,
        If,
        While,
        Semicolon,
        Assignment
    };

    ASTNode(ASTNode * parent = 0) : type(Node), parent(parent) {}
    virtual ~ASTNode() { }
    virtual void interpret(Context* context) = 0;

    void setParent(ASTNode* p) {
        parent = p;
    }

    int type;
    ASTNode* parent;
};

struct OperatorASTNode : ASTNode {
    OperatorASTNode(ASTNode* p = 0) : ASTNode(p) {
        type = Operator;
    }

    void interpret(Context *) { }
};

struct ExpressionASTNode : ASTNode {
    ExpressionASTNode(ASTNode* p = 0) : ASTNode(p), result(0) {
        type = Expression;
    }

    int_t result;

    void interpret(Context *) { }
};

struct VariableASTNode : ExpressionASTNode {
    VariableASTNode(std::string variable, ASTNode* p = 0)
            : ExpressionASTNode(p), name(variable) {
        type = Variable;
    }

    void interpret(Context *);

    std::string name;
};

struct NumberASTNode : ExpressionASTNode {
    NumberASTNode(int_t value, ASTNode* p = 0) : ExpressionASTNode(p) {
        type = Number;
        parent = p;
        result = value;
    }

    void interpret(Context *context) { }
};

struct OperationASTNode : ExpressionASTNode {
    enum {
        Plus,
        Minus,
        Star,
        Div,
        Mod,
        EqEq,
        NotEq,
        Gt,
        GtEq,
        Lt,
        LtEq,
        And,
        Or
    };

    OperationASTNode(int operation, ExpressionASTNode* left, ExpressionASTNode* right, ASTNode* p = 0)
            : ExpressionASTNode(p), op(operation), left(left), right(right) {
        type = Operation;
    }

    void interpret(Context *context);

    int op;
    ExpressionASTNode* left;
    ExpressionASTNode* right;
};

struct SkipASTNode : OperatorASTNode {
    SkipASTNode(ASTNode* p = 0) : OperatorASTNode(p) {
        type = Skip;
    }

    void interpret(Context *) { }
};

struct ReadASTNode : OperatorASTNode {
    ReadASTNode(VariableASTNode* variable, ASTNode* p = 0)
            : OperatorASTNode(p), variable(variable) {
        assert(variable != 0);
        type = Read;
    }

    void interpret(Context *context);

    VariableASTNode* variable;
};

struct WriteASTNode : OperatorASTNode {
    WriteASTNode(ExpressionASTNode* exp, ASTNode* p = 0) : OperatorASTNode(p), expression(exp) {
        assert(exp != 0);
        type = Write;
    }

    void interpret(Context *context);

    ExpressionASTNode* expression;
};

struct AssignmentASTNode : OperatorASTNode {
    AssignmentASTNode(VariableASTNode* variable, ExpressionASTNode* expression, ASTNode* p = 0)
            : OperatorASTNode(p), variable(variable), expression(expression) {
        assert(variable != 0);
        assert(expression != 0);
        type = Assignment;
    }

    void interpret(Context *context);

    VariableASTNode* variable;
    ExpressionASTNode* expression;
};

struct SemicolonASTNode : OperatorASTNode {
    SemicolonASTNode(OperatorASTNode* firstop, OperatorASTNode* secondop, ASTNode* p = 0)
            : OperatorASTNode(p), first(firstop), second(secondop) {
        assert(firstop != 0);
        assert(secondop != 0);
        type = Semicolon;
    }

    void interpret(Context *context);

    OperatorASTNode* first;
    OperatorASTNode* second;
};

struct IfASTNode : OperatorASTNode {
    IfASTNode(ExpressionASTNode* condexpr, OperatorASTNode* thenbr, OperatorASTNode* elsebr, ASTNode* p = 0)
            : OperatorASTNode(p), condition(condexpr), thenbr(thenbr), elsebr(elsebr) {
        assert(condexpr != 0);
        assert(thenbr != 0);
        assert(elsebr != 0);
        type = If;
    }

    void interpret(Context *context);

    ExpressionASTNode* condition;
    OperatorASTNode* thenbr;
    OperatorASTNode* elsebr;
};

struct WhileASTNode : OperatorASTNode {
    WhileASTNode(ExpressionASTNode* condition, OperatorASTNode* op, ASTNode* p = 0)
            : OperatorASTNode(p), condition(condition), op(op) {
        assert(condition != 0);
        assert(op != 0);
        type = While;
    }

    void interpret(Context *context);

    ExpressionASTNode* condition;
    OperatorASTNode* op;
};

#endif // ASTNODE_H
