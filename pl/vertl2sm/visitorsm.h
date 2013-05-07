#ifndef VISITORSM_H
#define VISITORSM_H

#include "global.h"
#include <string>
#include <sstream>
#include <iostream>
#include <unordered_map>
#include "astnode_fwd.h"

struct VisitorSM
{
    VisitorSM(std::ostream& out = std::cout) : out(out) { }

    std::string generateLabel();

    void visitRead(ReadASTNode* node);
    void visitWrite(WriteASTNode* node);
    void visitNumber(NumberASTNode* node);
    void visitVariable(VariableASTNode* node);
    void visitOperation(OperationASTNode* node);
    void visitAssignment(AssignmentASTNode* node);

    void makeC(int_t n) {
        out << "C " << n << std::endl;
    }

    void makeS(std::string const& var) {
        out << "S " << var << std::endl;
    }

    void makeL(std::string const& var) {
        out << "L " << var << std::endl;
    }

    void makeR() {
        out << "R" << std::endl;
    }

    void makeW() {
        out << "W" << std::endl;
    }

    void makeJump(std::string const& label) {
        out << "J $" << label << std::endl;
    }

    void makeJumpT(std::string const& label) {
        out << "JT $" << label << std::endl;
    }

    void makeJumpF(std::string const& label) {
        out << "JF $" << label << std::endl;
    }

    void makeOp(std::string const& op) {
        out << "B" << op << std::endl;
    }

    void makeE() {
        out << "E" << std::endl;
    }

    void makeLabel(std::string const& label) {
        out << "$" << label << ": ";// << std::endl;
    }

private:
    std::ostream& out;
    int curlabeln = 0;

    static std::unordered_map<int, std::string> m_op;
};

#include "astnode.h"
#endif // VISITORSM_H
