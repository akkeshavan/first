#pragma once

// Forward declarations
namespace first {
namespace ast {
    class ASTNode;
    class LiteralExpr;
    class BinaryExpr;
    class UnaryExpr;
    class VariableExpr;
    class FunctionCallExpr;
    class ArrayLiteralExpr;
    class ArrayIndexExpr;
    class RecordLiteralExpr;
    class FieldAccessExpr;
    class ConstructorExpr;
    class MatchExpr;
    class LambdaExpr;
    class VariableDecl;
    class ReturnStmt;
    class ExprStmt;
    class IfStmt;
    class WhileStmt;
    class AssignmentStmt;
    class FunctionDecl;
    class InteractionDecl;
    class TypeDecl;
    class ImportDecl;
    class Program;
}
}

namespace first {
namespace ast {

// Visitor interface for AST traversal
class ASTVisitor {
public:
    virtual ~ASTVisitor() = default;
    
    // Expression nodes
    virtual void visitLiteralExpr(LiteralExpr* node) {}
    virtual void visitBinaryExpr(BinaryExpr* node) {}
    virtual void visitUnaryExpr(UnaryExpr* node) {}
    virtual void visitVariableExpr(VariableExpr* node) {}
    virtual void visitFunctionCallExpr(FunctionCallExpr* node) {}
    virtual void visitArrayLiteralExpr(ArrayLiteralExpr* node) {}
    virtual void visitArrayIndexExpr(ArrayIndexExpr* node) {}
    virtual void visitRecordLiteralExpr(RecordLiteralExpr* node) {}
    virtual void visitFieldAccessExpr(FieldAccessExpr* node) {}
    virtual void visitConstructorExpr(ConstructorExpr* node) {}
    virtual void visitMatchExpr(MatchExpr* node) {}
    virtual void visitLambdaExpr(LambdaExpr* node) {}
    
    // Statement nodes
    virtual void visitVariableDecl(VariableDecl* node) {}
    virtual void visitReturnStmt(ReturnStmt* node) {}
    virtual void visitExprStmt(ExprStmt* node) {}
    virtual void visitIfStmt(IfStmt* node) {}
    virtual void visitWhileStmt(WhileStmt* node) {}
    virtual void visitAssignmentStmt(AssignmentStmt* node) {}
    
    // Declaration nodes
    virtual void visitFunctionDecl(FunctionDecl* node) {}
    virtual void visitInteractionDecl(InteractionDecl* node) {}
    virtual void visitTypeDecl(TypeDecl* node) {}
    virtual void visitImportDecl(ImportDecl* node) {}
    
    // Top-level nodes
    virtual void visitProgram(Program* node) {}
};

} // namespace ast
} // namespace first
