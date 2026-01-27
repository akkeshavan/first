#pragma once

#include "first/ast/program.h"
#include "first/ast/visitor.h"
#include "first/error_reporter.h"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <memory>
#include <string>
#include <unordered_map>

namespace first {
namespace ir {

// IR Generator - converts AST to LLVM IR
class IRGenerator : public ast::ASTVisitor {
public:
    IRGenerator(ErrorReporter& errorReporter, const std::string& moduleName = "first_module");
    ~IRGenerator();
    
    // Generate IR from AST program
    bool generate(ast::Program* program);
    
    // Get the generated LLVM module
    llvm::Module* getModule() const { return module_.get(); }
    
    // Get LLVM context
    llvm::LLVMContext& getContext() { return context_; }
    
    // Print generated IR
    void printIR() const;
    
    // Visitor methods for AST nodes
    void visitProgram(ast::Program* node) override;
    void visitFunctionDecl(ast::FunctionDecl* node) override;
    void visitInteractionDecl(ast::InteractionDecl* node) override;
    void visitLiteralExpr(ast::LiteralExpr* node) override;
    void visitBinaryExpr(ast::BinaryExpr* node) override;
    void visitUnaryExpr(ast::UnaryExpr* node) override;
    void visitVariableExpr(ast::VariableExpr* node) override;
    void visitFunctionCallExpr(ast::FunctionCallExpr* node) override;
    void visitArrayLiteralExpr(ast::ArrayLiteralExpr* node) override;
    void visitArrayIndexExpr(ast::ArrayIndexExpr* node) override;
    void visitVariableDecl(ast::VariableDecl* node) override;
    void visitReturnStmt(ast::ReturnStmt* node) override;
    void visitExprStmt(ast::ExprStmt* node) override;
    void visitIfStmt(ast::IfStmt* node) override;
    void visitWhileStmt(ast::WhileStmt* node) override;
    void visitAssignmentStmt(ast::AssignmentStmt* node) override;
    
    // Type conversion helpers
    llvm::Type* convertType(ast::Type* type);
    llvm::Type* convertPrimitiveType(ast::PrimitiveType* type);
    llvm::Type* convertArrayType(ast::ArrayType* type);
    
private:
    ErrorReporter& errorReporter_;
    llvm::LLVMContext context_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    
    // Current function being generated
    llvm::Function* currentFunction_;
    
    // Symbol table for local variables (maps variable name to LLVM Value)
    std::unordered_map<std::string, llvm::Value*> localVars_;
    
    // Array metadata: maps array pointer to (elementType, arraySize)
    struct ArrayMetadata {
        llvm::Type* elementType;
        size_t size;
        llvm::AllocaInst* alloca; // Original alloca for stack arrays
    };
    std::unordered_map<llvm::Value*, ArrayMetadata> arrayMetadata_;
    
    // Current return value (for expressions)
    llvm::Value* currentValue_;
    
    // Helper methods
    void enterFunction(llvm::Function* func);
    void exitFunction();
    llvm::Value* getVariable(const std::string& name);
    void setVariable(const std::string& name, llvm::Value* value);
    
    // Expression evaluation helpers
    llvm::Value* evaluateExpr(ast::Expr* expr);
    llvm::Value* evaluateLiteral(ast::LiteralExpr* expr);
    llvm::Value* evaluateBinary(ast::BinaryExpr* expr);
    llvm::Value* evaluateUnary(ast::UnaryExpr* expr);
    llvm::Value* evaluateVariable(ast::VariableExpr* expr);
    llvm::Value* evaluateFunctionCall(ast::FunctionCallExpr* expr);
    llvm::Value* evaluateArrayLiteral(ast::ArrayLiteralExpr* expr);
    llvm::Value* evaluateArrayIndex(ast::ArrayIndexExpr* expr);
    
    // Statement generation helpers
    void generateStatement(ast::Stmt* stmt);
    void generateVariableDecl(ast::VariableDecl* stmt);
    void generateReturnStmt(ast::ReturnStmt* stmt);
    void generateExprStmt(ast::ExprStmt* stmt);
    void generateIfStmt(ast::IfStmt* stmt);
    void generateWhileStmt(ast::WhileStmt* stmt);
    void generateAssignmentStmt(ast::AssignmentStmt* stmt);
    
    // Short-circuit evaluation helpers
    llvm::Value* evaluateShortCircuitAnd(ast::BinaryExpr* expr);
    llvm::Value* evaluateShortCircuitOr(ast::BinaryExpr* expr);
};

} // namespace ir
} // namespace first
