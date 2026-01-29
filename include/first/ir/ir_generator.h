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
#include <map>

namespace first {
namespace semantic {
    class ModuleResolver;
}
}

namespace first {
namespace ir {

// IR Generator - converts AST to LLVM IR
class IRGenerator : public ast::ASTVisitor {
public:
    IRGenerator(ErrorReporter& errorReporter, const std::string& moduleName = "first_module");
    // Use an external LLVMContext whose lifetime outlives the IRGenerator and any modules
    // moved/linked out of it (e.g., owned by Compiler for multi-module linking).
    IRGenerator(ErrorReporter& errorReporter, llvm::LLVMContext& externalContext,
                const std::string& moduleName = "first_module");
    ~IRGenerator();
    
    // Generate IR from AST program
    bool generate(ast::Program* program);
    
    // Get the generated LLVM module
    llvm::Module* getModule() const { return module_.get(); }
    // Transfer ownership of the module to the caller.
    // Safe only when using an external context that will outlive the module.
    std::unique_ptr<llvm::Module> takeModule() { return std::move(module_); }
    
    // Get LLVM context
    llvm::LLVMContext& getContext() { return context_; }
    
    // Print generated IR
    void printIR() const;
    
    // Write IR to file
    bool writeIRToFile(const std::string& filename) const;
    
    // Set module resolver (for symbol lookup during IR generation)
    void setModuleResolver(semantic::ModuleResolver* resolver) { moduleResolver_ = resolver; }
    
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
    void visitRecordLiteralExpr(ast::RecordLiteralExpr* node) override;
    void visitFieldAccessExpr(ast::FieldAccessExpr* node) override;
    void visitConstructorExpr(ast::ConstructorExpr* node) override;
    void visitMatchExpr(ast::MatchExpr* node) override;
    void visitLambdaExpr(ast::LambdaExpr* node) override;
    void visitVariableDecl(ast::VariableDecl* node) override;
    void visitReturnStmt(ast::ReturnStmt* node) override;
    void visitExprStmt(ast::ExprStmt* node) override;
    void visitIfStmt(ast::IfStmt* node) override;
    void visitWhileStmt(ast::WhileStmt* node) override;
    void visitAssignmentStmt(ast::AssignmentStmt* node) override;
    void visitImportDecl(ast::ImportDecl* node) override;
    void visitTypeDecl(ast::TypeDecl* node) override;
    
    // Type conversion helpers
    llvm::Type* convertType(ast::Type* type);
    llvm::Type* convertPrimitiveType(ast::PrimitiveType* type);
    llvm::Type* convertArrayType(ast::ArrayType* type);
    llvm::Type* convertRecordType(ast::RecordType* type);
    llvm::Type* convertADTType(ast::ADTType* type);
    llvm::Type* convertParameterizedType(ast::ParameterizedType* type);
    
    // Generic type helpers (public for testing)
    std::unique_ptr<ast::Type> substituteType(ast::Type* type, 
                                               const std::map<std::string, ast::Type*>& substitutions);
    std::string getMonomorphizedName(const std::string& baseName,
                                     const std::vector<ast::Type*>& typeArgs);
    
private:
    ErrorReporter& errorReporter_;
    // If no external context is provided, we own one.
    std::unique_ptr<llvm::LLVMContext> ownedContext_;
    // All IR objects (Module, IRBuilder, types/consts) are created in this context.
    llvm::LLVMContext& context_;
    std::unique_ptr<llvm::Module> module_;
    std::unique_ptr<llvm::IRBuilder<>> builder_;
    
    // Module resolver for symbol lookup
    semantic::ModuleResolver* moduleResolver_;
    
    // Generic function monomorphization cache
    std::map<std::string, llvm::Function*> monomorphizedFunctions_;
    
    // Current function being generated
    llvm::Function* currentFunction_;
    
    // Symbol table for local variables (maps variable name to stack slot)
    std::unordered_map<std::string, llvm::AllocaInst*> localVars_;
    // Track variable types for proper loading (needed with opaque pointers)
    std::unordered_map<std::string, llvm::Type*> localVarTypes_;
    
    // Array metadata: maps array pointer to (elementType, arraySize)
    struct ArrayMetadata {
        llvm::Type* elementType;
        size_t size;
        llvm::AllocaInst* alloca; // Original alloca for stack arrays
    };
    std::unordered_map<llvm::Value*, ArrayMetadata> arrayMetadata_;
    
    // Record metadata: maps record pointer to (structType, fieldNames, alloca)
    struct RecordMetadata {
        llvm::StructType* structType;
        std::vector<std::string> fieldNames; // Field names in order
        llvm::AllocaInst* alloca; // Original alloca for stack records
    };
    std::unordered_map<llvm::Value*, RecordMetadata> recordMetadata_;
    
    // Current return value (for expressions)
    llvm::Value* currentValue_;
    
    // Helper methods
    void enterFunction(llvm::Function* func);
    void exitFunction();
    llvm::AllocaInst* getVariable(const std::string& name);
    llvm::Value* getDefaultValue(llvm::Type* type);
    void setVariable(const std::string& name, llvm::AllocaInst* value, llvm::Type* type = nullptr);
    
    // Expression evaluation helpers
    llvm::Value* evaluateExpr(ast::Expr* expr);
    llvm::Value* evaluateLiteral(ast::LiteralExpr* expr);
    llvm::Value* evaluateBinary(ast::BinaryExpr* expr);
    llvm::Value* evaluateUnary(ast::UnaryExpr* expr);
    llvm::Value* evaluateVariable(ast::VariableExpr* expr);
    llvm::Value* evaluateFunctionCall(ast::FunctionCallExpr* expr);
    llvm::Value* evaluateArrayLiteral(ast::ArrayLiteralExpr* expr);
    llvm::Value* evaluateArrayIndex(ast::ArrayIndexExpr* expr);
    llvm::Value* evaluateRecordLiteral(ast::RecordLiteralExpr* expr);
    llvm::Value* evaluateFieldAccess(ast::FieldAccessExpr* expr);
    llvm::Value* evaluateConstructor(ast::ConstructorExpr* expr);
    llvm::Value* evaluateMatch(ast::MatchExpr* expr);
    
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
    
    // Pattern matching helpers
    bool generatePatternMatch(ast::Pattern* pattern, llvm::Value* value, 
                              llvm::BasicBlock* matchBB, llvm::BasicBlock* nextBB);
    void bindPatternVariables(ast::Pattern* pattern, llvm::Value* value);
    
    // Closure/lambda helpers
    llvm::Value* evaluateLambda(ast::LambdaExpr* expr);
    std::vector<std::string> analyzeCaptures(ast::LambdaExpr* expr);
    llvm::Function* generateClosureFunction(ast::LambdaExpr* expr, 
                                            const std::vector<std::string>& captures);
    llvm::Value* allocateClosure(llvm::Function* func, 
                                 const std::vector<std::string>& captures);
    llvm::Value* invokeClosure(llvm::Value* closure, 
                               const std::vector<llvm::Value*>& args,
                               llvm::Type* returnType);
    
    // Generic type helpers (private implementation)
    std::unique_ptr<ast::Type> copyType(ast::Type* type);
    llvm::Function* monomorphizeFunction(ast::FunctionDecl* func,
                                          const std::vector<ast::Type*>& typeArgs);
    
    // Module/import helpers
    void generateExternalDeclaration(const std::string& symbolName, const std::string& moduleName);
};

} // namespace ir
} // namespace first
