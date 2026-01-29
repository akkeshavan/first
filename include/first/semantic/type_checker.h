#pragma once

#include "first/ast/node.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/program.h"
#include "first/semantic/symbol_table.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include <memory>
#include <string>

namespace first {
namespace semantic {

class ModuleResolver;

// Type checker for semantic analysis
class TypeChecker {
public:
    TypeChecker(ErrorReporter& errorReporter)
        : errorReporter_(errorReporter)
        , symbolTable_()
        , moduleResolver_(nullptr)
        , currentProgram_(nullptr) {}

    void setModuleResolver(ModuleResolver* resolver) { moduleResolver_ = resolver; }
    
    // Main entry point: type check a program
    bool check(ast::Program* program);
    
    // Type inference: infer the type of an expression
    ast::Type* inferType(ast::Expr* expr);
    
    // Type checking: check if expression has expected type
    bool checkType(ast::Expr* expr, ast::Type* expectedType);
    
    // Check if two types are equal
    bool typesEqual(ast::Type* type1, ast::Type* type2);
    
    // Check if a type is assignable to another (subtyping)
    bool isAssignable(ast::Type* from, ast::Type* to);
    
    // Type copying helper (creates a deep copy of a type) - public for testing
    std::unique_ptr<ast::Type> copyType(ast::Type* type);
    
    // Function signature matching - public for testing
    bool matchFunctionSignature(ast::FunctionDecl* func, const std::vector<ast::Type*>& argTypes);
    bool matchFunctionSignature(ast::InteractionDecl* interaction, const std::vector<ast::Type*>& argTypes);

private:
    ErrorReporter& errorReporter_;
    SymbolTable symbolTable_;
    ModuleResolver* moduleResolver_;
    ast::Program* currentProgram_;
    
    // Helper methods
    void checkProgram(ast::Program* program);
    void checkFunction(ast::FunctionDecl* func);
    void checkInteraction(ast::InteractionDecl* interaction);
    void checkStatement(ast::Stmt* stmt);
    ast::Type* inferExpression(ast::Expr* expr);
    ast::Type* inferLiteral(ast::LiteralExpr* expr);
    ast::Type* inferBinary(ast::BinaryExpr* expr);
    ast::Type* inferUnary(ast::UnaryExpr* expr);
    ast::Type* inferVariable(ast::VariableExpr* expr);
    ast::Type* inferFunctionCall(ast::FunctionCallExpr* expr);
    ast::Type* inferConstructor(ast::ConstructorExpr* expr);
    ast::Type* inferMatch(ast::MatchExpr* expr);
    
    // Type checking helpers
    bool checkExpression(ast::Expr* expr, ast::Type* expectedType);
    
    // Error reporting helpers
    void reportTypeError(const SourceLocation& loc, 
                        const std::string& message,
                        ast::Type* actualType = nullptr,
                        ast::Type* expectedType = nullptr);
    
    // Type creation helpers
    std::unique_ptr<ast::Type> createPrimitiveType(ast::PrimitiveType::Kind kind);
    std::unique_ptr<ast::Type> createIntType();
    std::unique_ptr<ast::Type> createFloatType();
    std::unique_ptr<ast::Type> createBoolType();
    std::unique_ptr<ast::Type> createStringType();
    std::unique_ptr<ast::Type> createUnitType();
    std::unique_ptr<ast::Type> createArrayType(std::unique_ptr<ast::Type> elementType);
    std::unique_ptr<ast::Type> createFunctionType(
        std::vector<std::unique_ptr<ast::Type>> paramTypes,
        std::unique_ptr<ast::Type> returnType,
        bool isInteraction = false
    );
};

} // namespace semantic
} // namespace first
