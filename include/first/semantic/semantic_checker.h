#pragma once

#include "first/ast/node.h"
#include "first/ast/declarations.h"
#include "first/ast/statements.h"
#include "first/ast/expressions.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include <string>

namespace first {
namespace semantic {

// Semantic checker - enforces language restrictions (pure function rules, etc.)
class SemanticChecker {
public:
    SemanticChecker(ErrorReporter& errorReporter)
        : errorReporter_(errorReporter), inPureFunction_(false) {}
    
    // Check a function declaration for semantic restrictions
    void checkFunction(ast::FunctionDecl* func);
    
    // Check an interaction declaration (no restrictions)
    void checkInteraction(ast::InteractionDecl* interaction);
    
    // Check a statement for semantic violations
    void checkStatement(ast::Stmt* stmt);
    
    // Check an expression for semantic violations
    void checkExpression(ast::Expr* expr);
    
    // Check a variable declaration (public for testing)
    void checkVariableDecl(ast::VariableDecl* varDecl);

private:
    ErrorReporter& errorReporter_;
    bool inPureFunction_; // Track if we're currently in a pure function
    
    // Check for while loop (not allowed in pure functions)
    void checkWhileLoop(ast::Stmt* stmt);
    
    // Check for monadic operators in expressions
    void checkMonadicOperators(ast::Expr* expr);
    
    // Check for I/O operations (by name - print, input, readFile, etc.)
    void checkIOOperation(ast::FunctionCallExpr* call);
    
    // Helper to check if a function name is an I/O operation
    bool isIOOperation(const std::string& name);
    
    // Report semantic violation
    void reportViolation(const SourceLocation& loc, const std::string& message);
};

} // namespace semantic
} // namespace first
