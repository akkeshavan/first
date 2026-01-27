#pragma once

#include "first/ast/node.h"
#include "first/ast/program.h"
#include "first/error_reporter.h"
#include <string>

namespace first {
namespace ast {

// AST validator - checks AST structure and catches obvious issues
class ASTValidator {
public:
    ASTValidator(ErrorReporter& errorReporter)
        : errorReporter_(errorReporter), errorCount_(0) {}

    // Validate an AST program
    bool validate(Program* program);
    
    // Get number of validation errors found
    size_t getErrorCount() const { return errorCount_; }

    // Validation helpers (public for testing and incremental validation)
    void validateNode(ASTNode* node, const std::string& context);
    void validateExpr(Expr* expr, const std::string& context);
    void validateStmt(Stmt* stmt, const std::string& context);
    void validateType(Type* type, const std::string& context);

private:
    ErrorReporter& errorReporter_;
    size_t errorCount_;
    
    // Report validation error
    void reportError(const SourceLocation& loc, const std::string& message);
};

} // namespace ast
} // namespace first
