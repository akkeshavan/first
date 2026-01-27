#include "first/ast/validator.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"

namespace first {
namespace ast {

void ASTValidator::reportError(const SourceLocation& loc, const std::string& message) {
    errorReporter_.error(loc, "AST validation error: " + message);
    errorCount_++;
}

bool ASTValidator::validate(Program* program) {
    if (!program) {
        reportError(SourceLocation(), "Program node is null");
        return false;
    }
    
    // Validate all top-level declarations
    for (const auto& func : program->getFunctions()) {
        validateNode(func.get(), "function declaration");
    }
    
    for (const auto& interaction : program->getInteractions()) {
        validateNode(interaction.get(), "interaction declaration");
    }
    
    for (const auto& typeDecl : program->getTypeDecls()) {
        validateNode(typeDecl.get(), "type declaration");
    }
    
    return errorCount_ == 0;
}

void ASTValidator::validateNode(ASTNode* node, const std::string& context) {
    if (!node) {
        reportError(SourceLocation(), "Null node in " + context);
        return;
    }
    
    // Check source location is valid
    if (node->getLocation().getLine() == 0) {
        reportError(node->getLocation(), "Invalid source location (line 0) in " + context);
    }
    
    // Type-specific validation
    if (auto* expr = dynamic_cast<Expr*>(node)) {
        validateExpr(expr, context);
    } else if (auto* stmt = dynamic_cast<Stmt*>(node)) {
        validateStmt(stmt, context);
    } else if (auto* type = dynamic_cast<Type*>(node)) {
        validateType(type, context);
    }
}

void ASTValidator::validateExpr(Expr* expr, const std::string& context) {
    if (!expr) {
        reportError(SourceLocation(), "Null expression in " + context);
        return;
    }
    
    // Validate specific expression types
    if (auto* binary = dynamic_cast<BinaryExpr*>(expr)) {
        if (!binary->getLeft()) {
            reportError(binary->getLocation(), "Binary expression missing left operand in " + context);
        }
        if (!binary->getRight()) {
            reportError(binary->getLocation(), "Binary expression missing right operand in " + context);
        }
        if (binary->getLeft()) {
            validateExpr(binary->getLeft(), context + " (left operand)");
        }
        if (binary->getRight()) {
            validateExpr(binary->getRight(), context + " (right operand)");
        }
    } else if (auto* unary = dynamic_cast<UnaryExpr*>(expr)) {
        if (!unary->getOperand()) {
            reportError(unary->getLocation(), "Unary expression missing operand in " + context);
        } else {
            validateExpr(unary->getOperand(), context + " (operand)");
        }
    } else if (auto* call = dynamic_cast<FunctionCallExpr*>(expr)) {
        if (call->getName().empty()) {
            reportError(call->getLocation(), "Function call missing name in " + context);
        }
        for (const auto& arg : call->getArgs()) {
            if (arg) {
                validateExpr(arg.get(), context + " (function argument)");
            } else {
                reportError(call->getLocation(), "Null argument in function call in " + context);
            }
        }
    } else if (auto* var = dynamic_cast<VariableExpr*>(expr)) {
        if (var->getName().empty()) {
            reportError(var->getLocation(), "Variable expression missing name in " + context);
        }
    }
}

void ASTValidator::validateStmt(Stmt* stmt, const std::string& context) {
    if (!stmt) {
        reportError(SourceLocation(), "Null statement in " + context);
        return;
    }
    
    if (auto* ret = dynamic_cast<ReturnStmt*>(stmt)) {
        // Return value is optional, but if present, validate it
        if (ret->getValue()) {
            validateExpr(ret->getValue(), context + " (return value)");
        }
    } else if (auto* exprStmt = dynamic_cast<ExprStmt*>(stmt)) {
        if (!exprStmt->getExpr()) {
            reportError(exprStmt->getLocation(), "Expression statement missing expression in " + context);
        } else {
            validateExpr(exprStmt->getExpr(), context + " (expression statement)");
        }
    }
}

void ASTValidator::validateType(Type* type, const std::string& context) {
    if (!type) {
        reportError(SourceLocation(), "Null type in " + context);
        return;
    }
    
    if (auto* array = dynamic_cast<ArrayType*>(type)) {
        if (!array->getElementType()) {
            reportError(array->getLocation(), "Array type missing element type in " + context);
        } else {
            validateType(array->getElementType(), context + " (array element)");
        }
    } else if (auto* param = dynamic_cast<ParameterizedType*>(type)) {
        if (param->getBaseName().empty()) {
            reportError(param->getLocation(), "Parameterized type missing base name in " + context);
        }
        for (const auto& arg : param->getTypeArgs()) {
            if (arg) {
                validateType(arg.get(), context + " (type argument)");
            } else {
                reportError(param->getLocation(), "Null type argument in " + context);
            }
        }
    }
}

} // namespace ast
} // namespace first
