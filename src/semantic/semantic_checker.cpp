#include "first/semantic/semantic_checker.h"
#include "first/ast/expressions.h"
#include "first/ast/match_case.h"
#include <algorithm>

namespace first {
namespace semantic {

void SemanticChecker::checkFunction(ast::FunctionDecl* func) {
    if (!func) return;
    
    // Mark that we're in a pure function
    bool previousState = inPureFunction_;
    inPureFunction_ = true;
    
    // Check function body statements
    for (const auto& stmt : func->getBody()) {
        checkStatement(stmt.get());
    }
    
    // Restore previous state
    inPureFunction_ = previousState;
}

void SemanticChecker::checkInteraction(ast::InteractionDecl* interaction) {
    if (!interaction) return;
    
    // Interactions allow everything - no restrictions
    // But we still check for consistency
    bool previousState = inPureFunction_;
    inPureFunction_ = false;
    
    for (const auto& stmt : interaction->getBody()) {
        checkStatement(stmt.get());
    }
    
    inPureFunction_ = previousState;
}

void SemanticChecker::checkStatement(ast::Stmt* stmt) {
    if (!stmt) return;
    
    if (auto* varDecl = dynamic_cast<ast::VariableDecl*>(stmt)) {
        checkVariableDecl(varDecl);
    } else if (auto* exprStmt = dynamic_cast<ast::ExprStmt*>(stmt)) {
        if (exprStmt->getExpr()) {
            checkExpression(exprStmt->getExpr());
        }
    } else if (auto* retStmt = dynamic_cast<ast::ReturnStmt*>(stmt)) {
        if (retStmt->getValue()) {
            checkExpression(retStmt->getValue());
        }
    }
    // TODO: Check for while loops when WhileStmt AST node is added
    // TODO: Check for assignments when AssignmentStmt AST node is added
}

void SemanticChecker::checkExpression(ast::Expr* expr) {
    if (!expr) return;
    
    if (auto* call = dynamic_cast<ast::FunctionCallExpr*>(expr)) {
        checkIOOperation(call);
        // Check arguments recursively
        for (const auto& arg : call->getArgs()) {
            checkExpression(arg.get());
        }
    } else if (auto* binary = dynamic_cast<ast::BinaryExpr*>(expr)) {
        checkMonadicOperators(expr);
        checkExpression(binary->getLeft());
        checkExpression(binary->getRight());
    } else if (auto* unary = dynamic_cast<ast::UnaryExpr*>(expr)) {
        checkExpression(unary->getOperand());
    } else if (auto* constructor = dynamic_cast<ast::ConstructorExpr*>(expr)) {
        for (const auto& arg : constructor->getArguments()) {
            checkExpression(arg.get());
        }
    } else if (auto* match = dynamic_cast<ast::MatchExpr*>(expr)) {
        checkExpression(match->getMatchedExpr());
        // Check match case bodies
        for (const auto& matchCase : match->getCases()) {
            if (matchCase->getBody()) {
                checkExpression(matchCase->getBody());
            }
        }
    }
}

void SemanticChecker::checkVariableDecl(ast::VariableDecl* varDecl) {
    if (!varDecl) return;
    
    // Check if mutable variable is declared in pure function
    if (inPureFunction_ && varDecl->getMutability() == ast::VariableDecl::Mutability::Mutable) {
        reportViolation(
            varDecl->getLocation(),
            "Mutable variables (`var`) can only be used in interaction functions, not in pure functions"
        );
    }
}

void SemanticChecker::checkWhileLoop(ast::Stmt* stmt) {
    // TODO: Implement when WhileStmt AST node is added
    // if (inPureFunction_ && dynamic_cast<ast::WhileStmt*>(stmt)) {
    //     reportViolation(stmt->getLocation(), "While loops can only be used in interaction functions");
    // }
}

void SemanticChecker::checkMonadicOperators(ast::Expr* expr) {
    if (!inPureFunction_ || !expr) return;
    
    // Check for monadic operators in binary expressions
    if (auto* binary = dynamic_cast<ast::BinaryExpr*>(expr)) {
        // Note: We don't have monadic operator enum values yet, but we can check by operator type
        // Monadic operators would be: >>=, >>, <$>, <*>
        // For now, this is a placeholder - we'll need to add these operators to BinaryExpr::Op
        // or create a separate MonadicExpr node
        
        // TODO: Check for monadic operators when they're added to the AST
    }
}

void SemanticChecker::checkIOOperation(ast::FunctionCallExpr* call) {
    if (!call || !inPureFunction_) return;
    
    if (isIOOperation(call->getName())) {
        reportViolation(
            call->getLocation(),
            "I/O operations can only be used in interaction functions, not in pure functions. " +
            std::string("Function '") + call->getName() + "' is an I/O operation."
        );
    }
}

bool SemanticChecker::isIOOperation(const std::string& name) {
    // Common I/O operation names
    static const std::vector<std::string> ioOperations = {
        "print", "println", "input", "readFile", "writeFile",
        "readLine", "writeLine", "read", "write", "open", "close"
    };
    
    return std::find(ioOperations.begin(), ioOperations.end(), name) != ioOperations.end();
}

void SemanticChecker::reportViolation(const SourceLocation& loc, const std::string& message) {
    errorReporter_.error(
        loc,
        "Semantic restriction violation: " + message
    );
}

} // namespace semantic
} // namespace first
