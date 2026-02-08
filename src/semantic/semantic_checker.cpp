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
    } else if (auto* selectStmt = dynamic_cast<ast::SelectStmt*>(stmt)) {
        for (const auto& branch : selectStmt->getBranches()) {
            if (branch->getStatement()) {
                checkStatement(branch->getStatement());
            }
        }
    } else if (auto* forIn = dynamic_cast<ast::ForInStmt*>(stmt)) {
        if (inPureFunction_) {
            errorReporter_.error(
                forIn->getLocation(),
                "for-in loops are only allowed in interaction functions"
            );
        }
        checkExpression(forIn->getIterable());
        for (const auto& s : forIn->getBody()) {
            checkStatement(s.get());
        }
    }
}

void SemanticChecker::checkExpression(ast::Expr* expr) {
    if (!expr) return;
    
    if (auto* call = dynamic_cast<ast::FunctionCallExpr*>(expr)) {
        checkIOOperation(call);
        checkMonadicOperators(expr);
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
    } else if (auto* asyncExpr = dynamic_cast<ast::AsyncExpr*>(expr)) {
        checkExpression(asyncExpr->getOperand());
    } else if (auto* awaitExpr = dynamic_cast<ast::AwaitExpr*>(expr)) {
        checkExpression(awaitExpr->getOperand());
    } else if (auto* spawnExpr = dynamic_cast<ast::SpawnExpr*>(expr)) {
        checkExpression(spawnExpr->getOperand());
    } else if (auto* joinExpr = dynamic_cast<ast::JoinExpr*>(expr)) {
        checkExpression(joinExpr->getOperand());
    } else if (auto* selectExpr = dynamic_cast<ast::SelectExpr*>(expr)) {
        for (const auto& branch : selectExpr->getBranches()) {
            if (branch->getStatement()) {
                checkStatement(branch->getStatement());
            }
        }
    } else if (auto* blockExpr = dynamic_cast<ast::BlockExpr*>(expr)) {
        for (const auto& stmt : blockExpr->getStatements()) {
            checkStatement(stmt.get());
        }
        if (blockExpr->getValueExpr()) {
            checkExpression(blockExpr->getValueExpr());
        }
    } else if (auto* ifExpr = dynamic_cast<ast::IfExpr*>(expr)) {
        checkExpression(ifExpr->getCondition());
        if (auto* thenBlock = dynamic_cast<ast::BlockExpr*>(ifExpr->getThenBranch())) {
            checkNoReturnInIfBranchBlock(thenBlock);
        }
        if (auto* elseBlock = dynamic_cast<ast::BlockExpr*>(ifExpr->getElseBranch())) {
            checkNoReturnInIfBranchBlock(elseBlock);
        }
        checkExpression(ifExpr->getThenBranch());
        checkExpression(ifExpr->getElseBranch());
    } else if (auto* rangeExpr = dynamic_cast<ast::RangeExpr*>(expr)) {
        checkExpression(rangeExpr->getStart());
        checkExpression(rangeExpr->getEnd());
        if (rangeExpr->hasStepHint()) {
            checkExpression(rangeExpr->getStepHint());
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
    (void)stmt;
}

void SemanticChecker::checkMonadicOperators(ast::Expr* expr) {
    if (!inPureFunction_ || !expr) return;
    
    // After parser desugaring, monadic operators are represented as ordinary
    // function calls to bind/then/fmap/apply. We treat those specially in
    // pure functions and report a semantic restriction violation.
    auto* call = dynamic_cast<ast::FunctionCallExpr*>(expr);
    if (!call) return;

    const std::string& name = call->getName();
    if (name == "bind" || name == "then" ||
        name == "fmap" || name == "apply") {
        reportViolation(
            call->getLocation(),
            "Monadic operators (`>>=`, `>>`, `<$>`, `<*>`) can only be used in interaction functions, not in pure functions"
        );
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

void SemanticChecker::checkNoReturnInIfBranchBlock(ast::BlockExpr* block) {
    if (!block) return;
    for (const auto& stmt : block->getStatements()) {
        if (dynamic_cast<ast::ReturnStmt*>(stmt.get())) {
            reportViolation(
                stmt->getLocation(),
                "return is not allowed inside if-expression branches; "
                "return the result of the whole if-expression instead "
                "(e.g. return if (...) { ... } else { ... };)"
            );
        }
    }
}

void SemanticChecker::reportViolation(const SourceLocation& loc, const std::string& message) {
    errorReporter_.error(
        loc,
        "Semantic restriction violation: " + message
    );
}

} // namespace semantic
} // namespace first
