#include "first/semantic/semantic_checker.h"
#include "first/ast/declarations.h"
#include "first/ast/statements.h"
#include "first/ast/expressions.h"
#include "first/ast/types.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_framework.h"

using namespace first;

TEST(semantic_checker_mutable_var_in_pure_function) {
    ErrorReporter reporter;
    semantic::SemanticChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create a pure function with mutable variable (should fail)
    std::vector<std::unique_ptr<ast::Parameter>> params;
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    
    std::vector<std::unique_ptr<ast::Stmt>> body;
    auto mutableVar = std::make_unique<ast::VariableDecl>(
        loc, "x", ast::VariableDecl::Mutability::Mutable,
        std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int),
        std::make_unique<ast::LiteralExpr>(loc, ast::LiteralExpr::LiteralType::Int, "10")
    );
    body.push_back(std::move(mutableVar));
    
    auto func = std::make_unique<ast::FunctionDecl>(
        loc, "test", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    checker.checkFunction(func.get());
    
    // Should have reported an error
    ASSERT(reporter.hasErrors(), "Should report error for mutable variable in pure function");
}

TEST(semantic_checker_mutable_var_in_interaction) {
    ErrorReporter reporter;
    semantic::SemanticChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create an interaction with mutable variable (should be OK)
    std::vector<std::unique_ptr<ast::Parameter>> params;
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    std::vector<std::unique_ptr<ast::Stmt>> body;
    
    auto interaction = std::make_unique<ast::InteractionDecl>(
        loc, "test", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    checker.checkInteraction(interaction.get());
    
    // Should NOT have reported an error (interactions allow mutable variables)
    ASSERT(!reporter.hasErrors(), "Should not report error for mutable variable in interaction");
}

TEST(semantic_checker_io_in_pure_function) {
    ErrorReporter reporter;
    semantic::SemanticChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create a pure function with I/O operation (should fail)
    std::vector<std::unique_ptr<ast::Parameter>> params;
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Unit);
    
    std::vector<std::unique_ptr<ast::Stmt>> body;
    std::vector<std::unique_ptr<ast::Expr>> args;
    args.push_back(std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::String, "Hello"
    ));
    auto printCall = std::make_unique<ast::FunctionCallExpr>(loc, "print", std::move(args));
    body.push_back(std::make_unique<ast::ExprStmt>(loc, std::move(printCall)));
    
    auto func = std::make_unique<ast::FunctionDecl>(
        loc, "test", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    checker.checkFunction(func.get());
    
    // Should have reported an error
    ASSERT(reporter.hasErrors(), "Should report error for I/O operation in pure function");
}

TEST(semantic_checker_io_in_interaction) {
    ErrorReporter reporter;
    semantic::SemanticChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create an interaction with I/O operation (should be OK)
    std::vector<std::unique_ptr<ast::Parameter>> params;
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Unit);
    
    std::vector<std::unique_ptr<ast::Stmt>> body;
    std::vector<std::unique_ptr<ast::Expr>> args;
    args.push_back(std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::String, "Hello"
    ));
    auto printCall = std::make_unique<ast::FunctionCallExpr>(loc, "print", std::move(args));
    body.push_back(std::make_unique<ast::ExprStmt>(loc, std::move(printCall)));
    
    auto interaction = std::make_unique<ast::InteractionDecl>(
        loc, "test", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    checker.checkInteraction(interaction.get());
    
    // Should NOT have reported an error
    ASSERT(!reporter.hasErrors(), "Should not report error for I/O operation in interaction");
}

TEST(semantic_checker_immutable_var_in_pure_function) {
    ErrorReporter reporter;
    semantic::SemanticChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create a pure function with immutable variable (should be OK)
    std::vector<std::unique_ptr<ast::Parameter>> params;
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    std::vector<std::unique_ptr<ast::Stmt>> body;
    
    auto func = std::make_unique<ast::FunctionDecl>(
        loc, "test", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    checker.checkFunction(func.get());
    
    // Should NOT have reported an error
    ASSERT(!reporter.hasErrors(), "Should not report error for immutable variable in pure function");
}
