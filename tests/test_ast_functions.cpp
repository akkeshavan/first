#include "first/ast/declarations.h"
#include "first/ast/types.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/source_location.h"
#include "test_framework.h"

TEST(ast_parameter) {
    first::SourceLocation loc(1, 1);
    
    auto paramType = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Int
    );
    
    first::ast::Parameter param(loc, "x", std::move(paramType));
    
    ASSERT_EQ(param.getName(), "x", "Parameter name should be 'x'");
    ASSERT_NE(param.getType(), nullptr, "Parameter type should not be null");
}

TEST(ast_function_decl) {
    first::SourceLocation loc(1, 1);
    
    std::vector<std::string> genericParams;
    std::vector<std::unique_ptr<first::ast::Parameter>> parameters;
    
    auto paramType = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Int
    );
    parameters.push_back(std::make_unique<first::ast::Parameter>(
        loc, "x", std::make_unique<first::ast::PrimitiveType>(
            loc, first::ast::PrimitiveType::Kind::Int
        )
    ));
    
    auto returnType = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Int
    );
    
    std::vector<std::unique_ptr<first::ast::Stmt>> body;
    auto returnStmt = std::make_unique<first::ast::ReturnStmt>(
        loc, std::make_unique<first::ast::VariableExpr>(loc, "x")
    );
    body.push_back(std::move(returnStmt));
    
    first::ast::FunctionDecl func(
        loc,
        "add",
        std::move(genericParams),
        std::move(parameters),
        std::move(returnType),
        std::move(body)
    );
    
    ASSERT_EQ(func.getName(), "add", "Function name should be 'add'");
    ASSERT_EQ(func.getParameters().size(), 1, "Should have 1 parameter");
    ASSERT_NE(func.getReturnType(), nullptr, "Return type should not be null");
    ASSERT_EQ(func.getBody().size(), 1, "Should have 1 statement");
    ASSERT(!func.isSignature(), "Should not be a signature");
}

TEST(ast_function_signature) {
    first::SourceLocation loc(1, 1);
    
    std::vector<std::unique_ptr<first::ast::Parameter>> parameters;
    auto returnType = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Int
    );
    std::vector<std::unique_ptr<first::ast::Stmt>> body; // Empty body = signature
    
    first::ast::FunctionDecl func(
        loc,
        "forwardDecl",
        std::vector<std::string>(),
        std::move(parameters),
        std::move(returnType),
        std::move(body)
    );
    
    ASSERT(func.isSignature(), "Should be a signature");
    ASSERT_EQ(func.getBody().size(), 0, "Signature should have no body");
}

TEST(ast_interaction_decl) {
    first::SourceLocation loc(1, 1);
    
    std::vector<std::unique_ptr<first::ast::Parameter>> parameters;
    auto returnType = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Unit
    );
    
    std::vector<std::unique_ptr<first::ast::Stmt>> body;
    auto exprStmt = std::make_unique<first::ast::ExprStmt>(
        loc, std::make_unique<first::ast::FunctionCallExpr>(
            loc, "print", std::vector<std::unique_ptr<first::ast::Expr>>()
        )
    );
    body.push_back(std::move(exprStmt));
    
    first::ast::InteractionDecl interaction(
        loc,
        "main",
        std::vector<std::string>(),
        std::move(parameters),
        std::move(returnType),
        std::move(body)
    );
    
    ASSERT_EQ(interaction.getName(), "main", "Interaction name should be 'main'");
    ASSERT_EQ(interaction.getBody().size(), 1, "Should have 1 statement");
}

TEST(ast_function_with_generics) {
    first::SourceLocation loc(1, 1);
    
    std::vector<std::string> genericParams = {"T"};
    std::vector<std::unique_ptr<first::ast::Parameter>> parameters;
    parameters.push_back(std::make_unique<first::ast::Parameter>(
        loc, "x", std::make_unique<first::ast::GenericType>(loc, "T")
    ));
    
    auto returnType = std::make_unique<first::ast::GenericType>(loc, "T");
    std::vector<std::unique_ptr<first::ast::Stmt>> body;
    
    first::ast::FunctionDecl func(
        loc,
        "identity",
        std::move(genericParams),
        std::move(parameters),
        std::move(returnType),
        std::move(body)
    );
    
    ASSERT_EQ(func.getGenericParams().size(), 1, "Should have 1 generic parameter");
    ASSERT_EQ(func.getGenericParams()[0], "T", "Generic parameter should be 'T'");
}
