#include "first/semantic/type_checker.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_framework.h"

using namespace first;

TEST(type_checker_function_type_equality) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // function(Int, Int) -> Int
    std::vector<std::unique_ptr<ast::Type>> params1;
    params1.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    params1.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto returnType1 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto funcType1 = std::make_unique<ast::FunctionType>(
        loc, std::move(params1), std::move(returnType1), false
    );
    
    std::vector<std::unique_ptr<ast::Type>> params2;
    params2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    params2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto returnType2 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto funcType2 = std::make_unique<ast::FunctionType>(
        loc, std::move(params2), std::move(returnType2), false
    );
    
    bool equal = checker.typesEqual(funcType1.get(), funcType2.get());
    ASSERT(equal, "Function types with same signature should be equal");
}

TEST(type_checker_function_vs_interaction_type) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // function(Int) -> Int
    std::vector<std::unique_ptr<ast::Type>> params1;
    params1.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto returnType1 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto funcType = std::make_unique<ast::FunctionType>(
        loc, std::move(params1), std::move(returnType1), false
    );
    
    // interaction(Int) -> Int
    std::vector<std::unique_ptr<ast::Type>> params2;
    params2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto returnType2 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto interactionType = std::make_unique<ast::FunctionType>(
        loc, std::move(params2), std::move(returnType2), true
    );
    
    bool equal = checker.typesEqual(funcType.get(), interactionType.get());
    ASSERT(!equal, "Function type and interaction type should not be equal");
}

TEST(type_checker_function_call_argument_matching) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create a function: function add(x: Int, y: Int) -> Int
    std::vector<std::unique_ptr<ast::Parameter>> params;
    params.push_back(std::make_unique<ast::Parameter>(
        loc, "x", std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int)
    ));
    params.push_back(std::make_unique<ast::Parameter>(
        loc, "y", std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int)
    ));
    
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    std::vector<std::unique_ptr<ast::Stmt>> body;
    
    auto func = std::make_unique<ast::FunctionDecl>(
        loc, "add", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    // Test matching: add(10, 20) - should match
    std::vector<ast::Type*> argTypes;
    auto intType1 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto intType2 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    argTypes.push_back(intType1.get());
    argTypes.push_back(intType2.get());
    
    bool matches = checker.matchFunctionSignature(func.get(), argTypes);
    ASSERT(matches, "Function call with matching argument types should match");
    
    // Test non-matching: add(10) - wrong number of args
    std::vector<ast::Type*> argTypes2;
    auto intType3 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    argTypes2.push_back(intType3.get());
    
    bool matches2 = checker.matchFunctionSignature(func.get(), argTypes2);
    ASSERT(!matches2, "Function call with wrong number of arguments should not match");
    
    // Test non-matching: add(10.0, 20) - wrong type
    std::vector<ast::Type*> argTypes3;
    auto floatType1 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float);
    auto intType4 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    argTypes3.push_back(floatType1.get());
    argTypes3.push_back(intType4.get());
    
    bool matches3 = checker.matchFunctionSignature(func.get(), argTypes3);
    ASSERT(!matches3, "Function call with incompatible argument types should not match");
}

TEST(type_checker_function_call_with_promotion) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Create a function: function add(x: Float, y: Float) -> Float
    std::vector<std::unique_ptr<ast::Parameter>> params;
    params.push_back(std::make_unique<ast::Parameter>(
        loc, "x", std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float)
    ));
    params.push_back(std::make_unique<ast::Parameter>(
        loc, "y", std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float)
    ));
    
    auto returnType = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float);
    std::vector<std::unique_ptr<ast::Stmt>> body;
    
    auto func = std::make_unique<ast::FunctionDecl>(
        loc, "add", std::vector<std::string>(), std::move(params),
        std::move(returnType), std::move(body)
    );
    
    // Test: add(10, 20) - Int should be promotable to Float
    std::vector<ast::Type*> argTypes;
    auto intType1 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto intType2 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    argTypes.push_back(intType1.get());
    argTypes.push_back(intType2.get());
    
    bool matches = checker.matchFunctionSignature(func.get(), argTypes);
    ASSERT(matches, "Function call with Int args should match Float parameters (promotion)");
}

TEST(type_checker_higher_order_function_type) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // function(Int) -> Int
    std::vector<std::unique_ptr<ast::Type>> innerParams;
    innerParams.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto innerReturn = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto innerFuncType = std::make_unique<ast::FunctionType>(
        loc, std::move(innerParams), std::move(innerReturn), false
    );
    
    // function(function(Int) -> Int, Int) -> Int
    std::vector<std::unique_ptr<ast::Type>> outerParams;
    outerParams.push_back(checker.copyType(innerFuncType.get()));
    outerParams.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto outerReturn = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto outerFuncType = std::make_unique<ast::FunctionType>(
        loc, std::move(outerParams), std::move(outerReturn), false
    );
    
    // Test equality
    std::vector<std::unique_ptr<ast::Type>> outerParams2;
    outerParams2.push_back(checker.copyType(innerFuncType.get()));
    outerParams2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto outerReturn2 = std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int);
    auto outerFuncType2 = std::make_unique<ast::FunctionType>(
        loc, std::move(outerParams2), std::move(outerReturn2), false
    );
    
    bool equal = checker.typesEqual(outerFuncType.get(), outerFuncType2.get());
    ASSERT(equal, "Higher-order function types should be equal if signatures match");
}
