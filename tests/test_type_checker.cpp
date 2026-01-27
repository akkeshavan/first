#include "first/semantic/type_checker.h"
#include "first/ast/program.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_framework.h"

using namespace first;

TEST(type_checker_literal_int) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto literal = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "42"
    );
    
    ast::Type* type = checker.inferType(literal.get());
    ASSERT_NE(type, nullptr, "Should infer type for int literal");
    
    auto* primType = dynamic_cast<ast::PrimitiveType*>(type);
    ASSERT_NE(primType, nullptr, "Should be primitive type");
    bool isInt = (primType->getKind() == ast::PrimitiveType::Kind::Int);
    ASSERT(isInt, "Should be Int type");
}

TEST(type_checker_literal_float) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto literal = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Float, "3.14"
    );
    
    ast::Type* type = checker.inferType(literal.get());
    ASSERT_NE(type, nullptr, "Should infer type for float literal");
    
    auto* primType = dynamic_cast<ast::PrimitiveType*>(type);
    ASSERT_NE(primType, nullptr, "Should be primitive type");
    bool isFloat = (primType->getKind() == ast::PrimitiveType::Kind::Float);
    ASSERT(isFloat, "Should be Float type");
}

TEST(type_checker_binary_add_int) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto left = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "10"
    );
    auto right = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "20"
    );
    
    auto binary = std::make_unique<ast::BinaryExpr>(
        loc,
        ast::BinaryExpr::Op::Add,
        std::move(left),
        std::move(right)
    );
    
    ast::Type* type = checker.inferType(binary.get());
    ASSERT_NE(type, nullptr, "Should infer type for int addition");
    
    auto* primType = dynamic_cast<ast::PrimitiveType*>(type);
    ASSERT_NE(primType, nullptr, "Should be primitive type");
    bool isInt = (primType->getKind() == ast::PrimitiveType::Kind::Int);
    ASSERT(isInt, "Should be Int type");
}

TEST(type_checker_binary_add_float) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto left = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Float, "1.5"
    );
    auto right = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Float, "2.5"
    );
    
    auto binary = std::make_unique<ast::BinaryExpr>(
        loc,
        ast::BinaryExpr::Op::Add,
        std::move(left),
        std::move(right)
    );
    
    ast::Type* type = checker.inferType(binary.get());
    ASSERT_NE(type, nullptr, "Should infer type for float addition");
    
    auto* primType = dynamic_cast<ast::PrimitiveType*>(type);
    ASSERT_NE(primType, nullptr, "Should be primitive type");
    bool isFloat = (primType->getKind() == ast::PrimitiveType::Kind::Float);
    ASSERT(isFloat, "Should be Float type");
}

TEST(type_checker_binary_comparison) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto left = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "10"
    );
    auto right = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "20"
    );
    
    auto binary = std::make_unique<ast::BinaryExpr>(
        loc,
        ast::BinaryExpr::Op::Lt,
        std::move(left),
        std::move(right)
    );
    
    ast::Type* type = checker.inferType(binary.get());
    ASSERT_NE(type, nullptr, "Should infer type for comparison");
    
    auto* primType = dynamic_cast<ast::PrimitiveType*>(type);
    ASSERT_NE(primType, nullptr, "Should be primitive type");
    bool isBool = (primType->getKind() == ast::PrimitiveType::Kind::Bool);
    ASSERT(isBool, "Should be Bool type");
}

TEST(type_checker_unary_neg) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto operand = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "42"
    );
    
    auto unary = std::make_unique<ast::UnaryExpr>(
        loc,
        ast::UnaryExpr::Op::Neg,
        std::move(operand)
    );
    
    ast::Type* type = checker.inferType(unary.get());
    ASSERT_NE(type, nullptr, "Should infer type for negation");
    
    auto* primType = dynamic_cast<ast::PrimitiveType*>(type);
    ASSERT_NE(primType, nullptr, "Should be primitive type");
    bool isInt = (primType->getKind() == ast::PrimitiveType::Kind::Int);
    ASSERT(isInt, "Should be Int type");
}

TEST(type_checker_types_equal) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto type1 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto type2 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    
    bool equal = checker.typesEqual(type1.get(), type2.get());
    ASSERT(equal, "Same primitive types should be equal");
    
    auto type3 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Float
    );
    bool notEqual = checker.typesEqual(type1.get(), type3.get());
    ASSERT(!notEqual, "Different primitive types should not be equal");
}

TEST(type_checker_is_assignable) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto intType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto floatType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Float
    );
    
    // Int -> Int (same type)
    bool assignable1 = checker.isAssignable(intType.get(), intType.get());
    ASSERT(assignable1, "Int should be assignable to Int");
    
    // Int -> Float (promotion)
    bool assignable2 = checker.isAssignable(intType.get(), floatType.get());
    ASSERT(assignable2, "Int should be assignable to Float");
    
    // Float -> Int (not assignable)
    bool assignable3 = checker.isAssignable(floatType.get(), intType.get());
    ASSERT(!assignable3, "Float should not be assignable to Int");
}
