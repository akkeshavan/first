#include "first/semantic/type_checker.h"
#include "first/ast/types.h"
#include "first/ast/expressions.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_framework.h"

using namespace first;

TEST(type_checker_array_type_equality) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto intType1 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto intType2 = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    
    auto arrType1 = std::make_unique<ast::ArrayType>(
        loc, std::move(intType1)
    );
    auto arrType2 = std::make_unique<ast::ArrayType>(
        loc, std::move(intType2)
    );
    
    bool equal = checker.typesEqual(arrType1.get(), arrType2.get());
    ASSERT(equal, "Array types with same element type should be equal");
}

TEST(type_checker_array_type_inequality) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto intType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto floatType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Float
    );
    
    auto arrInt = std::make_unique<ast::ArrayType>(
        loc, std::move(intType)
    );
    auto arrFloat = std::make_unique<ast::ArrayType>(
        loc, std::move(floatType)
    );
    
    bool equal = checker.typesEqual(arrInt.get(), arrFloat.get());
    ASSERT(!equal, "Array types with different element types should not be equal");
}

TEST(type_checker_array_assignability) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto intType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    auto arrInt1 = std::make_unique<ast::ArrayType>(
        loc, std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int)
    );
    auto arrInt2 = std::make_unique<ast::ArrayType>(
        loc, std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int)
    );
    
    // Same array types should be assignable
    bool assignable1 = checker.isAssignable(arrInt1.get(), arrInt2.get());
    ASSERT(assignable1, "Same array types should be assignable");
    
    // Array<Int> and Array<Float> should not be assignable (no element promotion for arrays)
    auto arrFloat = std::make_unique<ast::ArrayType>(
        loc, std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float)
    );
    bool assignable2 = checker.isAssignable(arrInt1.get(), arrFloat.get());
    ASSERT(!assignable2, "Array<Int> should not be assignable to Array<Float>");
}

TEST(type_checker_nested_array_types) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    // Array<Array<Int>>
    auto innerArray = std::make_unique<ast::ArrayType>(
        loc, std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int)
    );
    auto outerArray1 = std::make_unique<ast::ArrayType>(
        loc, std::move(innerArray)
    );
    
    auto innerArray2 = std::make_unique<ast::ArrayType>(
        loc, std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int)
    );
    auto outerArray2 = std::make_unique<ast::ArrayType>(
        loc, std::move(innerArray2)
    );
    
    bool equal = checker.typesEqual(outerArray1.get(), outerArray2.get());
    ASSERT(equal, "Nested array types should be equal if element types match");
}

TEST(type_checker_generic_type_equality) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    auto genT1 = std::make_unique<ast::GenericType>(loc, "T");
    auto genT2 = std::make_unique<ast::GenericType>(loc, "T");
    auto genU = std::make_unique<ast::GenericType>(loc, "U");
    
    bool equal1 = checker.typesEqual(genT1.get(), genT2.get());
    ASSERT(equal1, "Generic types with same name should be equal");
    
    bool equal2 = checker.typesEqual(genT1.get(), genU.get());
    ASSERT(!equal2, "Generic types with different names should not be equal");
}

TEST(type_checker_parameterized_type_equality) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Option<Int>
    std::vector<std::unique_ptr<ast::Type>> args1;
    args1.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto optInt1 = std::make_unique<ast::ParameterizedType>(
        loc, "Option", std::move(args1)
    );
    
    std::vector<std::unique_ptr<ast::Type>> args2;
    args2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    auto optInt2 = std::make_unique<ast::ParameterizedType>(
        loc, "Option", std::move(args2)
    );
    
    bool equal = checker.typesEqual(optInt1.get(), optInt2.get());
    ASSERT(equal, "Parameterized types with same base name and args should be equal");
    
    // Option<Float>
    std::vector<std::unique_ptr<ast::Type>> args3;
    args3.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Float));
    auto optFloat = std::make_unique<ast::ParameterizedType>(
        loc, "Option", std::move(args3)
    );
    
    bool notEqual = checker.typesEqual(optInt1.get(), optFloat.get());
    ASSERT(!notEqual, "Parameterized types with different type args should not be equal");
}

TEST(type_checker_parameterized_type_multiple_args) {
    ErrorReporter reporter;
    semantic::TypeChecker checker(reporter);
    
    SourceLocation loc(1, 1);
    
    // Result<Int, String>
    std::vector<std::unique_ptr<ast::Type>> args1;
    args1.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    args1.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::String));
    auto result1 = std::make_unique<ast::ParameterizedType>(
        loc, "Result", std::move(args1)
    );
    
    std::vector<std::unique_ptr<ast::Type>> args2;
    args2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    args2.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::String));
    auto result2 = std::make_unique<ast::ParameterizedType>(
        loc, "Result", std::move(args2)
    );
    
    bool equal = checker.typesEqual(result1.get(), result2.get());
    ASSERT(equal, "Parameterized types with multiple args should compare correctly");
    
    // Different second arg
    std::vector<std::unique_ptr<ast::Type>> args3;
    args3.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Int));
    args3.push_back(std::make_unique<ast::PrimitiveType>(loc, ast::PrimitiveType::Kind::Bool));
    auto result3 = std::make_unique<ast::ParameterizedType>(
        loc, "Result", std::move(args3)
    );
    
    bool notEqual = checker.typesEqual(result1.get(), result3.get());
    ASSERT(!notEqual, "Parameterized types with different second arg should not be equal");
}
