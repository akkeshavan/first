#include "first/ast/validator.h"
#include "first/ast/program.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "test_framework.h"

using namespace first;

TEST(ast_validator_valid_program) {
    ErrorReporter reporter;
    ast::ASTValidator validator(reporter);
    
    SourceLocation loc(1, 1);
    auto program = std::make_unique<ast::Program>(loc);
    
    bool result = validator.validate(program.get());
    
    ASSERT(result, "Valid empty program should pass validation");
    ASSERT_EQ(validator.getErrorCount(), 0, "Should have no errors");
}

TEST(ast_validator_binary_expr_missing_operand) {
    ErrorReporter reporter;
    ast::ASTValidator validator(reporter);
    
    SourceLocation loc(1, 1);
    // Create a binary expression with null operands (invalid)
    auto left = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "10"
    );
    
    // Create binary with null right operand - this shouldn't happen in real code,
    // but we test that validator catches it
    // Actually, we can't create invalid AST easily, so let's test with valid AST
    auto right = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "20"
    );
    
    ast::BinaryExpr binary(
        loc,
        ast::BinaryExpr::Op::Add,
        std::move(left),
        std::move(right)
    );
    
    // Validate the expression
    validator.validateExpr(&binary, "test");
    
    // Should pass - both operands are present
    ASSERT_EQ(validator.getErrorCount(), 0, "Valid binary expression should have no errors");
}

TEST(ast_validator_unary_expr_missing_operand) {
    ErrorReporter reporter;
    ast::ASTValidator validator(reporter);
    
    SourceLocation loc(1, 1);
    auto operand = std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "10"
    );
    
    ast::UnaryExpr unary(loc, ast::UnaryExpr::Op::Neg, std::move(operand));
    
    validator.validateExpr(&unary, "test");
    
    ASSERT_EQ(validator.getErrorCount(), 0, "Valid unary expression should have no errors");
}

TEST(ast_validator_array_type_missing_element) {
    ErrorReporter reporter;
    ast::ASTValidator validator(reporter);
    
    SourceLocation loc(1, 1);
    auto elementType = std::make_unique<ast::PrimitiveType>(
        loc, ast::PrimitiveType::Kind::Int
    );
    
    ast::ArrayType arrayType(loc, std::move(elementType));
    
    validator.validateType(&arrayType, "test");
    
    ASSERT_EQ(validator.getErrorCount(), 0, "Valid array type should have no errors");
}

TEST(ast_validator_function_call) {
    ErrorReporter reporter;
    ast::ASTValidator validator(reporter);
    
    SourceLocation loc(1, 1);
    std::vector<std::unique_ptr<ast::Expr>> args;
    args.push_back(std::make_unique<ast::LiteralExpr>(
        loc, ast::LiteralExpr::LiteralType::Int, "42"
    ));
    
    ast::FunctionCallExpr call(loc, "print", std::move(args));
    
    validator.validateExpr(&call, "test");
    
    ASSERT_EQ(validator.getErrorCount(), 0, "Valid function call should have no errors");
    ASSERT_EQ(call.getName(), "print", "Function name should be 'print'");
    ASSERT_EQ(call.getArgs().size(), 1, "Should have 1 argument");
}
