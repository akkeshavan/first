#include "first/ast/node.h"
#include "first/ast/expressions.h"
#include "first/source_location.h"
#include "test_framework.h"

TEST(ast_base_node) {
    first::SourceLocation loc(1, 1, "test.first");
    
    // Test that we can create a literal expression
    first::ast::LiteralExpr literal(
        loc, 
        first::ast::LiteralExpr::LiteralType::Int, 
        "42"
    );
    
    ASSERT_EQ(literal.getLocation().getLine(), 1, "Line should be 1");
    ASSERT_EQ(literal.getLocation().getColumn(), 1, "Column should be 1");
    bool isInt = (literal.getType() == first::ast::LiteralExpr::LiteralType::Int);
    ASSERT(isInt, "Type should be Int");
    ASSERT_EQ(literal.getValue(), "42", "Value should be 42");
    ASSERT_EQ(literal.getNodeType(), "LiteralExpr", "Node type should be LiteralExpr");
}

TEST(ast_literal_types) {
    first::SourceLocation loc(1, 1);
    
    // Test Int literal
    first::ast::LiteralExpr intLit(loc, first::ast::LiteralExpr::LiteralType::Int, "42");
    bool isInt = (intLit.getType() == first::ast::LiteralExpr::LiteralType::Int);
    ASSERT(isInt, "Should be Int type");
    
    // Test Float literal
    first::ast::LiteralExpr floatLit(loc, first::ast::LiteralExpr::LiteralType::Float, "3.14");
    bool isFloat = (floatLit.getType() == first::ast::LiteralExpr::LiteralType::Float);
    ASSERT(isFloat, "Should be Float type");
    
    // Test Bool literal
    first::ast::LiteralExpr boolLit(loc, first::ast::LiteralExpr::LiteralType::Bool, "true");
    bool isBool = (boolLit.getType() == first::ast::LiteralExpr::LiteralType::Bool);
    ASSERT(isBool, "Should be Bool type");
    
    // Test String literal
    first::ast::LiteralExpr stringLit(loc, first::ast::LiteralExpr::LiteralType::String, "hello");
    bool isString = (stringLit.getType() == first::ast::LiteralExpr::LiteralType::String);
    ASSERT(isString, "Should be String type");
}

TEST(ast_binary_expr) {
    first::SourceLocation loc(1, 1);
    
    auto left = std::make_unique<first::ast::LiteralExpr>(
        loc, first::ast::LiteralExpr::LiteralType::Int, "10"
    );
    auto right = std::make_unique<first::ast::LiteralExpr>(
        loc, first::ast::LiteralExpr::LiteralType::Int, "20"
    );
    
    first::ast::BinaryExpr add(
        loc,
        first::ast::BinaryExpr::Op::Add,
        std::move(left),
        std::move(right)
    );
    
    // Compare enum values directly (can't use ASSERT_EQ with enums in our framework)
    bool opIsAdd = (add.getOp() == first::ast::BinaryExpr::Op::Add);
    ASSERT(opIsAdd, "Operator should be Add");
    ASSERT_NE(add.getLeft(), nullptr, "Left operand should not be null");
    ASSERT_NE(add.getRight(), nullptr, "Right operand should not be null");
}

TEST(ast_variable_expr) {
    first::SourceLocation loc(1, 1);
    
    first::ast::VariableExpr var(loc, "x");
    
    ASSERT_EQ(var.getName(), "x", "Variable name should be 'x'");
    ASSERT_EQ(var.getNodeType(), "VariableExpr", "Node type should be VariableExpr");
}

TEST(ast_function_call_expr) {
    first::SourceLocation loc(1, 1);
    
    std::vector<std::unique_ptr<first::ast::Expr>> args;
    args.push_back(std::make_unique<first::ast::LiteralExpr>(
        loc, first::ast::LiteralExpr::LiteralType::Int, "42"
    ));
    
    first::ast::FunctionCallExpr call(loc, "print", std::move(args));
    
    ASSERT_EQ(call.getName(), "print", "Function name should be 'print'");
    ASSERT_EQ(call.getArgs().size(), 1, "Should have 1 argument");
}
