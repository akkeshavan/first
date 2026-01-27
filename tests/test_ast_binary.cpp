#include "first/ast/builder.h"
#include "first/ast/expressions.h"
#include "first/error_reporter.h"
#include "FirstLexer.h"
#include "FirstParser.h"
#include <antlr4-runtime.h>
#include "test_framework.h"

using namespace first;

TEST(ast_builder_binary_add) {
    ErrorReporter reporter;
    ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "10 + 20";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    auto* binary = dynamic_cast<ast::BinaryExpr*>(astExpr.get());
    ASSERT_NE(binary, nullptr, "Should be a binary expression");
    
    bool isAdd = (binary->getOp() == ast::BinaryExpr::Op::Add);
    ASSERT(isAdd, "Operator should be Add");
    ASSERT_NE(binary->getLeft(), nullptr, "Left operand should not be null");
    ASSERT_NE(binary->getRight(), nullptr, "Right operand should not be null");
}

TEST(ast_builder_binary_multiply) {
    ErrorReporter reporter;
    ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "5 * 3";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    auto* binary = dynamic_cast<ast::BinaryExpr*>(astExpr.get());
    ASSERT_NE(binary, nullptr, "Should be a binary expression");
    
    bool isMul = (binary->getOp() == ast::BinaryExpr::Op::Mul);
    ASSERT(isMul, "Operator should be Multiply");
}

TEST(ast_builder_binary_chain) {
    ErrorReporter reporter;
    ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "1 + 2 + 3";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    // Should be left-associative: ((1 + 2) + 3)
    auto* outer = dynamic_cast<ast::BinaryExpr*>(astExpr.get());
    ASSERT_NE(outer, nullptr, "Outer expression should be binary");
    
    bool isAdd = (outer->getOp() == ast::BinaryExpr::Op::Add);
    ASSERT(isAdd, "Outer operator should be Add");
    
    // Left side should also be a binary expression
    auto* inner = dynamic_cast<ast::BinaryExpr*>(outer->getLeft());
    ASSERT_NE(inner, nullptr, "Left side should be binary expression");
}

TEST(ast_builder_binary_precedence) {
    ErrorReporter reporter;
    ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "2 + 3 * 4";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    // Due to precedence, should be: (2 + (3 * 4))
    // The parser already handles precedence, so the AST should reflect it
    auto* outer = dynamic_cast<ast::BinaryExpr*>(astExpr.get());
    ASSERT_NE(outer, nullptr, "Should be a binary expression");
    
    // The right side should be a multiplication
    auto* right = dynamic_cast<ast::BinaryExpr*>(outer->getRight());
    ASSERT_NE(right, nullptr, "Right side should be multiplication");
    
    bool isMul = (right->getOp() == ast::BinaryExpr::Op::Mul);
    ASSERT(isMul, "Right side should be multiplication");
}

TEST(ast_builder_binary_equality) {
    ErrorReporter reporter;
    ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "x == y";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    auto* binary = dynamic_cast<ast::BinaryExpr*>(astExpr.get());
    ASSERT_NE(binary, nullptr, "Should be a binary expression");
    
    bool isEq = (binary->getOp() == ast::BinaryExpr::Op::Eq);
    ASSERT(isEq, "Operator should be equality");
}

TEST(ast_builder_binary_logical) {
    ErrorReporter reporter;
    ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "x && y";
    antlr4::ANTLRInputStream input(source);
    antlr4::ANTLRInputStream input2(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    auto* binary = dynamic_cast<ast::BinaryExpr*>(astExpr.get());
    ASSERT_NE(binary, nullptr, "Should be a binary expression");
    
    bool isAnd = (binary->getOp() == ast::BinaryExpr::Op::And);
    ASSERT(isAnd, "Operator should be logical AND");
}
