#include "first/ast/builder.h"
#include "first/error_reporter.h"
#include "FirstLexer.h"
#include "FirstParser.h"
#include <antlr4-runtime.h>
#include "test_framework.h"

using namespace first;

TEST(ast_builder_literal_int) {
    first::ErrorReporter reporter;
    first::ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "42";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    // Check if it's a literal
    auto* literal = dynamic_cast<first::ast::LiteralExpr*>(astExpr.get());
    ASSERT_NE(literal, nullptr, "Should be a literal expression");
    
    bool isInt = (literal->getType() == first::ast::LiteralExpr::LiteralType::Int);
    ASSERT(isInt, "Should be Int literal");
    ASSERT_EQ(literal->getValue(), "42", "Value should be 42");
}

TEST(ast_builder_literal_string) {
    first::ErrorReporter reporter;
    first::ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "\"hello\"";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    auto* literal = dynamic_cast<first::ast::LiteralExpr*>(astExpr.get());
    ASSERT_NE(literal, nullptr, "Should be a literal expression");
    
    bool isString = (literal->getType() == first::ast::LiteralExpr::LiteralType::String);
    ASSERT(isString, "Should be String literal");
    ASSERT_EQ(literal->getValue(), "hello", "Value should be 'hello'");
}

TEST(ast_builder_variable_expr) {
    first::ErrorReporter reporter;
    first::ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "x";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* exprCtx = parser.expression();
    
    auto astExpr = builder.buildExpression(exprCtx);
    
    ASSERT_NE(astExpr, nullptr, "AST expression should not be null");
    
    auto* varExpr = dynamic_cast<first::ast::VariableExpr*>(astExpr.get());
    ASSERT_NE(varExpr, nullptr, "Should be a variable expression");
    ASSERT_EQ(varExpr->getName(), "x", "Variable name should be 'x'");
}

TEST(ast_builder_variable_decl) {
    first::ErrorReporter reporter;
    first::ast::ASTBuilder builder(reporter, "test.first");
    
    std::string source = "let x: Int = 42;";
    antlr4::ANTLRInputStream input(source);
    FirstLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    FirstParser parser(&tokens);
    auto* varDeclCtx = parser.varDecl();
    
    auto astNode = builder.buildVariableDeclNode(varDeclCtx);
    
    ASSERT_NE(astNode, nullptr, "AST node should not be null");
    
    auto* varDecl = dynamic_cast<first::ast::VariableDecl*>(astNode.get());
    ASSERT_NE(varDecl, nullptr, "Should be a variable declaration");
    ASSERT_EQ(varDecl->getName(), "x", "Variable name should be 'x'");
    
    bool isImmutable = (varDecl->getMutability() == first::ast::VariableDecl::Mutability::Immutable);
    ASSERT(isImmutable, "Should be immutable");
    ASSERT_NE(varDecl->getType(), nullptr, "Type should not be null");
    ASSERT_NE(varDecl->getInitializer(), nullptr, "Initializer should not be null");
}
