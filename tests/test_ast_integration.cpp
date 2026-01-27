#include "first/compiler.h"
#include "first/error_reporter.h"
#include "first/ast/program.h"
#include "test_framework.h"

TEST(ast_integration_simple_program) {
    first::Compiler compiler;
    
    std::string source = "let x = 42;";
    bool result = compiler.compileFromString(source, "test.first");
    
    ASSERT(result, "Compilation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "Should have no errors");
    
    auto* ast = compiler.getAST();
    ASSERT_NE(ast, nullptr, "AST should not be null");
}

TEST(ast_integration_multiple_statements) {
    first::Compiler compiler;
    
    std::string source = "let x = 42;\nlet y = 10;";
    bool result = compiler.compileFromString(source, "test.first");
    
    ASSERT(result, "Compilation should succeed");
    ASSERT_NE(compiler.getAST(), nullptr, "AST should not be null");
}

TEST(ast_integration_with_errors) {
    first::Compiler compiler;
    
    std::string source = "let x = 42\nlet y = 10;"; // Missing semicolon
    bool result = compiler.compileFromString(source, "test.first");
    
    ASSERT(!result, "Compilation should fail");
    ASSERT(compiler.getErrorReporter().hasErrors(), "Should have errors");
    // AST might be null or partial when there are errors
}
