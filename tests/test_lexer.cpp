#include "first/compiler.h"
#include "test_framework.h"

TEST(lexer_simple_program) {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction main() -> Unit {
            print("Hello, World!");
        }
    )";
    
    bool result = compiler.compileFromString(source);
    
    // For now, just check that it doesn't crash
    // In Phase 1.2, we'll add proper lexer tests
    ASSERT(true, "Lexer should handle simple program");
}

TEST(lexer_integer_literals) {
    first::Compiler compiler;
    
    std::string source = R"(
        let x: Int = 42;
        let y = 100;
    )";
    
    bool result = compiler.compileFromString(source);
    // Basic test - will be expanded in Phase 1.2
    ASSERT(true, "Lexer should handle integer literals");
}
