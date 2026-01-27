#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <cassert>
#include <iostream>

void test_simple_function() {
    first::Compiler compiler;
    
    std::string source = R"(
        function main() -> Int {
            return 42;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_simple.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    // Generate IR
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    // Print IR
    std::cout << "=== Generated IR ===" << std::endl;
    irGen.printIR();
    std::cout << "===================" << std::endl;
    
    std::cout << "✓ Simple function IR generation works\n";
}

void test_arithmetic() {
    first::Compiler compiler;
    
    std::string source = R"(
        function add(x: Int, y: Int) -> Int {
            return x + y;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_arithmetic.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    std::cout << "=== Arithmetic Function IR ===" << std::endl;
    irGen.printIR();
    std::cout << "=============================" << std::endl;
    
    std::cout << "✓ Arithmetic IR generation works\n";
}

void test_variables() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let x = 10;
            let y = 20;
            return x + y;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_variables.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    std::cout << "=== Variables IR ===" << std::endl;
    irGen.printIR();
    std::cout << "====================" << std::endl;
    
    std::cout << "✓ Variable IR generation works\n";
}

// Test functions are called from test_main.cpp
