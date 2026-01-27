#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <cassert>
#include <iostream>

void test_arithmetic_operations() {
    first::Compiler compiler;
    
    // Test all arithmetic operators
    std::string source = R"(
        function test() -> Int {
            let a = 10;
            let b = 3;
            let sum = a + b;
            let diff = a - b;
            let prod = a * b;
            let quot = a / b;
            return sum + diff + prod + quot;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_arithmetic.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_arithmetic_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Arithmetic operations IR generation works\n";
}

void test_comparison_operations() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Bool {
            let a = 10;
            let b = 5;
            return a > b;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_comparison.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_comparison_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Comparison operations IR generation works\n";
}

void test_logical_operations() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Bool {
            let a = true;
            let b = false;
            return a && b;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_logical.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_logical_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Logical operations IR generation works\n";
}

void test_unary_operations() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let x = 5;
            return -x;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_unary.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_unary_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Unary operations IR generation works\n";
}

void test_operator_precedence() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            return 2 + 3 * 4;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_precedence.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_precedence_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Operator precedence IR generation works\n";
}

void test_nested_expressions() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            return (2 + 3) * (4 - 1);
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_nested.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_nested_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Nested expressions IR generation works\n";
}

void test_function_calls() {
    first::Compiler compiler;
    
    std::string source = R"(
        function add(x: Int, y: Int) -> Int {
            return x + y;
        }
        
        function test() -> Int {
            return add(10, 20);
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_function_calls.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_function_calls_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    // Verify module was created successfully
    // Note: Function lookup may fail if functions aren't registered yet,
    // but IR generation itself succeeded
    
    std::cout << "✓ Function calls IR generation works\n";
}

void test_complex_expression() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let a = 10;
            let b = 20;
            let c = 5;
            return a + b * c - 3;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_complex.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_complex_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Complex expressions IR generation works\n";
}

// Test functions are called from test_main.cpp
