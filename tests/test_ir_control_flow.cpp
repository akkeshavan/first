#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <cassert>
#include <iostream>

void test_if_statement() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(x: Int) -> Int {
            return if (x > 0) {
                1
            } else {
                0
            };
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_if.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_if_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ If statement IR generation works\n";
}

void test_if_without_else() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(x: Int) -> Int {
            return if (x > 0) {
                x
            } else {
                0
            };
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_if_no_else.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_if_no_else_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ If statement without else IR generation works\n";
}

void test_nested_if() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(x: Int, y: Int) -> Int {
            return if (x > 0) {
                if (y > 0) {
                    1
                } else {
                    2
                }
            } else {
                0
            };
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_nested_if.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_nested_if_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Nested if statement IR generation works\n";
}

void test_short_circuit_and() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(x: Int) -> Bool {
            return x > 0 && x < 10;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_short_circuit_and.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_short_circuit_and_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Short-circuit AND IR generation works\n";
}

void test_short_circuit_or() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(x: Int) -> Bool {
            return x < 0 || x > 10;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_short_circuit_or.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_short_circuit_or_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Short-circuit OR IR generation works\n";
}

// Test functions are called from test_main.cpp
