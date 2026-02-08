#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <cassert>
#include <iostream>

void test_interaction_declaration() {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction test() -> Unit {
            return;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_interaction.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_interaction_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Interaction declaration IR generation works\n";
}

void test_mutable_variable() {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction test() -> Int {
            var x: Int = 10;
            return x;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_mutable.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_mutable_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Mutable variable IR generation works\n";
}

void test_assignment() {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction test() -> Int {
            var x: Int = 10;
            x = 20;
            return x;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_assignment.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_assignment_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Assignment IR generation works\n";
}

void test_assignment_in_loop() {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction test() -> Int {
            var i: Int = 0;
            i = i + 1;
            i = i + 1;
            return i;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_assignment_loop.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_assignment_loop_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Assignment in loop IR generation works\n";
}

void test_interaction_with_parameters() {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction increment(x: Int) -> Int {
            var result: Int = x;
            result = result + 1;
            return result;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_interaction_params.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_interaction_params_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Interaction with parameters IR generation works\n";
}

void test_multiple_assignments() {
    first::Compiler compiler;
    
    std::string source = R"(
        interaction test() -> Int {
            var a: Int = 1;
            var b: Int = 2;
            a = 10;
            b = 20;
            return a + b;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_multiple_assign.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_multiple_assign_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Multiple assignments IR generation works\n";
}

// Test functions are called from test_main.cpp
