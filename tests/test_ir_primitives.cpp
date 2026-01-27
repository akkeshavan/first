#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <cassert>
#include <iostream>
#include <sstream>

void test_int_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            return 42;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_int.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_int_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    // Verify module was created
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Int type IR generation works\n";
}

void test_float_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Float {
            return 3.14;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_float.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_float_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    // Verify module was created
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Float type IR generation works\n";
}

void test_bool_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Bool {
            return true;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_bool.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_bool_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Bool type IR generation works\n";
}

void test_unit_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Unit {
            return;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_unit.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_unit_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Unit type IR generation works\n";
}

void test_string_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> String {
            return "hello";
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_string.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_string_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ String type IR generation works\n";
}

void test_int_arithmetic() {
    first::Compiler compiler;
    
    std::string source = R"(
        function add(x: Int, y: Int) -> Int {
            return x + y;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_int_arithmetic.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_int_arithmetic_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Int arithmetic IR generation works\n";
}

void test_float_arithmetic() {
    first::Compiler compiler;
    
    std::string source = R"(
        function multiply(x: Float, y: Float) -> Float {
            return x * y;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_float_arithmetic.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_float_arithmetic_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Float arithmetic IR generation works\n";
}

void test_mixed_arithmetic() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(x: Int) -> Float {
            return x + 1.5;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_mixed.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_mixed_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Mixed Int/Float arithmetic IR generation works\n";
}

// Test functions are called from test_main.cpp
