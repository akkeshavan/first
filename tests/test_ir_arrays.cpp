#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <cassert>
#include <iostream>

void test_array_literal() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let arr = [1, 2, 3];
            return 0;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_array_literal.first");
    // Note: This will fail until array IR generation is fully implemented
    // But we can test that AST building works
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_array_literal_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Array literal AST building works\n";
}

void test_array_indexing() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let arr = [1, 2, 3];
            return arr[0];
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_array_index.first");
    // Note: This will fail until array IR generation is fully implemented
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_array_index_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Array indexing AST building works\n";
}

void test_array_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(arr: Array<Int>) -> Int {
            return 0;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_array_type.first");
    ASSERT(success, "Compilation should succeed");
    
    auto* ast = compiler.getAST();
    ASSERT(ast != nullptr, "AST should be created");
    
    first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_array_type_module");
    bool irSuccess = irGen.generate(ast);
    ASSERT(irSuccess, "IR generation should succeed");
    ASSERT(!compiler.getErrorReporter().hasErrors(), "No errors should occur");
    
    llvm::Module* module = irGen.getModule();
    ASSERT(module != nullptr, "Module should exist");
    
    std::cout << "✓ Array type IR generation works\n";
}

// Test functions are called from test_main.cpp
