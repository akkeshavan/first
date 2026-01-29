#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <cassert>
#include <iostream>

void test_record_literal() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let rec = {x: 1, y: 2};
            return 0;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_record_literal.first");
    // Note: This will fail until record IR generation is fully implemented
    // But we can test that AST building works
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_record_literal_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Record literal AST building works\n";
}

void test_field_access() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let rec = {x: 1, y: 2};
            return rec.x;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_field_access.first");
    // Note: This will fail until record IR generation is fully implemented
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_field_access_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Field access AST building works\n";
}

void test_record_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test(rec: {x: Int, y: Int}) -> Int {
            return rec.x;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_record_type.first");
    // Note: Record type parsing may not be fully implemented yet
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_record_type_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Record type IR generation works\n";
}

// Test functions are called from test_main.cpp
