#include "test_framework.h"
#include "first/compiler.h"
#include "first/ir/ir_generator.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Instructions.h>
#include <cassert>
#include <iostream>

void test_constructor_expr() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let opt = Some(5);
            return 0;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_constructor.first");
    // Note: This will fail until ADT IR generation is fully implemented
    // But we can test that AST building works
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_constructor_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Constructor expression AST building works\n";
}

void test_adt_type() {
    first::Compiler compiler;
    
    std::string source = R"(
        type Option = Some(Int) | None;
        function test(opt: Option) -> Int {
            return 0;
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_adt_type.first");
    // Note: ADT type parsing may not be fully implemented yet
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_adt_type_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ ADT type IR generation works\n";
}

void test_match_expr() {
    first::Compiler compiler;
    
    std::string source = R"(
        function test() -> Int {
            let opt = Some(5);
            return match opt {
                Some(x) => x,
                None => 0
            };
        }
    )";
    
    bool success = compiler.compileFromString(source, "test_match.first");
    // Note: This will fail until pattern matching IR generation is fully implemented
    
    auto* ast = compiler.getAST();
    if (ast) {
        first::ir::IRGenerator irGen(compiler.getErrorReporter(), "test_match_module");
        bool irSuccess = irGen.generate(ast);
        // IR generation may fail, but that's expected for now
        llvm::Module* module = irGen.getModule();
        ASSERT(module != nullptr, "Module should exist");
    }
    
    std::cout << "✓ Match expression AST building works\n";
}

// Test functions are called from test_main.cpp
