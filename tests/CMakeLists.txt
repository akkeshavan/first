cmake_minimum_required(VERSION 3.20)

# Find testing framework (Catch2 or Google Test)
# For now, we'll use a simple test framework

add_executable(test_compiler
    test_main.cpp
    test_error_reporter.cpp
    test_error_enhancements.cpp
    test_source_location.cpp
    test_lexer.cpp
    test_parser.cpp
    test_ast_basic.cpp
    test_ast_statements.cpp
    test_ast_functions.cpp
    test_ast_builder.cpp
    test_ast_integration.cpp
    test_ast_binary.cpp
    test_ast_validation.cpp
    test_symbol_table.cpp
    test_type_checker.cpp
    test_type_checker_composite.cpp
    test_type_checker_functions.cpp
    test_semantic_restrictions.cpp
    test_multimodule.cpp
    test_ir_generator.cpp
    test_ir_primitives.cpp
    test_ir_expressions.cpp
    test_ir_control_flow.cpp
    test_ir_functions.cpp
    test_ir_interactions.cpp
    test_ir_arrays.cpp
    test_ir_records.cpp
    test_ir_adts.cpp
    test_ir_closures.cpp
    test_ir_generics.cpp
)

# target_link_libraries moved below

# Add compiler source files to test executable
target_sources(test_compiler PRIVATE
    ${CMAKE_SOURCE_DIR}/src/compiler.cpp
    ${CMAKE_SOURCE_DIR}/src/error_reporter.cpp
    ${CMAKE_SOURCE_DIR}/src/source_location.cpp
    ${CMAKE_SOURCE_DIR}/src/parser/parser.cpp
    ${CMAKE_SOURCE_DIR}/src/ast/types.cpp
    ${CMAKE_SOURCE_DIR}/src/ast/builder.cpp
    ${CMAKE_SOURCE_DIR}/src/ast/validator.cpp
    ${CMAKE_SOURCE_DIR}/src/ast/match_case.cpp
    ${CMAKE_SOURCE_DIR}/src/ast/match_expr.cpp
    ${CMAKE_SOURCE_DIR}/src/ast/lambda_expr.cpp
    ${CMAKE_SOURCE_DIR}/src/semantic/symbol_table.cpp
    ${CMAKE_SOURCE_DIR}/src/semantic/type_checker.cpp
    ${CMAKE_SOURCE_DIR}/src/semantic/semantic_checker.cpp
    ${CMAKE_SOURCE_DIR}/src/semantic/module_resolver.cpp
    ${CMAKE_SOURCE_DIR}/src/ir/ir_generator.cpp
)

target_include_directories(test_compiler PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_BINARY_DIR}/generated
    ${LLVM_INCLUDE_DIRS}
)

target_compile_definitions(test_compiler PRIVATE
    ${LLVM_DEFINITIONS}
)

target_link_libraries(test_compiler
    first_parser
    ${ANTLR4_LIBRARY}
    ${llvm_libs}
)

add_test(NAME CompilerTests COMMAND test_compiler)
