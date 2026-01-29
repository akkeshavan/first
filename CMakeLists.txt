cmake_minimum_required(VERSION 3.20)
project(FirstCompiler VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Add runtime subdirectory
add_subdirectory(runtime)

# Build options
option(BUILD_TESTS "Build test suite" ON)
option(BUILD_EXAMPLES "Build example programs" ON)

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${CMAKE_BINARY_DIR}/generated)

# Find required packages
find_package(LLVM REQUIRED CONFIG)
find_package(antlr4-runtime REQUIRED)

# LLVM configuration
llvm_map_components_to_libnames(llvm_libs
    core
    support
    irreader
    bitreader
    bitwriter
    asmparser
    executionengine
    orcjit
    native
    target
)

# ANTLR4 grammar generation
set(ANTLR4_GENERATED_DIR ${CMAKE_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${ANTLR4_GENERATED_DIR})

# Find ANTLR4 tool
find_program(ANTLR4_TOOL antlr4)
if(NOT ANTLR4_TOOL)
    message(WARNING "antlr4 tool not found. Please install ANTLR4.")
    message(STATUS "You can install it via: pip install antlr4-tools")
endif()

# Generate parser from grammar
set(GRAMMAR_FILE ${CMAKE_SOURCE_DIR}/docs/First.g4)
set(GENERATED_FILES
    ${ANTLR4_GENERATED_DIR}/FirstLexer.cpp
    ${ANTLR4_GENERATED_DIR}/FirstParser.cpp
    ${ANTLR4_GENERATED_DIR}/FirstLexer.h
    ${ANTLR4_GENERATED_DIR}/FirstParser.h
    ${ANTLR4_GENERATED_DIR}/FirstBaseListener.h
    ${ANTLR4_GENERATED_DIR}/FirstBaseListener.cpp
    ${ANTLR4_GENERATED_DIR}/FirstListener.h
    ${ANTLR4_GENERATED_DIR}/FirstListener.cpp
)

add_custom_command(
    OUTPUT ${GENERATED_FILES}
    COMMAND ${ANTLR4_TOOL} -Dlanguage=Cpp -visitor -listener -o ${ANTLR4_GENERATED_DIR} -package first ${GRAMMAR_FILE}
    DEPENDS ${GRAMMAR_FILE}
    COMMENT "Generating ANTLR4 parser from First.g4"
)

# Find ANTLR4 runtime include directory
find_path(ANTLR4_INCLUDE_DIR
    NAMES antlr4-runtime.h
    PATHS
        /opt/homebrew/include/antlr4-runtime
        /usr/local/include/antlr4-runtime
        /usr/include/antlr4-runtime
)

if(NOT ANTLR4_INCLUDE_DIR)
    # Try to find via pkg-config
    find_package(PkgConfig)
    if(PkgConfig_FOUND)
        pkg_check_modules(ANTLR4 antlr4-runtime)
        if(ANTLR4_FOUND)
            set(ANTLR4_INCLUDE_DIR ${ANTLR4_INCLUDE_DIRS})
        endif()
    endif()
endif()

# Find ANTLR4 library
find_library(ANTLR4_LIBRARY
    NAMES antlr4-runtime
    PATHS
        /opt/homebrew/lib
        /usr/local/lib
        /usr/lib
)

if(ANTLR4_INCLUDE_DIR AND ANTLR4_LIBRARY)
    message(STATUS "Found ANTLR4 include dir: ${ANTLR4_INCLUDE_DIR}")
    message(STATUS "Found ANTLR4 library: ${ANTLR4_LIBRARY}")
else()
    message(FATAL_ERROR "ANTLR4 runtime not found. Please install antlr4-cpp-runtime")
endif()

# Generated lexer/parser library (ANTLR used only for lexing in the current
# compiler pipeline; the hand-written parser lives in src/parser/parser.cpp).
add_library(first_parser STATIC
    ${GENERATED_FILES}
)
target_link_libraries(first_parser
    ${ANTLR4_LIBRARY}
)
target_include_directories(first_parser PUBLIC
    ${ANTLR4_GENERATED_DIR}
    ${ANTLR4_INCLUDE_DIR}
)

# Compiler source files
set(COMPILER_SOURCES
    src/main.cpp
    src/compiler.cpp
    src/error_reporter.cpp
    src/source_location.cpp
    src/parser/parser.cpp
    src/ast/types.cpp
    src/ast/validator.cpp
    src/ast/match_case.cpp
    src/ast/match_expr.cpp
    src/ast/lambda_expr.cpp
    src/semantic/symbol_table.cpp
    src/semantic/type_checker.cpp
    src/semantic/semantic_checker.cpp
    src/semantic/module_resolver.cpp
    src/ir/ir_generator.cpp
)

# Compiler executable
add_executable(firstc ${COMPILER_SOURCES})
target_include_directories(firstc PRIVATE
    ${LLVM_INCLUDE_DIRS}
)
target_compile_definitions(firstc PRIVATE
    ${LLVM_DEFINITIONS}
)
target_link_libraries(firstc
    first_parser
    ${ANTLR4_LIBRARY}
    ${llvm_libs}
)

# Test suite
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Installation
install(TARGETS firstc DESTINATION bin)

# Multi-module compilation support
# This allows compiling multiple First modules and linking them together
# Usage: firstc -o output.bc module1.first module2.first module3.first
# The compiler will automatically resolve imports and link modules
