cmake_minimum_required(VERSION 3.20)
project(FirstCompiler VERSION 0.1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

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
    all-targets
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

# Generated parser library
add_library(first_parser STATIC
    ${GENERATED_FILES}
)
target_link_libraries(first_parser
    antlr4-runtime
)
target_include_directories(first_parser PUBLIC
    ${ANTLR4_GENERATED_DIR}
)

# Compiler source files
set(COMPILER_SOURCES
    src/main.cpp
    src/compiler.cpp
    src/error_reporter.cpp
    src/source_location.cpp
)

# Compiler executable
add_executable(firstc ${COMPILER_SOURCES})
target_link_libraries(firstc
    first_parser
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
