cmake_minimum_required(VERSION 3.20)
project(FirstCompiler VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Build options (must be before add_subdirectory(runtime) so runtime tests respect BUILD_TESTS)
option(BUILD_TESTS "Build test suite" ON)
option(BUILD_EXAMPLES "Build example programs" ON)

# Add runtime subdirectory
add_subdirectory(runtime)

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${CMAKE_BINARY_DIR}/generated)

# Find required packages
# Hint for LLVM when not set (e.g. Homebrew on macOS)
if(NOT LLVM_DIR)
  if(EXISTS "/opt/homebrew/opt/llvm/lib/cmake/llvm/LLVMConfig.cmake")
    set(LLVM_DIR "/opt/homebrew/opt/llvm/lib/cmake/llvm")
  elseif(EXISTS "/usr/local/opt/llvm/lib/cmake/llvm/LLVMConfig.cmake")
    set(LLVM_DIR "/usr/local/opt/llvm/lib/cmake/llvm")
  endif()
endif()
find_package(LLVM REQUIRED CONFIG)

# ANTLR4 runtime: use manual find_path/find_library (Ubuntu libantlr4-runtime-dev
# does not provide a CMake config; Homebrew and pkg-config paths work below)

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
    Passes
    ipo
    X86
    AArch64
)

# ANTLR4 grammar generation
set(ANTLR4_GENERATED_DIR ${CMAKE_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${ANTLR4_GENERATED_DIR})

# Find ANTLR4 tool (antlr4 from pip/antlr4-tools, or antlr from Homebrew)
find_program(ANTLR4_TOOL NAMES antlr4 antlr)
if(NOT ANTLR4_TOOL)
    message(WARNING "antlr4/antlr tool not found. Please install ANTLR4.")
    message(STATUS "  pip: pip install antlr4-tools")
    message(STATUS "  Homebrew: brew install antlr")
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
        /usr/lib/x86_64-linux-gnu
        /usr/lib/aarch64-linux-gnu
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
    src/ast/select_branch.cpp
    src/semantic/symbol_table.cpp
    src/semantic/type_checker.cpp
    src/semantic/semantic_checker.cpp
    src/semantic/module_resolver.cpp
    src/semantic/derive_pass.cpp
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
# When runtime uses Boehm GC (FIRST_USE_GC=ON), compiler must add -lgc when linking user programs
if(FIRST_USE_GC)
    target_compile_definitions(firstc PRIVATE FIRST_RUNTIME_USES_GC)
endif()
target_link_libraries(firstc
    first_parser
    ${ANTLR4_LIBRARY}
    ${llvm_libs}
)
# Ensure first_runtime is built when building firstc (needed for fir build / linking user programs)
add_dependencies(firstc first_runtime)

# Test suite
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Examples
if(BUILD_EXAMPLES)
    add_subdirectory(examples)
endif()

# Installation (component "first" so productbuild .pkg has payload)
install(TARGETS firstc DESTINATION bin COMPONENT first)
# Stdlib (Prelude.first, etc.) next to binary scheme: PREFIX/lib/first/
install(DIRECTORY lib/ DESTINATION lib/first COMPONENT first)

# --- Packaging (installables for Apple and Linux, x86_64 and arm64) ---
set(CPACK_PACKAGE_NAME "first-compiler")
set(CPACK_PACKAGE_VERSION ${PROJECT_VERSION})
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "First programming language compiler and runtime")
set(CPACK_PACKAGE_VENDOR "First")
# Subdir under prefix for .pkg (empty = install directly to prefix)
set(CPACK_PACKAGE_INSTALL_DIRECTORY "")
if(APPLE)
    if(CMAKE_OSX_ARCHITECTURES)
        list(GET CMAKE_OSX_ARCHITECTURES 0 _cpack_arch)
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "arm64|aarch64")
        set(_cpack_arch "arm64")
    else()
        set(_cpack_arch "x86_64")
    endif()
    set(CPACK_SYSTEM_NAME "Darwin-${_cpack_arch}")
else()
    if(CMAKE_SYSTEM_PROCESSOR MATCHES "aarch64|arm64|ARM64")
        set(_cpack_arch "aarch64")
    elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "x86_64|amd64|AMD64")
        set(_cpack_arch "x86_64")
    else()
        set(_cpack_arch "${CMAKE_SYSTEM_PROCESSOR}")
    endif()
    set(CPACK_SYSTEM_NAME "${CMAKE_SYSTEM_NAME}-${_cpack_arch}")
endif()
set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-${CPACK_PACKAGE_VERSION}-${CPACK_SYSTEM_NAME}")
# Put all installables in project dist/ folder
set(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_SOURCE_DIR}/dist")
# Ensure dist/ exists when cpack runs: bake absolute path into generated script
set(_cpack_dist_abs "${CMAKE_SOURCE_DIR}/dist")
string(REPLACE "\\" "/" _cpack_dist_abs "${_cpack_dist_abs}")
set(_cpack_makedir "${CMAKE_BINARY_DIR}/cpack_makedir.cmake")
file(WRITE "${_cpack_makedir}" "file(MAKE_DIRECTORY \"${_cpack_dist_abs}\")\n")
set(CPACK_PRE_BUILD_SCRIPTS "${_cpack_makedir}")
set(CPACK_GENERATOR "TGZ;ZIP")
if(APPLE)
  list(APPEND CPACK_GENERATOR "productbuild")
  # .pkg installs to /usr/local (bin, lib, include) so firstc is on PATH when /usr/local/bin is
  set(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local")
  set(CPACK_PRODUCTBUILD_IDENTIFIER "org.first.compiler")
  # Include install payload in .pkg (required for productbuild to show files)
  set(CPACK_COMPONENTS_ALL first)
  include(CPackComponent)
  cpack_add_component(first
    DISPLAY_NAME "First compiler and runtime"
    DESCRIPTION "firstc binary, libfirst_runtime.a, and runtime headers"
  )
endif()
if(UNIX AND NOT APPLE)
  list(APPEND CPACK_GENERATOR "DEB")
endif()
include(CPack)

# Multi-module compilation support
# This allows compiling multiple First modules and linking them together
# Usage: firstc -o output.bc module1.first module2.first module3.first
# The compiler will automatically resolve imports and link modules
