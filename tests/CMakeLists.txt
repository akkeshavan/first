cmake_minimum_required(VERSION 3.20)

# Find testing framework (Catch2 or Google Test)
# For now, we'll use a simple test framework

add_executable(test_compiler
    test_main.cpp
    test_error_reporter.cpp
    test_source_location.cpp
    test_lexer.cpp
    test_parser.cpp
)

target_link_libraries(test_compiler
    first_parser
    ${ANTLR4_LIBRARY}
)

# Add compiler source files to test executable
target_sources(test_compiler PRIVATE
    ${CMAKE_SOURCE_DIR}/src/compiler.cpp
    ${CMAKE_SOURCE_DIR}/src/error_reporter.cpp
    ${CMAKE_SOURCE_DIR}/src/source_location.cpp
)

target_include_directories(test_compiler PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_BINARY_DIR}/generated
)

add_test(NAME CompilerTests COMMAND test_compiler)
