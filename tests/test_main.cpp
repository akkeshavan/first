#include "test_framework.h"

// Test framework variables
int tests_run = 0;
int tests_passed = 0;
int tests_failed = 0;

void run_all_tests();

int main() {
    std::cout << "Running First Compiler Tests...\n\n";
    
    run_all_tests();
    
    std::cout << "\n";
    std::cout << "Tests run: " << tests_run << "\n";
    std::cout << "Tests passed: " << tests_passed << "\n";
    std::cout << "Tests failed: " << tests_failed << "\n";
    
    return tests_failed == 0 ? 0 : 1;
}

// Declare all test functions
extern void test_error_reporter_basic();
extern void test_error_reporter_warnings();
extern void test_error_reporter_clear();
extern void test_source_location_basic();
extern void test_source_location_default();
extern void test_source_location_to_string();
extern void test_lexer_simple_program();
extern void test_lexer_integer_literals();
extern void test_parser_simple_program();
extern void test_parser_variables();
extern void test_parser_types();
extern void test_parser_expressions();
extern void test_parser_control_flow();
extern void test_parser_pattern_matching();
extern void test_parser_interfaces();
extern void test_parser_test_files();

void run_all_tests() {
    std::cout << "Testing ErrorReporter...\n";
    test_error_reporter_basic();
    test_error_reporter_warnings();
    test_error_reporter_clear();
    
    std::cout << "Testing SourceLocation...\n";
    test_source_location_basic();
    test_source_location_default();
    test_source_location_to_string();
    
    std::cout << "Testing Lexer...\n";
    test_lexer_simple_program();
    test_lexer_integer_literals();
    
    std::cout << "Testing Parser...\n";
    test_parser_simple_program();
    test_parser_variables();
    test_parser_types();
    test_parser_expressions();
    test_parser_control_flow();
    test_parser_pattern_matching();
    test_parser_interfaces();
    test_parser_test_files();
}
