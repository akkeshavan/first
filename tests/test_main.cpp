#include <iostream>
#include <cassert>

// Simple test framework
int tests_run = 0;
int tests_passed = 0;
int tests_failed = 0;

#define TEST(name) \
    void test_##name(); \
    void test_##name()

#define ASSERT(condition, message) \
    do { \
        tests_run++; \
        if (condition) { \
            tests_passed++; \
        } else { \
            tests_failed++; \
            std::cerr << "FAIL: " << __FILE__ << ":" << __LINE__ << " - " << message << "\n"; \
        } \
    } while(0)

#define ASSERT_EQ(a, b, message) \
    ASSERT((a) == (b), message << " (expected " << (b) << ", got " << (a) << ")")

#define ASSERT_NE(a, b, message) \
    ASSERT((a) != (b), message)

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

// Include test files
extern void test_error_reporter();
extern void test_source_location();
extern void test_lexer();

void run_all_tests() {
    std::cout << "Testing ErrorReporter...\n";
    test_error_reporter();
    
    std::cout << "Testing SourceLocation...\n";
    test_source_location();
    
    std::cout << "Testing Lexer...\n";
    test_lexer();
}
