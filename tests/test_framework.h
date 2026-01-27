#pragma once

#include <iostream>
#include <cassert>

// Simple test framework
extern int tests_run;
extern int tests_passed;
extern int tests_failed;

#define TEST(name) \
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
