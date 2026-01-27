#include "first/runtime/string.h"
#include <cassert>
#include <iostream>

using namespace first::runtime;

void test_string_creation() {
    String s1 = makeString("Hello");
    assert(s1->toString() == "Hello");
    assert(s1->length() > 0);
    
    String s2 = makeString(std::string("World"));
    assert(s2->toString() == "World");
    
    std::cout << "✓ String creation works\n";
}

void test_string_concatenation() {
    String s1 = makeString("Hello");
    String s2 = makeString(" World");
    
    FirstString result = *s1 + *s2;
    assert(result.toString() == "Hello World");
    
    std::cout << "✓ String concatenation works\n";
}

void test_string_comparison() {
    String s1 = makeString("test");
    String s2 = makeString("test");
    String s3 = makeString("other");
    
    assert(*s1 == *s2);
    assert(*s1 != *s3);
    
    std::cout << "✓ String comparison works\n";
}

void test_string_conversions() {
    String s1 = makeString(42LL);
    assert(s1->toString() == "42");
    
    String s2 = makeString(3.14);
    assert(s2->toString().find("3.14") != std::string::npos || s2->toString().find("3.1") != std::string::npos);
    
    String s3 = makeString("123");
    assert(s3->toInt() == 123);
    
    String s4 = makeString("3.14");
    double fval = s4->toFloat();
    assert(fval > 3.13 && fval < 3.15);
    
    std::cout << "✓ String conversions work\n";
}

int main() {
    std::cout << "Testing String Runtime...\n\n";
    
    test_string_creation();
    test_string_concatenation();
    test_string_comparison();
    test_string_conversions();
    
    std::cout << "\nAll string runtime tests passed!\n";
    return 0;
}
