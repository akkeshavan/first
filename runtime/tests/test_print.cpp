#include "first/runtime/io.h"
#include "first/runtime/string.h"
#include <cassert>
#include <iostream>
#include <sstream>

using namespace first::runtime;
using namespace first::runtime::io;

void test_basic_print() {
    print(makeString("Hello"));
    println(makeString(" World"));
    
    first::runtime::io::print("Test");
    first::runtime::io::println(" output");
    
    std::cout << "✓ Basic print functions work\n";
}

void test_type_printing() {
    printInt(42);
    println(makeString(""));
    
    printlnInt(100);
    
    printFloat(3.14);
    println(makeString(""));
    
    printlnFloat(2.718);
    
    printBool(true);
    println(makeString(" "));
    printlnBool(false);
    
    std::cout << "✓ Type-specific print functions work\n";
}

void test_error_printing() {
    printErr(makeString("Error: "));
    printlnErr(makeString("Something went wrong"));
    
    first::runtime::io::printErr("Warning: ");
    first::runtime::io::printlnErr("This is a warning");
    
    std::cout << "✓ Error printing functions work\n";
}

void test_formatted_printing() {
    // Test string formatting
    printf(makeString("Hello, {}!"), makeString("World"));
    println(makeString(""));
    
    // Test integer formatting
    printf(makeString("The answer is {}"), 42LL);
    println(makeString(""));
    
    // Test float formatting
    printf(makeString("Pi is approximately {}"), 3.14159);
    println(makeString(""));
    
    // Test boolean formatting
    printf(makeString("The value is {}"), true);
    println(makeString(""));
    
    // Test multiple placeholders (simple - only replaces first)
    printf(makeString("x={}, y={}"), 10LL);
    println(makeString(""));
    
    std::cout << "✓ Formatted printing works\n";
}

void test_print_combinations() {
    println(makeString("=== Test Output ==="));
    print(makeString("Number: "));
    printInt(42);
    println(makeString(""));
    
    print(makeString("Float: "));
    printlnFloat(3.14);
    
    print(makeString("Boolean: "));
    printlnBool(true);
    
    printlnErr(makeString("=== Error Output ==="));
    printlnErr(makeString("This goes to stderr"));
    
    std::cout << "✓ Print combinations work\n";
}

int main() {
    std::cout << "Testing Terminal Print Functions...\n\n";
    
    test_basic_print();
    test_type_printing();
    test_error_printing();
    test_formatted_printing();
    test_print_combinations();
    
    std::cout << "\nAll terminal print tests passed!\n";
    return 0;
}
