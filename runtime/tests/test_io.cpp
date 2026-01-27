#include "first/runtime/io.h"
#include "first/runtime/string.h"
#include <cassert>
#include <iostream>
#include <fstream>
#include <cstdio>

using namespace first::runtime;
using namespace first::runtime::io;

void test_print() {
    // Just verify it compiles and runs without crashing
    print(makeString("Test output"));
    println(makeString("Test line"));
    
    std::cout << "✓ Print functions work\n";
}

void test_file_operations() {
    // Create a test file
    const char* testFile = "/tmp/first_runtime_test.txt";
    const char* testContent = "Hello, First Runtime!";
    
    // Write file
    auto writeResult = writeFile(makeString(testFile), makeString(testContent));
    assert(writeResult.isOk());
    
    // Read file
    auto readResult = readFile(makeString(testFile));
    assert(readResult.isOk());
    assert(readResult.unwrap()->toString() == testContent);
    
    // Clean up
    std::remove(testFile);
    
    std::cout << "✓ File operations work\n";
}

void test_file_error_handling() {
    // Try to read non-existent file
    auto result = readFile(makeString("/nonexistent/path/file.txt"));
    assert(result.isErr());
    
    std::cout << "✓ File error handling works\n";
}

int main() {
    std::cout << "Testing I/O Runtime...\n\n";
    
    test_print();
    test_file_operations();
    test_file_error_handling();
    
    std::cout << "\nAll I/O runtime tests passed!\n";
    return 0;
}
