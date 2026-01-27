#include "first/runtime/refcount.h"
#include "first/runtime/string.h"
#include "first/runtime/array.h"
#include "first/runtime/result.h"
#include "first/runtime/io.h"
#include <cassert>
#include <iostream>

using namespace first::runtime;

void test_string_array() {
    // Create array with initializer list
    Array<String> arr = makeArray<String>({makeString("Hello"), makeString("World")});
    
    assert(arr->length() == 2);
    assert(arr->at(0)->toString() == "Hello");
    assert(arr->at(1)->toString() == "World");
    
    // Test push (returns new array)
    Array<String> arr2 = arr->push(makeString("!"));
    assert(arr2->length() == 3);
    assert(arr->length() == 2); // Original unchanged
    
    std::cout << "✓ String array works\n";
}

void test_result_type() {
    auto okResult = Result<int, String>::ok(42);
    assert(okResult.isOk());
    assert(okResult.unwrap() == 42);
    
    auto errResult = Result<int, String>::err(makeString("Error message"));
    assert(errResult.isErr());
    assert(errResult.unwrapErr()->toString() == "Error message");
    
    std::cout << "✓ Result type works\n";
}

void test_option_type() {
    auto some = Option<int>::some(42);
    assert(some.isSome());
    assert(some.unwrap() == 42);
    
    auto none = Option<int>::none();
    assert(none.isNone());
    assert(none.unwrapOr(0) == 0);
    
    std::cout << "✓ Option type works\n";
}

void test_array_operations() {
    Array<int> arr = makeArray<int>({1, 2, 3, 4, 5});
    
    // Map: double each element (returns Array<T>)
    Array<int> doubled = arr->map([](int x) { return x * 2; });
    assert(doubled->length() == 5);
    assert(doubled->at(0) == 2);
    assert(doubled->at(4) == 10);
    
    // Filter: keep even numbers
    Array<int> evens = arr->filter([](int x) { return x % 2 == 0; });
    assert(evens->length() == 2);
    assert(evens->at(0) == 2);
    assert(evens->at(1) == 4);
    
    // Reduce: sum
    int sum = arr->reduce([](int acc, int x) { return acc + x; }, 0);
    assert(sum == 15);
    
    std::cout << "✓ Array operations work\n";
}

int main() {
    std::cout << "Testing Runtime Integration...\n\n";
    
    test_string_array();
    test_result_type();
    test_option_type();
    test_array_operations();
    
    std::cout << "\nAll runtime integration tests passed!\n";
    return 0;
}
