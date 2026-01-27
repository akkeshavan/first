#include "first/runtime/array.h"
#include "first/runtime/string.h"
#include <cassert>
#include <iostream>

using namespace first::runtime;

void test_array_creation() {
    Array<int> arr1 = makeArray<int>();
    assert(arr1->length() == 0);
    assert(arr1->isEmpty());
    
    Array<int> arr2 = makeArray<int>(5);
    assert(arr2->length() == 5);
    
    Array<int> arr3 = makeArray<int>({1, 2, 3});
    assert(arr3->length() == 3);
    assert(arr3->at(0) == 1);
    assert(arr3->at(2) == 3);
    
    std::cout << "✓ Array creation works\n";
}

void test_array_access() {
    Array<int> arr = makeArray<int>({10, 20, 30});
    
    assert(arr->at(0) == 10);
    assert(arr->at(1) == 20);
    assert(arr->at(2) == 30);
    
    // Test bounds checking
    try {
        arr->at(10);
        assert(false && "Should have thrown");
    } catch (const std::out_of_range&) {
        // Expected
    }
    
    std::cout << "✓ Array access works\n";
}

void test_array_immutability() {
    Array<int> arr1 = makeArray<int>({1, 2, 3});
    Array<int> arr2 = arr1->push(4);
    
    assert(arr1->length() == 3);
    assert(arr2->length() == 4);
    assert(arr2->at(3) == 4);
    
    Array<int> arr3 = arr2->pop();
    assert(arr2->length() == 4);
    assert(arr3->length() == 3);
    
    std::cout << "✓ Array immutability works\n";
}

void test_array_map() {
    Array<int> arr = makeArray<int>({1, 2, 3, 4, 5});
    Array<int> doubled = arr->map([](int x) { return x * 2; });
    
    assert(doubled->length() == 5);
    assert(doubled->at(0) == 2);
    assert(doubled->at(4) == 10);
    assert(arr->at(0) == 1); // Original unchanged
    
    std::cout << "✓ Array map works\n";
}

void test_array_filter() {
    Array<int> arr = makeArray<int>({1, 2, 3, 4, 5, 6});
    Array<int> evens = arr->filter([](int x) { return x % 2 == 0; });
    
    assert(evens->length() == 3);
    assert(evens->at(0) == 2);
    assert(evens->at(1) == 4);
    assert(evens->at(2) == 6);
    
    std::cout << "✓ Array filter works\n";
}

void test_array_reduce() {
    Array<int> arr = makeArray<int>({1, 2, 3, 4, 5});
    
    int sum = arr->reduce([](int acc, int x) { return acc + x; }, 0);
    assert(sum == 15);
    
    int product = arr->reduce([](int acc, int x) { return acc * x; }, 1);
    assert(product == 120);
    
    std::cout << "✓ Array reduce works\n";
}

void test_string_array() {
    Array<String> arr = makeArray<String>({
        makeString("Hello"),
        makeString("World")
    });
    
    assert(arr->length() == 2);
    assert(arr->at(0)->toString() == "Hello");
    assert(arr->at(1)->toString() == "World");
    
    Array<String> extended = arr->push(makeString("!"));
    assert(extended->length() == 3);
    assert(extended->at(2)->toString() == "!");
    
    std::cout << "✓ String array works\n";
}

int main() {
    std::cout << "Testing Array Runtime...\n\n";
    
    test_array_creation();
    test_array_access();
    test_array_immutability();
    test_array_map();
    test_array_filter();
    test_array_reduce();
    test_string_array();
    
    std::cout << "\nAll array runtime tests passed!\n";
    return 0;
}
