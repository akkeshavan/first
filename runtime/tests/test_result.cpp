#include "first/runtime/result.h"
#include "first/runtime/string.h"
#include <cassert>
#include <iostream>

using namespace first::runtime;

void test_result_ok() {
    auto result = Result<int, String>::ok(42);
    assert(result.isOk());
    assert(!result.isErr());
    assert(result.unwrap() == 42);
    
    std::cout << "✓ Result::ok works\n";
}

void test_result_err() {
    auto result = Result<int, String>::err(makeString("Error occurred"));
    assert(result.isErr());
    assert(!result.isOk());
    assert(result.unwrapErr()->toString() == "Error occurred");
    
    std::cout << "✓ Result::err works\n";
}

void test_result_unwrap_or() {
    auto okResult = Result<int, String>::ok(42);
    assert(okResult.unwrapOr(0) == 42);
    
    auto errResult = Result<int, String>::err(makeString("Error"));
    assert(errResult.unwrapOr(0) == 0);
    
    std::cout << "✓ Result::unwrapOr works\n";
}

void test_result_copy() {
    auto result1 = Result<int, String>::ok(100);
    auto result2 = result1; // Copy
    
    assert(result1.isOk());
    assert(result2.isOk());
    assert(result1.unwrap() == 100);
    assert(result2.unwrap() == 100);
    
    std::cout << "✓ Result copy works\n";
}

void test_result_move() {
    auto result1 = Result<int, String>::ok(200);
    auto result2 = std::move(result1); // Move
    
    assert(result2.isOk());
    assert(result2.unwrap() == 200);
    
    std::cout << "✓ Result move works\n";
}

void test_option_some() {
    auto opt = Option<int>::some(42);
    assert(opt.isSome());
    assert(!opt.isNone());
    assert(opt.unwrap() == 42);
    
    std::cout << "✓ Option::some works\n";
}

void test_option_none() {
    auto opt = Option<int>::none();
    assert(opt.isNone());
    assert(!opt.isSome());
    assert(opt.unwrapOr(0) == 0);
    
    std::cout << "✓ Option::none works\n";
}

void test_option_unwrap_or() {
    auto some = Option<int>::some(42);
    assert(some.unwrapOr(0) == 42);
    
    auto none = Option<int>::none();
    assert(none.unwrapOr(100) == 100);
    
    std::cout << "✓ Option::unwrapOr works\n";
}

int main() {
    std::cout << "Testing Result/Option Runtime...\n\n";
    
    test_result_ok();
    test_result_err();
    test_result_unwrap_or();
    test_result_copy();
    test_result_move();
    test_option_some();
    test_option_none();
    test_option_unwrap_or();
    
    std::cout << "\nAll Result/Option runtime tests passed!\n";
    return 0;
}
