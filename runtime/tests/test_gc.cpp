// Test GC build: when FIRST_USE_GC is ON, runtime uses Boehm GC for heap allocations.
// This test links first_runtime and exercises string allocations (no explicit free).
#include "first/runtime/stdlib.h"
#include "first/runtime/gc_alloc.h"
#include <cassert>
#include <cstring>
#include <iostream>

int main() {
    first_gc_init();  // no-op when not using GC; safe to call always

    // Allocations that would be "caller must free" without GC
    char* a = first_string_concat("hello", " world");
    assert(a && std::strcmp(a, "hello world") == 0);

    char* b = first_int_to_string(42);
    assert(b && std::strcmp(b, "42") == 0);

    char* c = first_string_slice("abcdef", 1, 4);
    assert(c && std::strcmp(c, "bcd") == 0);

    // No free: with GC these are collected when unreachable
    std::cout << "GC runtime test OK: " << a << " " << b << " " << c << "\n";
    return 0;
}
