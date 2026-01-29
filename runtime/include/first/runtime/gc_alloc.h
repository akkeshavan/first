#pragma once

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Heap allocator for runtime objects returned to First code (strings, etc.).
// When FIRST_USE_GC is defined (Boehm GC build), allocations are GC-managed
// and never explicitly freed. Otherwise uses malloc; caller must free.

void* first_alloc(size_t size);

// Call once at process start when using Boehm GC (no-op when not using GC).
void first_gc_init(void);

#ifdef __cplusplus
}
#endif
