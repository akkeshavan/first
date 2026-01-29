#include "first/runtime/gc_alloc.h"
#include <cstdlib>
#include <cstring>

#ifdef FIRST_USE_GC
#include <gc.h>
#endif

extern "C" {

void* first_alloc(size_t size) {
#ifdef FIRST_USE_GC
    return GC_MALLOC(size);
#else
    return std::malloc(size);
#endif
}

void first_gc_init(void) {
#ifdef FIRST_USE_GC
    GC_INIT();
#endif
}

} // extern "C"

#ifdef FIRST_USE_GC
namespace {
struct GcInitializer {
    GcInitializer() { first_gc_init(); }
} gc_init;
} // namespace
#endif
