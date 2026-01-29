# Runtime library CMakeLists.txt
# This is included as a subdirectory from the main CMakeLists.txt

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/runtime/include)
include_directories(${CMAKE_SOURCE_DIR}/include)

# Optional: Boehm-Demers-Weiser GC (Step 9.4)
# When ON, runtime uses GC_malloc for "caller must free" returns; no explicit free.
option(FIRST_USE_GC "Use Boehm GC for runtime heap allocations" OFF)
if(FIRST_USE_GC)
    # Prefer Homebrew on macOS (arm64) so we don't pick up x86_64 /usr/local lib
    if(APPLE)
        find_library(GC_LIBRARY NAMES gc PATHS /opt/homebrew/lib NO_DEFAULT_PATH)
    endif()
    if(NOT GC_LIBRARY)
        find_library(GC_LIBRARY NAMES gc gcbdwgc bdwgc)
    endif()
    find_path(GC_INCLUDE_DIR NAMES gc/gc.h gc.h
        PATHS /opt/homebrew/include
        NO_DEFAULT_PATH)
    if(NOT GC_INCLUDE_DIR)
        find_path(GC_INCLUDE_DIR NAMES gc/gc.h gc.h)
    endif()
    if(NOT GC_LIBRARY)
        message(FATAL_ERROR "FIRST_USE_GC=ON but libgc not found. Install: apt-get install libgc-dev, brew install bdw-gc, etc.")
    endif()
    if(NOT GC_INCLUDE_DIR)
        set(GC_INCLUDE_DIR "")
    endif()
endif()

# Runtime library sources
set(RUNTIME_SOURCES
    src/runtime/refcount.cpp
    src/runtime/string.cpp
    src/runtime/io.cpp
    src/runtime/gc_alloc.cpp
    src/runtime/stdlib.cpp
)

# Create runtime library
add_library(first_runtime STATIC ${RUNTIME_SOURCES})
if(UNIX)
    target_link_libraries(first_runtime PUBLIC m)  # libm for math
endif()
if(FIRST_USE_GC)
    target_compile_definitions(first_runtime PRIVATE FIRST_USE_GC)
    target_include_directories(first_runtime PRIVATE ${GC_INCLUDE_DIR})
    target_link_libraries(first_runtime PUBLIC ${GC_LIBRARY})
endif()

target_include_directories(first_runtime PUBLIC
    ${CMAKE_SOURCE_DIR}/runtime/include
    ${CMAKE_SOURCE_DIR}/include
)

# Test executables
add_executable(test_runtime_refcount
    tests/test_refcount.cpp
)

add_executable(test_runtime_string
    tests/test_string.cpp
)

add_executable(test_runtime_integration
    tests/test_runtime_integration.cpp
)

add_executable(test_runtime_array
    tests/test_array.cpp
)

add_executable(test_runtime_result
    tests/test_result.cpp
)

add_executable(test_runtime_io
    tests/test_io.cpp
)

add_executable(test_runtime_print
    tests/test_print.cpp
)

add_executable(test_runtime_gc
    tests/test_gc.cpp
)

target_link_libraries(test_runtime_refcount first_runtime)
target_link_libraries(test_runtime_string first_runtime)
target_link_libraries(test_runtime_integration first_runtime)
target_link_libraries(test_runtime_array first_runtime)
target_link_libraries(test_runtime_result first_runtime)
target_link_libraries(test_runtime_io first_runtime)
target_link_libraries(test_runtime_print first_runtime)
target_link_libraries(test_runtime_gc first_runtime)

# Set output directories
set_target_properties(
    test_runtime_refcount 
    test_runtime_string 
    test_runtime_integration
    test_runtime_array
    test_runtime_result
    test_runtime_io
    test_runtime_print
    test_runtime_gc
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/runtime/tests
)

# Install rules (for linking with compiler)
install(TARGETS first_runtime
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)

install(DIRECTORY include/first/runtime
    DESTINATION include/first
)
