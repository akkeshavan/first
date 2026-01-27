# Runtime library CMakeLists.txt
# This is included as a subdirectory from the main CMakeLists.txt

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include directories
include_directories(${CMAKE_SOURCE_DIR}/runtime/include)
include_directories(${CMAKE_SOURCE_DIR}/include)

# Runtime library sources
set(RUNTIME_SOURCES
    src/runtime/refcount.cpp
    src/runtime/string.cpp
    src/runtime/io.cpp
)

# Create runtime library
add_library(first_runtime STATIC ${RUNTIME_SOURCES})

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

target_link_libraries(test_runtime_refcount first_runtime)
target_link_libraries(test_runtime_string first_runtime)
target_link_libraries(test_runtime_integration first_runtime)
target_link_libraries(test_runtime_array first_runtime)
target_link_libraries(test_runtime_result first_runtime)
target_link_libraries(test_runtime_io first_runtime)
target_link_libraries(test_runtime_print first_runtime)

# Set output directories
set_target_properties(
    test_runtime_refcount 
    test_runtime_string 
    test_runtime_integration
    test_runtime_array
    test_runtime_result
    test_runtime_io
    test_runtime_print
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
