#pragma once

#include "first/runtime/refcount.h"
#include <string>
#include <cstddef>
#include <vector>

namespace first {
namespace runtime {

// UTF-8 string with reference counting
class FirstString : public RefCounted {
public:
    // Constructors
    FirstString();
    explicit FirstString(const char* utf8);
    explicit FirstString(const std::string& utf8);
    FirstString(const FirstString& other);
    
    // Assignment
    FirstString& operator=(const FirstString& other);
    
    // String operations
    size_t length() const; // UTF-8 code point count (approximate)
    size_t byteLength() const; // Byte length
    const char* data() const;
    std::string toString() const;
    
    // Concatenation
    FirstString operator+(const FirstString& other) const;
    
    // Comparison
    bool operator==(const FirstString& other) const;
    bool operator!=(const FirstString& other) const;
    int compare(const FirstString& other) const; // -1, 0, 1
    
    // Slicing (returns new string)
    FirstString slice(size_t start, size_t end) const;
    
    // Character access (returns UTF-8 code point)
    char32_t charAt(size_t index) const;
    
    // String-to-number conversions
    int64_t toInt() const;
    double toFloat() const;
    
    // Number-to-string conversions (static)
    static FirstString fromInt(int64_t value);
    static FirstString fromFloat(double value);

private:
    std::string data_;
    
    // Helper to count UTF-8 code points
    size_t countCodePoints() const;
};

// Convenience typedef
using String = Ref<FirstString>;

// String creation helpers
String makeString(const char* utf8);
String makeString(const std::string& utf8);
String makeString(int64_t value);
String makeString(double value);

} // namespace runtime
} // namespace first
