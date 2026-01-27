#include "first/runtime/string.h"
#include <sstream>
#include <stdexcept>
#include <cstring>

namespace first {
namespace runtime {

FirstString::FirstString() : data_("") {}

FirstString::FirstString(const char* utf8) : data_(utf8 ? utf8 : "") {}

FirstString::FirstString(const std::string& utf8) : data_(utf8) {}

FirstString::FirstString(const FirstString& other) : RefCounted(), data_(other.data_) {}

FirstString& FirstString::operator=(const FirstString& other) {
    if (this != &other) {
        data_ = other.data_;
    }
    return *this;
}

size_t FirstString::length() const {
    return countCodePoints();
}

size_t FirstString::byteLength() const {
    return data_.size();
}

const char* FirstString::data() const {
    return data_.c_str();
}

std::string FirstString::toString() const {
    return data_;
}

FirstString FirstString::operator+(const FirstString& other) const {
    FirstString result;
    result.data_ = data_ + other.data_;
    return result;
}

bool FirstString::operator==(const FirstString& other) const {
    return data_ == other.data_;
}

bool FirstString::operator!=(const FirstString& other) const {
    return data_ != other.data_;
}

int FirstString::compare(const FirstString& other) const {
    if (data_ < other.data_) return -1;
    if (data_ > other.data_) return 1;
    return 0;
}

FirstString FirstString::slice(size_t start, size_t end) const {
    FirstString result;
    // Simple byte-level slicing for now
    // TODO: Implement proper UTF-8 code point slicing
    if (start <= data_.size() && end <= data_.size() && start <= end) {
        result.data_ = data_.substr(start, end - start);
    }
    return result;
}

char32_t FirstString::charAt(size_t index) const {
    // Simple implementation - returns byte value
    // TODO: Implement proper UTF-8 code point extraction
    if (index < data_.size()) {
        return static_cast<char32_t>(static_cast<unsigned char>(data_[index]));
    }
    return 0;
}

int64_t FirstString::toInt() const {
    try {
        return std::stoll(data_);
    } catch (...) {
        return 0;
    }
}

double FirstString::toFloat() const {
    try {
        return std::stod(data_);
    } catch (...) {
        return 0.0;
    }
}

FirstString FirstString::fromInt(int64_t value) {
    FirstString result;
    result.data_ = std::to_string(value);
    return result;
}

FirstString FirstString::fromFloat(double value) {
    FirstString result;
    std::ostringstream oss;
    oss << value;
    result.data_ = oss.str();
    return result;
}

size_t FirstString::countCodePoints() const {
    // Simple approximation: count non-continuation bytes
    // A proper UTF-8 implementation would decode code points
    size_t count = 0;
    for (size_t i = 0; i < data_.size(); ++i) {
        unsigned char c = static_cast<unsigned char>(data_[i]);
        // Count bytes that are start of UTF-8 sequence (not continuation bytes 0x80-0xBF)
        if ((c & 0xC0) != 0x80) {
            count++;
        }
    }
    return count;
}

// Helper functions
String makeString(const char* utf8) {
    FirstString* str = new FirstString(utf8);
    return Ref<FirstString>(str);
}

String makeString(const std::string& utf8) {
    FirstString* str = new FirstString(utf8);
    return Ref<FirstString>(str);
}

String makeString(int64_t value) {
    FirstString* str = new FirstString(FirstString::fromInt(value));
    return Ref<FirstString>(str);
}

String makeString(double value) {
    FirstString* str = new FirstString(FirstString::fromFloat(value));
    return Ref<FirstString>(str);
}

} // namespace runtime
} // namespace first
