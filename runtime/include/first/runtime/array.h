#pragma once

#include "first/runtime/refcount.h"
#include <vector>
#include <cstddef>
#include <stdexcept>

namespace first {
namespace runtime {

// Forward declaration
class FirstString;

// Generic array with reference counting
template<typename T>
class FirstArray : public RefCounted {
public:
    FirstArray() {}
    
    explicit FirstArray(size_t size) : data_(size) {}
    
    FirstArray(std::initializer_list<T> init) : data_(init) {}
    
    // Copy constructor (for immutability - creates new array)
    FirstArray(const FirstArray& other) : RefCounted(), data_(other.data_) {}
    
    // Assignment operator
    FirstArray& operator=(const FirstArray& other) {
        if (this != &other) {
            data_ = other.data_;
        }
        return *this;
    }
    
    // Element access with bounds checking
    T& at(size_t index) {
        if (index >= data_.size()) {
            throw std::out_of_range("Array index out of bounds");
        }
        return data_[index];
    }
    
    const T& at(size_t index) const {
        if (index >= data_.size()) {
            throw std::out_of_range("Array index out of bounds");
        }
        return data_[index];
    }
    
    // Unsafe access (for performance)
    T& operator[](size_t index) { return data_[index]; }
    const T& operator[](size_t index) const { return data_[index]; }
    
    // Array operations
    size_t length() const { return data_.size(); }
    bool isEmpty() const { return data_.empty(); }
    
    // Mutations (create new array for immutability) - return Ref-wrapped arrays
    Ref<FirstArray<T>> push(const T& value) const {
        FirstArray<T>* result = new FirstArray<T>(*this);
        result->data_.push_back(value);
        return Ref<FirstArray<T>>(result);
    }
    
    Ref<FirstArray<T>> pop() const {
        FirstArray<T>* result = new FirstArray<T>(*this);
        if (!result->data_.empty()) {
            result->data_.pop_back();
        }
        return Ref<FirstArray<T>>(result);
    }
    
    // Functional operations (return new arrays)
    template<typename F>
    Ref<FirstArray<T>> map(F func) const {
        FirstArray<T>* result = new FirstArray<T>();
        result->data_.reserve(data_.size());
        for (const auto& item : data_) {
            result->data_.push_back(func(item));
        }
        return Ref<FirstArray<T>>(result);
    }
    
    template<typename F>
    Ref<FirstArray<T>> filter(F predicate) const {
        FirstArray<T>* result = new FirstArray<T>();
        for (const auto& item : data_) {
            if (predicate(item)) {
                result->data_.push_back(item);
            }
        }
        return Ref<FirstArray<T>>(result);
    }
    
    template<typename F, typename Acc>
    Acc reduce(F func, Acc initial) const {
        Acc acc = initial;
        for (const auto& item : data_) {
            acc = func(acc, item);
        }
        return acc;
    }

private:
    std::vector<T> data_;
};

// Convenience typedef for reference-counted arrays
template<typename T>
using Array = Ref<FirstArray<T>>;

// Array creation helpers
template<typename T>
Array<T> makeArray() {
    return makeRef<FirstArray<T>>();
}

template<typename T>
Array<T> makeArray(size_t size) {
    return makeRef<FirstArray<T>>(size);
}

template<typename T>
Array<T> makeArray(std::initializer_list<T> init) {
    return makeRef<FirstArray<T>>(init);
}

} // namespace runtime
} // namespace first
