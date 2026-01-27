#pragma once

#include "first/runtime/refcount.h"
#include <type_traits>
#include <utility>
#include <stdexcept>

namespace first {
namespace runtime {

// Result<T, E> - represents success (Ok) or failure (Err)
template<typename T, typename E>
class Result {
public:
    // Create Ok result
    static Result<T, E> ok(const T& value) {
        Result<T, E> result;
        result.isOk_ = true;
        new(&result.okValue_) T(value);
        return result;
    }
    
    // Create Err result
    static Result<T, E> err(const E& error) {
        Result<T, E> result;
        result.isOk_ = false;
        new(&result.errValue_) E(error);
        return result;
    }
    
    // Default constructor (creates Ok with default value)
    Result() : isOk_(true) {
        new(&okValue_) T();
    }
    
    // Destructor
    ~Result() {
        if (isOk_) {
            okValue_.~T();
        } else {
            errValue_.~E();
        }
    }
    
    // Copy constructor
    Result(const Result& other) : isOk_(other.isOk_) {
        if (isOk_) {
            new(&okValue_) T(other.okValue_);
        } else {
            new(&errValue_) E(other.errValue_);
        }
    }
    
    // Move constructor
    Result(Result&& other) noexcept : isOk_(other.isOk_) {
        if (isOk_) {
            new(&okValue_) T(std::move(other.okValue_));
        } else {
            new(&errValue_) E(std::move(other.errValue_));
        }
    }
    
    // Copy assignment
    Result& operator=(const Result& other) {
        if (this != &other) {
            // Destroy current value
            if (isOk_) {
                okValue_.~T();
            } else {
                errValue_.~E();
            }
            
            // Copy new value
            isOk_ = other.isOk_;
            if (isOk_) {
                new(&okValue_) T(other.okValue_);
            } else {
                new(&errValue_) E(other.errValue_);
            }
        }
        return *this;
    }
    
    // Move assignment
    Result& operator=(Result&& other) noexcept {
        if (this != &other) {
            // Destroy current value
            if (isOk_) {
                okValue_.~T();
            } else {
                errValue_.~E();
            }
            
            // Move new value
            isOk_ = other.isOk_;
            if (isOk_) {
                new(&okValue_) T(std::move(other.okValue_));
            } else {
                new(&errValue_) E(std::move(other.errValue_));
            }
        }
        return *this;
    }
    
    // Check if result is Ok
    bool isOk() const { return isOk_; }
    bool isErr() const { return !isOk_; }
    
    // Get values (unsafe - caller must check isOk/isErr first)
    const T& unwrap() const {
        if (!isOk_) {
            throw std::runtime_error("Attempted to unwrap Err result");
        }
        return okValue_;
    }
    
    const E& unwrapErr() const {
        if (isOk_) {
            throw std::runtime_error("Attempted to unwrapErr Ok result");
        }
        return errValue_;
    }
    
    // Safe value access
    T unwrapOr(const T& defaultValue) const {
        return isOk_ ? okValue_ : defaultValue;
    }

private:
    bool isOk_;
    union {
        T okValue_;
        E errValue_;
    };
};

// Option<T> - represents Some(value) or None
template<typename T>
class Option {
public:
    // Create Some(value)
    static Option<T> some(const T& value) {
        Option<T> opt;
        opt.isSome_ = true;
        opt.value_ = value;
        return opt;
    }
    
    // Create None
    static Option<T> none() {
        Option<T> opt;
        opt.isSome_ = false;
        return opt;
    }
    
    // Check if Some
    bool isSome() const { return isSome_; }
    bool isNone() const { return !isSome_; }
    
    // Get value (unsafe)
    const T& unwrap() const {
        if (!isSome_) {
            throw std::runtime_error("Attempted to unwrap None");
        }
        return value_;
    }
    
    // Safe value access
    T unwrapOr(const T& defaultValue) const {
        return isSome_ ? value_ : defaultValue;
    }

private:
    bool isSome_;
    T value_;
    
    Option() : isSome_(false), value_() {}
};

} // namespace runtime
} // namespace first
