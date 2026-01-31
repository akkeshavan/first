#include "first/runtime/regex.h"
#include "first/runtime/gc_alloc.h"
#include <regex>
#include <string>
#include <vector>
#include <cstring>

extern "C" {

int64_t first_regex_matches(const char* str, const char* pattern) {
    if (!str || !pattern) return -1;
    
    try {
        std::regex re(pattern);
        return std::regex_match(str, re) ? 1 : 0;
    } catch (const std::regex_error&) {
        return -1; // Invalid regex pattern
    }
}

int64_t first_regex_search(const char* str, const char* pattern) {
    if (!str || !pattern) return -1;
    
    try {
        std::regex re(pattern);
        std::cmatch match;
        if (std::regex_search(str, match, re)) {
            return static_cast<int64_t>(match.position(0));
        }
        return -1; // Not found
    } catch (const std::regex_error&) {
        return -1; // Invalid regex pattern
    }
}

char* first_regex_replace(const char* str, const char* pattern, const char* replacement) {
    if (!str || !pattern || !replacement) return nullptr;
    
    try {
        std::regex re(pattern);
        std::string input(str);
        std::string result = std::regex_replace(
            input, re, replacement,
            std::regex_constants::format_first_only
        );
        
        // Allocate result string using First's memory allocator
        size_t len = result.size() + 1;
        char* output = static_cast<char*>(first_alloc(len));
        if (output) {
            std::memcpy(output, result.c_str(), len);
        }
        return output;
    } catch (const std::regex_error&) {
        return nullptr; // Invalid regex pattern
    }
}

char* first_regex_replace_all(const char* str, const char* pattern, const char* replacement) {
    if (!str || !pattern || !replacement) return nullptr;
    
    try {
        std::regex re(pattern);
        std::string input(str);
        std::string result = std::regex_replace(input, re, replacement);
        
        // Allocate result string using First's memory allocator
        size_t len = result.size() + 1;
        char* output = static_cast<char*>(first_alloc(len));
        if (output) {
            std::memcpy(output, result.c_str(), len);
        }
        return output;
    } catch (const std::regex_error&) {
        return nullptr; // Invalid regex pattern
    }
}

char** first_regex_split(const char* str, const char* pattern, int64_t* count_out) {
    if (!str || !pattern || !count_out) {
        if (count_out) *count_out = 0;
        return nullptr;
    }
    
    try {
        std::regex re(pattern);
        std::string input(str);
        
        // Use regex_token_iterator to split
        std::sregex_token_iterator iter(input.begin(), input.end(), re, -1);
        std::sregex_token_iterator end;
        
        std::vector<std::string> parts;
        for (; iter != end; ++iter) {
            parts.push_back(*iter);
        }
        
        // Allocate array of string pointers (null-terminated)
        size_t count = parts.size();
        char** result = static_cast<char**>(first_alloc((count + 1) * sizeof(char*)));
        if (!result) {
            *count_out = 0;
            return nullptr;
        }
        
        // Allocate and copy each string
        for (size_t i = 0; i < count; ++i) {
            size_t len = parts[i].size() + 1;
            result[i] = static_cast<char*>(first_alloc(len));
            if (result[i]) {
                std::memcpy(result[i], parts[i].c_str(), len);
            }
        }
        result[count] = nullptr; // Null terminator
        
        *count_out = static_cast<int64_t>(count);
        return result;
    } catch (const std::regex_error&) {
        *count_out = 0;
        return nullptr; // Invalid regex pattern
    }
}

char* first_regex_extract(const char* str, const char* pattern, int64_t group_index) {
    if (!str || !pattern) return nullptr;
    
    try {
        std::regex re(pattern);
        std::cmatch match;
        
        if (std::regex_search(str, match, re)) {
            if (group_index >= 0 && static_cast<size_t>(group_index) < match.size()) {
                std::string captured = match[group_index].str();
                
                // Allocate result string using First's memory allocator
                size_t len = captured.size() + 1;
                char* output = static_cast<char*>(first_alloc(len));
                if (output) {
                    std::memcpy(output, captured.c_str(), len);
                }
                return output;
            }
        }
        return nullptr; // No match or invalid group index
    } catch (const std::regex_error&) {
        return nullptr; // Invalid regex pattern
    }
}

} // extern "C"
