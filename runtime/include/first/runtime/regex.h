#pragma once

#include <cstdint>

// C-linkage regex functions for First language
extern "C" {

// Check if string matches regex pattern
// Returns 1 if matches, 0 if not, -1 on error
int64_t first_regex_matches(const char* str, const char* pattern);

// Search for first match of regex pattern in string
// Returns index of first match, or -1 if not found
int64_t first_regex_search(const char* str, const char* pattern);

// Replace first occurrence of pattern with replacement
// Returns newly allocated string (caller must manage memory)
char* first_regex_replace(const char* str, const char* pattern, const char* replacement);

// Replace all occurrences of pattern with replacement
// Returns newly allocated string (caller must manage memory)
char* first_regex_replace_all(const char* str, const char* pattern, const char* replacement);

// Split string by regex pattern
// Returns array of strings (null-terminated array, caller must manage memory)
// count_out receives the number of splits
char** first_regex_split(const char* str, const char* pattern, int64_t* count_out);

// Extract first capturing group from regex match
// Returns newly allocated string or NULL if no match
char* first_regex_extract(const char* str, const char* pattern, int64_t group_index);

} // extern "C"
