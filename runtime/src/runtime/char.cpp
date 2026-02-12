#include "first/runtime/gc_alloc.h"
#include <cstdint>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cctype>

// Char type: 32-bit Unicode scalar value (UTF-32).
// Valid ranges: 0..0xD7FF and 0xE000..0x10FFFF (surrogates 0xD800..0xDFFF invalid).

namespace {

inline bool is_valid_scalar(uint32_t u) {
    return (u <= 0xD7FF) || (u >= 0xE000 && u <= 0x10FFFF);
}

// Letter ranges (L*): ASCII, Latin-1, Latin Extended A/B, Greek, Cyrillic, common symbols
inline bool is_letter(uint32_t u) {
    if (u <= 0xFF) return std::isalpha(static_cast<unsigned char>(u)) != 0;
    if (u >= 0x0100 && u <= 0x024F) return true;  // Latin Extended
    if (u >= 0x0370 && u <= 0x03FF) return true; // Greek
    if (u >= 0x0400 && u <= 0x04FF) return true;  // Cyrillic
    if (u >= 0x0600 && u <= 0x06FF) return true; // Arabic
    if (u >= 0x4E00 && u <= 0x9FFF) return true; // CJK Unified Ideographs
    return false;
}

inline bool is_decimal_digit(uint32_t u) {
    if (u <= 0x39 && u >= 0x30) return true;     // 0-9
    if (u >= 0xFF10 && u <= 0xFF19) return true;  // Fullwidth digits
    return false;
}

inline bool is_whitespace_cp(uint32_t u) {
    if (u <= 0xFF) return std::isspace(static_cast<unsigned char>(u)) != 0;
    if (u == 0x00A0) return true;   // NBSP
    if (u >= 0x2000 && u <= 0x200A) return true; // EN QUAD..HAIR SPACE
    if (u == 0x202F || u == 0x205F || u == 0x3000) return true;
    return false;
}

inline bool is_upper_cp(uint32_t u) {
    if (u <= 0xFF) return std::isupper(static_cast<unsigned char>(u)) != 0;
    // Greek/Cyrillic uppercase blocks
    if (u >= 0x0391 && u <= 0x03A9) return true;
    if (u >= 0x0400 && u <= 0x042F) return true;
    return false;
}

inline bool is_lower_cp(uint32_t u) {
    if (u <= 0xFF) return std::islower(static_cast<unsigned char>(u)) != 0;
    if (u >= 0x03B1 && u <= 0x03C9) return true;
    if (u >= 0x0430 && u <= 0x044F) return true;
    return false;
}

const char* category_name(uint32_t u) {
    if (u <= 0xFF) {
        unsigned char c = static_cast<unsigned char>(u);
        if (std::isalpha(c)) return "Letter";
        if (std::isdigit(c)) return "Number";
        if (std::isspace(c)) return "Space";
        if (std::ispunct(c)) return "Punctuation";
        return "Other";
    }
    if (is_letter(u)) return "Letter";
    if (is_decimal_digit(u)) return "Number";
    if (is_whitespace_cp(u)) return "Space";
    // Punctuation: common ranges (P* in Unicode)
    if (u >= 0x2000 && u <= 0x206F) return "Punctuation"; // General Punctuation
    if (u >= 0x3000 && u <= 0x303F) return "Punctuation"; // CJK Symbols and Punctuation
    return "Other";
}

char* strdup_alloc(const char* s) {
    if (!s) return nullptr;
    size_t n = std::strlen(s) + 1;
    char* p = static_cast<char*>(first_alloc(n));
    if (p) std::memcpy(p, s, n);
    return p;
}

} // namespace

extern "C" {

// charToInt(c: Char) -> Int  (Haskell ord)
int64_t first_char_to_int(int32_t c) {
    return static_cast<int64_t>(static_cast<uint32_t>(c));
}

// charFromInt(i: Int) -> Char  (Haskell chr); invalid range aborts
int32_t first_char_from_int(int64_t i) {
    uint32_t u = static_cast<uint32_t>(i);
    if (u <= 0xD7FF) return static_cast<int32_t>(u);
    if (u >= 0xE000 && u <= 0x10FFFF) return static_cast<int32_t>(u);
    std::fprintf(stderr, "first_char_from_int: invalid Unicode code point U+%04X\n", static_cast<unsigned>(u));
    std::abort();
}

// charSucc(c: Char) -> Char  (Haskell succ; next scalar, skip surrogate range)
int32_t first_char_succ(int32_t c) {
    uint32_t u = static_cast<uint32_t>(c);
    if (u >= 0x10FFFF) {
        std::fprintf(stderr, "first_char_succ: no successor for U+10FFFF\n");
        std::abort();
    }
    u++;
    if (u >= 0xD800 && u <= 0xDFFF) u = 0xE000; // skip surrogates
    return static_cast<int32_t>(u);
}

// charPred(c: Char) -> Char  (Haskell pred; previous scalar, skip surrogate range)
int32_t first_char_pred(int32_t c) {
    uint32_t u = static_cast<uint32_t>(c);
    if (u == 0) {
        std::fprintf(stderr, "first_char_pred: no predecessor for U+0000\n");
        std::abort();
    }
    u--;
    if (u >= 0xD800 && u <= 0xDFFF) u = 0xD7FF; // skip surrogates
    return static_cast<int32_t>(u);
}

// charCategory(c: Char) -> String  (Unicode General Category label)
char* first_char_category(int32_t c) {
    uint32_t u = static_cast<uint32_t>(c);
    return strdup_alloc(category_name(u));
}

// Categorization predicates (return 0 = false, 1 = true for First Bool)
int64_t first_char_is_alpha(int32_t c) {
    return is_letter(static_cast<uint32_t>(c)) ? 1 : 0;
}
int64_t first_char_is_digit(int32_t c) {
    return is_decimal_digit(static_cast<uint32_t>(c)) ? 1 : 0;
}
int64_t first_char_is_whitespace(int32_t c) {
    return is_whitespace_cp(static_cast<uint32_t>(c)) ? 1 : 0;
}
int64_t first_char_is_upper(int32_t c) {
    return is_upper_cp(static_cast<uint32_t>(c)) ? 1 : 0;
}
int64_t first_char_is_lower(int32_t c) {
    return is_lower_cp(static_cast<uint32_t>(c)) ? 1 : 0;
}

// charToString(c: Char) -> String  (single code point encoded as UTF-8)
char* first_char_to_string(int32_t c) {
    uint32_t u = static_cast<uint32_t>(c);
    unsigned char buf[5];
    size_t n = 0;
    if (u <= 0x7F) {
        buf[n++] = static_cast<unsigned char>(u);
    } else if (u <= 0x7FF) {
        buf[n++] = static_cast<unsigned char>(0xC0 | (u >> 6));
        buf[n++] = static_cast<unsigned char>(0x80 | (u & 0x3F));
    } else if (u <= 0xFFFF) {
        buf[n++] = static_cast<unsigned char>(0xE0 | (u >> 12));
        buf[n++] = static_cast<unsigned char>(0x80 | ((u >> 6) & 0x3F));
        buf[n++] = static_cast<unsigned char>(0x80 | (u & 0x3F));
    } else {
        buf[n++] = static_cast<unsigned char>(0xF0 | (u >> 18));
        buf[n++] = static_cast<unsigned char>(0x80 | ((u >> 12) & 0x3F));
        buf[n++] = static_cast<unsigned char>(0x80 | ((u >> 6) & 0x3F));
        buf[n++] = static_cast<unsigned char>(0x80 | (u & 0x3F));
    }
    char* p = static_cast<char*>(first_alloc(n + 1));
    if (p) {
        std::memcpy(p, buf, n);
        p[n] = '\0';
    }
    return p;
}

} // extern "C"
