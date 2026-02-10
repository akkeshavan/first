#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Date is an opaque handle (Unix timestamp in seconds, or 0 for invalid).
typedef int64_t first_date_t;

// Current time (UTC) as a Date handle.
first_date_t first_date_now(void);

// Format a date with strftime-style format (e.g. "%Y-%m-%d %H:%M:%S"). Returns heap-allocated string; caller must free.
const char* first_date_format(first_date_t d, const char* fmt);

// Parse ISO-like "YYYY-MM-DD" or "YYYY-MM-DDTHH:MM:SS"; returns 0 on failure.
first_date_t first_date_parse(const char* s);

// Getters (UTC). Undefined if d is 0.
int64_t first_date_get_year(first_date_t d);
int64_t first_date_get_month(first_date_t d);   // 1-12
int64_t first_date_get_day(first_date_t d);     // 1-31
int64_t first_date_get_hours(first_date_t d);   // 0-23
int64_t first_date_get_minutes(first_date_t d); // 0-59
int64_t first_date_get_seconds(first_date_t d); // 0-59

// Arithmetic: add seconds to a date (can be negative). Returns new handle.
first_date_t first_date_add_seconds(first_date_t d, int64_t seconds);

#ifdef __cplusplus
}
#endif
