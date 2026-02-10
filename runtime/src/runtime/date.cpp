#include "first/runtime/date.h"
#include "first/runtime/gc_alloc.h"
#include <ctime>
#include <cstring>
#include <cstdlib>
#include <sstream>
#include <iomanip>

namespace {

first_date_t to_handle(std::time_t t) {
    return static_cast<first_date_t>(t);
}

std::time_t from_handle(first_date_t d) {
    return static_cast<std::time_t>(d);
}

} // namespace

extern "C" {

first_date_t first_date_now(void) {
    return to_handle(std::time(nullptr));
}

const char* first_date_format(first_date_t d, const char* fmt) {
    if (!fmt) return nullptr;
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    if (!utc) return nullptr;
    char buf[256];
    size_t n = std::strftime(buf, sizeof(buf), fmt, utc);
    if (n == 0) return nullptr;
    char* p = static_cast<char*>(first_alloc(n + 1));
    if (!p) return nullptr;
    std::memcpy(p, buf, n + 1);
    return p;
}

first_date_t first_date_parse(const char* s) {
    if (!s || !*s) return 0;
    int year = 0, month = 1, day = 1, hour = 0, min = 0, sec = 0;
    int n = std::sscanf(s, "%d-%d-%dT%d:%d:%d", &year, &month, &day, &hour, &min, &sec);
    if (n < 3) n = std::sscanf(s, "%d-%d-%d", &year, &month, &day);
    if (n < 3) return 0;
    std::tm t = {};
    t.tm_year = year - 1900;
    t.tm_mon = month - 1;
    t.tm_mday = day;
    t.tm_hour = hour;
    t.tm_min = min;
    t.tm_sec = sec;
    t.tm_isdst = 0;
    std::time_t tt = std::mktime(&t);
    if (tt == static_cast<std::time_t>(-1)) return 0;
    return to_handle(tt);
}

int64_t first_date_get_year(first_date_t d) {
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    return utc ? (utc->tm_year + 1900) : 0;
}

int64_t first_date_get_month(first_date_t d) {
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    return utc ? (utc->tm_mon + 1) : 0;
}

int64_t first_date_get_day(first_date_t d) {
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    return utc ? utc->tm_mday : 0;
}

int64_t first_date_get_hours(first_date_t d) {
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    return utc ? utc->tm_hour : 0;
}

int64_t first_date_get_minutes(first_date_t d) {
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    return utc ? utc->tm_min : 0;
}

int64_t first_date_get_seconds(first_date_t d) {
    std::time_t t = from_handle(d);
    std::tm* utc = std::gmtime(&t);
    return utc ? utc->tm_sec : 0;
}

first_date_t first_date_add_seconds(first_date_t d, int64_t seconds) {
    return to_handle(from_handle(d) + static_cast<std::time_t>(seconds));
}

} // extern "C"
