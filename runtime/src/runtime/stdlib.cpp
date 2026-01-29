#include "first/runtime/stdlib.h"
#include "first/runtime/io.h"
#include "first/runtime/string.h"
#include <cmath>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <algorithm>
#include <limits>

#ifndef _WIN32
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#endif

namespace {

const size_t kLineBuf = 65536;
const size_t kFileBuf = 1024 * 1024;
thread_local char g_readLineBuf[kLineBuf];
thread_local char g_readFileBuf[kFileBuf];

void copy_to_cstr(const std::string& s, char* buf, size_t cap) {
    size_t n = std::min(s.size(), cap - 1);
    std::memcpy(buf, s.data(), n);
    buf[n] = '\0';
}

char* strdup_heap(const char* s) {
    if (!s) return nullptr;
    size_t n = std::strlen(s) + 1;
    char* p = static_cast<char*>(std::malloc(n));
    if (p) std::memcpy(p, s, n);
    return p;
}

} // namespace

extern "C" {

// --- I/O ---
const char* readLine(void) {
    std::string line;
    if (std::getline(std::cin, line)) {
        copy_to_cstr(line, g_readLineBuf, kLineBuf);
    } else {
        g_readLineBuf[0] = '\0';
    }
    return g_readLineBuf;
}

const char* readFile(const char* filename) {
    if (!filename) return nullptr;
    std::ifstream f(filename);
    if (!f.is_open()) return nullptr;
    std::ostringstream oss;
    oss << f.rdbuf();
    std::string s = oss.str();
    size_t n = std::min(s.size(), kFileBuf - 1);
    std::memcpy(g_readFileBuf, s.data(), n);
    g_readFileBuf[n] = '\0';
    return g_readFileBuf;
}

void writeFile(const char* filename, const char* content) {
    if (!filename) return;
    std::ofstream f(filename);
    if (f.is_open() && content) f << content;
}

// --- Math (libm) ---
double first_sin(double x) { return std::sin(x); }
double first_cos(double x) { return std::cos(x); }
double first_sqrt(double x) { return std::sqrt(x); }
double first_abs(double x) { return std::abs(x); }
double first_floor(double x) { return std::floor(x); }
double first_ceil(double x) { return std::ceil(x); }
double first_min(double a, double b) { return std::min(a, b); }
double first_max(double a, double b) { return std::max(a, b); }
int64_t first_min_int(int64_t a, int64_t b) { return std::min(a, b); }
int64_t first_max_int(int64_t a, int64_t b) { return std::max(a, b); }

// --- String ---
int64_t first_string_length(const char* s) {
    return s ? static_cast<int64_t>(std::strlen(s)) : 0;
}

char* first_string_concat(const char* s1, const char* s2) {
    if (!s1) s1 = "";
    if (!s2) s2 = "";
    size_t n1 = std::strlen(s1), n2 = std::strlen(s2);
    char* p = static_cast<char*>(std::malloc(n1 + n2 + 1));
    if (!p) return nullptr;
    std::memcpy(p, s1, n1);
    std::memcpy(p + n1, s2, n2 + 1);
    return p;
}

char* first_string_slice(const char* s, int64_t start, int64_t end) {
    if (!s) return strdup_heap("");
    int64_t len = static_cast<int64_t>(std::strlen(s));
    if (start < 0) start = 0;
    if (end > len) end = len;
    if (start >= end) return strdup_heap("");
    size_t n = static_cast<size_t>(end - start);
    char* p = static_cast<char*>(std::malloc(n + 1));
    if (!p) return nullptr;
    std::memcpy(p, s + start, n);
    p[n] = '\0';
    return p;
}

int64_t first_string_to_int(const char* s) {
    if (!s) return 0;
    return std::strtoll(s, nullptr, 10);
}

double first_string_to_float(const char* s) {
    if (!s) return 0.0;
    return std::strtod(s, nullptr);
}

char* first_int_to_string(int64_t n) {
    std::string s = std::to_string(n);
    return strdup_heap(s.c_str());
}

char* first_float_to_string(double x) {
    std::string s = std::to_string(x);
    return strdup_heap(s.c_str());
}

// --- Array ---
int64_t first_array_length(const void* arr, int64_t known_len) {
    (void)arr;
    return known_len;
}

// --- Socket (TCP client) ---
int64_t first_socket_connect(const char* host, int64_t port) {
#ifndef _WIN32
    if (!host || port < 0 || port > 65535) return -1;
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    struct hostent* he = gethostbyname(host);
    if (!he) { close(fd); return -1; }
    struct sockaddr_in addr;
    std::memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(static_cast<uint16_t>(port));
    std::memcpy(&addr.sin_addr, he->h_addr, static_cast<size_t>(he->h_length));
    if (connect(fd, reinterpret_cast<struct sockaddr*>(&addr), sizeof(addr)) < 0) {
        close(fd);
        return -1;
    }
    return static_cast<int64_t>(fd);
#else
    (void)host;
    (void)port;
    return -1;
#endif
}

int64_t first_socket_send(int64_t fd, const char* buf) {
#ifndef _WIN32
    if (fd < 0 || !buf) return -1;
    size_t len = std::strlen(buf);
    ssize_t n = send(static_cast<int>(fd), buf, len, 0);
    return n >= 0 ? static_cast<int64_t>(n) : -1;
#else
    (void)fd;
    (void)buf;
    return -1;
#endif
}

int64_t first_socket_recv(int64_t fd, char* buf, int64_t cap) {
#ifndef _WIN32
    if (fd < 0 || !buf || cap <= 0) return -1;
    ssize_t n = recv(static_cast<int>(fd), buf, static_cast<size_t>(cap - 1), 0);
    if (n <= 0) return n;
    buf[n] = '\0';
    return static_cast<int64_t>(n);
#else
    (void)fd;
    (void)buf;
    (void)cap;
    return -1;
#endif
}

static thread_local char g_socket_recv_buf[kLineBuf];

const char* first_socket_recv_str(int64_t fd) {
#ifndef _WIN32
    if (fd < 0) return nullptr;
    ssize_t n = recv(static_cast<int>(fd), g_socket_recv_buf, kLineBuf - 1, 0);
    if (n <= 0) { g_socket_recv_buf[0] = '\0'; return g_socket_recv_buf; }
    g_socket_recv_buf[n] = '\0';
    return g_socket_recv_buf;
#else
    (void)fd;
    return nullptr;
#endif
}

void first_socket_close(int64_t fd) {
#ifndef _WIN32
    if (fd >= 0) close(static_cast<int>(fd));
#else
    (void)fd;
#endif
}

// --- HTTP (minimal: socket + simple GET/POST) ---
static char* http_via_socket(const char* url, const char* method, const char* body) {
    (void)url;
    (void)method;
    (void)body;
    return nullptr;  // TODO: parse URL, connect, send "METHOD path HTTP/1.0\r\nHost: ...\r\n\r\n", read response
}

char* first_http_get(const char* url) {
    return http_via_socket(url, "GET", nullptr);
}

char* first_http_post(const char* url, const char* body) {
    return http_via_socket(url, "POST", body);
}

// --- JSON (minimal: prettify = indent; stringify = trivial) ---
char* first_json_prettify(const char* json_str) {
    if (!json_str) return nullptr;
    std::string s;
    int depth = 0;
    bool after_value = false;
    for (const char* p = json_str; *p; ++p) {
        char c = *p;
        if (c == '{' || c == '[') {
            if (after_value) s += '\n';
            s += std::string(2 * depth, ' ') + c + '\n';
            depth++;
            after_value = false;
        } else if (c == '}' || c == ']') {
            depth--;
            s += '\n' + std::string(2 * depth, ' ') + c;
            after_value = true;
        } else if (c == ',') {
            s += ",\n";
            after_value = false;
        } else if (c == ':') {
            s += ": ";
            after_value = false;
        } else if (c != ' ' && c != '\n' && c != '\r' && c != '\t') {
            s += c;
            after_value = true;
        }
    }
    return strdup_heap(s.c_str());
}

char* first_json_stringify_int(int64_t n) {
    return first_int_to_string(n);
}

char* first_json_stringify_float(double x) {
    return first_float_to_string(x);
}

char* first_json_stringify_string(const char* s) {
    if (!s) return strdup_heap("\"\"");
    std::string out = "\"";
    for (; *s; ++s) {
        if (*s == '"') out += "\\\"";
        else if (*s == '\\') out += "\\\\";
        else if (*s == '\n') out += "\\n";
        else if (*s == '\r') out += "\\r";
        else if (*s == '\t') out += "\\t";
        else out += *s;
    }
    out += '"';
    return strdup_heap(out.c_str());
}

} // extern "C"
