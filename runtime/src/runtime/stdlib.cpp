#include "first/runtime/stdlib.h"
#include "first/runtime/io.h"
#include "first/runtime/string.h"
#include "first/runtime/regex.h"
#include "first/runtime/gc_alloc.h"
#include <cmath>
#include <cstring>
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <algorithm>
#include <limits>
#include <map>
#include <vector>
#include <string>

#ifndef _WIN32
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
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
    char* p = static_cast<char*>(first_alloc(n));
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
    char* p = static_cast<char*>(first_alloc(n1 + n2 + 1));
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
    char* p = static_cast<char*>(first_alloc(n + 1));
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

char* first_bool_to_string(int64_t b) {
    return strdup_heap(b ? "true" : "false");
}

char* first_unit_to_string(int64_t) {
    return strdup_heap("()");
}

// --- String Comparison (exposed to First) ---
// Note: The low-level C functions are in string.cpp
// These wrappers use the same signatures for consistency
bool stringEquals(const char* a, const char* b) {
    return first_string_equals(a, b);
}

int64_t stringCompare(const char* a, const char* b) {
    return first_string_compare(a, b);
}

// --- Regular Expressions (exposed to First) ---
// Returns 1 if matches, 0 if not, -1 on error
int64_t regexMatches(const char* str, const char* pattern) {
    return first_regex_matches(str, pattern);
}

// Returns index of first match, or -1 if not found
int64_t regexSearch(const char* str, const char* pattern) {
    return first_regex_search(str, pattern);
}

// Replace first occurrence
char* regexReplace(const char* str, const char* pattern, const char* replacement) {
    return first_regex_replace(str, pattern, replacement);
}

// Replace all occurrences
char* regexReplaceAll(const char* str, const char* pattern, const char* replacement) {
    return first_regex_replace_all(str, pattern, replacement);
}

// Split string by pattern
// Returns count via count_out parameter
char** regexSplit(const char* str, const char* pattern, int64_t* count_out) {
    return first_regex_split(str, pattern, count_out);
}

// Extract capturing group (0 = whole match, 1+ = groups)
char* regexExtract(const char* str, const char* pattern, int64_t group_index) {
    return first_regex_extract(str, pattern, group_index);
}

// --- Array ---
int64_t first_array_length(const void* arr, int64_t known_len) {
    (void)arr;
    return known_len;
}

int64_t first_array_reduce_int_sum(const int64_t* arr, int64_t len) {
    if (!arr || len <= 0) return 0;
    int64_t sum = 0;
    for (int64_t i = 0; i < len; ++i) sum += arr[i];
    return sum;
}

int64_t* first_array_map_int_double(const int64_t* arr, int64_t len) {
    if (!arr || len <= 0) return nullptr;
    int64_t* out = static_cast<int64_t*>(first_alloc(static_cast<size_t>(len) * sizeof(int64_t)));
    if (!out) return nullptr;
    for (int64_t i = 0; i < len; ++i) out[i] = arr[i] * 2;
    return out;
}

int64_t* first_array_filter_int_positive(const int64_t* arr, int64_t len, int64_t* out_len) {
    if (!arr || !out_len) { if (out_len) *out_len = 0; return nullptr; }
    std::vector<int64_t> tmp;
    for (int64_t i = 0; i < len; ++i) if (arr[i] > 0) tmp.push_back(arr[i]);
    *out_len = static_cast<int64_t>(tmp.size());
    if (tmp.empty()) return nullptr;
    int64_t* out = static_cast<int64_t*>(first_alloc(tmp.size() * sizeof(int64_t)));
    if (!out) return nullptr;
    for (size_t i = 0; i < tmp.size(); ++i) out[i] = tmp[i];
    return out;
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

// --- HTTP Client/Server (dev implementation) ---
#ifndef _WIN32
namespace {

struct HttpResponse {
    int64_t status = 0;
    std::string headers_json;
    std::string body;
};

struct HttpRequest {
    std::string method;
    std::string path;
    std::string params_json;
    std::string query_json;
    std::string headers_json;
    std::string body;
};

using HandlerFn = int64_t(*)(int64_t); // (reqHandle) -> respHandle

static int64_t ptr_to_handle(void* p) {
    return static_cast<int64_t>(reinterpret_cast<intptr_t>(p));
}
static void* handle_to_ptr(int64_t h) {
    return reinterpret_cast<void*>(static_cast<intptr_t>(h));
}

static bool parse_http_url(const std::string& url, std::string& host, int64_t& port, std::string& path) {
    // Very small parser: http://host[:port]/path
    std::string u = url;
    const std::string prefix = "http://";
    if (u.rfind(prefix, 0) == 0) u = u.substr(prefix.size());
    auto slash = u.find('/');
    std::string hostport = (slash == std::string::npos) ? u : u.substr(0, slash);
    path = (slash == std::string::npos) ? "/" : u.substr(slash);
    auto colon = hostport.find(':');
    if (colon == std::string::npos) {
        host = hostport;
        port = 80;
    } else {
        host = hostport.substr(0, colon);
        port = std::strtoll(hostport.substr(colon + 1).c_str(), nullptr, 10);
    }
    return !host.empty() && port > 0;
}

static std::string json_escape(const std::string& s) {
    std::string out;
    out.reserve(s.size() + 8);
    for (char c : s) {
        switch (c) {
        case '\"': out += "\\\""; break;
        case '\\': out += "\\\\"; break;
        case '\n': out += "\\n"; break;
        case '\r': out += "\\r"; break;
        case '\t': out += "\\t"; break;
        default: out += c; break;
        }
    }
    return out;
}

static std::string kv_to_json_object(const std::map<std::string, std::string>& kv) {
    std::string out = "{";
    bool first = true;
    for (const auto& [k, v] : kv) {
        if (!first) out += ",";
        first = false;
        out += "\"" + json_escape(k) + "\":\"" + json_escape(v) + "\"";
    }
    out += "}";
    return out;
}

static std::map<std::string, std::string> parse_query_kv(const std::string& qs) {
    std::map<std::string, std::string> m;
    size_t i = 0;
    while (i < qs.size()) {
        size_t amp = qs.find('&', i);
        std::string pair = qs.substr(i, amp == std::string::npos ? std::string::npos : amp - i);
        size_t eq = pair.find('=');
        std::string k = (eq == std::string::npos) ? pair : pair.substr(0, eq);
        std::string v = (eq == std::string::npos) ? "" : pair.substr(eq + 1);
        if (!k.empty()) m[k] = v;
        if (amp == std::string::npos) break;
        i = amp + 1;
    }
    return m;
}

static void split_path_query(const std::string& full, std::string& path, std::string& query) {
    auto q = full.find('?');
    if (q == std::string::npos) { path = full; query.clear(); return; }
    path = full.substr(0, q);
    query = full.substr(q + 1);
}

static bool route_match(const std::string& route, const std::string& path, std::map<std::string, std::string>& params) {
    // route like /users/:id/books/:bookId
    auto split = [](const std::string& s) {
        std::vector<std::string> parts;
        size_t i = 0;
        while (i < s.size()) {
            while (i < s.size() && s[i] == '/') ++i;
            if (i >= s.size()) break;
            size_t j = s.find('/', i);
            parts.push_back(s.substr(i, j == std::string::npos ? std::string::npos : j - i));
            if (j == std::string::npos) break;
            i = j + 1;
        }
        return parts;
    };
    auto r = split(route);
    auto p = split(path);
    if (r.size() != p.size()) return false;
    for (size_t i = 0; i < r.size(); ++i) {
        const std::string& seg = r[i];
        if (!seg.empty() && seg[0] == ':') {
            params[seg.substr(1)] = p[i];
        } else if (seg != p[i]) {
            return false;
        }
    }
    return true;
}

struct Route {
    std::string method;
    std::string route;
    HandlerFn handler = nullptr;
};

struct HttpServer {
    std::string host;
    int64_t port = 0;
    int listen_fd = -1;
    bool should_close = false;
    std::vector<Route> routes;
};

static bool read_line(int fd, std::string& line) {
    line.clear();
    char c;
    while (true) {
        ssize_t n = recv(fd, &c, 1, 0);
        if (n <= 0) return false;
        if (c == '\r') continue;
        if (c == '\n') break;
        line.push_back(c);
        if (line.size() > 8192) break;
    }
    return true;
}

static bool read_exact(int fd, std::string& out, size_t n) {
    out.clear();
    out.resize(n);
    size_t off = 0;
    while (off < n) {
        ssize_t r = recv(fd, &out[off], n - off, 0);
        if (r <= 0) return false;
        off += static_cast<size_t>(r);
    }
    return true;
}

static void write_all(int fd, const std::string& s) {
    size_t off = 0;
    while (off < s.size()) {
        ssize_t n = send(fd, s.data() + off, s.size() - off, 0);
        if (n <= 0) return;
        off += static_cast<size_t>(n);
    }
}

static std::string status_text(int64_t code) {
    switch (code) {
    case 200: return "OK";
    case 400: return "Bad Request";
    case 404: return "Not Found";
    case 500: return "Internal Server Error";
    default: return "OK";
    }
}

static int create_listen_socket(const std::string& host, int64_t port) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) return -1;
    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    sockaddr_in addr{};
    addr.sin_family = AF_INET;
    addr.sin_port = htons(static_cast<uint16_t>(port));
    addr.sin_addr.s_addr = host.empty() ? htonl(INADDR_ANY) : inet_addr(host.c_str());
    if (bind(fd, reinterpret_cast<sockaddr*>(&addr), sizeof(addr)) != 0) { close(fd); return -1; }
    if (listen(fd, 64) != 0) { close(fd); return -1; }
    return fd;
}

static int64_t http_response_create_internal(int64_t status, const char* headers_json, const char* body) {
    auto* r = new HttpResponse();
    r->status = status;
    r->headers_json = headers_json ? headers_json : "{}";
    r->body = body ? body : "";
    return ptr_to_handle(r);
}

static void handle_client(HttpServer* s, int cfd) {
    std::string requestLine;
    if (!read_line(cfd, requestLine)) { close(cfd); return; }
    std::string method, target, version;
    {
        std::istringstream iss(requestLine);
        iss >> method >> target >> version;
    }
    std::map<std::string, std::string> headers;
    while (true) {
        std::string line;
        if (!read_line(cfd, line)) { close(cfd); return; }
        if (line.empty()) break;
        auto colon = line.find(':');
        if (colon != std::string::npos) {
            std::string k = line.substr(0, colon);
            std::string v = line.substr(colon + 1);
            while (!v.empty() && v[0] == ' ') v.erase(v.begin());
            headers[k] = v;
        }
    }
    size_t contentLen = 0;
    if (headers.find("Content-Length") != headers.end()) {
        contentLen = static_cast<size_t>(std::strtoll(headers["Content-Length"].c_str(), nullptr, 10));
    }
    std::string body;
    if (contentLen > 0) {
        if (!read_exact(cfd, body, contentLen)) body.clear();
    }
    std::string path, query;
    split_path_query(target, path, query);
    auto queryKv = parse_query_kv(query);

    Route* matched = nullptr;
    std::map<std::string, std::string> params;
    for (auto& r : s->routes) {
        if (r.method != method) continue;
        std::map<std::string, std::string> tmp;
        if (route_match(r.route, path, tmp)) {
            matched = &r;
            params = std::move(tmp);
            break;
        }
    }

    int64_t respHandle = 0;
    if (matched && matched->handler) {
        auto* req = new HttpRequest();
        req->method = method;
        req->path = path;
        req->params_json = kv_to_json_object(params);
        req->query_json = kv_to_json_object(queryKv);
        req->headers_json = kv_to_json_object(headers);
        req->body = body;
        int64_t reqHandle = ptr_to_handle(req);
        respHandle = matched->handler(reqHandle);
        delete req;
    } else {
        respHandle = http_response_create_internal(404, "{}", "Not Found");
    }

    auto* resp = static_cast<HttpResponse*>(handle_to_ptr(respHandle));
    if (!resp) resp = static_cast<HttpResponse*>(handle_to_ptr(http_response_create_internal(500, "{}", "Internal Server Error")));

    std::string outBody = resp->body;
    std::string out =
        "HTTP/1.1 " + std::to_string(resp->status) + " " + status_text(resp->status) + "\r\n"
        "Content-Type: text/plain\r\n"
        "Content-Length: " + std::to_string(outBody.size()) + "\r\n"
        "Connection: close\r\n"
        "\r\n" + outBody;
    write_all(cfd, out);
    close(cfd);
}

} // namespace
#endif // !_WIN32

int64_t first_http_server_create(const char* host, int64_t port) {
#ifndef _WIN32
    auto* s = new HttpServer();
    s->host = host ? host : "";
    s->port = port;
    return ptr_to_handle(s);
#else
    (void)host; (void)port;
    return 0;
#endif
}

void first_http_server_get(int64_t server, const char* route, int64_t handler_fn_ptr) {
#ifndef _WIN32
    auto* s = static_cast<HttpServer*>(handle_to_ptr(server));
    if (!s || !route) return;
    Route r;
    r.method = "GET";
    r.route = route;
    r.handler = reinterpret_cast<HandlerFn>(static_cast<intptr_t>(handler_fn_ptr));
    s->routes.push_back(std::move(r));
#else
    (void)server; (void)route; (void)handler_fn_ptr;
#endif
}

void first_http_server_post(int64_t server, const char* route, int64_t handler_fn_ptr) {
#ifndef _WIN32
    auto* s = static_cast<HttpServer*>(handle_to_ptr(server));
    if (!s || !route) return;
    Route r;
    r.method = "POST";
    r.route = route;
    r.handler = reinterpret_cast<HandlerFn>(static_cast<intptr_t>(handler_fn_ptr));
    s->routes.push_back(std::move(r));
#else
    (void)server; (void)route; (void)handler_fn_ptr;
#endif
}

void first_http_server_listen(int64_t server) {
#ifndef _WIN32
    auto* s = static_cast<HttpServer*>(handle_to_ptr(server));
    if (!s) return;
    if (s->listen_fd < 0) {
        s->listen_fd = create_listen_socket(s->host, s->port);
    }
    if (s->listen_fd < 0) return;
    while (!s->should_close) {
        int cfd = accept(s->listen_fd, nullptr, nullptr);
        if (cfd < 0) continue;
        handle_client(s, cfd);
    }
#else
    (void)server;
#endif
}

void first_http_server_close(int64_t server) {
#ifndef _WIN32
    auto* s = static_cast<HttpServer*>(handle_to_ptr(server));
    if (!s) return;
    s->should_close = true;
    if (s->listen_fd >= 0) close(s->listen_fd);
#else
    (void)server;
#endif
}

const char* first_http_req_method(int64_t req) {
#ifndef _WIN32
    auto* r = static_cast<HttpRequest*>(handle_to_ptr(req));
    return r ? r->method.c_str() : nullptr;
#else
    (void)req; return nullptr;
#endif
}
const char* first_http_req_path(int64_t req) {
#ifndef _WIN32
    auto* r = static_cast<HttpRequest*>(handle_to_ptr(req));
    return r ? r->path.c_str() : nullptr;
#else
    (void)req; return nullptr;
#endif
}
const char* first_http_req_params_json(int64_t req) {
#ifndef _WIN32
    auto* r = static_cast<HttpRequest*>(handle_to_ptr(req));
    return r ? r->params_json.c_str() : nullptr;
#else
    (void)req; return nullptr;
#endif
}
const char* first_http_req_query_json(int64_t req) {
#ifndef _WIN32
    auto* r = static_cast<HttpRequest*>(handle_to_ptr(req));
    return r ? r->query_json.c_str() : nullptr;
#else
    (void)req; return nullptr;
#endif
}
const char* first_http_req_headers_json(int64_t req) {
#ifndef _WIN32
    auto* r = static_cast<HttpRequest*>(handle_to_ptr(req));
    return r ? r->headers_json.c_str() : nullptr;
#else
    (void)req; return nullptr;
#endif
}
const char* first_http_req_body(int64_t req) {
#ifndef _WIN32
    auto* r = static_cast<HttpRequest*>(handle_to_ptr(req));
    return r ? r->body.c_str() : nullptr;
#else
    (void)req; return nullptr;
#endif
}

int64_t first_http_response_create(int64_t status, const char* headers_json, const char* body) {
#ifndef _WIN32
    return http_response_create_internal(status, headers_json, body);
#else
    (void)status; (void)headers_json; (void)body;
    return 0;
#endif
}
int64_t first_http_resp_status(int64_t resp) {
#ifndef _WIN32
    auto* r = static_cast<HttpResponse*>(handle_to_ptr(resp));
    return r ? r->status : 0;
#else
    (void)resp; return 0;
#endif
}
const char* first_http_resp_headers_json(int64_t resp) {
#ifndef _WIN32
    auto* r = static_cast<HttpResponse*>(handle_to_ptr(resp));
    return r ? r->headers_json.c_str() : nullptr;
#else
    (void)resp; return nullptr;
#endif
}
const char* first_http_resp_body(int64_t resp) {
#ifndef _WIN32
    auto* r = static_cast<HttpResponse*>(handle_to_ptr(resp));
    return r ? r->body.c_str() : nullptr;
#else
    (void)resp; return nullptr;
#endif
}

int64_t first_http_request(const char* method,
                           const char* url,
                           const char* path_params_json,
                           const char* query_json,
                           const char* headers_json,
                           const char* body) {
#ifndef _WIN32
    (void)path_params_json;
    (void)headers_json;
    // Minimal generic client: supports http://host[:port]/path and sends body for POST.
    if (!method || !url) return 0;
    std::string host, path;
    int64_t port = 80;
    if (!parse_http_url(url, host, port, path)) return 0;
    // Append query_json as raw query string if provided and not "{}"
    if (query_json && std::strcmp(query_json, "{}") != 0) {
        // Expect query_json already URL-encoded key/values is out-of-scope for now.
        // For dev use: allow passing "?a=b" literal via query_json by convention.
        if (query_json[0] == '?') path += query_json;
    }
    int64_t fd = first_socket_connect(host.c_str(), port);
    if (fd < 0) return 0;
    std::string b = body ? body : "";
    std::string req =
        std::string(method) + " " + path + " HTTP/1.1\r\n" +
        "Host: " + host + "\r\n" +
        "Connection: close\r\n";
    if (!b.empty()) {
        req += "Content-Length: " + std::to_string(b.size()) + "\r\n";
        req += "Content-Type: text/plain\r\n";
    }
    req += "\r\n";
    req += b;
    write_all(static_cast<int>(fd), req);
    // Read entire response (best-effort)
    std::string resp;
    char buf[4096];
    while (true) {
        ssize_t n = recv(static_cast<int>(fd), buf, sizeof(buf), 0);
        if (n <= 0) break;
        resp.append(buf, buf + n);
    }
    first_socket_close(fd);
    // Very small parsing: status line + body after \r\n\r\n
    int64_t status = 200;
    auto sp = resp.find(' ');
    if (sp != std::string::npos && resp.rfind("HTTP/", 0) == 0) {
        auto sp2 = resp.find(' ', sp + 1);
        status = std::strtoll(resp.substr(sp + 1, sp2 - sp - 1).c_str(), nullptr, 10);
    }
    std::string bodyOut;
    auto sep = resp.find("\r\n\r\n");
    if (sep != std::string::npos) bodyOut = resp.substr(sep + 4);
    return http_response_create_internal(status, "{}", bodyOut.c_str());
#else
    (void)method; (void)url; (void)path_params_json; (void)query_json; (void)headers_json; (void)body;
    return 0;
#endif
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
