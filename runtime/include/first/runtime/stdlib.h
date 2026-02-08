#pragma once

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// --- I/O (Phase 7.3) ---
// print, println in io.h
const char* readLine(void);   // reads one line from stdin; returns pointer to static buffer (thread_local)
const char* readFile(const char* filename);  // returns pointer to static buffer or null on error
void writeFile(const char* filename, const char* content);  // overwrites file

// --- Math ---
double first_sin(double x);
double first_cos(double x);
double first_sqrt(double x);
double first_abs(double x);
double first_floor(double x);
double first_ceil(double x);
double first_min(double a, double b);
double first_max(double a, double b);
int64_t first_min_int(int64_t a, int64_t b);
int64_t first_max_int(int64_t a, int64_t b);

// --- String ---
int64_t first_string_length(const char* s);
char* first_string_concat(const char* s1, const char* s2);  // caller must free
char* first_string_slice(const char* s, int64_t start, int64_t end);  // caller must free
int64_t first_string_to_int(const char* s);
double first_string_to_float(const char* s);
char* first_int_to_string(int64_t n);   // caller must free
char* first_float_to_string(double x);  // caller must free

// --- Array (length: compiler intrinsic; map/reduce/filter for Int arrays) ---
int64_t first_array_length(const void* arr, int64_t known_len);  // identity: returns known_len for ABI
int64_t first_array_reduce_int_sum(const int64_t* arr, int64_t len);  // sum of elements
int64_t* first_array_map_int_double(const int64_t* arr, int64_t len);  // new array: each element * 2; caller/GC frees
int64_t* first_array_filter_int_positive(const int64_t* arr, int64_t len, int64_t* out_len);  // new array: elements > 0; out_len set to result length

// --- Socket (minimal TCP) ---
int64_t first_socket_connect(const char* host, int64_t port);  // returns fd or -1
int64_t first_socket_send(int64_t fd, const char* buf);
int64_t first_socket_recv(int64_t fd, char* buf, int64_t cap);
const char* first_socket_recv_str(int64_t fd);  // reads into static buffer, returns string (for First socketRecv(fd)->String)
void first_socket_close(int64_t fd);

// --- HTTP (minimal) ---
char* first_http_get(const char* url);   // caller must free; null on error
char* first_http_post(const char* url, const char* body);  // caller must free; null on error

// --- HTTP Client (generic) ---
// Returns Response handle (opaque); 0 on error.
int64_t first_http_request(const char* method,
                           const char* url,
                           const char* path_params_json,
                           const char* query_json,
                           const char* headers_json,
                           const char* body);

// --- HTTP Server (blocking dev server) ---
// Opaque handles are returned as int64_t. 0 indicates failure.
int64_t first_http_server_create(const char* host, int64_t port);
void first_http_server_get(int64_t server, const char* route, int64_t handler_fn_ptr);
void first_http_server_post(int64_t server, const char* route, int64_t handler_fn_ptr);
void first_http_server_listen(int64_t server);  // blocking
void first_http_server_close(int64_t server);

// --- HTTP Request/Response objects (handles) ---
// Request accessors (return pointers valid while request is alive during handler execution).
const char* first_http_req_method(int64_t req);
const char* first_http_req_path(int64_t req);
const char* first_http_req_params_json(int64_t req);
const char* first_http_req_query_json(int64_t req);
const char* first_http_req_headers_json(int64_t req);
const char* first_http_req_body(int64_t req);

// Response creation/accessors.
int64_t first_http_response_create(int64_t status, const char* headers_json, const char* body);
int64_t first_http_resp_status(int64_t resp);
const char* first_http_resp_headers_json(int64_t resp);
const char* first_http_resp_body(int64_t resp);

// --- JSON (minimal: stringify only for now) ---
char* first_json_prettify(const char* json_str);  // caller must free; null on error
char* first_json_stringify_int(int64_t n);        // trivial
char* first_json_stringify_float(double x);
char* first_json_stringify_string(const char* s);

#ifdef __cplusplus
}
#endif
