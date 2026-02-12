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
double first_tan(double x);
double first_sqrt(double x);
double first_pow(double base, double exp);
double first_exp(double x);
double first_log(double x);
double first_log10(double x);
double first_abs(double x);
double first_floor(double x);
double first_ceil(double x);
double first_round(double x);
double first_sign(double x);
double first_min(double a, double b);
double first_max(double a, double b);
int64_t first_min_int(int64_t a, int64_t b);
int64_t first_max_int(int64_t a, int64_t b);
double first_pi(void);
double first_e(void);

// --- String ---
int64_t first_string_length(const char* s);
char* first_string_concat(const char* s1, const char* s2);  // caller must free
char* first_string_slice(const char* s, int64_t start, int64_t end);  // caller must free
int64_t first_string_to_int(const char* s);
double first_string_to_float(const char* s);
char* first_int_to_string(int64_t n);   // caller must free
char* first_float_to_string(double x);  // caller must free
char* first_bool_to_string(int64_t b);  // 0 -> "false", non-zero -> "true"; caller must free
char* first_unit_to_string(int64_t);    // ignores arg, returns "()"; caller must free
// JS-like string functions (caller must free returned char*)
int64_t first_string_char_at(const char* s, int64_t index);   // byte at index (0-255) or -1
int64_t first_string_code_point_at(const char* s, int64_t index);  // Unicode code point or -1
int64_t first_string_index_of(const char* s, const char* search, int64_t from);
int64_t first_string_last_index_of(const char* s, const char* search, int64_t from);
int64_t first_string_includes(const char* s, const char* search, int64_t position);  // 1/0
int64_t first_string_starts_with(const char* s, const char* search, int64_t position);  // 1/0
int64_t first_string_ends_with(const char* s, const char* search, int64_t end_position);  // 1/0
char* first_string_to_lower(const char* s);
char* first_string_to_upper(const char* s);
char* first_string_trim(const char* s);
char* first_string_trim_start(const char* s);
char* first_string_trim_end(const char* s);
char* first_string_pad_start(const char* s, int64_t target_length, const char* pad_string);
char* first_string_pad_end(const char* s, int64_t target_length, const char* pad_string);
char* first_string_repeat(const char* s, int64_t count);
char* first_string_normalize(const char* s, const char* form);  // "NFC","NFD","NFKC","NFKD" or null -> NFC

// --- Array (length: compiler intrinsic; map/reduce/filter for Int arrays; generic insert/delete) ---
int64_t first_array_length(const void* arr, int64_t known_len);  // identity: returns known_len for ABI
int64_t first_array_reduce_int_sum(const int64_t* arr, int64_t len);  // sum of elements
int64_t* first_array_map_int_double(const int64_t* arr, int64_t len);  // new array: each element * 2; caller/GC frees
int64_t* first_array_filter_int_positive(const int64_t* arr, int64_t len, int64_t* out_len);  // new array: elements > 0; out_len set to result length
// Generic insertAt/deleteAt (type-erased; elem_size in bytes). Return NULL for invalid index → Option None.
void* first_array_insert_at(const void* arr, int64_t len, int64_t pos, const void* value, size_t elem_size);
void* first_array_delete_at(const void* arr, int64_t len, int64_t pos, size_t elem_size);

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

// --- ArrayBuf (JS/TS-style buffer: layout [int64_t length][uint8_t data[]], ptr = first byte of block) ---
void* first_arraybuf_alloc(int64_t len);           // alloc 8+len, store len at 0, zero data; returns ptr
int64_t first_arraybuf_length(void* buf);          // *(int64_t*)buf; 0 if buf null
int64_t first_arraybuf_get(void* buf, int64_t i);  // byte 0-255; 0 if out of range
void first_arraybuf_set(void* buf, int64_t i, int64_t v);  // v&0xff; no-op if out of range
void* first_read_file_bytes(const char* path);      // returns arraybuf (empty on error)
void first_write_file_bytes(const char* path, void* buf);   // writes buf's data to file
char* first_base64_encode(void* buf);               // caller must free
void* first_base64_decode(const char* s);           // returns arraybuf (empty on error); caller/GC frees
char* first_arraybuf_to_string(void* buf);         // "<ArrayBuf length=N>"; caller must free

#ifdef __cplusplus
}
#endif
