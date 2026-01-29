#pragma once

#include "first/runtime/string.h"
#include "first/runtime/result.h"
#include <string>
#include <vector>

namespace first {
namespace runtime {

// I/O runtime functions
namespace io {

// Print to stdout
void print(const String& str);
void println(const String& str);
void print(const char* str);
void println(const char* str);

// Print different types
void printInt(int64_t value);
void printlnInt(int64_t value);
void printFloat(double value);
void printlnFloat(double value);
void printBool(bool value);
void printlnBool(bool value);

// Print to stderr (for errors)
void printErr(const String& str);
void printlnErr(const String& str);
void printErr(const char* str);
void printlnErr(const char* str);

// Formatted printing (simple version)
void printf(const String& format, const String& arg);
void printf(const String& format, int64_t arg);
void printf(const String& format, double arg);
void printf(const String& format, bool arg);

// Read from stdin
String readLine();
String readAll();

// File operations (basic)
using FileResult = Result<String, String>;

FileResult readFile(const String& filename);
FileResult writeFile(const String& filename, const String& content);

} // namespace io

// Standard library C linkage exports (for First compiler-generated calls)
extern "C" void print(const char* s);
extern "C" void println(const char* s);

// Refinement failure: report message to stderr and abort (C linkage for compiler-generated calls)
extern "C" void __first_refinement_fail(const char* message);

} // namespace runtime
} // namespace first
