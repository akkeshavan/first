#include "first/runtime/io.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>

namespace first {
namespace runtime {
namespace io {

void print(const String& str) {
    if (str) {
        std::cout << str->toString();
        std::cout.flush();
    }
}

void println(const String& str) {
    if (str) {
        std::cout << str->toString() << std::endl;
    } else {
        std::cout << std::endl;
    }
}

void print(const char* str) {
    if (str) {
        std::cout << str;
        std::cout.flush();
    }
}

void println(const char* str) {
    if (str) {
        std::cout << str << std::endl;
    } else {
        std::cout << std::endl;
    }
}

void printInt(int64_t value) {
    std::cout << value;
    std::cout.flush();
}

void printlnInt(int64_t value) {
    std::cout << value << std::endl;
}

void printFloat(double value) {
    std::cout << value;
    std::cout.flush();
}

void printlnFloat(double value) {
    std::cout << value << std::endl;
}

void printBool(bool value) {
    std::cout << (value ? "true" : "false");
    std::cout.flush();
}

void printlnBool(bool value) {
    std::cout << (value ? "true" : "false") << std::endl;
}

void printErr(const String& str) {
    if (str) {
        std::cerr << str->toString();
        std::cerr.flush();
    }
}

void printlnErr(const String& str) {
    if (str) {
        std::cerr << str->toString() << std::endl;
    } else {
        std::cerr << std::endl;
    }
}

void printErr(const char* str) {
    if (str) {
        std::cerr << str;
        std::cerr.flush();
    }
}

void printlnErr(const char* str) {
    if (str) {
        std::cerr << str << std::endl;
    } else {
        std::cerr << std::endl;
    }
}

void printf(const String& format, const String& arg) {
    std::string fmt = format->toString();
    std::string result;
    size_t pos = 0;
    
    while ((pos = fmt.find("{}", pos)) != std::string::npos) {
        result += fmt.substr(0, pos);
        result += arg->toString();
        pos += 2;
        fmt = fmt.substr(pos);
        pos = 0;
    }
    result += fmt;
    
    std::cout << result;
    std::cout.flush();
}

void printf(const String& format, int64_t arg) {
    std::string fmt = format->toString();
    std::string result;
    size_t pos = 0;
    
    while ((pos = fmt.find("{}", pos)) != std::string::npos) {
        result += fmt.substr(0, pos);
        result += std::to_string(arg);
        pos += 2;
        fmt = fmt.substr(pos);
        pos = 0;
    }
    result += fmt;
    
    std::cout << result;
    std::cout.flush();
}

void printf(const String& format, double arg) {
    std::string fmt = format->toString();
    std::string result;
    size_t pos = 0;
    
    while ((pos = fmt.find("{}", pos)) != std::string::npos) {
        result += fmt.substr(0, pos);
        std::ostringstream oss;
        oss << arg;
        result += oss.str();
        pos += 2;
        fmt = fmt.substr(pos);
        pos = 0;
    }
    result += fmt;
    
    std::cout << result;
    std::cout.flush();
}

void printf(const String& format, bool arg) {
    std::string fmt = format->toString();
    std::string result;
    size_t pos = 0;
    
    while ((pos = fmt.find("{}", pos)) != std::string::npos) {
        result += fmt.substr(0, pos);
        result += (arg ? "true" : "false");
        pos += 2;
        fmt = fmt.substr(pos);
        pos = 0;
    }
    result += fmt;
    
    std::cout << result;
    std::cout.flush();
}

String readLine() {
    std::string line;
    if (std::getline(std::cin, line)) {
        return makeString(line);
    }
    return makeString("");
}

String readAll() {
    std::ostringstream oss;
    std::string line;
    while (std::getline(std::cin, line)) {
        oss << line << "\n";
    }
    return makeString(oss.str());
}

FileResult readFile(const String& filename) {
    if (!filename) {
        return FileResult::err(makeString("Invalid filename"));
    }
    
    std::ifstream file(filename->toString());
    if (!file.is_open()) {
        std::string errorMsg = "Failed to open file: " + filename->toString();
        return FileResult::err(makeString(errorMsg));
    }
    
    std::ostringstream oss;
    oss << file.rdbuf();
    file.close();
    
    return FileResult::ok(makeString(oss.str()));
}

FileResult writeFile(const String& filename, const String& content) {
    if (!filename) {
        return FileResult::err(makeString("Invalid filename"));
    }
    
    if (!content) {
        return FileResult::err(makeString("Invalid content"));
    }
    
    std::ofstream file(filename->toString());
    if (!file.is_open()) {
        std::string errorMsg = "Failed to open file for writing: " + filename->toString();
        return FileResult::err(makeString(errorMsg));
    }
    
    file << content->toString();
    file.close();
    
    return FileResult::ok(makeString("Success"));
}

} // namespace io

extern "C" void __first_refinement_fail(const char* message) {
    std::cerr << "Refinement predicate failed: " << (message ? message : "(no message)") << std::endl;
    std::abort();
}

} // namespace runtime
} // namespace first
