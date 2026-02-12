#include <iostream>
#include <string>
#include <filesystem>
#include <llvm/Support/InitLLVM.h>
#include "first/compiler.h"
#include "first/error_reporter.h"

#if defined(__linux__)
#include <unistd.h>
#elif defined(__APPLE__)
#include <mach-o/dyld.h>
#elif defined(_WIN32)
#include <windows.h>
#endif

namespace {

// Resolve path to the running executable so we can compute PREFIX/lib/first for stdlib.
std::string getExecutablePath(const char* argv0) {
#if defined(__linux__)
    char buf[4096];
    ssize_t n = readlink("/proc/self/exe", buf, sizeof(buf) - 1);
    if (n > 0) {
        buf[n] = '\0';
        return std::string(buf);
    }
    return "";
#elif defined(__APPLE__)
    char buf[1024];
    uint32_t size = sizeof(buf);
    if (_NSGetExecutablePath(buf, &size) == 0) return std::string(buf);
    return "";
#elif defined(_WIN32)
    char buf[MAX_PATH];
    if (GetModuleFileNameA(nullptr, buf, MAX_PATH)) return std::string(buf);
    return "";
#else
    (void)argv0;
    return "";
#endif
}

// Given executable path (e.g. /usr/local/bin/firstc), return PREFIX/lib/first (e.g. /usr/local/lib/first).
std::string getInstallLibDir(const std::string& executablePath) {
    if (executablePath.empty()) return "";
    std::filesystem::path exe(executablePath);
    std::filesystem::path prefix = exe.parent_path().parent_path();
    return (prefix / "lib" / "first").string();
}

}  // namespace

void printUsage(const char* programName) {
    std::cout << "First Programming Language Compiler\n";
    std::cout << "Usage: " << programName << " [options] <source_file>\n";
    std::cout << "\nOptions:\n";
    std::cout << "  -o <file>        Output file: .o = object only; .ll = LLVM IR; else executable (object + link)\n";
    std::cout << "  --version, -v    Print version information\n";
    std::cout << "  --help, -h       Print this help message\n";
    std::cout << "  --verbose        Enable verbose output\n";
    std::cout << "\n";
}

void printVersion() {
    std::cout << "firstc version 0.1.0\n";
    std::cout << "First Programming Language Compiler\n";
}

int main(int argc, char* argv[]) {
    // Handle --version and --help before InitLLVM to avoid LLVM/ANTLR init on Linux
    // where library initialization can segfault (static init order, aarch64).
    if (argc >= 2) {
        std::string arg = argv[1];
        if (arg == "--version" || arg == "-v") {
            printVersion();
            return 0;
        }
        if (arg == "--help" || arg == "-h") {
            printUsage(argv[0]);
            return 0;
        }
    }

    llvm::InitLLVM init(argc, argv);
    if (argc < 2) {
        printUsage(argv[0]);
        return 1;
    }

    std::string sourceFile;
    std::string outputFile;
    bool verbose = false;

    // Parse command line arguments
    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        
        if (arg == "--help" || arg == "-h") {
            printUsage(argv[0]);
            return 0;
        } else if (arg == "--version" || arg == "-v") {
            printVersion();
            return 0;
        } else if (arg == "--verbose") {
            verbose = true;
        } else if (arg == "-o" && i + 1 < argc) {
            outputFile = argv[++i];
        } else if (arg.size() > 2 && arg.compare(0, 2, "-o") == 0) {
            outputFile = arg.substr(2);
        } else if (arg[0] != '-') {
            sourceFile = arg;
        } else {
            std::cerr << "Unknown option: " << arg << "\n";
            printUsage(argv[0]);
            return 1;
        }
    }

    if (sourceFile.empty()) {
        std::cerr << "Error: No source file specified\n";
        printUsage(argv[0]);
        return 1;
    }

    // Create compiler and set install lib path (PREFIX/lib/first) so stdlib is found when run from install.
    first::Compiler compiler;
    std::string exePath = getExecutablePath(argv[0]);
    if (!exePath.empty()) {
        std::string installLib = getInstallLibDir(exePath);
        if (!installLib.empty()) compiler.setInstallLibPath(installLib);
    }

    if (verbose) {
        std::cout << "Compiling: " << sourceFile << "\n";
    }

    bool success;
    if (!outputFile.empty()) {
        // Phase 7: generate IR, then object (and optionally link to executable) or emit .ll
        success = compiler.compileToIR(sourceFile);
        if (success && !compiler.getErrorReporter().hasErrors()) {
            bool emitLLVMIR = (outputFile.size() >= 3 &&
                outputFile.compare(outputFile.size() - 3, 3, ".ll") == 0);
            if (emitLLVMIR) {
                success = compiler.writeIRToFile(outputFile);
                if (success && verbose) {
                    std::cout << "Wrote LLVM IR: " << outputFile << "\n";
                }
            } else {
                bool isObjectOnly = (outputFile.size() >= 2 &&
                    outputFile.compare(outputFile.size() - 2, 2, ".o") == 0);
                std::string objectPath;
                if (isObjectOnly) {
                    objectPath = outputFile;
                } else {
                    objectPath = outputFile + ".o";
                }
                success = compiler.writeObjectToFile(objectPath);
                if (success && !isObjectOnly) {
                    success = compiler.linkToExecutable(objectPath, outputFile);
                    if (success && verbose) {
                        std::cout << "Linked: " << outputFile << "\n";
                    }
                }
            }
        }
    } else {
        success = compiler.compile(sourceFile);
    }

    if (success && !compiler.getErrorReporter().hasErrors()) {
        if (verbose) {
            std::cout << "Compilation successful!\n";
        }
        return 0;
    } else {
        compiler.getErrorReporter().printErrors();
        return 1;
    }
}
