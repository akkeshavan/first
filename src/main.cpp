#include <iostream>
#include <string>
#include "first/compiler.h"
#include "first/error_reporter.h"

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

    // Create compiler and compile
    first::Compiler compiler;
    
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
