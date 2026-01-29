#pragma once

#include <string>
#include <vector>
#include <memory>

namespace llvm {
    class Module;
    class LLVMContext;
}

namespace first {

class ErrorReporter;
class SourceLocation;

namespace ast {
    class Program;
}

class Compiler {
public:
    Compiler();
    ~Compiler();

    // Compile a source file
    bool compile(const std::string& sourceFile);

    // Compile a source file and generate/link LLVM IR (multi-module aware)
    bool compileToIR(const std::string& sourceFile);
    
    // Compile from source string (for testing)
    bool compileFromString(const std::string& source);
    
    // Compile from source string with a virtual filename (for better diagnostics)
    bool compileFromString(const std::string& source, const std::string& virtualFile);
    
    // Compile from source string without module resolution (for loading modules)
    // resolveModules: if false, skip module resolution to avoid recursion
    bool compileFromStringNoModules(const std::string& source, const std::string& virtualFile, bool resolveModules = true);

    // Get error reporter
    ErrorReporter& getErrorReporter() { return *errorReporter_; }
    
    // Get the AST (nullptr if compilation failed or AST not built yet)
    ast::Program* getAST() const { return ast_.get(); }
    
    // Get the generated IR module (nullptr if IR not generated yet)
    llvm::Module* getIRModule() const { return irModule_.get(); }
    
    // Write IR to file
    bool writeIRToFile(const std::string& filename) const;
    
    // Link multiple IR modules together
    // Returns true on success, false on failure
    static bool linkModules(std::vector<std::unique_ptr<llvm::Module>> modules,
                            llvm::Module* destModule,
                            ErrorReporter& errorReporter);
    
    // Link with runtime library
    // Returns true on success, false on failure
    bool linkRuntimeLibrary(const std::string& runtimeLibPath = "");

private:
    std::unique_ptr<ErrorReporter> errorReporter_;
    std::unique_ptr<ast::Program> ast_;
    bool generateIR_;
    // Shared LLVM context for all modules produced/linked by this Compiler instance.
    // Must outlive any llvm::Module owned by this Compiler.
    std::unique_ptr<llvm::LLVMContext> irContext_;
    std::unique_ptr<llvm::Module> irModule_;
};

} // namespace first
