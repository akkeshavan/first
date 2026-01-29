#include "first/compiler.h"
#include "first/error_reporter.h"
#include "first/source_location.h"
#include "first/ast/builder.h"
#include "first/ast/validator.h"
#include "first/semantic/type_checker.h"
#include "first/semantic/semantic_checker.h"
#include "first/semantic/module_resolver.h"
#include "first/ir/ir_generator.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Linker/Linker.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/IR/PassManager.h>
#include <system_error>

// ANTLR4 includes (used only for lexing)
#include "FirstLexer.h"
#include <antlr4-runtime.h>

// Custom parser
#include "first/parser/parser.h"

namespace {

void runOptimizationPasses(llvm::Module& module) {
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    llvm::ModulePassManager MPM =
        PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O2);
    MPM.run(module, MAM);
}

}  // namespace

namespace first {

Compiler::Compiler()
    : errorReporter_(std::make_unique<ErrorReporter>())
    , generateIR_(false)
    , irContext_(std::make_unique<llvm::LLVMContext>()) {
}

Compiler::~Compiler() = default;

bool Compiler::compile(const std::string& sourceFile) {
    errorReporter_->clear();
    generateIR_ = false;

    // Read source file
    std::ifstream file(sourceFile);
    if (!file.is_open()) {
        errorReporter_->error(
            SourceLocation(1, 1, sourceFile),
            "Cannot open file: " + sourceFile
        );
        return false;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    return compileFromString(source, sourceFile);
}

bool Compiler::compileToIR(const std::string& sourceFile) {
    errorReporter_->clear();
    generateIR_ = true;

    std::ifstream file(sourceFile);
    if (!file.is_open()) {
        errorReporter_->error(
            SourceLocation(1, 1, sourceFile),
            "Cannot open file: " + sourceFile
        );
        return false;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string source = buffer.str();
    file.close();

    bool ok = compileFromString(source, sourceFile);
    // Do not leak IR-generation mode into subsequent compilations/tests.
    generateIR_ = false;
    return ok;
}

bool Compiler::compileFromString(const std::string& source) {
    return compileFromString(source, "");
}

bool Compiler::compileFromString(const std::string& source, const std::string& virtualFile) {
    return compileFromStringNoModules(source, virtualFile, true);
}

bool Compiler::compileFromStringNoModules(const std::string& source, const std::string& virtualFile, bool resolveModules) {
    errorReporter_->clear();
    errorReporter_->setSource(virtualFile, source);

    // Create input stream
    antlr4::ANTLRInputStream input(source);
    
    // Create lexer
    FirstLexer lexer(&input);
    lexer.removeErrorListeners();
    
    // Add custom error listener
    class CustomErrorListener : public antlr4::BaseErrorListener {
    public:
        CustomErrorListener(ErrorReporter& reporter, const std::string& file)
            : reporter_(reporter), file_(file) {}
        
        void syntaxError(antlr4::Recognizer* recognizer,
                        antlr4::Token* offendingSymbol,
                        size_t line,
                        size_t charPositionInLine,
                        const std::string& msg,
                        std::exception_ptr e) override {
            SourceLocation loc(line, charPositionInLine + 1, file_);
            size_t len = 1;
            std::string tokenText;
            
            if (offendingSymbol) {
                tokenText = offendingSymbol->getText();
                if (!tokenText.empty()) {
                    len = tokenText.size();
                }
            }
            
            // Determine error code based on message content
            ErrorCode code(ErrorCodes::SYNTAX_ERROR, "syntax");
            std::string errorMsg = "Syntax error: " + msg;
            
            // Try to categorize the error
            if (msg.find("mismatched") != std::string::npos || 
                msg.find("extraneous") != std::string::npos) {
                code = ErrorCode(ErrorCodes::UNEXPECTED_TOKEN, "syntax");
                if (!tokenText.empty()) {
                    errorMsg = "Unexpected token: '" + tokenText + "'";
                }
            } else if (msg.find("missing") != std::string::npos) {
                code = ErrorCode(ErrorCodes::MISSING_TOKEN, "syntax");
            }
            
            reporter_.error(loc, errorMsg, code, len);
            
            // Add helpful notes for common errors
            if (!tokenText.empty()) {
                if (tokenText == "var") {
                    reporter_.addNote("'var' is only allowed in interaction functions");
                    reporter_.addHelp("Use 'let' for immutable bindings, or mark the function as 'interaction'");
                } else if (tokenText == "while") {
                    reporter_.addNote("'while' loops are only allowed in interaction functions");
                    reporter_.addHelp("Use recursion or mark the function as 'interaction'");
                } else if (tokenText == ">>=" || tokenText == ">>" || tokenText == "<$>" || tokenText == "<*>") {
                    reporter_.addNote("Monadic operators are only allowed in interaction functions");
                    reporter_.addHelp("Mark the function as 'interaction' to use monadic operators");
                }
            }
        }
        
    private:
        ErrorReporter& reporter_;
        std::string file_;
    };
    
    CustomErrorListener errorListener(*errorReporter_, virtualFile);
    lexer.addErrorListener(&errorListener);
    
    // Tokenize via ANTLR and adapt to custom parser tokens
    antlr4::CommonTokenStream tokens(&lexer);
    tokens.fill();
    
    // Check for lexer errors
    if (errorReporter_->hasErrors()) {
        return false;
    }

    // Use custom parser (no ANTLR parse tree)
    parser::TokenStreamAdapter tokenAdapter(tokens, virtualFile);
    parser::FirstParser parser(tokenAdapter, *errorReporter_, virtualFile);
    ast_ = parser.parseProgram();
    
    if (!ast_) {
        errorReporter_->error(
            SourceLocation(1, 1, virtualFile),
            "Failed to build AST"
        );
        return false;
    }
    
    // Phase 2.3: Validate AST structure
    ast::ASTValidator validator(*errorReporter_);
    if (!validator.validate(ast_.get())) {
        // Validation errors already reported
        return false;
    }
    // If any errors were reported during parsing or validation, treat
    // compilation as failed even when we are not generating IR. This
    // keeps the behaviour consistent for compileFromString-based tests.
    if (errorReporter_->hasErrors()) {
        return false;
    }
    
    // Phase 3: Semantic analysis (only when generating IR)
    semantic::ModuleResolver* moduleResolverPtr = nullptr;
    std::unique_ptr<semantic::ModuleResolver> moduleResolver;
    if (generateIR_) {
        // 3.3: Module system (import resolution) — do this BEFORE type checking
        // so imported symbols are available to the type checker.
        // Skip module resolution if this is a module being loaded (to avoid recursion)
        if (resolveModules) {
            moduleResolver = std::make_unique<semantic::ModuleResolver>(*errorReporter_);
            moduleResolverPtr = moduleResolver.get();
            // Register current module first
            moduleResolver->registerModule(ast_->getModuleName().empty() ? "main" : ast_->getModuleName(), ast_.get());
            moduleResolver->clearImportStack();
            if (!moduleResolver->resolveImports(ast_.get())) {
                // Import errors already reported
                return false;
            }
            
            if (errorReporter_->hasErrors()) {
                return false;
            }
        }

        // 3.1/3.2: Type checking and semantic restrictions (including multi-module:
        // TypeChecker resolves imported symbols via moduleResolver when set).
        semantic::TypeChecker typeChecker(*errorReporter_);
        if (moduleResolverPtr) {
            typeChecker.setModuleResolver(moduleResolverPtr);
        }
        typeChecker.check(ast_.get());
        
        if (errorReporter_->hasErrors()) {
            return false;
        }
        
        semantic::SemanticChecker semanticChecker(*errorReporter_);
        for (const auto& func : ast_->getFunctions()) {
            semanticChecker.checkFunction(func.get());
        }
        for (const auto& interaction : ast_->getInteractions()) {
            semanticChecker.checkInteraction(interaction.get());
        }
        
        if (errorReporter_->hasErrors()) {
            return false;
        }
    }
    
    // Phase 5: Generate LLVM IR for main module
    // Skip IR generation if this is a module being loaded (to avoid recursion and complexity)
    if (resolveModules && generateIR_) {
        ir::IRGenerator irGenerator(*errorReporter_, *irContext_, virtualFile);
        
        // Set module resolver for IR generation (for import handling)
        if (moduleResolverPtr) {
            irGenerator.setModuleResolver(moduleResolverPtr);
        }
        
        if (!irGenerator.generate(ast_.get())) {
            // IR generation errors already reported
            return false;
        }
        
        // Take ownership of the generated IR module.
        // This is safe because we passed an external LLVMContext owned by Compiler.
        irModule_ = irGenerator.takeModule();
        if (!irModule_) {
            errorReporter_->error(
                SourceLocation(1, 1, virtualFile),
                "Failed to take ownership of IR module"
            );
            return false;
        }
        
        // Phase 5.1: Generate IR for imported modules and link them (in-place, unique_ptr ownership)
        if (moduleResolverPtr) {
            std::vector<std::string> loadedModuleNames = moduleResolverPtr->getLoadedModuleNames();
            std::string mainModuleName = ast_->getModuleName().empty() ? "main" : ast_->getModuleName();

            if (!loadedModuleNames.empty() && irModule_) {
                llvm::Linker linker(*irModule_);

                for (const std::string& moduleName : loadedModuleNames) {
                    if (moduleName == mainModuleName) {
                        continue;
                    }

                    // getLoadedModuleNames() only returns modules that were loaded via loadModule(),
                    // so we can proceed directly without checking isModuleLoaded() again.
                    ast::Program* moduleAST = moduleResolverPtr->getModule(moduleName);
                    if (!moduleAST) {
                        errorReporter_->error(
                            SourceLocation(1, 1, virtualFile),
                            "Failed to get AST for module: " + moduleName
                        );
                        continue;
                    }

                    ir::IRGenerator moduleIRGenerator(*errorReporter_, *irContext_, moduleName + ".first");
                    moduleIRGenerator.setModuleResolver(moduleResolverPtr);

                    if (!moduleIRGenerator.generate(moduleAST)) {
                        errorReporter_->error(
                            SourceLocation(1, 1, virtualFile),
                            "Failed to generate IR for module: " + moduleName
                        );
                        continue;
                    }

                    std::unique_ptr<llvm::Module> ownedModule = moduleIRGenerator.takeModule();
                    if (!ownedModule) {
                        errorReporter_->error(
                            SourceLocation(1, 1, virtualFile),
                            "Failed to take IR module for module: " + moduleName
                        );
                        continue;
                    }

                    // Link and consume the module immediately (ownership is explicit).
                    if (linker.linkInModule(std::move(ownedModule))) {
                        errorReporter_->error(
                            SourceLocation(1, 1, virtualFile),
                            "Failed to link module: " + moduleName
                        );
                        return false;
                    }
                }
            }
        }
        
        // Link with runtime library if available
        // This is optional - runtime can also be linked separately
        linkRuntimeLibrary();
    }

    // Run LLVM optimization passes on the module (Phase 6.1)
    if (irModule_) {
        runOptimizationPasses(*irModule_);
    }
    
    return true;
}

bool Compiler::writeIRToFile(const std::string& filename) const {
    if (!irModule_) {
        errorReporter_->error(
            SourceLocation(1, 1, filename),
            "No IR module available to write"
        );
        return false;
    }
    
    std::error_code ec;
    llvm::raw_fd_ostream outFile(filename, ec);
    if (ec) {
        errorReporter_->error(
            SourceLocation(1, 1, filename),
            "Cannot open file for writing: " + filename + " - " + ec.message()
        );
        return false;
    }
    
    irModule_->print(outFile, nullptr);
    outFile.close();
    
    return true;
}

bool Compiler::linkModules(std::vector<std::unique_ptr<llvm::Module>> modules,
                           llvm::Module* destModule,
                           ErrorReporter& errorReporter) {
    if (!destModule) {
        errorReporter.error(
            SourceLocation(1, 1, "unknown"),
            "Destination module is null"
        );
        return false;
    }
    
    // Use LLVM's linker to combine modules
    llvm::Linker linker(*destModule);
    
    for (auto& moduleToLink : modules) {
        if (!moduleToLink) {
            continue;
        }

        std::string moduleName = moduleToLink->getName().str();

        // Link the module into the destination (consumes moduleToLink)
        bool linkError = linker.linkInModule(std::move(moduleToLink));
        if (linkError) {
            errorReporter.error(
                SourceLocation(1, 1, moduleName),
                "Failed to link module: " + moduleName
            );
            return false;
        }
    }
    
    return true;
}

bool Compiler::linkRuntimeLibrary(const std::string& runtimeLibPath) {
    if (!irModule_) {
        errorReporter_->error(
            SourceLocation(1, 1, "unknown"),
            "No IR module available to link with runtime"
        );
        return false;
    }
    
    // Determine runtime library path
    std::string libPath = runtimeLibPath;
    if (libPath.empty()) {
        // Try to find runtime library in common locations
        // First, try relative to executable
        libPath = "runtime/lib/libfirst_runtime.bc"; // Bitcode format
        
        // Check if file exists
        std::ifstream testFile(libPath);
        if (!testFile.good()) {
            // Try other common locations
            libPath = "../runtime/lib/libfirst_runtime.bc";
            testFile.open(libPath);
            if (!testFile.good()) {
                libPath = "../../runtime/lib/libfirst_runtime.bc";
                testFile.open(libPath);
                if (!testFile.good()) {
                    // Runtime library not found, but don't error - it might be linked separately
                    return true;
                }
            }
        }
        testFile.close();
    }
    
    // Load the runtime library IR
    llvm::SMDiagnostic diag;
    llvm::LLVMContext& context = irModule_->getContext();
    std::unique_ptr<llvm::Module> runtimeModule = 
        llvm::parseIRFile(libPath, diag, context);
    
    if (!runtimeModule) {
        // Runtime library not found or invalid - this is OK, it might be linked separately
        // Only warn, don't error
        return true;
    }
    
    // Link runtime library into the main module
    bool linkError = llvm::Linker::linkModules(*irModule_, std::move(runtimeModule));
    
    if (linkError) {
        errorReporter_->error(
            SourceLocation(1, 1, libPath),
            "Failed to link runtime library: " + libPath
        );
        return false;
    }
    
    return true;
}

} // namespace first
