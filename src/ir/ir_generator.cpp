#include "first/ir/ir_generator.h"
#include "first/ast/expressions.h"
#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/declarations.h"
#include "first/ast/match_case.h"
#include "first/ast/patterns.h"
#include "first/semantic/module_resolver.h"
#include "first/source_location.h"
#include <set>
#include <map>
#include <algorithm>
#include <functional>
#include <sstream>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/Support/Alignment.h>
#include <llvm/Support/raw_ostream.h>
#include <iostream>

namespace first {
namespace ir {

// Ensure no null type is ever passed to StructType::get (avoids backend crashes in ConstantHoisting/getTypeAtIndex).
static llvm::StructType* createStructTypeSafe(llvm::LLVMContext& ctx, std::vector<llvm::Type*> types) {
    llvm::Type* fallback = llvm::Type::getInt8Ty(ctx);
    for (auto& t : types)
        if (!t) t = fallback;
    return llvm::StructType::get(ctx, types);
}

IRGenerator::IRGenerator(ErrorReporter& errorReporter, const std::string& moduleName)
    : errorReporter_(errorReporter)
    , ownedContext_(std::make_unique<llvm::LLVMContext>())
    , context_(*ownedContext_)
    , module_(std::make_unique<llvm::Module>(moduleName, context_))
    , builder_(std::make_unique<llvm::IRBuilder<>>(context_))
    , moduleResolver_(nullptr)
    , currentFunction_(nullptr)
    , currentValue_(nullptr)
    , currentProgram_(nullptr)
{
}

IRGenerator::IRGenerator(ErrorReporter& errorReporter, llvm::LLVMContext& externalContext,
                         const std::string& moduleName)
    : errorReporter_(errorReporter)
    , ownedContext_(nullptr)
    , context_(externalContext)
    , module_(std::make_unique<llvm::Module>(moduleName, context_))
    , builder_(std::make_unique<llvm::IRBuilder<>>(context_))
    , moduleResolver_(nullptr)
    , currentFunction_(nullptr)
    , currentValue_(nullptr)
    , currentProgram_(nullptr)
{
}

IRGenerator::~IRGenerator() = default;

bool IRGenerator::generate(ast::Program* program) {
    if (!program) {
        errorReporter_.error(
            SourceLocation(1, 1, "unknown"),
            "Cannot generate IR: AST is null"
        );
        return false;
    }
    
    // Visit the program node to generate IR
    program->accept(*this);
    
    return !errorReporter_.hasErrors();
}

void IRGenerator::printIR() const {
    if (module_) {
        module_->print(llvm::errs(), nullptr);
    }
}

bool IRGenerator::writeIRToFile(const std::string& filename) const {
    if (!module_) {
        return false;
    }
    
    std::error_code ec;
    llvm::raw_fd_ostream outFile(filename, ec);
    if (ec) {
        errorReporter_.error(
            SourceLocation(1, 1, filename),
            "Cannot open file for writing: " + filename + " - " + ec.message()
        );
        return false;
    }
    
    module_->print(outFile, nullptr);
    outFile.close();
    
    return true;
}

void IRGenerator::visitProgram(ast::Program* node) {
    if (!node) {
        return;
    }
    currentProgram_ = node;
    constructorIndexMap_.clear();
    nextConstructorTag_ = 0;
    typeDeclsMap_.clear();
    typeDeclParamsMap_.clear();
    for (const auto& typeDecl : node->getTypeDecls()) {
        if (typeDecl && typeDecl->getType()) {
            typeDeclsMap_[typeDecl->getTypeName()] = typeDecl->getType();
            if (typeDecl->isGeneric())
                typeDeclParamsMap_[typeDecl->getTypeName()] = typeDecl->getTypeParams();
        }
    }
    // Merge type decls from loaded modules (e.g. Prelude's Option) so pattern match and convertType work
    if (moduleResolver_) {
        for (const std::string& modName : moduleResolver_->getLoadedModuleNames()) {
            ast::Program* mod = moduleResolver_->getModule(modName);
            if (!mod) continue;
            for (const auto& typeDecl : mod->getTypeDecls()) {
                if (!typeDecl || !typeDecl->getType()) continue;
                const std::string& name = typeDecl->getTypeName();
                if (typeDeclsMap_.find(name) != typeDeclsMap_.end()) continue;
                typeDeclsMap_[name] = typeDecl->getType();
                if (typeDecl->isGeneric())
                    typeDeclParamsMap_[name] = typeDecl->getTypeParams();
            }
        }
    }
    buildConstructorIndexMap(node);
    // Register ADT constructors from loaded modules' type decls (e.g. Option -> Some, None) so match works
    if (moduleResolver_) {
        for (const std::string& modName : moduleResolver_->getLoadedModuleNames()) {
            ast::Program* mod = moduleResolver_->getModule(modName);
            if (!mod) continue;
            for (const auto& typeDecl : mod->getTypeDecls()) {
                if (!typeDecl || !typeDecl->getType()) continue;
                registerADTFromType(typeDecl->getType());
            }
        }
    }

    // Set module name for this IR generation
    std::string moduleName = node->getModuleName();
    if (moduleName.empty()) {
        moduleName = "main";
    }

    // Update module name if different
    if (module_->getName() != moduleName) {
        module_->setModuleIdentifier(llvm::StringRef(moduleName));
    }

    // IMPORTANT: process imports first so we have external declarations available
    // when generating function bodies (so calls like `square(5)` resolve).
    for (const auto& import : node->getImports()) {
        import->accept(*this);
    }

    // Generate IR for all declarations in the program
    // Functions and interactions are generated with appropriate linkage
    for (const auto& func : node->getFunctions()) {
        func->accept(*this);
    }

    for (const auto& interaction : node->getInteractions()) {
        interaction->accept(*this);
    }

    // Generate IR for type declarations (if needed)
    // Type declarations typically don't generate IR directly,
    // but they may affect how other types are generated
    for (const auto& typeDecl : node->getTypeDecls()) {
        typeDecl->accept(*this);
    }
}

void IRGenerator::visitFunctionDecl(ast::FunctionDecl* node) {
    // Get function name
    std::string funcName = node->getName();
    currentFunctionDecl_ = node;

    // Convert parameter types (generic params -> erased pointer when converting)
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : node->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
            currentFunctionDecl_ = nullptr;
            errorReporter_.error(
                node->getLocation(),
                "Failed to convert parameter type for function: " + funcName
            );
            return;
        }
        paramTypes.push_back(paramType);
    }

    // Convert return type
    llvm::Type* returnType = convertType(node->getReturnType());
    if (!returnType) {
        currentFunctionDecl_ = nullptr;
        errorReporter_.error(
            node->getLocation(),
            "Failed to convert return type for function: " + funcName
        );
        return;
    }
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    
    // Determine linkage: exported functions are externally visible,
    // non-exported are internal (unless they're in the main module)
    llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
    if (node->isExported()) {
        linkage = llvm::Function::ExternalLinkage;
    } else if (module_->getName() == "main") {
        // Main module functions are externally visible by default
        linkage = llvm::Function::ExternalLinkage;
    }
    
    // Use existing declaration if present (e.g. created when generating a forward call in another function)
    llvm::Function* func = module_->getFunction(funcName);
    if (func && func->isDeclaration() && func->getFunctionType() == funcType) {
        func->setLinkage(linkage);
    } else {
        if (func && func->isDeclaration()) {
            func->eraseFromParent();
        }
        func = llvm::Function::Create(
            funcType,
            linkage,
            funcName,
            module_.get()
        );
    }

    // Phase 6.2: Pure function optimization - mark pointer args as readonly where valid.
    // Skip adding ReadOnly to the function itself: some LLVM versions reject FnAttr ReadOnly
    // (e.g. "Attribute 'readonly' does not apply to functions!"), especially with tail calls.
    if (node->getGenericParams().empty()) {
        for (auto& arg : func->args()) {
            if (arg.getType()->isPointerTy()) {
                arg.addAttr(llvm::Attribute::ReadOnly);
            }
        }
    }
    
    // Set parameter names
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            arg.setName(node->getParameters()[idx]->getName());
        }
        idx++;
    }
    
    // Generic functions: emit declaration only; monomorphized versions get bodies at call sites
    if (!node->getGenericParams().empty()) {
        currentFunctionDecl_ = nullptr;
        return;
    }
    
    // Generate function body
    enterFunction(func);
    
    // Create entry block
    llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context_, "entry", func);
    builder_->SetInsertPoint(entryBlock);

    // NOTE (LLVM opaque pointers / LLVM 21):
    // IRBuilder::CreateAlloca consults DataLayout to compute alignment. With some LLVM builds,
    // certain types (notably involving opaque pointers / target extension types) can trigger
    // extremely slow or non-terminating alignment computation.
    // We avoid that by constructing allocas with an explicit conservative alignment.
    auto createAlloca = [&](llvm::Type* ty, const std::string& name) -> llvm::AllocaInst* {
        // 8-byte alignment is safe for all primitive scalars we use (i64/double/ptr).
        // It may be over-aligned for smaller types, which is fine for correctness.
        return new llvm::AllocaInst(ty, 0, nullptr, llvm::Align(8), name, entryBlock);
    };
    
    // Store parameters in local variables (need to allocate space for them)
    idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            std::string paramName = node->getParameters()[idx]->getName();
            ast::Parameter* paramNode = node->getParameters()[idx].get();
            // Allocate space for parameter
            llvm::Type* paramType = arg.getType();
            llvm::AllocaInst* alloca = createAlloca(paramType, paramName);
            // Avoid DataLayout-driven alignment computation (can hang with some LLVM builds).
            builder_->CreateAlignedStore(&arg, alloca, llvm::Align(8));
            setVariable(paramName, alloca, paramType);
            // Register record metadata for record-typed parameters (so match and field access work)
            ast::Type* paramAstType = paramNode->getType();
            if (auto* recType = dynamic_cast<ast::RecordType*>(paramAstType)) {
                RecordMetadata meta;
                meta.structType = llvm::dyn_cast<llvm::StructType>(paramType);
                meta.alloca = alloca;
                for (const auto& f : recType->getFields()) {
                    meta.fieldNames.push_back(f->getName());
                }
                if (meta.structType) {
                    recordMetadata_[alloca] = meta;
                    recordMetadata_[builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0))] = meta;
                }
            } else if (auto* genType = dynamic_cast<ast::GenericType*>(paramAstType)) {
                auto it = typeDeclsMap_.find(genType->getName());
                if (it != typeDeclsMap_.end() && it->second) {
                    if (auto* recType = dynamic_cast<ast::RecordType*>(it->second)) {
                        RecordMetadata meta;
                        meta.structType = llvm::dyn_cast<llvm::StructType>(paramType);
                        meta.alloca = alloca;
                        for (const auto& f : recType->getFields()) {
                            meta.fieldNames.push_back(f->getName());
                        }
                        if (meta.structType) {
                            recordMetadata_[alloca] = meta;
                            recordMetadata_[builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0))] = meta;
                        }
                    }
                }
            }
        }
        idx++;
    }
    
    // Emit runtime refinement checks for parameters with refinement types
    std::vector<ast::Parameter*> paramPtrs;
    for (const auto& p : node->getParameters()) {
        paramPtrs.push_back(p.get());
    }
    emitRefinementChecksForParams(paramPtrs, func, entryBlock);
    
    // Generate statements
    const auto& body = node->getBody();
    bool hasReturn = false;
    for (const auto& stmt : body) {
        generateStatement(stmt.get());
        // Check if this was a return statement
        if (dynamic_cast<ast::ReturnStmt*>(stmt.get())) {
            hasReturn = true;
            // Don't break - continue to check for errors, but we know we have a return
        }
    }
    
    // If function doesn't have a return statement, add one
    if (!hasReturn) {
        if (returnType->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            // Return default value for non-void functions
            if (returnType->isIntegerTy(64)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true)));
            } else if (returnType->isDoubleTy()) {
                builder_->CreateRet(llvm::ConstantFP::get(context_, llvm::APFloat(0.0)));
            } else if (returnType->isIntegerTy(1)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(1, 0)));
            } else {
                // ptr, struct, or other types: use null/default (matches generateMonomorphizedBody)
                builder_->CreateRet(llvm::Constant::getNullValue(returnType));
            }
        }
    }

    currentFunctionDecl_ = nullptr;
    exitFunction();
}

void IRGenerator::visitInteractionDecl(ast::InteractionDecl* node) {
    // Interactions are similar to functions but allow side effects
    // For now, generate IR the same way as functions
    std::string funcName = node->getName();
    currentInteractionDecl_ = node;

    // Convert parameter types (generic params -> erased pointer when converting)
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : node->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
            currentInteractionDecl_ = nullptr;
            errorReporter_.error(
                node->getLocation(),
                "Failed to convert parameter type for interaction: " + funcName
            );
            return;
        }
        paramTypes.push_back(paramType);
    }

    // Convert return type
    llvm::Type* returnType = convertType(node->getReturnType());
    if (!returnType) {
        currentInteractionDecl_ = nullptr;
        errorReporter_.error(
            node->getLocation(),
            "Failed to convert return type for interaction: " + funcName
        );
        return;
    }
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    
    // Determine linkage: exported interactions are externally visible,
    // non-exported are internal (unless they're in the main module)
    llvm::Function::LinkageTypes linkage = llvm::Function::InternalLinkage;
    if (node->isExported()) {
        linkage = llvm::Function::ExternalLinkage;
    } else if (module_->getName() == "main") {
        // Main module interactions are externally visible by default
        linkage = llvm::Function::ExternalLinkage;
    }
    
    // Create function
    llvm::Function* func = llvm::Function::Create(
        funcType,
        linkage,
        funcName,
        module_.get()
    );
    
    // Set parameter names
    unsigned idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            arg.setName(node->getParameters()[idx]->getName());
        }
        idx++;
    }
    
    // Generate function body
    enterFunction(func);
    
    // Create entry block
    llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context_, "entry", func);
    builder_->SetInsertPoint(entryBlock);

    auto createAlloca = [&](llvm::Type* ty, const std::string& name) -> llvm::AllocaInst* {
        return new llvm::AllocaInst(ty, 0, nullptr, llvm::Align(8), name, entryBlock);
    };
    
    // Store parameters in local variables (allocate space for them)
    idx = 0;
    for (auto& arg : func->args()) {
        if (idx < node->getParameters().size()) {
            std::string paramName = node->getParameters()[idx]->getName();
            ast::Parameter* paramNode = node->getParameters()[idx].get();
            // Allocate space for parameter
            llvm::Type* paramType = arg.getType();
            llvm::AllocaInst* alloca = createAlloca(paramType, paramName);
            builder_->CreateAlignedStore(&arg, alloca, llvm::Align(8));
            setVariable(paramName, alloca, paramType);
            // Register record metadata for record-typed parameters
            ast::Type* paramAstType = paramNode->getType();
            if (auto* recType = dynamic_cast<ast::RecordType*>(paramAstType)) {
                RecordMetadata meta;
                meta.structType = llvm::dyn_cast<llvm::StructType>(paramType);
                meta.alloca = alloca;
                for (const auto& f : recType->getFields()) {
                    meta.fieldNames.push_back(f->getName());
                }
                if (meta.structType) {
                    recordMetadata_[alloca] = meta;
                    recordMetadata_[builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0))] = meta;
                }
            } else if (auto* genType = dynamic_cast<ast::GenericType*>(paramAstType)) {
                auto it = typeDeclsMap_.find(genType->getName());
                if (it != typeDeclsMap_.end() && it->second) {
                    if (auto* recType = dynamic_cast<ast::RecordType*>(it->second)) {
                        RecordMetadata meta;
                        meta.structType = llvm::dyn_cast<llvm::StructType>(paramType);
                        meta.alloca = alloca;
                        for (const auto& f : recType->getFields()) {
                            meta.fieldNames.push_back(f->getName());
                        }
                        if (meta.structType) {
                            recordMetadata_[alloca] = meta;
                            recordMetadata_[builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0))] = meta;
                        }
                    }
                }
            }
        }
        idx++;
    }
    
    // Emit runtime refinement checks for parameters with refinement types
    std::vector<ast::Parameter*> interactionParamPtrs;
    for (const auto& p : node->getParameters()) {
        interactionParamPtrs.push_back(p.get());
    }
    emitRefinementChecksForParams(interactionParamPtrs, func, entryBlock);
    
    // Generate statements
    const auto& body = node->getBody();
    bool hasReturn = false;
    for (const auto& stmt : body) {
        generateStatement(stmt.get());
        // Check if this was a return statement
        if (dynamic_cast<ast::ReturnStmt*>(stmt.get())) {
            hasReturn = true;
        }
    }
    
    // If function doesn't have a return statement, add one
    if (!hasReturn) {
        if (returnType->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            // Return default value for non-void functions
            if (returnType->isIntegerTy(64)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true)));
            } else if (returnType->isDoubleTy()) {
                builder_->CreateRet(llvm::ConstantFP::get(context_, llvm::APFloat(0.0)));
            } else if (returnType->isIntegerTy(1)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(1, 0)));
            } else {
                builder_->CreateRet(llvm::Constant::getNullValue(returnType));
            }
        }
    }

    currentInteractionDecl_ = nullptr;
    exitFunction();
}

void IRGenerator::visitLiteralExpr(ast::LiteralExpr* node) {
    currentValue_ = evaluateLiteral(node);
}

void IRGenerator::visitBinaryExpr(ast::BinaryExpr* node) {
    currentValue_ = evaluateBinary(node);
}

void IRGenerator::visitUnaryExpr(ast::UnaryExpr* node) {
    currentValue_ = evaluateUnary(node);
}

void IRGenerator::visitVariableExpr(ast::VariableExpr* node) {
    currentValue_ = evaluateVariable(node);
}

void IRGenerator::visitFunctionCallExpr(ast::FunctionCallExpr* node) {
    // Check if this is a closure call or regular function call
    // For now, we'll try regular function call first, and if that fails,
    // we can check if it's a closure
    // Mark as tail call when in return context (e.g. return f() or return match { ... => f() })
    currentValue_ = evaluateFunctionCall(node, inTailContext_);
    
    // TODO: Add closure detection - if the function name refers to a closure variable,
    // use invokeClosure instead
}

void IRGenerator::visitMethodCallExpr(ast::MethodCallExpr* node) {
    llvm::Value* receiver = evaluateExpr(node->getReceiver());
    if (!receiver) {
        return;
    }
    std::vector<llvm::Value*> args;
    args.push_back(receiver);
    for (const auto& argExpr : node->getArgs()) {
        llvm::Value* argVal = evaluateExpr(argExpr.get());
        if (!argVal) {
            return;
        }
        args.push_back(argVal);
    }
    // Use resolved implementation function (e.g. optionMap) when set by type checker
    std::string funcName = node->hasImplFunctionName() ? node->getImplFunctionName() : node->getMethodName();
    currentValue_ = evaluateCallWithValues(funcName, args,
        &node->getInferredTypeArgs(), &node->getArgs(), node->getLocation(), false);
}

void IRGenerator::visitRangeExpr(ast::RangeExpr* node) {
    currentValue_ = evaluateRangeExpr(node);
}

void IRGenerator::visitArrayLiteralExpr(ast::ArrayLiteralExpr* node) {
    currentValue_ = evaluateArrayLiteral(node);
}

void IRGenerator::visitArrayIndexExpr(ast::ArrayIndexExpr* node) {
    currentValue_ = evaluateArrayIndex(node);
}

void IRGenerator::visitRecordLiteralExpr(ast::RecordLiteralExpr* node) {
    currentValue_ = evaluateRecordLiteral(node);
}

void IRGenerator::visitFieldAccessExpr(ast::FieldAccessExpr* node) {
    currentValue_ = evaluateFieldAccess(node);
}

void IRGenerator::visitConstructorExpr(ast::ConstructorExpr* node) {
    currentValue_ = evaluateConstructor(node);
}

void IRGenerator::visitMatchExpr(ast::MatchExpr* node) {
    currentValue_ = evaluateMatch(node);
}

void IRGenerator::visitLambdaExpr(ast::LambdaExpr* node) {
    currentValue_ = evaluateLambda(node);
}

void IRGenerator::visitAsyncExpr(ast::AsyncExpr* node) {
    llvm::Function* thunk = createTaskThunk(node->getOperand());
    if (thunk) {
        llvm::Function* spawnFn = getOrCreateTaskSpawn();
        llvm::Value* fnPtr = builder_->CreateBitCast(thunk, llvm::PointerType::get(context_, 0), "async.fnptr");
        currentValue_ = builder_->CreateCall(spawnFn, {fnPtr}, "async.handle");
    } else {
        currentValue_ = evaluateExpr(node->getOperand());
    }
}

void IRGenerator::visitAwaitExpr(ast::AwaitExpr* node) {
    llvm::Value* handle = evaluateExpr(node->getOperand());
    if (!handle) return;
    llvm::Function* joinFn = getOrCreateTaskJoin();
    llvm::Value* raw = builder_->CreateCall(joinFn, {handle}, "await.raw");
    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
    currentValue_ = builder_->CreatePtrToInt(raw, i64, "await.result");
}

void IRGenerator::visitSpawnExpr(ast::SpawnExpr* node) {
    llvm::Function* thunk = createTaskThunk(node->getOperand());
    if (thunk) {
        llvm::Function* spawnFn = getOrCreateTaskSpawn();
        llvm::Value* fnPtr = builder_->CreateBitCast(thunk, llvm::PointerType::get(context_, 0), "spawn.fnptr");
        currentValue_ = builder_->CreateCall(spawnFn, {fnPtr}, "spawn.handle");
    } else {
        currentValue_ = evaluateExpr(node->getOperand());
    }
}

void IRGenerator::visitJoinExpr(ast::JoinExpr* node) {
    llvm::Value* handle = evaluateExpr(node->getOperand());
    if (!handle) return;
    llvm::Function* joinFn = getOrCreateTaskJoin();
    llvm::Value* raw = builder_->CreateCall(joinFn, {handle}, "join.raw");
    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
    currentValue_ = builder_->CreatePtrToInt(raw, i64, "join.result");
}

void IRGenerator::visitSelectExpr(ast::SelectExpr* node) {
    // select { branches }: stub - run first branch's statement, return unit
    const auto& branches = node->getBranches();
    if (!branches.empty() && branches[0]->getStatement()) {
        generateStatement(branches[0]->getStatement());
    }
    currentValue_ = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
}

void IRGenerator::visitBlockExpr(ast::BlockExpr* node) {
    currentValue_ = evaluateBlockExpr(node);
}

void IRGenerator::visitIfExpr(ast::IfExpr* node) {
    currentValue_ = evaluateIfExpr(node);
}

void IRGenerator::visitForInStmt(ast::ForInStmt* node) {
    generateForInStmt(node);
}

void IRGenerator::visitSelectStmt(ast::SelectStmt* node) {
    // Handled by generateStatement
}

void IRGenerator::visitVariableDecl(ast::VariableDecl* node) {
    // Handled by generateStatement
}

void IRGenerator::visitReturnStmt(ast::ReturnStmt* node) {
    // Handled by generateStatement
}

void IRGenerator::visitExprStmt(ast::ExprStmt* node) {
    // Handled by generateStatement
}

llvm::Type* IRGenerator::convertType(ast::Type* type) {
    if (!type) {
        return nullptr;
    }
    // Break recursion cycles (e.g. substitute T -> type that eventually refers back to T)
    if (convertingTypes_.count(type)) {
        return llvm::PointerType::get(context_, 0);
    }
    struct Guard {
        IRGenerator* g;
        ast::Type* t;
        ~Guard() { if (g && t) g->convertingTypes_.erase(t); }
    };
    convertingTypes_.insert(type);
    Guard guard{this, type};
    
    // Dispatch to specific type converters
    if (auto* primType = dynamic_cast<ast::PrimitiveType*>(type)) {
        return convertPrimitiveType(primType);
    } else if (auto* arrType = dynamic_cast<ast::ArrayType*>(type)) {
        return convertArrayType(arrType);
    } else if (auto* recType = dynamic_cast<ast::RecordType*>(type)) {
        return convertRecordType(recType);
    } else if (auto* adtType = dynamic_cast<ast::ADTType*>(type)) {
        return convertADTType(adtType);
    } else if (auto* genType = dynamic_cast<ast::GenericType*>(type)) {
        const std::string& name = genType->getName();
        // When generating a monomorphized body, substitute generic params with concrete types
        if (currentTypeSubst_) {
            auto it = currentTypeSubst_->find(name);
            if (it != currentTypeSubst_->end() && it->second) {
                return convertType(it->second);
            }
        }
        // Resolve type alias / sum type name from type declarations
        auto it = typeDeclsMap_.find(name);
        if (it != typeDeclsMap_.end() && it->second) {
            return convertType(it->second);
        }
        // Type parameter of current generic function/interaction: use erased pointer representation
        if (currentFunctionDecl_) {
            for (const auto& gp : currentFunctionDecl_->getGenericParams()) {
                if (gp.name == name) {
                    return llvm::PointerType::get(context_, 0);
                }
            }
        }
        if (currentInteractionDecl_) {
            for (const auto& gp : currentInteractionDecl_->getGenericParams()) {
                if (gp.name == name) {
                    return llvm::PointerType::get(context_, 0);
                }
            }
        }
        // Unsubstituted generic (e.g. when generating external decl for some<T>, optionMap<A,B>): use opaque ptr
        return llvm::PointerType::get(context_, 0);
    } else if (auto* paramType = dynamic_cast<ast::ParameterizedType*>(type)) {
        return convertParameterizedType(paramType);
    } else if (auto* refType = dynamic_cast<ast::RefinementType*>(type)) {
        // Refinement types have the same LLVM representation as their base type.
        // Runtime check is emitted at function entry.
        return convertType(refType->getBaseType());
    } else if (auto* idxType = dynamic_cast<ast::IndexedType*>(type)) {
        // Indexed types (e.g. Vector[n], Array<Int>[n]) have the same LLVM representation as base type.
        // Indices are used only for type-checking / dependent typing.
        return convertType(idxType->getBaseType());
    } else if (auto* piType = dynamic_cast<ast::DependentFunctionType*>(type)) {
        // Pi type (x: A) -> B: same representation as function(A) -> B (one parameter).
        llvm::Type* paramTy = convertType(piType->getParamType());
        llvm::Type* returnTy = convertType(piType->getReturnType());
        if (!paramTy || !returnTy) {
            return nullptr;
        }
        (void)llvm::FunctionType::get(returnTy, {paramTy}, false);
        return llvm::PointerType::get(context_, 0);
    } else if (auto* sigmaType = dynamic_cast<ast::DependentPairType*>(type)) {
        // Sigma type (x: A) * B: struct { A; B } (first component, second component).
        llvm::Type* varTy = convertType(sigmaType->getVarType());
        llvm::Type* bodyTy = convertType(sigmaType->getBodyType());
        if (!varTy || !bodyTy) {
            return nullptr;
        }
        return createStructTypeSafe(context_, std::vector<llvm::Type*>{varTy, bodyTy});
    } else if (auto* forallType = dynamic_cast<ast::ForallType*>(type)) {
        // Forall T U. Body: representation is the body type (instantiation happens at use sites).
        // If body contains unsubstituted generics, convertType will fail.
        return convertType(forallType->getBodyType());
    } else if (auto* exType = dynamic_cast<ast::ExistentialType*>(type)) {
        // Existential exists x: A. B: struct { A; B } (witness/value pair; B may depend on x).
        llvm::Type* varTy = convertType(exType->getVarType());
        llvm::Type* bodyTy = convertType(exType->getBodyType());
        if (!varTy || !bodyTy) {
            return nullptr;
        }
        return createStructTypeSafe(context_, std::vector<llvm::Type*>{varTy, bodyTy});
    } else if (auto* funcType = dynamic_cast<ast::FunctionType*>(type)) {
        // function(A) -> B: function pointer (opaque ptr for simplicity; params/return may contain generics)
        std::vector<llvm::Type*> paramTys;
        for (const auto& p : funcType->getParamTypes()) {
            if (!p) continue;
            llvm::Type* pt = convertType(p.get());
            if (!pt) return nullptr;
            paramTys.push_back(pt);
        }
        llvm::Type* retTy = funcType->getReturnType() ? convertType(funcType->getReturnType()) : llvm::Type::getVoidTy(context_);
        if (!retTy) return nullptr;
        (void)llvm::FunctionType::get(retTy, paramTys, false);
        return llvm::PointerType::get(context_, 0);
    }
    
    errorReporter_.error(
        type->getLocation(),
        "Unsupported type for IR generation"
    );
    return nullptr;
}

llvm::Type* IRGenerator::convertPrimitiveType(ast::PrimitiveType* type) {
    switch (type->getKind()) {
        case ast::PrimitiveType::Kind::Int:
            return llvm::Type::getInt64Ty(context_);
        case ast::PrimitiveType::Kind::Float:
            return llvm::Type::getDoubleTy(context_);
        case ast::PrimitiveType::Kind::Bool:
            return llvm::Type::getInt1Ty(context_);
        case ast::PrimitiveType::Kind::String:
            // String is a pointer to runtime string type
            // Use an opaque pointer for LLVM opaque-pointer mode (LLVM 15+ / 17+).
            return llvm::PointerType::get(context_, 0);
        case ast::PrimitiveType::Kind::Unit:
            return llvm::Type::getVoidTy(context_);
        case ast::PrimitiveType::Kind::Null:
            return llvm::PointerType::get(context_, 0);  // null is represented as null pointer
        case ast::PrimitiveType::Kind::ArrayBuf:
            return llvm::PointerType::get(context_, 0);  // pointer to [length][data] block
        default:
            return nullptr;
    }
}

llvm::Type* IRGenerator::convertArrayType(ast::ArrayType* type) {
    // Arrays are represented as pointers to runtime array objects
    // Use an opaque pointer for LLVM opaque-pointer mode.
    // (We track element types separately via metadata/maps where needed.)
    return llvm::PointerType::get(context_, 0);
}

llvm::Type* IRGenerator::convertParameterizedType(ast::ParameterizedType* type) {
    if (!type) {
        return nullptr;
    }
    
    std::string baseName = type->getBaseName();
    const auto& typeArgs = type->getTypeArgs();
    
    // Handle built-in parameterized types
    // Array<T> - should be handled as ArrayType, but if we get it as ParameterizedType,
    // convert it to ArrayType and then to LLVM type
    if (baseName == "Array" && typeArgs.size() == 1) {
        // Convert ParameterizedType to ArrayType
        auto elementType = copyType(typeArgs[0].get());
        if (elementType) {
            ast::ArrayType arrType(type->getLocation(), std::move(elementType));
            return convertArrayType(&arrType);
        }
    }
    
    // User-defined generic types (e.g. List<T>, Set<T>): use base ADT for representation.
    // All ADTs use the same LLVM layout (tag + payload), so we avoid substituteType here
    // to prevent dynamic_cast on a temporary substituted type (possible use-after-free).
    auto pit = typeDeclParamsMap_.find(baseName);
    if (pit != typeDeclParamsMap_.end() && !pit->second.empty()) {
        auto it = typeDeclsMap_.find(baseName);
        if (it != typeDeclsMap_.end() && it->second) {
            const std::vector<std::string>& params = pit->second;
            if (params.size() != typeArgs.size()) {
                errorReporter_.error(
                    type->getLocation(),
                    "Type '" + baseName + "' expects " + std::to_string(params.size()) +
                    " type argument(s), got " + std::to_string(typeArgs.size()));
                return nullptr;
            }
            ast::Type* baseType = it->second;
            if (dynamic_cast<ast::ADTType*>(baseType)) {
                // If any type arg is a generic param of the current function, use erased ptr so return/param types match
                auto isCurrentGenericParam = [this](ast::Type* t) {
                    auto* g = dynamic_cast<ast::GenericType*>(t);
                    if (!g) return false;
                    const std::string& n = g->getName();
                    if (currentFunctionDecl_)
                        for (const auto& gp : currentFunctionDecl_->getGenericParams())
                            if (gp.name == n) return true;
                    if (currentInteractionDecl_)
                        for (const auto& gp : currentInteractionDecl_->getGenericParams())
                            if (gp.name == n) return true;
                    return false;
                };
                for (const auto& arg : typeArgs) {
                    if (arg.get() && isCurrentGenericParam(arg.get()))
                        return llvm::PointerType::get(context_, 0);
                }
                return convertType(baseType);
            }
        }
    }

    // Optional/union T | null: use the non-null type so match (Cons => h, Nil => null) returns i64 (null -> 0)
    if (typeArgs.size() == 2) {
        ast::Type* valueType = nullptr;
        for (const auto& a : typeArgs) {
            if (!a.get()) continue;
            auto* p = dynamic_cast<ast::PrimitiveType*>(a.get());
            if (p && p->getKind() == ast::PrimitiveType::Kind::Null) continue;
            valueType = a.get();
            break;
        }
        if (valueType) {
            llvm::Type* vt = convertType(valueType);
            if (vt) return vt;
        }
    }

    // For other parameterized types (Option<T>, Result<T, E>, etc.),
    // treat as ADTs (tagged unions) with opaque payload
    bool allConcrete = true;
    for (const auto& arg : typeArgs) {
        if (dynamic_cast<ast::GenericType*>(arg.get())) {
            allConcrete = false;
            break;
        }
    }
    if (!allConcrete) {
        // Generic args (e.g. Option<B>): use opaque pointer so external decls and calls work; layout is same as concrete Option<T>
        return llvm::PointerType::get(context_, 0);
    }
    // Use ptr to match constructor convention (struct built in alloca, returned as ptr)
    return llvm::PointerType::get(context_, 0);
}

llvm::Type* IRGenerator::convertRecordType(ast::RecordType* type) {
    // Records are represented as LLVM struct types (caller passes by pointer; callee may load for by-value param)
    std::vector<llvm::Type*> fieldTypes;
    for (const auto& field : type->getFields()) {
        llvm::Type* fieldType = convertType(field->getType());
        if (!fieldType) {
            errorReporter_.error(
                field->getLocation(),
                "Failed to convert record field type: " + field->getName()
            );
            return nullptr;
        }
        fieldTypes.push_back(fieldType);
    }
    return createStructTypeSafe(context_, fieldTypes);
}

llvm::Type* IRGenerator::convertADTType(ast::ADTType* type) {
    // ADTs are represented as pointer to tagged union (matches constructor return convention)
    // Structure in memory: { tag: i64, payload: union of all constructor argument types }
    return llvm::PointerType::get(context_, 0);
}

void IRGenerator::enterFunction(llvm::Function* func) {
    currentFunction_ = func;
    localVars_.clear();
    localVarTypes_.clear();
}

void IRGenerator::exitFunction() {
    currentFunction_ = nullptr;
    localVars_.clear();
    localVarTypes_.clear();
    refinementVarOverrides_.clear();
    arrayMetadata_.clear();
    recordMetadata_.clear();
}

llvm::AllocaInst* IRGenerator::getVariable(const std::string& name) {
    auto overrideIt = refinementVarOverrides_.find(name);
    if (overrideIt != refinementVarOverrides_.end()) {
        return overrideIt->second;
    }
    auto it = localVars_.find(name);
    if (it != localVars_.end()) {
        return it->second;
    }
    return nullptr;
}

void IRGenerator::pushRefinementBinding(const std::string& varName, llvm::AllocaInst* alloca, llvm::Type* type) {
    refinementVarOverrides_[varName] = alloca;
    localVarTypes_[varName] = type;
}

void IRGenerator::popRefinementBinding(const std::string& varName) {
    refinementVarOverrides_.erase(varName);
    localVarTypes_.erase(varName);
}

llvm::Function* IRGenerator::getOrCreateFirstAlloc() {
    llvm::Function* f = module_->getFunction("first_alloc");
    if (f) return f;
    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
    llvm::Type* ptr = llvm::PointerType::get(context_, 0);
    return llvm::Function::Create(
        llvm::FunctionType::get(ptr, {i64}, false),
        llvm::Function::ExternalLinkage,
        "first_alloc",
        module_.get());
}

llvm::Function* IRGenerator::getOrCreateTaskSpawn() {
    llvm::Function* f = module_->getFunction("first_task_spawn");
    if (f) return f;
    llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
    return llvm::Function::Create(
        llvm::FunctionType::get(i8ptr, {i8ptr}, false),
        llvm::Function::ExternalLinkage,
        "first_task_spawn",
        module_.get());
}

llvm::Function* IRGenerator::getOrCreateTaskJoin() {
    llvm::Function* f = module_->getFunction("first_task_join");
    if (f) return f;
    llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
    return llvm::Function::Create(
        llvm::FunctionType::get(i8ptr, {i8ptr}, false),
        llvm::Function::ExternalLinkage,
        "first_task_join",
        module_.get());
}

llvm::Function* IRGenerator::createTaskThunk(ast::Expr* operand) {
    ast::FunctionCallExpr* call = dynamic_cast<ast::FunctionCallExpr*>(operand);
    if (!call || !call->getArgs().empty()) {
        return nullptr;
    }
    const std::string& name = call->getName();
    llvm::Function* callee = module_->getFunction(name);
    if (!callee || callee->isDeclaration()) {
        return nullptr;
    }
    ast::Type* returnAstType = nullptr;
    if (currentProgram_) {
        for (const auto& in : currentProgram_->getInteractions()) {
            if (in && in->getName() == name) {
                returnAstType = in->getReturnType();
                break;
            }
        }
        if (!returnAstType) {
            for (const auto& fn : currentProgram_->getFunctions()) {
                if (fn && fn->getName() == name) {
                    returnAstType = fn->getReturnType();
                    break;
                }
            }
        }
    }
    if (!returnAstType) {
        return nullptr;
    }
    llvm::Type* retLlvm = convertType(returnAstType);
    if (!retLlvm) {
        return nullptr;
    }
    llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
    llvm::FunctionType* thunkType = llvm::FunctionType::get(i8ptr, {}, false);
    std::string thunkName = "__first_spawn_thunk_" + std::to_string(spawnThunkCounter_++);
    llvm::Function* thunk = llvm::Function::Create(
        thunkType,
        llvm::Function::PrivateLinkage,
        thunkName,
        module_.get());
    llvm::BasicBlock* savedBlock = builder_->GetInsertBlock();
    llvm::BasicBlock* entry = llvm::BasicBlock::Create(context_, "entry", thunk);
    builder_->SetInsertPoint(entry);
    llvm::Value* callResult = builder_->CreateCall(callee, {}, "thunk.call");
    llvm::Value* asPtr = nullptr;
    if (retLlvm->isIntegerTy(64)) {
        asPtr = builder_->CreateIntToPtr(callResult, i8ptr, "thunk.asptr");
    } else if (retLlvm->isVoidTy()) {
        asPtr = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i8ptr));
    } else if (retLlvm->isPointerTy()) {
        asPtr = builder_->CreateBitCast(callResult, i8ptr, "thunk.asptr");
    } else {
        thunk->eraseFromParent();
        builder_->SetInsertPoint(savedBlock);
        return nullptr;
    }
    builder_->CreateRet(asPtr);
    builder_->SetInsertPoint(savedBlock);
    return thunk;
}

void IRGenerator::emitRefinementChecksForParams(const std::vector<ast::Parameter*>& params, llvm::Function* func, llvm::BasicBlock* entryBlock) {
    llvm::BasicBlock* currentBlock = entryBlock;
    llvm::Function* refinementFailFn = nullptr;  // lazily declared when first needed
    for (ast::Parameter* param : params) {
        ast::Type* paramType = param->getType();
        auto* refType = dynamic_cast<ast::RefinementType*>(paramType);
        if (!refType || !refType->getPredicate()) {
            continue;
        }
        const std::string& paramName = param->getName();
        const std::string& refinementVarName = refType->getVariableName();
        llvm::AllocaInst* paramAlloca = getVariable(paramName);
        if (!paramAlloca) {
            continue;
        }
        bool needOverride = (paramName != refinementVarName);
        if (needOverride) {
            llvm::Type* baseLlvmType = convertType(refType->getBaseType());
            pushRefinementBinding(refinementVarName, paramAlloca, baseLlvmType);
        }
        llvm::Value* predVal = evaluateExpr(refType->getPredicate());
        if (needOverride) {
            popRefinementBinding(refinementVarName);
        }
        if (!predVal) {
            continue;
        }
        if (!predVal->getType()->isIntegerTy(1)) {
            errorReporter_.error(
                refType->getPredicate()->getLocation(),
                "Refinement predicate must be of type Bool"
            );
            continue;
        }
        // Build error message: function 'foo', parameter 'n' at file:line:col
        std::ostringstream msg;
        msg << "function '" << func->getName().str() << "', parameter '" << paramName << "'";
        const SourceLocation& loc = param->getLocation();
        if (!loc.getFile().empty() || loc.getLine() > 0) {
            msg << " at ";
            if (!loc.getFile().empty()) {
                msg << loc.getFile();
            }
            if (loc.getLine() > 0) {
                if (!loc.getFile().empty()) msg << ":";
                msg << loc.getLine() << ":" << loc.getColumn();
            }
        }
        std::string messageStr = msg.str();
        llvm::BasicBlock* failBlock = llvm::BasicBlock::Create(context_, "refinement.fail", func);
        llvm::BasicBlock* nextBlock = llvm::BasicBlock::Create(context_, "refinement.cont", func);
        builder_->SetInsertPoint(currentBlock);
        builder_->CreateCondBr(predVal, nextBlock, failBlock);
        builder_->SetInsertPoint(failBlock);
        if (!refinementFailFn) {
            llvm::FunctionType* failFnType = llvm::FunctionType::get(
                llvm::Type::getVoidTy(context_),
                llvm::PointerType::get(context_, 0),
                false
            );
            refinementFailFn = llvm::Function::Create(
                failFnType,
                llvm::Function::ExternalLinkage,
                "__first_refinement_fail",
                module_.get()
            );
        }
        llvm::Value* msgPtr = builder_->CreateGlobalString(messageStr, "refinement.fail.msg", 0);
        builder_->CreateCall(refinementFailFn, msgPtr);
        builder_->CreateUnreachable();
        currentBlock = nextBlock;
    }
    builder_->SetInsertPoint(currentBlock);
}

void IRGenerator::setVariable(const std::string& name, llvm::AllocaInst* value, llvm::Type* type) {
    localVars_[name] = value;
    if (type) {
        localVarTypes_[name] = type;
    } else if (value) {
        // Fallback: try to get type from alloca
        localVarTypes_[name] = value->getAllocatedType();
    }
}

llvm::Value* IRGenerator::getDefaultValue(llvm::Type* type) {
    if (!type) {
        return nullptr;
    }
    
    if (type->isIntegerTy(64)) {
        return llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    } else if (type->isDoubleTy()) {
        return llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
    } else if (type->isIntegerTy(1)) {
        return llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
    } else if (type->isVoidTy()) {
        return nullptr;
    } else if (type->isPointerTy()) {
        return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(type));
    }
    
    // Default: return zero value
    return llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
}

llvm::Value* IRGenerator::evaluateExpr(ast::Expr* expr) {
    if (!expr) {
        return nullptr;
    }
    expr->accept(*this);
    return currentValue_;
}

llvm::Value* IRGenerator::evaluateLiteral(ast::LiteralExpr* expr) {
    std::string valueStr = expr->getValue();
    
    switch (expr->getType()) {
        case ast::LiteralExpr::LiteralType::Int: {
            int64_t value = std::stoll(valueStr);
            return llvm::ConstantInt::get(context_, llvm::APInt(64, value, true));
        }
        case ast::LiteralExpr::LiteralType::Float: {
            double value = std::stod(valueStr);
            return llvm::ConstantFP::get(context_, llvm::APFloat(value));
        }
        case ast::LiteralExpr::LiteralType::Bool: {
            bool value = (valueStr == "true");
            return llvm::ConstantInt::get(context_, llvm::APInt(1, value ? 1 : 0));
        }
        case ast::LiteralExpr::LiteralType::String: {
            // String literals need runtime allocation
            // For now, create a global string constant
            llvm::Constant* strConstant = llvm::ConstantDataArray::getString(context_, valueStr);
            llvm::GlobalVariable* globalStr = new llvm::GlobalVariable(
                *module_,
                strConstant->getType(),
                true, // isConstant
                llvm::GlobalValue::PrivateLinkage,
                strConstant,
                ".str"
            );
            return builder_->CreateBitCast(globalStr, llvm::PointerType::get(context_, 0));
        }
        case ast::LiteralExpr::LiteralType::Null:
            return llvm::ConstantPointerNull::get(llvm::PointerType::get(context_, 0));
        default:
            return nullptr;
    }
}

llvm::Value* IRGenerator::evaluateBinary(ast::BinaryExpr* expr) {
    llvm::Value* left = evaluateExpr(expr->getLeft());
    llvm::Value* right = evaluateExpr(expr->getRight());
    
    if (!left || !right) {
        return nullptr;
    }
    
    // Get types
    llvm::Type* leftType = left->getType();
    llvm::Type* rightType = right->getType();
    
    switch (expr->getOp()) {
        case ast::BinaryExpr::Op::Add:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateAdd(left, right, "addtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                // Promote to float if either operand is float
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFAdd(left, right, "addtmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                // String concatenation via runtime first_string_concat
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::FunctionType* concatType = llvm::FunctionType::get(i8ptr, {i8ptr, i8ptr}, false);
                llvm::Function* concatFn = module_->getFunction("first_string_concat");
                if (!concatFn)
                    concatFn = llvm::Function::Create(concatType, llvm::Function::ExternalLinkage, "first_string_concat", module_.get());
                return builder_->CreateCall(concatFn, {left, right}, "concat");
            }
            break;
        case ast::BinaryExpr::Op::Sub:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateSub(left, right, "subtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFSub(left, right, "subtmp");
            }
            break;
        case ast::BinaryExpr::Op::Mul:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateMul(left, right, "multmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFMul(left, right, "multmp");
            }
            break;
        case ast::BinaryExpr::Op::Div:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateSDiv(left, right, "divtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFDiv(left, right, "divtmp");
            }
            break;
        case ast::BinaryExpr::Op::Eq: {
            const std::string& eqName = expr->getEqFunctionName();
            if (!eqName.empty()) {
                llvm::Function* eqFunc = module_->getFunction(eqName);
                if (eqFunc && eqFunc->getFunctionType()->getNumParams() == 2) {
                    llvm::Value* a = left;
                    llvm::Value* b = right;
                    llvm::Type* p0 = eqFunc->getFunctionType()->getParamType(0);
                    if (p0->isStructTy() && leftType->isPointerTy()) {
                        a = builder_->CreateLoad(p0, left, "loadrec");
                        b = builder_->CreateLoad(p0, right, "loadrec");
                    }
                    llvm::Value* eqResult = builder_->CreateCall(eqFunc, {a, b}, "eqtmp");
                    return eqResult;
                }
            }
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpEQ(left, right, "eqtmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOEQ(left, right, "eqtmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                // String equality via runtime first_string_equals
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::Type* i1 = llvm::Type::getInt1Ty(context_);
                llvm::FunctionType* eqType = llvm::FunctionType::get(i1, {i8ptr, i8ptr}, false);
                llvm::Function* eqFn = module_->getFunction("first_string_equals");
                if (!eqFn)
                    eqFn = llvm::Function::Create(eqType, llvm::Function::ExternalLinkage, "first_string_equals", module_.get());
                return builder_->CreateCall(eqFn, {left, right}, "streq");
            }
            break;
        }
        case ast::BinaryExpr::Op::Ne: {
            const std::string& eqName = expr->getEqFunctionName();
            if (!eqName.empty()) {
                llvm::Function* eqFunc = module_->getFunction(eqName);
                if (eqFunc && eqFunc->getFunctionType()->getNumParams() == 2) {
                    llvm::Value* a = left;
                    llvm::Value* b = right;
                    llvm::Type* p0 = eqFunc->getFunctionType()->getParamType(0);
                    if (p0->isStructTy() && leftType->isPointerTy()) {
                        a = builder_->CreateLoad(p0, left, "loadrec");
                        b = builder_->CreateLoad(p0, right, "loadrec");
                    }
                    llvm::Value* eqResult = builder_->CreateCall(eqFunc, {a, b}, "eqtmp");
                    return builder_->CreateNot(eqResult, "netmp");
                }
            }
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpNE(left, right, "netmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpONE(left, right, "netmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::Type* i1 = llvm::Type::getInt1Ty(context_);
                llvm::FunctionType* eqType = llvm::FunctionType::get(i1, {i8ptr, i8ptr}, false);
                llvm::Function* eqFn = module_->getFunction("first_string_equals");
                if (!eqFn)
                    eqFn = llvm::Function::Create(eqType, llvm::Function::ExternalLinkage, "first_string_equals", module_.get());
                llvm::Value* eq = builder_->CreateCall(eqFn, {left, right}, "streq");
                return builder_->CreateNot(eq, "strne");
            }
            break;
        }
        case ast::BinaryExpr::Op::Lt: {
            const std::string& cmpName = expr->getCompareFunctionName();
            if (!cmpName.empty()) {
                llvm::Function* cmpFunc = module_->getFunction(cmpName);
                if (cmpFunc && cmpFunc->getFunctionType()->getNumParams() == 2) {
                    llvm::Value* a = left;
                    llvm::Value* b = right;
                    llvm::Type* p0 = cmpFunc->getFunctionType()->getParamType(0);
                    if (p0->isStructTy() && leftType->isPointerTy()) {
                        a = builder_->CreateLoad(p0, left, "loadrec");
                        b = builder_->CreateLoad(p0, right, "loadrec");
                    }
                    llvm::Value* cmpResult = builder_->CreateCall(cmpFunc, {a, b}, "cmp");
                    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                    if (cmpResult->getType() != i64)
                        cmpResult = builder_->CreateIntCast(cmpResult, i64, true, "cmp.i64");
                    return builder_->CreateICmpSLT(cmpResult, llvm::ConstantInt::get(i64, 0), "lttmp");
                }
            }
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSLT(left, right, "lttmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOLT(left, right, "lttmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                // String less-than via runtime first_string_compare
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                llvm::FunctionType* cmpType = llvm::FunctionType::get(i64, {i8ptr, i8ptr}, false);
                llvm::Function* cmpFn = module_->getFunction("first_string_compare");
                if (!cmpFn)
                    cmpFn = llvm::Function::Create(cmpType, llvm::Function::ExternalLinkage, "first_string_compare", module_.get());
                llvm::Value* cmpResult = builder_->CreateCall(cmpFn, {left, right}, "strcmp");
                return builder_->CreateICmpSLT(cmpResult, llvm::ConstantInt::get(i64, 0), "strlt");
            }
            break;
        }
        case ast::BinaryExpr::Op::Le: {
            const std::string& cmpName = expr->getCompareFunctionName();
            if (!cmpName.empty()) {
                llvm::Function* cmpFunc = module_->getFunction(cmpName);
                if (cmpFunc && cmpFunc->getFunctionType()->getNumParams() == 2) {
                    llvm::Value* a = left;
                    llvm::Value* b = right;
                    llvm::Type* p0 = cmpFunc->getFunctionType()->getParamType(0);
                    if (p0->isStructTy() && leftType->isPointerTy()) {
                        a = builder_->CreateLoad(p0, left, "loadrec");
                        b = builder_->CreateLoad(p0, right, "loadrec");
                    }
                    llvm::Value* cmpResult = builder_->CreateCall(cmpFunc, {a, b}, "cmp");
                    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                    if (cmpResult->getType() != i64)
                        cmpResult = builder_->CreateIntCast(cmpResult, i64, true, "cmp.i64");
                    return builder_->CreateICmpSLE(cmpResult, llvm::ConstantInt::get(i64, 0), "letmp");
                }
            }
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSLE(left, right, "letmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOLE(left, right, "letmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                // String less-than-or-equal via runtime first_string_compare
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                llvm::FunctionType* cmpType = llvm::FunctionType::get(i64, {i8ptr, i8ptr}, false);
                llvm::Function* cmpFn = module_->getFunction("first_string_compare");
                if (!cmpFn)
                    cmpFn = llvm::Function::Create(cmpType, llvm::Function::ExternalLinkage, "first_string_compare", module_.get());
                llvm::Value* cmpResult = builder_->CreateCall(cmpFn, {left, right}, "strcmp");
                return builder_->CreateICmpSLE(cmpResult, llvm::ConstantInt::get(i64, 0), "strle");
            }
            break;
        }
        case ast::BinaryExpr::Op::Gt: {
            const std::string& cmpName = expr->getCompareFunctionName();
            if (!cmpName.empty()) {
                llvm::Function* cmpFunc = module_->getFunction(cmpName);
                if (cmpFunc && cmpFunc->getFunctionType()->getNumParams() == 2) {
                    llvm::Value* a = left;
                    llvm::Value* b = right;
                    llvm::Type* p0 = cmpFunc->getFunctionType()->getParamType(0);
                    if (p0->isStructTy() && leftType->isPointerTy()) {
                        a = builder_->CreateLoad(p0, left, "loadrec");
                        b = builder_->CreateLoad(p0, right, "loadrec");
                    }
                    llvm::Value* cmpResult = builder_->CreateCall(cmpFunc, {a, b}, "cmp");
                    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                    if (cmpResult->getType() != i64)
                        cmpResult = builder_->CreateIntCast(cmpResult, i64, true, "cmp.i64");
                    return builder_->CreateICmpSGT(cmpResult, llvm::ConstantInt::get(i64, 0), "gttmp");
                }
            }
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSGT(left, right, "gttmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOGT(left, right, "gttmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                // String greater-than via runtime first_string_compare
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                llvm::FunctionType* cmpType = llvm::FunctionType::get(i64, {i8ptr, i8ptr}, false);
                llvm::Function* cmpFn = module_->getFunction("first_string_compare");
                if (!cmpFn)
                    cmpFn = llvm::Function::Create(cmpType, llvm::Function::ExternalLinkage, "first_string_compare", module_.get());
                llvm::Value* cmpResult = builder_->CreateCall(cmpFn, {left, right}, "strcmp");
                return builder_->CreateICmpSGT(cmpResult, llvm::ConstantInt::get(i64, 0), "strgt");
            }
            break;
        }
        case ast::BinaryExpr::Op::Ge: {
            const std::string& cmpName = expr->getCompareFunctionName();
            if (!cmpName.empty()) {
                llvm::Function* cmpFunc = module_->getFunction(cmpName);
                if (cmpFunc && cmpFunc->getFunctionType()->getNumParams() == 2) {
                    llvm::Value* a = left;
                    llvm::Value* b = right;
                    llvm::Type* p0 = cmpFunc->getFunctionType()->getParamType(0);
                    if (p0->isStructTy() && leftType->isPointerTy()) {
                        a = builder_->CreateLoad(p0, left, "loadrec");
                        b = builder_->CreateLoad(p0, right, "loadrec");
                    }
                    llvm::Value* cmpResult = builder_->CreateCall(cmpFunc, {a, b}, "cmp");
                    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                    if (cmpResult->getType() != i64)
                        cmpResult = builder_->CreateIntCast(cmpResult, i64, true, "cmp.i64");
                    return builder_->CreateICmpSGE(cmpResult, llvm::ConstantInt::get(i64, 0), "getmp");
                }
            }
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateICmpSGE(left, right, "getmp");
            } else if (leftType->isFloatingPointTy() || rightType->isFloatingPointTy()) {
                if (leftType->isIntegerTy()) {
                    left = builder_->CreateSIToFP(left, llvm::Type::getDoubleTy(context_));
                }
                if (rightType->isIntegerTy()) {
                    right = builder_->CreateSIToFP(right, llvm::Type::getDoubleTy(context_));
                }
                return builder_->CreateFCmpOGE(left, right, "getmp");
            } else if (leftType->isPointerTy() && rightType->isPointerTy()) {
                // String greater-than-or-equal via runtime first_string_compare
                llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
                llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
                llvm::FunctionType* cmpType = llvm::FunctionType::get(i64, {i8ptr, i8ptr}, false);
                llvm::Function* cmpFn = module_->getFunction("first_string_compare");
                if (!cmpFn)
                    cmpFn = llvm::Function::Create(cmpType, llvm::Function::ExternalLinkage, "first_string_compare", module_.get());
                llvm::Value* cmpResult = builder_->CreateCall(cmpFn, {left, right}, "strcmp");
                return builder_->CreateICmpSGE(cmpResult, llvm::ConstantInt::get(i64, 0), "strge");
            }
            break;
        }
        case ast::BinaryExpr::Op::Mod:
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                return builder_->CreateSRem(left, right, "modtmp");
            } else {
                errorReporter_.error(
                    expr->getLocation(),
                    "Modulo operator only supported for integers"
                );
                return nullptr;
            }
        case ast::BinaryExpr::Op::And:
            // Logical AND - use short-circuit evaluation
            return evaluateShortCircuitAnd(expr);
        case ast::BinaryExpr::Op::Or:
            // Logical OR - use short-circuit evaluation
            return evaluateShortCircuitOr(expr);
        default:
            errorReporter_.error(
                expr->getLocation(),
                "Unsupported binary operator"
            );
            return nullptr;
    }
    
    return nullptr;
}

llvm::Value* IRGenerator::evaluateUnary(ast::UnaryExpr* expr) {
    llvm::Value* operand = evaluateExpr(expr->getOperand());
    if (!operand) {
        return nullptr;
    }
    
    switch (expr->getOp()) {
        case ast::UnaryExpr::Op::Neg:
            if (operand->getType()->isIntegerTy()) {
                return builder_->CreateNeg(operand, "negtmp");
            } else if (operand->getType()->isFloatingPointTy()) {
                return builder_->CreateFNeg(operand, "negtmp");
            }
            break;
        case ast::UnaryExpr::Op::Not:
            if (operand->getType()->isIntegerTy(1)) {
                return builder_->CreateNot(operand, "nottmp");
            }
            break;
        default:
            errorReporter_.error(
                expr->getLocation(),
                "Unsupported unary operator"
            );
            return nullptr;
    }
    
    return nullptr;
}

llvm::Value* IRGenerator::evaluateVariable(ast::VariableExpr* expr) {
    std::string name = expr->getName();
    llvm::AllocaInst* alloca = getVariable(name);
    
    if (!alloca) {
        // Allow referencing a function/interaction by name as a value (function pointer-like).
        if (llvm::Function* f = module_->getFunction(name)) {
            return builder_->CreateBitCast(f, llvm::PointerType::get(context_, 0), name + "_fnptr");
        }
        errorReporter_.error(expr->getLocation(), "Undefined variable: " + name);
        return nullptr;
    }
    
    // Check if this variable has array metadata
    auto arrayIt = arrayMetadata_.find(alloca);
    if (arrayIt != arrayMetadata_.end()) {
        // This is an array variable - return the alloca directly
        // (arrays are represented as pointers, so the alloca itself is the value)
        // Cast to opaque ptr to match array representation
        return builder_->CreateBitCast(
            alloca,
            llvm::PointerType::get(context_, 0),
            name + "_ptr"
        );
    }
    
    // Check if this variable has record metadata
    auto recordIt = recordMetadata_.find(alloca);
    if (recordIt != recordMetadata_.end()) {
        // Return the alloca cast to opaque ptr (or load if alloca holds a pointer to record).
        RecordMetadata meta = recordIt->second;
        llvm::Type* allocTy = alloca->getAllocatedType();
        if (allocTy->isPointerTy()) {
            llvm::Value* loadedPtr = builder_->CreateLoad(llvm::PointerType::get(context_, 0), alloca, name + "_load");
            recordMetadata_[loadedPtr] = meta;
            recordMetadata_[builder_->CreateBitCast(loadedPtr, llvm::PointerType::get(context_, 0))] = meta;
            return loadedPtr;
        }
        llvm::Value* recordPtr = builder_->CreateBitCast(
            alloca,
            llvm::PointerType::get(context_, 0),
            name + "_ptr"
        );
        meta.alloca = alloca;
        recordMetadata_[recordPtr] = meta;
        return recordPtr;
    }
    
    // Load value from alloca.
    // Use tracked type if available, otherwise fall back to getAllocatedType()
    llvm::Type* loadType = nullptr;
    auto typeIt = localVarTypes_.find(name);
    if (typeIt != localVarTypes_.end()) {
        loadType = typeIt->second;
    } else {
        loadType = alloca->getAllocatedType();
    }
    if (!loadType) {
        errorReporter_.error(
            expr->getLocation(),
            "Cannot determine type for variable: " + name
        );
        return nullptr;
    }
    // Avoid DataLayout-driven alignment lookup on some LLVM builds.
    return builder_->CreateAlignedLoad(loadType, alloca, llvm::Align(8), name);
}

// Helper: extract Int literal value from an expression (LiteralExpr or BlockExpr with single literal).
// Returns true and sets out when the expr is an Int literal.
static bool getConsFirstArgLiteral(ast::Expr* e, int64_t& out) {
    if (!e) return false;
    ast::Expr* inner = e;
    if (auto* block = dynamic_cast<ast::BlockExpr*>(e)) {
        const auto& stmts = block->getStatements();
        if (stmts.size() != 1) return false;
        if (auto* exprStmt = dynamic_cast<ast::ExprStmt*>(stmts[0].get()))
            inner = exprStmt->getExpr();
    }
    auto* lit = dynamic_cast<ast::LiteralExpr*>(inner);
    if (!lit || lit->getType() != ast::LiteralExpr::LiteralType::Int) return false;
    try {
        out = std::stoll(lit->getValue());
        return true;
    } catch (...) {
        return false;
    }
}

static bool isConsCall(const std::string& funcName) {
    return funcName == "cons" || (funcName.size() > 5 && funcName.compare(funcName.size() - 4, 4, "cons") == 0 && funcName[funcName.size() - 5] == '.');
}

llvm::Value* IRGenerator::evaluateCallWithValues(const std::string& funcName,
                                                std::vector<llvm::Value*>& args,
                                                const std::vector<std::unique_ptr<ast::Type>>* inferredTypeArgs,
                                                const std::vector<std::unique_ptr<ast::Expr>>* argExprsForFallback,
                                                const SourceLocation& loc,
                                                bool tailCall) {
    static const std::vector<std::unique_ptr<ast::Type>> kEmptyInferred;
    const auto& inferred = (inferredTypeArgs && !inferredTypeArgs->empty()) ? *inferredTypeArgs : kEmptyInferred;
    const auto* argExprs = argExprsForFallback;

    // toString(x) dispatch: rewrite to type-specific function or identity (String)
    std::string name = funcName;
    if (name == "toString" && args.size() == 1) {
        if (!inferred.empty() && inferred[0]) {
            ast::Type* argType = inferred[0].get();
            if (currentTypeSubst_) {
                auto sub = substituteType(argType, *currentTypeSubst_);
                if (sub) argType = sub.get();
            }
            if (auto* prim = dynamic_cast<ast::PrimitiveType*>(argType)) {
                switch (prim->getKind()) {
                    case ast::PrimitiveType::Kind::Int:    name = "intToString"; break;
                    case ast::PrimitiveType::Kind::Float: name = "floatToString"; break;
                    case ast::PrimitiveType::Kind::Bool:  name = "boolToString"; break;
                    case ast::PrimitiveType::Kind::String: return args[0];  // identity
                    case ast::PrimitiveType::Kind::Unit:  name = "unitToString"; break;
                    case ast::PrimitiveType::Kind::ArrayBuf: name = "arrayBufToString"; break;
                    default: break;
                }
            } else if (auto* gen = dynamic_cast<ast::GenericType*>(argType)) {
                name = "toString_" + gen->getName();
            } else if (auto* rec = dynamic_cast<ast::RecordType*>(argType)) {
                for (const auto& [typeName, declType] : typeDeclsMap_) {
                    auto* declRec = dynamic_cast<ast::RecordType*>(declType);
                    if (!declRec || declRec->getFields().size() != rec->getFields().size()) continue;
                    bool match = true;
                    for (size_t i = 0; i < rec->getFields().size(); ++i) {
                        if (rec->getFields()[i]->getName() != declRec->getFields()[i]->getName()) {
                            match = false;
                            break;
                        }
                    }
                    if (match) {
                        name = "toString_" + typeName;
                        break;
                    }
                }
            }
        }
    }

    // Call via closure when the name is a local variable (e.g. parameter f in optionMap body)
    llvm::AllocaInst* calleeAlloca = getVariable(name);
    if (calleeAlloca && calleeAlloca->getAllocatedType()->isPointerTy()) {
        llvm::Value* closure = builder_->CreateLoad(calleeAlloca->getAllocatedType(), calleeAlloca, name + ".load");
        // Determine the closure's return type from the AST type of the variable.
        // Previously this was hard-coded to i64, which breaks when calling closures
        // that return non-i64 values (e.g. Option<T> represented as ptr).
        llvm::Type* retTy = nullptr;
        auto getVarAstType = [&](const std::string& var) -> ast::Type* {
            if (currentFunctionDecl_) {
                for (const auto& p : currentFunctionDecl_->getParameters()) {
                    if (p && p->getName() == var) return p->getType();
                }
            }
            if (currentInteractionDecl_) {
                for (const auto& p : currentInteractionDecl_->getParameters()) {
                    if (p && p->getName() == var) return p->getType();
                }
            }
            return nullptr;
        };
        ast::Type* varAstType = getVarAstType(name);
        std::unique_ptr<ast::Type> varAstTypeSubstituted;
        if (varAstType) {
            if (currentTypeSubst_) {
                varAstTypeSubstituted = substituteType(varAstType, *currentTypeSubst_);
                if (varAstTypeSubstituted) varAstType = varAstTypeSubstituted.get();
            }
            if (auto* fty = dynamic_cast<ast::FunctionType*>(varAstType)) {
                if (fty->getReturnType()) {
                    retTy = convertType(fty->getReturnType());
                } else {
                    retTy = llvm::Type::getVoidTy(context_);
                }
            }
        }
        if (!retTy) {
            // Fallback: i64 is the most common scalar; better than emitting invalid IR.
            retTy = llvm::Type::getInt64Ty(context_);
        }
        llvm::Value* result = invokeClosure(closure, args, retTy);
        if (!result && retTy->isVoidTy()) {
            // Expression position expects a value; use i64 0 for Unit/void.
            return llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
        }
        return result;
    }

    // Look up function in module
    llvm::Function* func = module_->getFunction(name);

    // Call monomorphized generic when we have inferred type args or can infer from context
    ast::FunctionDecl* astFunc = nullptr;
    if (currentProgram_) {
        for (const auto& f : currentProgram_->getFunctions()) {
            if (f && f->getName() == name) {
                if (!astFunc)
                    astFunc = f.get();
                else if (astFunc->isSignature() && !f->isSignature())
                    astFunc = f.get();
                if (astFunc && !astFunc->isSignature())
                    break;
            }
        }
    }
    // Imported generic (e.g. optionMap, some from Prelude): resolve from loaded modules to monomorphize
    if (!astFunc && moduleResolver_) {
        for (const std::string& modName : moduleResolver_->getLoadedModuleNames()) {
            astFunc = moduleResolver_->getFunction(modName, name);
            if (astFunc) break;
        }
    }
    if (astFunc && !astFunc->getGenericParams().empty()) {
            std::vector<ast::Type*> typeArgs;
            std::vector<std::unique_ptr<ast::Type>> typeArgsStorage;
            if (!inferred.empty()) {
                if (currentTypeSubst_) {
                    for (const auto& t : inferred) {
                        if (!t) continue;
                        auto sub = substituteType(t.get(), *currentTypeSubst_);
                        if (sub) {
                            typeArgsStorage.push_back(std::move(sub));
                            typeArgs.push_back(typeArgsStorage.back().get());
                        } else {
                            typeArgs.push_back(t.get());
                        }
                    }
                } else {
                    for (const auto& t : inferred)
                        if (t) typeArgs.push_back(t.get());
                }
            } else if (currentTypeSubst_) {
                for (const auto& gp : astFunc->getGenericParams()) {
                    auto it = currentTypeSubst_->find(gp.name);
                    if (it == currentTypeSubst_->end() || !it->second) break;
                    typeArgs.push_back(it->second);
                }
                // In monomorphized body, infer type for no-arg generics (e.g. none()) from current function return type
                if (typeArgs.empty() && astFunc->getGenericParams().size() == 1 && currentFunctionDecl_ &&
                    currentFunctionDecl_->getReturnType()) {
                    std::unique_ptr<ast::Type> subRet = substituteType(currentFunctionDecl_->getReturnType(), *currentTypeSubst_);
                    if (subRet) {
                        if (auto* p = dynamic_cast<ast::ParameterizedType*>(subRet.get()))
                            if (p->getTypeArgs().size() == 1 && p->getTypeArgs()[0]) {
                                std::unique_ptr<ast::Type> elemCopy = copyType(p->getTypeArgs()[0].get());
                                if (elemCopy) {
                                    typeArgsStorage.push_back(std::move(elemCopy));
                                    typeArgs.push_back(typeArgsStorage.back().get());
                                }
                            }
                    }
                }
            } else if (argExprs && argExprs->size() == astFunc->getParameters().size()) {
                auto getDeclBody = [this]() -> const std::vector<std::unique_ptr<ast::Stmt>>* {
                    if (currentFunctionDecl_) return &currentFunctionDecl_->getBody();
                    if (currentInteractionDecl_) return &currentInteractionDecl_->getBody();
                    return nullptr;
                };
                const auto* body = getDeclBody();
                if (body) {
                    std::map<std::string, ast::Type*> subst;
                    for (size_t i = 0; i < argExprs->size(); ++i) {
                        ast::Type* paramType = astFunc->getParameters()[i]->getType();
                        ast::Type* argType = nullptr;
                        if (auto* varExpr = dynamic_cast<ast::VariableExpr*>((*argExprs)[i].get())) {
                            for (const auto& stmt : *body) {
                                if (auto* vd = dynamic_cast<ast::VariableDecl*>(stmt.get())) {
                                    if (vd->getName() == varExpr->getName() && vd->getType()) {
                                        argType = vd->getType();
                                        break;
                                    }
                                }
                            }
                        }
                        if (!argType) continue;
                        auto* pParam = dynamic_cast<ast::ParameterizedType*>(paramType);
                        auto* pArg = dynamic_cast<ast::ParameterizedType*>(argType);
                        if (pParam && pArg && pParam->getBaseName() == pArg->getBaseName() &&
                            pParam->getTypeArgs().size() == pArg->getTypeArgs().size()) {
                            for (size_t j = 0; j < pParam->getTypeArgs().size(); ++j) {
                                if (auto* g = dynamic_cast<ast::GenericType*>(pParam->getTypeArgs()[j].get())) {
                                    if (pArg->getTypeArgs()[j]) subst[g->getName()] = pArg->getTypeArgs()[j].get();
                                }
                            }
                        } else if (auto* gParam = dynamic_cast<ast::GenericType*>(paramType)) {
                            subst[gParam->getName()] = argType;
                        }
                    }
                    if (subst.size() == astFunc->getGenericParams().size()) {
                        for (const auto& gp : astFunc->getGenericParams()) {
                            auto it = subst.find(gp.name);
                            if (it == subst.end() || !it->second) break;
                            typeArgs.push_back(it->second);
                        }
                    }
                }
            }
            if (typeArgs.size() == astFunc->getGenericParams().size()) {
                bool allConcrete = true;
                for (ast::Type* ta : typeArgs) {
                    if (ta && dynamic_cast<ast::GenericType*>(ta)) {
                        allConcrete = false;
                        break;
                    }
                }
                if (allConcrete) {
                    llvm::Function* monoFunc = monomorphizeFunction(astFunc, typeArgs);
                    if (monoFunc) {
                        bool needBody = monoFunc->isDeclaration() && generatingMonomorphized_.count(monoFunc) == 0;
                        if (needBody)
                            generateMonomorphizedBody(astFunc, typeArgs, monoFunc);
                        func = monoFunc;
                    }
                }
            }
    }

    auto i8ptr = llvm::PointerType::get(context_, 0);
    auto i64 = llvm::Type::getInt64Ty(context_);
    auto f64 = llvm::Type::getDoubleTy(context_);
    auto voidTy = llvm::Type::getVoidTy(context_);

    // Stdlib (Phase 7.3): map First name -> C symbol and signature when not in module
    auto getStdlibSig = [&](const std::string& n) -> std::pair<std::string, llvm::FunctionType*> {
        if (n == "print" || n == "println")
            return {n, llvm::FunctionType::get(voidTy, {i8ptr}, false)};
        if (n == "sleep")
            return {"first_sleep", llvm::FunctionType::get(voidTy, {i64}, false)};
        if (n == "readLine")
            return {"readLine", llvm::FunctionType::get(i8ptr, false)};
        if (n == "readFile")
            return {"readFile", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "writeFile")
            return {"writeFile", llvm::FunctionType::get(voidTy, {i8ptr, i8ptr}, false)};
        if (n == "sin" || n == "cos" || n == "sqrt" || n == "abs" || n == "floor" || n == "ceil" || n == "tan" || n == "exp" || n == "log" || n == "log10" || n == "round" || n == "sign")
            return {"first_" + n, llvm::FunctionType::get(f64, {f64}, false)};
        if (n == "pow")
            return {"first_pow", llvm::FunctionType::get(f64, {f64, f64}, false)};
        if (n == "min" || n == "max")
            return {"first_" + n, llvm::FunctionType::get(f64, {f64, f64}, false)};
        if (n == "minInt") return {"first_min_int", llvm::FunctionType::get(i64, {i64, i64}, false)};
        if (n == "maxInt") return {"first_max_int", llvm::FunctionType::get(i64, {i64, i64}, false)};
        if (n == "pi" || n == "e")
            return {"first_" + n, llvm::FunctionType::get(f64, false)};
        // Date (opaque Int handle)
        if (n == "now") return {"first_date_now", llvm::FunctionType::get(i64, false)};
        if (n == "format") return {"first_date_format", llvm::FunctionType::get(i8ptr, {i64, i8ptr}, false)};
        if (n == "parse") return {"first_date_parse", llvm::FunctionType::get(i64, {i8ptr}, false)};
        if (n == "getYear") return {"first_date_get_year", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "getMonth") return {"first_date_get_month", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "getDay") return {"first_date_get_day", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "getHours") return {"first_date_get_hours", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "getMinutes") return {"first_date_get_minutes", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "getSeconds") return {"first_date_get_seconds", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "addSeconds") return {"first_date_add_seconds", llvm::FunctionType::get(i64, {i64, i64}, false)};
        // ArrayBuf and binary I/O
        if (n == "arrayBufCreate") return {"first_arraybuf_alloc", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "arrayBufLength") return {"first_arraybuf_length", llvm::FunctionType::get(i64, {i8ptr}, false)};
        if (n == "arrayBufGet") return {"first_arraybuf_get", llvm::FunctionType::get(i64, {i8ptr, i64}, false)};
        if (n == "arrayBufSet") return {"first_arraybuf_set", llvm::FunctionType::get(voidTy, {i8ptr, i64, i64}, false)};
        if (n == "readFileBytes") return {"first_read_file_bytes", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "writeFileBytes") return {"first_write_file_bytes", llvm::FunctionType::get(voidTy, {i8ptr, i8ptr}, false)};
        if (n == "base64Encode") return {"first_base64_encode", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "base64Decode") return {"first_base64_decode", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "arrayBufToString") return {"first_arraybuf_to_string", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "stringLength") return {"first_string_length", llvm::FunctionType::get(i64, {i8ptr}, false)};
        if (n == "stringConcat") return {"first_string_concat", llvm::FunctionType::get(i8ptr, {i8ptr, i8ptr}, false)};
        if (n == "stringSlice") return {"first_string_slice", llvm::FunctionType::get(i8ptr, {i8ptr, i64, i64}, false)};
        if (n == "stringToInt") return {"first_string_to_int", llvm::FunctionType::get(i64, {i8ptr}, false)};
        if (n == "stringToFloat") return {"first_string_to_float", llvm::FunctionType::get(f64, {i8ptr}, false)};
        if (n == "intToString") return {"first_int_to_string", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "floatToString") return {"first_float_to_string", llvm::FunctionType::get(i8ptr, {f64}, false)};
        if (n == "boolToString") return {"first_bool_to_string", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "unitToString") return {"first_unit_to_string", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "socketConnect") return {"first_socket_connect", llvm::FunctionType::get(i64, {i8ptr, i64}, false)};
        if (n == "socketSend") return {"first_socket_send", llvm::FunctionType::get(i64, {i64, i8ptr}, false)};
        if (n == "socketRecv") return {"first_socket_recv_str", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "socketClose") return {"first_socket_close", llvm::FunctionType::get(voidTy, {i64}, false)};
        if (n == "httpGet") return {"first_http_get", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "httpPost") return {"first_http_post", llvm::FunctionType::get(i8ptr, {i8ptr, i8ptr}, false)};
        if (n == "httpRequest") return {"first_http_request", llvm::FunctionType::get(i64, {i8ptr, i8ptr, i8ptr, i8ptr, i8ptr, i8ptr}, false)};
        if (n == "httpServerCreate") return {"first_http_server_create", llvm::FunctionType::get(i64, {i8ptr, i64}, false)};
        if (n == "httpServerGet") return {"first_http_server_get", llvm::FunctionType::get(voidTy, {i64, i8ptr, i64}, false)};
        if (n == "httpServerPost") return {"first_http_server_post", llvm::FunctionType::get(voidTy, {i64, i8ptr, i64}, false)};
        if (n == "httpServerListen") return {"first_http_server_listen", llvm::FunctionType::get(voidTy, {i64}, false)};
        if (n == "httpServerClose") return {"first_http_server_close", llvm::FunctionType::get(voidTy, {i64}, false)};
        if (n == "httpReqMethod") return {"first_http_req_method", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpReqPath") return {"first_http_req_path", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpReqParamsJson") return {"first_http_req_params_json", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpReqQueryJson") return {"first_http_req_query_json", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpReqHeadersJson") return {"first_http_req_headers_json", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpReqBody") return {"first_http_req_body", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpResponseCreate") return {"first_http_response_create", llvm::FunctionType::get(i64, {i64, i8ptr, i8ptr}, false)};
        if (n == "httpRespStatus") return {"first_http_resp_status", llvm::FunctionType::get(i64, {i64}, false)};
        if (n == "httpRespHeadersJson") return {"first_http_resp_headers_json", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "httpRespBody") return {"first_http_resp_body", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "jsonPrettify") return {"first_json_prettify", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "jsonStringifyString") return {"first_json_stringify_string", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (n == "jsonStringifyInt") return {"first_json_stringify_int", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (n == "jsonStringifyFloat") return {"first_json_stringify_float", llvm::FunctionType::get(i8ptr, {f64}, false)};
        return {"", nullptr};
    };

    if (!func) {
        auto [cSymbol, funcType] = getStdlibSig(name);
        if (funcType) {
            func = module_->getFunction(cSymbol);
            if (!func)
                func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, cSymbol, module_.get());
        } else {
            std::vector<llvm::Type*> paramTypes;
            for (auto* v : args) paramTypes.push_back(v ? v->getType() : i64);
            llvm::FunctionType* defaultType = llvm::FunctionType::get(i64, paramTypes, false);
            func = llvm::Function::Create(defaultType, llvm::Function::ExternalLinkage, name, module_.get());
        }
    }

    if (!func) {
        errorReporter_.error(loc, "Undefined function: " + name);
        return nullptr;
    }

    // Ensure first_bool_to_string and first_unit_to_string receive i64 (runtime ABI)
    llvm::StringRef funcNameRef = func->getName();
    if ((funcNameRef == "first_bool_to_string" || funcNameRef == "first_unit_to_string") &&
        args.size() == 1 && args[0] && args[0]->getType()->isIntegerTy(1)) {
        args[0] = builder_->CreateZExt(args[0], i64, "arg.bool.i64");
    }

    if (func->getFunctionType() && func->getFunctionType()->getNumParams() == args.size()) {
        for (size_t i = 0; i < args.size(); ++i) {
            llvm::Type* pt = func->getFunctionType()->getParamType(static_cast<unsigned>(i));
            if (!args[i]) continue;
            llvm::Type* at = args[i]->getType();
            if (pt == at) continue;
            if (pt->isIntegerTy(64) && at->isPointerTy()) {
                args[i] = builder_->CreatePtrToInt(args[i], i64, "arg.i64");
            } else if (pt->isIntegerTy(64) && at->isIntegerTy(1)) {
                // Stdlib (e.g. first_bool_to_string) expects i64 for Bool; extend i1 to i64
                args[i] = builder_->CreateZExt(args[i], i64, "arg.bool.i64");
            } else if (pt->isDoubleTy() && at->isIntegerTy(64)) {
                args[i] = builder_->CreateSIToFP(args[i], llvm::Type::getDoubleTy(context_), "arg.f64");
            } else if (pt->isIntegerTy(64) && at->isDoubleTy()) {
                args[i] = builder_->CreateFPToSI(args[i], i64, "arg.i64");
            } else if (pt->isIntegerTy(1) && at->isIntegerTy(64)) {
                args[i] = builder_->CreateICmpNE(args[i], llvm::ConstantInt::get(i64, 0), "arg.bool");
            } else if (pt->isPointerTy() && at->isIntegerTy(64)) {
                llvm::AllocaInst* slot = builder_->CreateAlloca(llvm::Type::getInt64Ty(context_), nullptr, "scalar.arg");
                builder_->CreateAlignedStore(args[i], slot, llvm::Align(8));
                args[i] = builder_->CreateBitCast(slot, i8ptr, "scalar.arg.ptr");
            } else if (pt->isPointerTy() && at->isDoubleTy()) {
                llvm::AllocaInst* slot = builder_->CreateAlloca(llvm::Type::getDoubleTy(context_), nullptr, "scalar.arg");
                builder_->CreateAlignedStore(args[i], slot, llvm::Align(8));
                args[i] = builder_->CreateBitCast(slot, i8ptr, "scalar.arg.ptr");
            } else if (pt->isPointerTy() && at->isIntegerTy(1)) {
                llvm::AllocaInst* slot = builder_->CreateAlloca(llvm::Type::getInt1Ty(context_), nullptr, "scalar.arg");
                builder_->CreateAlignedStore(args[i], slot, llvm::Align(1));
                args[i] = builder_->CreateBitCast(slot, i8ptr, "scalar.arg.ptr");
            } else if (pt->isStructTy() && at->isPointerTy()) {
                args[i] = builder_->CreateLoad(pt, args[i], "loadrec");
            }
        }
    }

    llvm::Value* callValue = builder_->CreateCall(func, args,
        func->getReturnType()->isVoidTy() ? "" : "calltmp");
    if (tailCall) {
        if (auto* callInst = llvm::dyn_cast<llvm::CallInst>(callValue)) {
            callInst->setTailCall(true);
        }
    }
    return callValue;
}

llvm::Value* IRGenerator::evaluateFunctionCall(ast::FunctionCallExpr* expr, bool tailCall) {
    std::string funcName = expr->getName();

    // Evaluate arguments first (so we can synthesize an extern declaration if needed).
    std::vector<llvm::Value*> args;
    const auto& argExprs = expr->getArgs();
    for (size_t i = 0; i < argExprs.size(); ++i) {
        llvm::Value* argValue = evaluateExpr(argExprs[i].get());
        if (!argValue) {
            return nullptr;
        }
        // Root-cause fix for head(xs)=0: for cons(1, cons(2, ...)), the outer call's first argument
        // can incorrectly evaluate to 0. Use the literal value from the AST when the first arg is an Int literal.
        if (isConsCall(funcName) && i == 0) {
            int64_t literalVal = 0;
            if (getConsFirstArgLiteral(argExprs[i].get(), literalVal)) {
                argValue = llvm::ConstantInt::get(context_, llvm::APInt(64, static_cast<uint64_t>(literalVal), true));
            }
        }
        args.push_back(argValue);
    }

    // Safety net: force cons first arg from AST literal (in case in-loop fix didn't apply).
    if (isConsCall(funcName) && !argExprs.empty() && args.size() >= 1 && args[0]) {
        int64_t literalVal = 0;
        if (getConsFirstArgLiteral(argExprs[0].get(), literalVal) && args[0]->getType()->isIntegerTy(64)) {
            args[0] = llvm::ConstantInt::get(context_, llvm::APInt(64, static_cast<uint64_t>(literalVal), true));
        }
    }

    // Option helpers: desugar some(x)/none() to real constructors Some(x)/None
    // so we don't depend on linking Prelude, and we use the ADT constructor layout.
    if (funcName == "none" && args.empty()) funcName = "None";
    if (funcName == "some" && args.size() == 1) funcName = "Some";

    // Constructor call (sum type): Name(arg1, ...) -> build tagged union (single heap block, inline payload)
    auto [adtAstType, tagIndex] = getConstructorIndex(funcName);
    if (adtAstType) {
        llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
        llvm::Type* payloadPtrType = llvm::PointerType::get(context_, 0);
        const llvm::DataLayout& DL = module_->getDataLayout();
        llvm::StructType* fullType = nullptr;
        llvm::Value* rawPtr = nullptr;
        if (!args.empty()) {
            // Use constructor's declared argument types for payload layout so it matches pattern matching.
            std::vector<llvm::Type*> fieldTypes;
            ast::Constructor* ctor = nullptr;
            for (const auto& c : adtAstType->getConstructors()) {
                if (c && c->getName() == funcName) { ctor = c.get(); break; }
            }
            if (ctor && ctor->getArgumentTypes().size() == args.size()) {
                for (const auto& at : ctor->getArgumentTypes()) {
                    llvm::Type* ft = nullptr;
                    if (currentTypeSubst_ && at.get()) {
                        auto subTy = substituteType(at.get(), *currentTypeSubst_);
                        if (subTy) ft = convertType(subTy.get());
                    }
                    if (!ft) ft = convertType(at.get());
                    fieldTypes.push_back(ft ? ft : llvm::PointerType::get(context_, 0));
                }
            }
            if (fieldTypes.size() != args.size()) {
                fieldTypes.clear();
                for (llvm::Value* v : args) fieldTypes.push_back(v->getType());
            }
            llvm::StructType* payloadStructType = createStructTypeSafe(context_, fieldTypes);
            fullType = createStructTypeSafe(context_, {tagType, payloadStructType});
            uint64_t fullSize = DL.getTypeAllocSize(fullType);
            rawPtr = builder_->CreateCall(
                getOrCreateFirstAlloc(),
                {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context_), fullSize)},
                "adtheap"
            );
            llvm::Value* structPtr = builder_->CreateBitCast(rawPtr, llvm::PointerType::get(context_, 0), "adtstruct");
            llvm::Value* tagGep = builder_->CreateStructGEP(fullType, structPtr, 0, "tagptr");
            builder_->CreateAlignedStore(
                llvm::ConstantInt::get(context_, llvm::APInt(64, static_cast<uint64_t>(tagIndex))),
                tagGep, llvm::Align(8));
            llvm::Value* payloadPtr = builder_->CreateStructGEP(fullType, structPtr, 1, "payloadptr");
            for (size_t i = 0; i < args.size(); ++i) {
                llvm::Value* toStore = args[i];
                llvm::Value* fieldPtr = builder_->CreateStructGEP(payloadStructType, payloadPtr, static_cast<unsigned>(i), "fieldptr");
                builder_->CreateAlignedStore(toStore, fieldPtr, llvm::Align(8));
            }
        } else {
            fullType = createStructTypeSafe(context_, std::vector<llvm::Type*>{tagType});
            uint64_t fullSize = DL.getTypeAllocSize(fullType);
            rawPtr = builder_->CreateCall(
                getOrCreateFirstAlloc(),
                {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context_), fullSize)},
                "adtheap"
            );
            llvm::Value* structPtr = builder_->CreateBitCast(rawPtr, llvm::PointerType::get(context_, 0), "adtstruct");
            builder_->CreateAlignedStore(
                llvm::ConstantInt::get(context_, llvm::APInt(64, static_cast<uint64_t>(tagIndex))),
                builder_->CreateStructGEP(fullType, structPtr, 0, "tagptr"), llvm::Align(8));
        }
        return builder_->CreateBitCast(rawPtr, payloadPtrType, "adtptr");
    }

    // Phase 7.3: arrayLength and Array<Int> intrinsics; generic append/insertAt/map via runtime
    llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
    llvm::Type* f64 = llvm::Type::getDoubleTy(context_);
    llvm::Type* voidTy = llvm::Type::getVoidTy(context_);
    if (funcName == "arrayLength" && args.size() == 1) {
        auto it = arrayMetadata_.find(args[0]);
        if (it != arrayMetadata_.end()) {
            return builder_->getInt64(static_cast<int64_t>(it->second.size));
        }
        // No metadata: call runtime first_array_length(ptr, 0)
        llvm::Function* flen = module_->getFunction("first_array_length");
        if (!flen) {
            flen = llvm::Function::Create(
                llvm::FunctionType::get(i64, {i8ptr, i64}, false),
                llvm::Function::ExternalLinkage, "first_array_length", module_.get());
        }
        llvm::Value* zero = builder_->getInt64(0);
        return builder_->CreateCall(flen, {args[0], zero}, "arraylen");
    }

    // Helper to resolve array metadata from an array value (e.g. args[0])
    auto resolveArrayMetadata = [this](llvm::Value* arrayVal) -> std::pair<llvm::AllocaInst*, int64_t> {
        auto it = arrayMetadata_.find(arrayVal);
        if (it != arrayMetadata_.end()) {
            int64_t len = it->second.lengthAlloca ? -1 : static_cast<int64_t>(it->second.size);
            return {it->second.alloca, len};
        }
        if (llvm::LoadInst* loadInst = llvm::dyn_cast<llvm::LoadInst>(arrayVal)) {
            it = arrayMetadata_.find(loadInst->getPointerOperand());
            if (it != arrayMetadata_.end()) {
                int64_t len = it->second.lengthAlloca ? -1 : static_cast<int64_t>(it->second.size);
                return {it->second.alloca, len};
            }
        }
        for (const auto& varPair : localVars_) {
            it = arrayMetadata_.find(varPair.second);
            if (it != arrayMetadata_.end()) {
                int64_t len = it->second.lengthAlloca ? -1 : static_cast<int64_t>(it->second.size);
                return {it->second.alloca, len};
            }
        }
        return {nullptr, 0};
    };

    // Get (basePtr, lenVal) for Array ops: from metadata or via first_array_length(ptr, 0)
    auto getArrayBaseAndLength = [this, i8ptr, i64, resolveArrayMetadata](llvm::Value* arrayArg) -> std::pair<llvm::Value*, llvm::Value*> {
        llvm::Type* i64ptr = llvm::PointerType::get(context_, 0);
        auto [baseAlloca, len] = resolveArrayMetadata(arrayArg);
        if (baseAlloca) {
            llvm::Value* lenVal = nullptr;
            if (len >= 0) {
                lenVal = builder_->getInt64(len);
            } else {
                // Dynamic length: find metadata by alloca and load from lengthAlloca
                for (const auto& [val, meta] : arrayMetadata_) {
                    if (meta.alloca == baseAlloca && meta.lengthAlloca) {
                        lenVal = builder_->CreateLoad(i64, meta.lengthAlloca, "dynlen");
                        break;
                    }
                }
            }
            if (lenVal) {
                llvm::Value* basePtr = builder_->CreateBitCast(baseAlloca, i64ptr, "arrbase");
                return {basePtr, lenVal};
            }
        }
        llvm::Function* flen = module_->getFunction("first_array_length");
        if (!flen) flen = llvm::Function::Create(
            llvm::FunctionType::get(i64, {i8ptr, i64}, false),
            llvm::Function::ExternalLinkage, "first_array_length", module_.get());
        llvm::Value* lenVal = builder_->CreateCall(flen, {arrayArg, builder_->getInt64(0)}, "arrlen");
        llvm::Value* basePtr = builder_->CreateBitCast(arrayArg, i64ptr, "arrbase");
        return {basePtr, lenVal};
    };

    // arrayReduceIntSum(arr: Array<Int>) -> Int
    if (funcName == "arrayReduceIntSum" && args.size() == 1) {
        auto [basePtr, lenVal] = getArrayBaseAndLength(args[0]);
        llvm::Type* i64ptr = llvm::PointerType::get(context_, 0);
        llvm::Function* fn = module_->getFunction("first_array_reduce_int_sum");
        if (!fn) fn = llvm::Function::Create(
            llvm::FunctionType::get(i64, {i64ptr, i64}, false),
            llvm::Function::ExternalLinkage, "first_array_reduce_int_sum", module_.get());
        return builder_->CreateCall(fn, {basePtr, lenVal}, "reduce_sum");
    }

    // arrayMapIntDouble(arr: Array<Int>) -> Array<Int>
    if (funcName == "arrayMapIntDouble" && args.size() == 1) {
        auto [basePtr, lenVal] = getArrayBaseAndLength(args[0]);
        llvm::Type* i64ptr = llvm::PointerType::get(context_, 0);
        llvm::Function* fn = module_->getFunction("first_array_map_int_double");
        if (!fn) fn = llvm::Function::Create(
            llvm::FunctionType::get(i64ptr, {i64ptr, i64}, false),
            llvm::Function::ExternalLinkage, "first_array_map_int_double", module_.get());
        llvm::Value* outPtr = builder_->CreateCall(fn, {basePtr, lenVal}, "map_out");
        if (!outPtr) return nullptr;
        llvm::AllocaInst* resultAlloca = builder_->CreateAlloca(i64, lenVal, "map_result");
        llvm::BasicBlock* mapEntry = builder_->GetInsertBlock();
        llvm::BasicBlock* loopCond = llvm::BasicBlock::Create(context_, "map_loop_cond", currentFunction_);
        llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(context_, "map_loop_body", currentFunction_);
        llvm::BasicBlock* loopEnd = llvm::BasicBlock::Create(context_, "map_loop_end", currentFunction_);
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopCond);
        llvm::PHINode* iPhi = builder_->CreatePHI(i64, 2, "i");
        iPhi->addIncoming(builder_->getInt64(0), mapEntry);
        llvm::Value* cmp = builder_->CreateICmpSLT(iPhi, lenVal, "cmp");
        builder_->CreateCondBr(cmp, loopBody, loopEnd);
        builder_->SetInsertPoint(loopBody);
        llvm::Value* srcElem = builder_->CreateLoad(i64, builder_->CreateGEP(i64, outPtr, iPhi), "mapelem");
        llvm::Value* dstGep = builder_->CreateGEP(i64, resultAlloca, iPhi, "dst");
        builder_->CreateStore(srcElem, dstGep);
        llvm::Value* iNext = builder_->CreateAdd(iPhi, builder_->getInt64(1), "i_next");
        iPhi->addIncoming(iNext, loopBody);
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopEnd);
        ArrayMetadata meta;
        meta.elementType = i64;
        meta.size = 0;
        meta.alloca = resultAlloca;
        arrayMetadata_[resultAlloca] = meta;
        arrayMetadata_[builder_->CreateBitCast(resultAlloca, i8ptr)] = meta;
        return builder_->CreateBitCast(resultAlloca, i8ptr, "map_arr");
    }

    // arrayFilterIntPositive(arr: Array<Int>) -> Array<Int>
    if (funcName == "arrayFilterIntPositive" && args.size() == 1) {
        auto [basePtr, lenVal] = getArrayBaseAndLength(args[0]);
        llvm::Type* i64ptr = llvm::PointerType::get(context_, 0);
        llvm::AllocaInst* outLenAlloca = builder_->CreateAlloca(i64, nullptr, "out_len");
        llvm::Function* fn = module_->getFunction("first_array_filter_int_positive");
        if (!fn) fn = llvm::Function::Create(
            llvm::FunctionType::get(i64ptr, {i64ptr, i64, llvm::PointerType::get(context_, 0)}, false),
            llvm::Function::ExternalLinkage, "first_array_filter_int_positive", module_.get());
        llvm::Value* outPtr = builder_->CreateCall(fn, {basePtr, lenVal, outLenAlloca}, "filter_out");
        llvm::Value* outLenVal = builder_->CreateLoad(i64, outLenAlloca, "out_len_val");
        llvm::Value* outLenZero = builder_->CreateICmpEQ(outLenVal, builder_->getInt64(0), "is_zero");
        llvm::BasicBlock* hasResult = llvm::BasicBlock::Create(context_, "filter_has", currentFunction_);
        llvm::BasicBlock* emptyResult = llvm::BasicBlock::Create(context_, "filter_empty", currentFunction_);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "filter_merge", currentFunction_);
        builder_->CreateCondBr(outLenZero, emptyResult, hasResult);
        builder_->SetInsertPoint(hasResult);
        llvm::AllocaInst* resultAlloca = builder_->CreateAlloca(i64, outLenVal, "filter_result");
        llvm::BasicBlock* loopCond = llvm::BasicBlock::Create(context_, "filter_loop_cond", currentFunction_);
        llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(context_, "filter_loop_body", currentFunction_);
        llvm::BasicBlock* loopEnd = llvm::BasicBlock::Create(context_, "filter_loop_end", currentFunction_);
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopCond);
        llvm::PHINode* iPhi = builder_->CreatePHI(i64, 2, "i");
        iPhi->addIncoming(builder_->getInt64(0), hasResult);
        llvm::Value* cmp = builder_->CreateICmpSLT(iPhi, outLenVal, "cmp");
        builder_->CreateCondBr(cmp, loopBody, loopEnd);
        builder_->SetInsertPoint(loopBody);
        llvm::Value* srcElem = builder_->CreateLoad(i64, builder_->CreateGEP(i64, outPtr, iPhi), "felem");
        llvm::Value* dstGep = builder_->CreateGEP(i64, resultAlloca, iPhi, "fdst");
        builder_->CreateStore(srcElem, dstGep);
        llvm::Value* iNext = builder_->CreateAdd(iPhi, builder_->getInt64(1), "i_next");
        iPhi->addIncoming(iNext, loopBody);
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopEnd);
        ArrayMetadata meta;
        meta.elementType = i64;
        meta.size = 0;
        meta.alloca = resultAlloca;
        arrayMetadata_[resultAlloca] = meta;
        arrayMetadata_[builder_->CreateBitCast(resultAlloca, i8ptr)] = meta;
        llvm::Value* resultPtr = builder_->CreateBitCast(resultAlloca, i8ptr, "filter_arr");
        builder_->CreateBr(mergeBB);
        builder_->SetInsertPoint(emptyResult);
        llvm::AllocaInst* emptyAlloca = builder_->CreateAlloca(llvm::ArrayType::get(i64, 1), nullptr, "empty_arr");
        ArrayMetadata emptyMeta;
        emptyMeta.elementType = i64;
        emptyMeta.size = 0;
        emptyMeta.alloca = emptyAlloca;
        arrayMetadata_[emptyAlloca] = emptyMeta;
        arrayMetadata_[builder_->CreateBitCast(emptyAlloca, i8ptr)] = emptyMeta;
        llvm::Value* emptyPtr = builder_->CreateBitCast(emptyAlloca, i8ptr);
        builder_->CreateBr(mergeBB);
        builder_->SetInsertPoint(mergeBB);
        llvm::PHINode* phi = builder_->CreatePHI(i8ptr, 2, "filter_phi");
        phi->addIncoming(resultPtr, loopEnd);
        phi->addIncoming(emptyPtr, emptyResult);
        return phi;
    }

    // insertAt<T>(a, value, position) -> Option<Array<T>>; deleteAt<T>(a, position) -> Option<Array<T>>
    const llvm::DataLayout& DL = module_->getDataLayout();
    auto buildOptionFromPtr = [this, i8ptr, i64, &DL](llvm::Value* ptr, bool isNone) -> llvm::Value* {
        auto [optAdt, noneTag] = getConstructorIndex("None");
        auto [optAdtSome, someTag] = getConstructorIndex("Some");
        (void)optAdt;
        (void)optAdtSome;
        llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
        if (isNone || !ptr) {
            llvm::StructType* noneType = createStructTypeSafe(context_, {tagType});
            uint64_t sz = DL.getTypeAllocSize(noneType);
            llvm::Value* rawPtr = builder_->CreateCall(
                getOrCreateFirstAlloc(),
                {llvm::ConstantInt::get(i64, static_cast<uint64_t>(sz))},
                "option_none");
            llvm::Value* structPtr = builder_->CreateBitCast(rawPtr, llvm::PointerType::get(context_, 0), "noneptr");
            llvm::Value* tagGep = builder_->CreateStructGEP(noneType, builder_->CreateBitCast(structPtr, llvm::PointerType::get(context_, 0)), 0, "tag");
            builder_->CreateAlignedStore(llvm::ConstantInt::get(context_, llvm::APInt(64, noneTag)), tagGep, llvm::Align(8));
            return builder_->CreateBitCast(structPtr, i8ptr, "option_none_val");
        }
        llvm::StructType* somePayload = createStructTypeSafe(context_, {llvm::PointerType::get(context_, 0)});
        llvm::StructType* someType = createStructTypeSafe(context_, {tagType, somePayload});
        uint64_t fullSz = DL.getTypeAllocSize(someType);
        llvm::Value* rawPtr = builder_->CreateCall(
            getOrCreateFirstAlloc(),
            {llvm::ConstantInt::get(i64, static_cast<uint64_t>(fullSz))},
            "option_some");
        llvm::StructType* someTypeCast = createStructTypeSafe(context_, {tagType, somePayload});
        llvm::Value* structPtr = builder_->CreateBitCast(rawPtr, llvm::PointerType::get(context_, 0), "someptr");
        llvm::Value* typedPtr = builder_->CreateBitCast(structPtr, llvm::PointerType::get(context_, 0), "sometyped");
        llvm::Value* tagGep = builder_->CreateStructGEP(someTypeCast, typedPtr, 0, "tag");
        builder_->CreateAlignedStore(llvm::ConstantInt::get(context_, llvm::APInt(64, someTag)), tagGep, llvm::Align(8));
        llvm::Value* payloadGep = builder_->CreateStructGEP(someTypeCast, typedPtr, 1, "payload");
        llvm::Value* elemGep = builder_->CreateStructGEP(somePayload, payloadGep, 0, "elem");
        builder_->CreateStore(ptr, elemGep);
        return builder_->CreateBitCast(structPtr, i8ptr, "option_some_val");
    };
    if (funcName == "insertAt" && args.size() == 3) {
        const auto& insertInferred = expr->getInferredTypeArgs();
        if (insertInferred.empty() || !insertInferred[0]) return nullptr;
        ast::Type* elemType = insertInferred[0].get();
        if (currentTypeSubst_) {
            auto sub = substituteType(elemType, *currentTypeSubst_);
            if (sub) elemType = sub.get();
        }
        llvm::Type* elemLlvm = convertType(elemType);
        if (!elemLlvm) return nullptr;
        uint64_t elemSize = DL.getTypeAllocSize(elemLlvm);
        llvm::Value* arrPtr = args[0];
        llvm::Value* lenVal = nullptr;
        auto [baseAlloca, lenConst] = resolveArrayMetadata(arrPtr);
        if (baseAlloca && lenConst >= 0) lenVal = builder_->getInt64(lenConst);
        else {
            llvm::Function* flen = module_->getFunction("first_array_length");
            if (!flen) flen = llvm::Function::Create(llvm::FunctionType::get(i64, {i8ptr, i64}, false), llvm::Function::ExternalLinkage, "first_array_length", module_.get());
            lenVal = builder_->CreateCall(flen, {arrPtr, builder_->getInt64(0)}, "arrlen");
        }
        llvm::Value* posVal = args[2];
        llvm::AllocaInst* valueSlot = builder_->CreateAlloca(elemLlvm, nullptr, "insert_value");
        builder_->CreateStore(args[1], valueSlot);
        llvm::Function* fn = module_->getFunction("first_array_insert_at");
        if (!fn) fn = llvm::Function::Create(
            llvm::FunctionType::get(i8ptr, {i8ptr, i64, i64, i8ptr, llvm::Type::getInt64Ty(context_)}, false),
            llvm::Function::ExternalLinkage, "first_array_insert_at", module_.get());
        llvm::Value* result = builder_->CreateCall(fn, {
            arrPtr, lenVal, posVal,
            builder_->CreateBitCast(valueSlot, i8ptr, "valueptr"),
            builder_->getInt64(static_cast<int64_t>(elemSize))
        }, "insert_at_result");
        llvm::Value* isNull = builder_->CreateICmpEQ(result, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i8ptr)), "is_none");
        llvm::BasicBlock* someBB = llvm::BasicBlock::Create(context_, "insert_some", currentFunction_);
        llvm::BasicBlock* noneBB = llvm::BasicBlock::Create(context_, "insert_none", currentFunction_);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "insert_merge", currentFunction_);
        builder_->CreateCondBr(isNull, noneBB, someBB);
        builder_->SetInsertPoint(noneBB);
        llvm::Value* noneVal = buildOptionFromPtr(nullptr, true);
        builder_->CreateBr(mergeBB);
        builder_->SetInsertPoint(someBB);
        llvm::Value* someVal = buildOptionFromPtr(result, false);
        builder_->CreateBr(mergeBB);
        builder_->SetInsertPoint(mergeBB);
        llvm::PHINode* phi = builder_->CreatePHI(i8ptr, 2, "option_result");
        phi->addIncoming(noneVal, noneBB);
        phi->addIncoming(someVal, someBB);
        return phi;
    }
    if (funcName == "deleteAt" && args.size() == 2) {
        const auto& deleteInferred = expr->getInferredTypeArgs();
        if (deleteInferred.empty() || !deleteInferred[0]) return nullptr;
        ast::Type* elemType = deleteInferred[0].get();
        if (currentTypeSubst_) {
            auto sub = substituteType(elemType, *currentTypeSubst_);
            if (sub) elemType = sub.get();
        }
        llvm::Type* elemLlvm = convertType(elemType);
        if (!elemLlvm) return nullptr;
        uint64_t elemSize = DL.getTypeAllocSize(elemLlvm);
        llvm::Value* arrPtr = args[0];
        llvm::Value* lenVal = nullptr;
        auto [baseAlloca, lenConst] = resolveArrayMetadata(arrPtr);
        if (baseAlloca && lenConst >= 0) lenVal = builder_->getInt64(lenConst);
        else {
            llvm::Function* flen = module_->getFunction("first_array_length");
            if (!flen) flen = llvm::Function::Create(llvm::FunctionType::get(i64, {i8ptr, i64}, false), llvm::Function::ExternalLinkage, "first_array_length", module_.get());
            lenVal = builder_->CreateCall(flen, {arrPtr, builder_->getInt64(0)}, "arrlen");
        }
        llvm::Function* fn = module_->getFunction("first_array_delete_at");
        if (!fn) fn = llvm::Function::Create(
            llvm::FunctionType::get(i8ptr, {i8ptr, i64, i64, llvm::Type::getInt64Ty(context_)}, false),
            llvm::Function::ExternalLinkage, "first_array_delete_at", module_.get());
        llvm::Value* result = builder_->CreateCall(fn, {arrPtr, lenVal, args[1], builder_->getInt64(static_cast<int64_t>(elemSize))}, "delete_at_result");
        llvm::Value* isNull = builder_->CreateICmpEQ(result, llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i8ptr)), "is_none");
        llvm::BasicBlock* someBB = llvm::BasicBlock::Create(context_, "del_some", currentFunction_);
        llvm::BasicBlock* noneBB = llvm::BasicBlock::Create(context_, "del_none", currentFunction_);
        llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "del_merge", currentFunction_);
        builder_->CreateCondBr(isNull, noneBB, someBB);
        builder_->SetInsertPoint(noneBB);
        llvm::Value* noneVal = buildOptionFromPtr(nullptr, true);
        builder_->CreateBr(mergeBB);
        builder_->SetInsertPoint(someBB);
        llvm::Value* someVal = buildOptionFromPtr(result, false);
        builder_->CreateBr(mergeBB);
        builder_->SetInsertPoint(mergeBB);
        llvm::PHINode* phi = builder_->CreatePHI(i8ptr, 2, "option_result");
        phi->addIncoming(noneVal, noneBB);
        phi->addIncoming(someVal, someBB);
        return phi;
    }

    // reduce<T,U>(a, init, f: (acc:U, cur:T)->U) -> U  (foldLeft)
    if (funcName == "reduce" && args.size() == 3) {
        const auto& redInferred = expr->getInferredTypeArgs();
        if (redInferred.size() < 2 || !redInferred[0] || !redInferred[1]) return nullptr;
        ast::Type* elemType = redInferred[0].get();
        ast::Type* retType = redInferred[1].get();
        if (currentTypeSubst_) {
            auto sub = substituteType(elemType, *currentTypeSubst_);
            if (sub) elemType = sub.get();
            sub = substituteType(retType, *currentTypeSubst_);
            if (sub) retType = sub.get();
        }
        llvm::Type* elemLlvm = convertType(elemType);
        llvm::Type* retLlvm = convertType(retType);
        if (!elemLlvm || !retLlvm) return nullptr;
        auto [basePtr, lenVal] = getArrayBaseAndLength(args[0]);
        llvm::Value* acc = args[1];
        llvm::Value* closure = args[2];
        llvm::BasicBlock* loopCond = llvm::BasicBlock::Create(context_, "reduce_cond", currentFunction_);
        llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(context_, "reduce_body", currentFunction_);
        llvm::BasicBlock* loopEnd = llvm::BasicBlock::Create(context_, "reduce_end", currentFunction_);
        llvm::Value* baseTyped = builder_->CreateBitCast(basePtr, llvm::PointerType::get(context_, 0), "base");
        llvm::BasicBlock* reduceEntry = builder_->GetInsertBlock();
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopCond);
        llvm::PHINode* iPhi = builder_->CreatePHI(i64, 2, "i");
        llvm::PHINode* accPhi = builder_->CreatePHI(retLlvm, 2, "acc");
        iPhi->addIncoming(builder_->getInt64(0), reduceEntry);
        accPhi->addIncoming(acc, reduceEntry);
        llvm::Value* cmp = builder_->CreateICmpSLT(iPhi, lenVal, "cmp");
        builder_->CreateCondBr(cmp, loopBody, loopEnd);
        builder_->SetInsertPoint(loopBody);
        llvm::Value* elemPtr = builder_->CreateGEP(elemLlvm, baseTyped, iPhi, "elem_ptr");
        llvm::Value* cur = builder_->CreateLoad(elemLlvm, elemPtr, "cur");
        std::vector<llvm::Value*> closureArgs = {accPhi, cur};
        llvm::Value* newAcc = invokeClosure(closure, closureArgs, retLlvm);
        if (!newAcc) return nullptr;
        llvm::Value* iNext = builder_->CreateAdd(iPhi, builder_->getInt64(1), "i_next");
        llvm::BasicBlock* loopBack = builder_->GetInsertBlock();
        iPhi->addIncoming(iNext, loopBack);
        accPhi->addIncoming(newAcc, loopBack);
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopEnd);
        return accPhi;
    }

    // reduceRight<T,U>(a, init, f: (cur:T, acc:U)->U) -> U  (foldRight)
    if (funcName == "reduceRight" && args.size() == 3) {
        const auto& redInferred = expr->getInferredTypeArgs();
        if (redInferred.size() < 2 || !redInferred[0] || !redInferred[1]) return nullptr;
        ast::Type* elemType = redInferred[0].get();
        ast::Type* retType = redInferred[1].get();
        if (currentTypeSubst_) {
            auto sub = substituteType(elemType, *currentTypeSubst_);
            if (sub) elemType = sub.get();
            sub = substituteType(retType, *currentTypeSubst_);
            if (sub) retType = sub.get();
        }
        llvm::Type* elemLlvm = convertType(elemType);
        llvm::Type* retLlvm = convertType(retType);
        if (!elemLlvm || !retLlvm) return nullptr;
        auto [basePtr, lenVal] = getArrayBaseAndLength(args[0]);
        llvm::Value* acc = args[1];
        llvm::Value* closure = args[2];
        llvm::BasicBlock* loopCond = llvm::BasicBlock::Create(context_, "reduceR_cond", currentFunction_);
        llvm::BasicBlock* loopBody = llvm::BasicBlock::Create(context_, "reduceR_body", currentFunction_);
        llvm::BasicBlock* loopEnd = llvm::BasicBlock::Create(context_, "reduceR_end", currentFunction_);
        llvm::Value* baseTyped = builder_->CreateBitCast(basePtr, llvm::PointerType::get(context_, 0), "base");
        llvm::Value* startIndex = builder_->CreateSub(lenVal, builder_->getInt64(1), "start");
        llvm::BasicBlock* reduceREntry = builder_->GetInsertBlock();
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopCond);
        llvm::PHINode* iPhi = builder_->CreatePHI(i64, 2, "i");
        llvm::PHINode* accPhi = builder_->CreatePHI(retLlvm, 2, "acc");
        iPhi->addIncoming(startIndex, reduceREntry);
        accPhi->addIncoming(acc, reduceREntry);
        llvm::Value* cmp = builder_->CreateICmpSGE(iPhi, builder_->getInt64(0), "cmp");
        builder_->CreateCondBr(cmp, loopBody, loopEnd);
        builder_->SetInsertPoint(loopBody);
        llvm::Value* elemPtr = builder_->CreateGEP(elemLlvm, baseTyped, iPhi, "elem_ptr");
        llvm::Value* cur = builder_->CreateLoad(elemLlvm, elemPtr, "cur");
        std::vector<llvm::Value*> closureArgs = {cur, accPhi};
        llvm::Value* newAcc = invokeClosure(closure, closureArgs, retLlvm);
        if (!newAcc) return nullptr;
        llvm::Value* iNext = builder_->CreateSub(iPhi, builder_->getInt64(1), "i_prev");
        llvm::BasicBlock* loopBack = builder_->GetInsertBlock();
        iPhi->addIncoming(iNext, loopBack);
        accPhi->addIncoming(newAcc, loopBack);
        builder_->CreateBr(loopCond);
        builder_->SetInsertPoint(loopEnd);
        return accPhi;
    }

    // filter<T>(a: Array<T>, p: (item: T) -> Bool) -> Array<T>
    if (funcName == "filter" && args.size() == 2) {
        const auto& filterInferred = expr->getInferredTypeArgs();
        if (filterInferred.empty() || !filterInferred[0]) return nullptr;
        ast::Type* elemType = filterInferred[0].get();
        if (currentTypeSubst_) {
            auto sub = substituteType(elemType, *currentTypeSubst_);
            if (sub) elemType = sub.get();
        }
        llvm::Type* elemLlvm = convertType(elemType);
        llvm::Type* i1 = llvm::Type::getInt1Ty(context_);
        if (!elemLlvm) return nullptr;
        auto [basePtr, lenVal] = getArrayBaseAndLength(args[0]);
        llvm::Value* closure = args[1];
        llvm::Value* baseTyped = builder_->CreateBitCast(basePtr, llvm::PointerType::get(context_, 0), "base");
        // Pass 1: count elements where p(item) is true
        llvm::AllocaInst* countAlloca = builder_->CreateAlloca(i64, nullptr, "filter_count");
        builder_->CreateStore(builder_->getInt64(0), countAlloca);
        llvm::BasicBlock* countCond = llvm::BasicBlock::Create(context_, "filter_count_cond", currentFunction_);
        llvm::BasicBlock* countBody = llvm::BasicBlock::Create(context_, "filter_count_body", currentFunction_);
        llvm::BasicBlock* countEnd = llvm::BasicBlock::Create(context_, "filter_count_end", currentFunction_);
        llvm::BasicBlock* countEntry = builder_->GetInsertBlock();
        builder_->CreateBr(countCond);
        builder_->SetInsertPoint(countCond);
        llvm::PHINode* iCountPhi = builder_->CreatePHI(i64, 2, "i_count");
        llvm::PHINode* countPhi = builder_->CreatePHI(i64, 2, "count");
        iCountPhi->addIncoming(builder_->getInt64(0), countEntry);
        countPhi->addIncoming(builder_->getInt64(0), countEntry);
        llvm::Value* cmpCount = builder_->CreateICmpSLT(iCountPhi, lenVal, "cmp_count");
        builder_->CreateCondBr(cmpCount, countBody, countEnd);
        builder_->SetInsertPoint(countBody);
        llvm::Value* itemPtr = builder_->CreateGEP(elemLlvm, baseTyped, iCountPhi, "item_ptr");
        llvm::Value* item = builder_->CreateLoad(elemLlvm, itemPtr, "item");
        llvm::Value* pred = invokeClosure(closure, {item}, i1);
        if (!pred) return nullptr;
        llvm::Value* predExt = builder_->CreateZExt(pred, i64, "pred_ext");
        llvm::Value* newCount = builder_->CreateAdd(countPhi, predExt, "new_count");
        llvm::Value* iCountNext = builder_->CreateAdd(iCountPhi, builder_->getInt64(1), "i_count_next");
        llvm::BasicBlock* countBack = builder_->GetInsertBlock();
        iCountPhi->addIncoming(iCountNext, countBack);
        countPhi->addIncoming(newCount, countBack);
        builder_->CreateBr(countCond);
        builder_->SetInsertPoint(countEnd);
        builder_->CreateStore(countPhi, countAlloca);
        // Allocate result array of size countPhi
        llvm::AllocaInst* resultAlloca = builder_->CreateAlloca(elemLlvm, countPhi, "filter_result");
        // Pass 2: fill result with elements where p(item) is true
        llvm::AllocaInst* nextAlloca = builder_->CreateAlloca(i64, nullptr, "filter_next");
        builder_->CreateStore(builder_->getInt64(0), nextAlloca);
        llvm::BasicBlock* fillCond = llvm::BasicBlock::Create(context_, "filter_fill_cond", currentFunction_);
        llvm::BasicBlock* fillBody = llvm::BasicBlock::Create(context_, "filter_fill_body", currentFunction_);
        llvm::BasicBlock* fillEnd = llvm::BasicBlock::Create(context_, "filter_fill_end", currentFunction_);
        llvm::BasicBlock* fillEntry = builder_->GetInsertBlock();
        builder_->CreateBr(fillCond);
        builder_->SetInsertPoint(fillCond);
        llvm::PHINode* iFillPhi = builder_->CreatePHI(i64, 2, "i_fill");
        iFillPhi->addIncoming(builder_->getInt64(0), fillEntry);
        llvm::Value* cmpFill = builder_->CreateICmpSLT(iFillPhi, lenVal, "cmp_fill");
        builder_->CreateCondBr(cmpFill, fillBody, fillEnd);
        builder_->SetInsertPoint(fillBody);
        llvm::Value* itemPtr2 = builder_->CreateGEP(elemLlvm, baseTyped, iFillPhi, "item_ptr2");
        llvm::Value* item2 = builder_->CreateLoad(elemLlvm, itemPtr2, "item2");
        llvm::Value* pred2 = invokeClosure(closure, {item2}, i1);
        if (!pred2) return nullptr;
        llvm::BasicBlock* storeBB = llvm::BasicBlock::Create(context_, "filter_store", currentFunction_);
        llvm::BasicBlock* skipBB = llvm::BasicBlock::Create(context_, "filter_skip", currentFunction_);
        builder_->CreateCondBr(pred2, storeBB, skipBB);
        builder_->SetInsertPoint(storeBB);
        llvm::Value* nextVal = builder_->CreateLoad(i64, nextAlloca, "next");
        llvm::Value* dstPtr = builder_->CreateGEP(elemLlvm, resultAlloca, nextVal, "dst_ptr");
        builder_->CreateStore(item2, dstPtr);
        builder_->CreateStore(builder_->CreateAdd(nextVal, builder_->getInt64(1), "next_inc"), nextAlloca);
        builder_->CreateBr(skipBB);
        builder_->SetInsertPoint(skipBB);
        llvm::Value* iFillNext = builder_->CreateAdd(iFillPhi, builder_->getInt64(1), "i_fill_next");
        iFillPhi->addIncoming(iFillNext, skipBB);
        builder_->CreateBr(fillCond);
        builder_->SetInsertPoint(fillEnd);
        ArrayMetadata meta;
        meta.elementType = elemLlvm;
        meta.size = 0;
        meta.alloca = resultAlloca;
        meta.lengthAlloca = countAlloca;
        arrayMetadata_[resultAlloca] = meta;
        arrayMetadata_[builder_->CreateBitCast(resultAlloca, i8ptr)] = meta;
        return builder_->CreateBitCast(resultAlloca, i8ptr, "filter_arr");
    }

    // toString(x) dispatch: rewrite to type-specific function or identity (String)
    if (funcName == "toString" && args.size() == 1) {
        const auto& inferred = expr->getInferredTypeArgs();
        if (!inferred.empty() && inferred[0]) {
            ast::Type* argType = inferred[0].get();
            if (currentTypeSubst_) {
                auto sub = substituteType(argType, *currentTypeSubst_);
                if (sub) argType = sub.get();
            }
            if (auto* prim = dynamic_cast<ast::PrimitiveType*>(argType)) {
                switch (prim->getKind()) {
                    case ast::PrimitiveType::Kind::Int:    funcName = "intToString"; break;
                    case ast::PrimitiveType::Kind::Float: funcName = "floatToString"; break;
                    case ast::PrimitiveType::Kind::Bool:  funcName = "boolToString"; break;
                    case ast::PrimitiveType::Kind::String: return args[0];  // identity
                    case ast::PrimitiveType::Kind::Unit:  funcName = "unitToString"; break;
                    case ast::PrimitiveType::Kind::ArrayBuf: funcName = "arrayBufToString"; break;
                    default: break;
                }
            } else if (auto* gen = dynamic_cast<ast::GenericType*>(argType)) {
                funcName = "toString_" + gen->getName();
            } else if (auto* rec = dynamic_cast<ast::RecordType*>(argType)) {
                // Inferred type may be RecordType (e.g. for let p: Point = {...}); find nominal type name
                for (const auto& [name, declType] : typeDeclsMap_) {
                    auto* declRec = dynamic_cast<ast::RecordType*>(declType);
                    if (!declRec || declRec->getFields().size() != rec->getFields().size()) continue;
                    bool match = true;
                    for (size_t i = 0; i < rec->getFields().size(); ++i) {
                        if (rec->getFields()[i]->getName() != declRec->getFields()[i]->getName()) {
                            match = false;
                            break;
                        }
                    }
                    if (match) {
                        funcName = "toString_" + name;
                        break;
                    }
                }
            }
        }
    }

    return evaluateCallWithValues(funcName, args, &expr->getInferredTypeArgs(), &expr->getArgs(), expr->getLocation(), tailCall);
}

void IRGenerator::generateStatement(ast::Stmt* stmt) {
    if (!stmt) {
        return;
    }
    
    if (auto* varDecl = dynamic_cast<ast::VariableDecl*>(stmt)) {
        generateVariableDecl(varDecl);
    } else if (auto* returnStmt = dynamic_cast<ast::ReturnStmt*>(stmt)) {
        generateReturnStmt(returnStmt);
    } else if (auto* exprStmt = dynamic_cast<ast::ExprStmt*>(stmt)) {
        generateExprStmt(exprStmt);
    } else if (auto* ifStmt = dynamic_cast<ast::IfStmt*>(stmt)) {
        generateIfStmt(ifStmt);
    } else if (auto* assignStmt = dynamic_cast<ast::AssignmentStmt*>(stmt)) {
        generateAssignmentStmt(assignStmt);
    } else if (auto* forInStmt = dynamic_cast<ast::ForInStmt*>(stmt)) {
        generateForInStmt(forInStmt);
    } else if (auto* selectStmt = dynamic_cast<ast::SelectStmt*>(stmt)) {
        generateSelectStmt(selectStmt);
    }
}

void IRGenerator::generateVariableDecl(ast::VariableDecl* stmt) {
    // Evaluate initializer expression (if present)
    llvm::Value* initValue = nullptr;
    if (stmt->getInitializer()) {
        initValue = evaluateExpr(stmt->getInitializer());
        if (!initValue) {
            errorReporter_.error(
                stmt->getLocation(),
                "Failed to evaluate variable initializer"
            );
            return;
        }
    } else {
        // No initializer - use default value based on type
        // This is mainly for mutable variables that can be assigned later
        if (stmt->getType()) {
            llvm::Type* varType = convertType(stmt->getType());
            if (varType) {
                if (varType->isIntegerTy(64)) {
                    initValue = llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
                } else if (varType->isDoubleTy()) {
                    initValue = llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
                } else if (varType->isIntegerTy(1)) {
                    initValue = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
                } else {
                    errorReporter_.error(
                        stmt->getLocation(),
                        "Variable declaration missing initializer"
                    );
                    return;
                }
            } else {
                errorReporter_.error(
                    stmt->getLocation(),
                    "Variable declaration missing initializer and type"
                );
                return;
            }
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "Variable declaration missing initializer"
            );
            return;
        }
    }
    
    // Allocate space for variable (alloca instruction)
    llvm::Type* varType = initValue->getType();
    llvm::AllocaInst* alloca = builder_->CreateAlloca(varType, nullptr, stmt->getName());
    
    // Store initial value
    builder_->CreateStore(initValue, alloca);
    
    // Register variable in symbol table (store the alloca, not the value)
    // When accessing, we'll load from the alloca
    // Track the type explicitly for proper loading with opaque pointers
    setVariable(stmt->getName(), alloca, varType);
    
    // If this is an array variable, preserve metadata so indexing via this variable
    // can find the original array storage. Key by variable's alloca; keep metadata.alloca
    // as the original array alloca (do not overwrite with variable's alloca).
    auto arrayIt = arrayMetadata_.find(initValue);
    if (arrayIt != arrayMetadata_.end()) {
        ArrayMetadata metadata = arrayIt->second;
        arrayMetadata_[alloca] = metadata;
    }
    
    // If this is a record variable, preserve metadata so field access and match find the struct.
    // Key by variable's alloca; keep metadata.alloca as the original struct alloca (initValue
    // is the record ptr from the literal, and we store that in the variable's alloca).
    auto recordIt = recordMetadata_.find(initValue);
    if (recordIt != recordMetadata_.end()) {
        RecordMetadata metadata = recordIt->second;
        recordMetadata_[alloca] = metadata;
        llvm::Value* castPtr = builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0), stmt->getName() + "_ptr");
        recordMetadata_[castPtr] = metadata;
    }
}

void IRGenerator::generateReturnStmt(ast::ReturnStmt* stmt) {
    ast::Expr* returnExpr = stmt->getValue();
    if (returnExpr) {
        // Phase 6.2: Tail call optimization - emit tail call when return value is a direct call
        llvm::Value* returnValue = nullptr;
        if (auto* callExpr = dynamic_cast<ast::FunctionCallExpr*>(returnExpr)) {
            returnValue = evaluateFunctionCall(callExpr, /*tailCall=*/ true);
            // If tail path returned null (e.g. constructor "Cons" not in map), try normal eval (ConstructorExpr path)
            if (!returnValue && getConstructorIndex(callExpr->getName()).first)
                returnValue = evaluateExpr(returnExpr);
        } else {
            // Mark tail context so calls inside match/if/block (e.g. case => f()) are emitted as tail calls
            bool savedTailContext = inTailContext_;
            inTailContext_ = true;
            returnValue = evaluateExpr(returnExpr);
            inTailContext_ = savedTailContext;
        }
        if (returnValue) {
            builder_->CreateRet(returnValue);
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "Failed to evaluate return expression"
            );
        }
    } else {
        // Void return (no value)
        builder_->CreateRetVoid();
    }
}

void IRGenerator::generateExprStmt(ast::ExprStmt* stmt) {
    // Evaluate expression but discard result
    evaluateExpr(stmt->getExpr());
}

void IRGenerator::visitIfStmt(ast::IfStmt* node) {
    generateIfStmt(node);
}

void IRGenerator::generateIfStmt(ast::IfStmt* stmt) {
    if (!stmt || !currentFunction_) {
        return;
    }
    
    // Evaluate condition
    llvm::Value* condValue = evaluateExpr(stmt->getCondition());
    if (!condValue) {
        return;
    }
    
    // Convert condition to boolean if needed
    llvm::Type* condType = condValue->getType();
    if (!condType->isIntegerTy(1)) {
        // Convert to boolean: compare with zero/false
        if (condType->isIntegerTy()) {
            condValue = builder_->CreateICmpNE(
                condValue,
                llvm::ConstantInt::get(condType, 0),
                "ifcond"
            );
        } else if (condType->isFloatingPointTy()) {
            condValue = builder_->CreateFCmpONE(
                condValue,
                llvm::ConstantFP::get(condType, 0.0),
                "ifcond"
            );
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "If condition must be a boolean, integer, or floating-point value"
            );
            return;
        }
    }
    
    // Get current function and basic blocks
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context_, "then", func);
    llvm::BasicBlock* elseBB = stmt->hasElse() 
        ? llvm::BasicBlock::Create(context_, "else", func)
        : nullptr;
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "ifcont", func);
    
    // Create conditional branch
    if (elseBB) {
        builder_->CreateCondBr(condValue, thenBB, elseBB);
    } else {
        builder_->CreateCondBr(condValue, thenBB, mergeBB);
    }
    
    // Generate then branch
    builder_->SetInsertPoint(thenBB);
    generateStatement(stmt->getThenBranch());
    // If then branch doesn't terminate (no return), branch to merge
    if (!thenBB->getTerminator()) {
        builder_->CreateBr(mergeBB);
    }
    
    // Generate else branch if present
    if (elseBB) {
        builder_->SetInsertPoint(elseBB);
        generateStatement(stmt->getElseBranch());
        // If else branch doesn't terminate, branch to merge
        if (!elseBB->getTerminator()) {
            builder_->CreateBr(mergeBB);
        }
    }
    
    // Set insert point to merge block
    builder_->SetInsertPoint(mergeBB);
}

llvm::Value* IRGenerator::evaluateShortCircuitAnd(ast::BinaryExpr* expr) {
    if (!expr || !currentFunction_) {
        return nullptr;
    }
    
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Create basic blocks
    llvm::BasicBlock* rightBB = llvm::BasicBlock::Create(context_, "andright", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "andmerge", func);
    
    // Evaluate left operand
    llvm::Value* left = evaluateExpr(expr->getLeft());
    if (!left) {
        return nullptr;
    }
    
    // Convert left to boolean if needed
    llvm::Type* leftType = left->getType();
    if (!leftType->isIntegerTy(1)) {
        if (leftType->isIntegerTy()) {
            left = builder_->CreateICmpNE(
                left,
                llvm::ConstantInt::get(leftType, 0),
                "andleftbool"
            );
        } else if (leftType->isFloatingPointTy()) {
            left = builder_->CreateFCmpONE(
                left,
                llvm::ConstantFP::get(leftType, 0.0),
                "andleftbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical AND requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    // Branch: if left is false, go to merge (result is false)
    // Otherwise, evaluate right operand
    builder_->CreateCondBr(left, rightBB, mergeBB);
    
    // Evaluate right operand
    builder_->SetInsertPoint(rightBB);
    llvm::Value* right = evaluateExpr(expr->getRight());
    if (!right) {
        return nullptr;
    }
    
    // Convert right to boolean if needed
    llvm::Type* rightType = right->getType();
    if (!rightType->isIntegerTy(1)) {
        if (rightType->isIntegerTy()) {
            right = builder_->CreateICmpNE(
                right,
                llvm::ConstantInt::get(rightType, 0),
                "andrightbool"
            );
        } else if (rightType->isFloatingPointTy()) {
            right = builder_->CreateFCmpONE(
                right,
                llvm::ConstantFP::get(rightType, 0.0),
                "andrightbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical AND requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    builder_->CreateBr(mergeBB);
    
    // Merge block: phi node selects result
    builder_->SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder_->CreatePHI(llvm::Type::getInt1Ty(context_), 2, "andtmp");
    phi->addIncoming(llvm::ConstantInt::getFalse(context_), currentBB);
    phi->addIncoming(right, rightBB);
    
    return phi;
}

llvm::Value* IRGenerator::evaluateShortCircuitOr(ast::BinaryExpr* expr) {
    if (!expr || !currentFunction_) {
        return nullptr;
    }
    
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Create basic blocks
    llvm::BasicBlock* rightBB = llvm::BasicBlock::Create(context_, "orright", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "ormerge", func);
    
    // Evaluate left operand
    llvm::Value* left = evaluateExpr(expr->getLeft());
    if (!left) {
        return nullptr;
    }
    
    // Convert left to boolean if needed
    llvm::Type* leftType = left->getType();
    if (!leftType->isIntegerTy(1)) {
        if (leftType->isIntegerTy()) {
            left = builder_->CreateICmpNE(
                left,
                llvm::ConstantInt::get(leftType, 0),
                "orleftbool"
            );
        } else if (leftType->isFloatingPointTy()) {
            left = builder_->CreateFCmpONE(
                left,
                llvm::ConstantFP::get(leftType, 0.0),
                "orleftbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical OR requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    // Branch: if left is true, go to merge (result is true)
    // Otherwise, evaluate right operand
    builder_->CreateCondBr(left, mergeBB, rightBB);
    
    // Evaluate right operand
    builder_->SetInsertPoint(rightBB);
    llvm::Value* right = evaluateExpr(expr->getRight());
    if (!right) {
        return nullptr;
    }
    
    // Convert right to boolean if needed
    llvm::Type* rightType = right->getType();
    if (!rightType->isIntegerTy(1)) {
        if (rightType->isIntegerTy()) {
            right = builder_->CreateICmpNE(
                right,
                llvm::ConstantInt::get(rightType, 0),
                "orrightbool"
            );
        } else if (rightType->isFloatingPointTy()) {
            right = builder_->CreateFCmpONE(
                right,
                llvm::ConstantFP::get(rightType, 0.0),
                "orrightbool"
            );
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Logical OR requires boolean, integer, or floating-point operands"
            );
            return nullptr;
        }
    }
    
    builder_->CreateBr(mergeBB);
    
    // Merge block: phi node selects result
    builder_->SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder_->CreatePHI(llvm::Type::getInt1Ty(context_), 2, "ortmp");
    phi->addIncoming(left, currentBB);
    phi->addIncoming(right, rightBB);
    
    return phi;
}

void IRGenerator::visitAssignmentStmt(ast::AssignmentStmt* node) {
    generateAssignmentStmt(node);
}

llvm::Value* IRGenerator::evaluateRecordLiteral(ast::RecordLiteralExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    const auto& fields = expr->getFields();
    if (fields.empty()) {
        errorReporter_.error(
            expr->getLocation(),
            "Empty record literals require explicit type annotation"
        );
        return nullptr;
    }
    
    // Evaluate all field values
    std::vector<llvm::Value*> fieldValues;
    std::vector<llvm::Type*> fieldTypes;
    std::vector<std::string> fieldNames;
    
    for (const auto& field : fields) {
        llvm::Value* fieldValue = evaluateExpr(field.value.get());
        if (!fieldValue) {
            return nullptr;
        }
        fieldValues.push_back(fieldValue);
        fieldTypes.push_back(fieldValue->getType());
        fieldNames.push_back(field.name);
    }
    
    // Create struct type from field types
    llvm::StructType* structType = createStructTypeSafe(context_, fieldTypes);
    
    // Allocate struct on stack
    llvm::AllocaInst* structAlloca = builder_->CreateAlloca(structType, nullptr, "rectmp");
    
    // Store each field value
    for (size_t i = 0; i < fields.size(); ++i) {
        llvm::Value* fieldPtr = builder_->CreateStructGEP(structType, structAlloca, static_cast<unsigned>(i), "recfield");
        
        // Store field value
        builder_->CreateStore(fieldValues[i], fieldPtr);
    }
    
    // Store metadata for this record
    RecordMetadata metadata;
    metadata.structType = structType;
    metadata.fieldNames = fieldNames;
    metadata.alloca = structAlloca;
    
    // Return pointer to struct (cast to i8* for compatibility)
    llvm::Value* recordPtr = builder_->CreateBitCast(
        structAlloca,
        llvm::PointerType::get(context_, 0),
        "recptr"
    );
    
    // Store metadata for later field access
    recordMetadata_[recordPtr] = metadata;
    recordMetadata_[structAlloca] = metadata; // Also key by alloca
    
    return recordPtr;
}

llvm::Value* IRGenerator::evaluateFieldAccess(ast::FieldAccessExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Evaluate record expression
    llvm::Value* recordValue = evaluateExpr(expr->getRecord());
    if (!recordValue) {
        return nullptr;
    }
    
    const std::string& fieldName = expr->getFieldName();
    
    // Look up record metadata
    auto it = recordMetadata_.find(recordValue);
    bool recordIsIndirect = false;  // true when variable holds pointer to record (e.g. ADT payload)
    
    // If not found, try to find it by checking local variables
    if (it == recordMetadata_.end()) {
        for (const auto& varPair : localVars_) {
            llvm::Value* varAlloca = varPair.second;
            auto varIt = recordMetadata_.find(varAlloca);
            if (varIt != recordMetadata_.end()) {
                it = varIt;
                recordIsIndirect = true;  // variable holds pointer; record is at recordValue
                break;
            }
        }
    }
    
    if (it == recordMetadata_.end()) {
        errorReporter_.error(
            expr->getLocation(),
            "Field access: record type information not available"
        );
        return nullptr;
    }
    
    RecordMetadata& metadata = it->second;
    llvm::StructType* structType = metadata.structType;
    const std::vector<std::string>& fieldNames = metadata.fieldNames;
    llvm::AllocaInst* recordAlloca = metadata.alloca;
    
    // Find field index by name
    size_t fieldIndex = fieldNames.size();
    for (size_t i = 0; i < fieldNames.size(); ++i) {
        if (fieldNames[i] == fieldName) {
            fieldIndex = i;
            break;
        }
    }
    
    if (fieldIndex >= fieldNames.size()) {
        // Single-constructor ADT with one field (e.g. Operator(op: String)): allow any name for the single field
        if (fieldNames.size() == 1) {
            fieldIndex = 0;
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "Field '" + fieldName + "' not found in record"
            );
            return nullptr;
        }
    }
    
    // In LLVM opaque-pointer mode, use recordAlloca or the loaded recordValue as base.
    // When recordIsIndirect, the variable holds a pointer to the record (e.g. ADT payload).
    llvm::Value* typedPtr = recordIsIndirect
        ? builder_->CreateBitCast(recordValue, llvm::PointerType::get(context_, 0), "recbase")
        : recordAlloca;
    
    llvm::Value* fieldPtr = builder_->CreateStructGEP(structType, typedPtr, static_cast<unsigned>(fieldIndex), "recfield");
    
    // Get field type
    llvm::Type* fieldType = structType->getElementType(fieldIndex);
    
    // Load field value
    return builder_->CreateAlignedLoad(fieldType, fieldPtr, llvm::Align(8), "recfieldval");
}

llvm::Value* IRGenerator::evaluateConstructor(ast::ConstructorExpr* expr) {
    if (!expr) {
        return nullptr;
    }

    const std::string& constructorName = expr->getConstructorName();
    const auto& arguments = expr->getArguments();

    std::vector<llvm::Value*> argValues;
    for (const auto& arg : arguments) {
        llvm::Value* argValue = evaluateExpr(arg.get());
        if (!argValue) return nullptr;
        argValues.push_back(argValue);
    }

    auto [adtAstType, tagIndex] = getConstructorIndex(constructorName);
    (void)adtAstType;

    llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
    llvm::Type* payloadPtrType = llvm::PointerType::get(context_, 0);
    const llvm::DataLayout& DL = module_->getDataLayout();
    llvm::StructType* fullType = nullptr;
    llvm::Value* rawPtr = nullptr;
    if (!argValues.empty()) {
        std::vector<llvm::Type*> fieldTypes;
        for (llvm::Value* v : argValues) fieldTypes.push_back(v->getType());
        llvm::StructType* payloadStructType = createStructTypeSafe(context_, fieldTypes);
        fullType = createStructTypeSafe(context_, {tagType, payloadStructType});
        uint64_t fullSize = DL.getTypeAllocSize(fullType);
        rawPtr = builder_->CreateCall(
            getOrCreateFirstAlloc(),
            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context_), fullSize)},
            "adtheap"
        );
        llvm::Value* structPtr = builder_->CreateBitCast(rawPtr, llvm::PointerType::get(context_, 0), "adtstruct");
        llvm::Value* tagGep = builder_->CreateStructGEP(fullType, structPtr, 0, "tagptr");
        builder_->CreateAlignedStore(
            llvm::ConstantInt::get(context_, llvm::APInt(64, tagIndex)),
            tagGep, llvm::Align(8));
        llvm::Value* payloadPtr = builder_->CreateStructGEP(fullType, structPtr, 1, "payloadptr");
        for (size_t i = 0; i < argValues.size(); ++i) {
            llvm::Value* toStore = argValues[i];
            llvm::Value* fieldPtr = builder_->CreateStructGEP(payloadStructType, payloadPtr, static_cast<unsigned>(i), "fieldptr");
            builder_->CreateAlignedStore(toStore, fieldPtr, llvm::Align(8));
        }
    } else {
        fullType = createStructTypeSafe(context_, std::vector<llvm::Type*>{tagType});
        uint64_t fullSize = DL.getTypeAllocSize(fullType);
        rawPtr = builder_->CreateCall(
            getOrCreateFirstAlloc(),
            {llvm::ConstantInt::get(llvm::Type::getInt64Ty(context_), fullSize)},
            "adtheap"
        );
        llvm::Value* structPtr = builder_->CreateBitCast(rawPtr, llvm::PointerType::get(context_, 0), "adtstruct");
        builder_->CreateAlignedStore(
            llvm::ConstantInt::get(context_, llvm::APInt(64, tagIndex)),
            builder_->CreateStructGEP(fullType, structPtr, 0, "tagptr"), llvm::Align(8));
    }
    return builder_->CreateBitCast(rawPtr, payloadPtrType, "adtptr");
}

llvm::Value* IRGenerator::evaluateMatch(ast::MatchExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Pattern matching IR generation:
    // 1. Evaluate the matched expression
    // 2. For each case, check if pattern matches
    // 3. If match, bind pattern variables and evaluate case body
    // 4. Use PHI nodes to merge results from all cases
    llvm::Value* matchedValue = evaluateExpr(expr->getMatchedExpr());
    if (!matchedValue) {
        return nullptr;
    }
    // Use concrete scrutinee type when in monomorphized body (e.g. Option<Int> not Option<A>)
    ast::Type* scrutineeType = expr->getScrutineeType();
    std::unique_ptr<ast::Type> scrutineeSubstituted;
    if (currentTypeSubst_ && scrutineeType) {
        scrutineeSubstituted = substituteType(scrutineeType, *currentTypeSubst_);
        if (scrutineeSubstituted) scrutineeType = scrutineeSubstituted.get();
    }
    // ADT parameters may be passed by value (struct); constructor pattern matching
    // expects a pointer (to load tag and payload). Store in an alloca if needed.
    if (!matchedValue->getType()->isPointerTy() && matchedValue->getType()->isStructTy()) {
        llvm::AllocaInst* adtAlloca = builder_->CreateAlloca(
            matchedValue->getType(), nullptr, "match_scrutinee");
        builder_->CreateAlignedStore(matchedValue, adtAlloca, llvm::Align(8));
        matchedValue = builder_->CreateBitCast(
            adtAlloca,
            llvm::PointerType::get(context_, 0),
            "match_scrutinee_ptr");
    }
    
    const auto& cases = expr->getCases();
    if (cases.empty()) {
        errorReporter_.error(
            expr->getLocation(),
            "Match expression must have at least one case"
        );
        return nullptr;
    }
    
    // Get current basic block
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Create basic blocks for each case and a merge block
    std::vector<llvm::BasicBlock*> caseBlocks;
    std::vector<llvm::BasicBlock*> matchBlocks; // Blocks where pattern matches
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "matchmerge", currentFunction_);
    
    // Create blocks for each case
    for (size_t i = 0; i < cases.size(); ++i) {
        caseBlocks.push_back(llvm::BasicBlock::Create(context_, "case" + std::to_string(i), currentFunction_));
        matchBlocks.push_back(llvm::BasicBlock::Create(context_, "match" + std::to_string(i), currentFunction_));
    }
    
    // Create a default block for when no pattern matches
    llvm::BasicBlock* defaultBB = llvm::BasicBlock::Create(context_, "matchdefault", currentFunction_);

    // If the match includes an explicit catch-all (wildcard/variable) case without a guard,
    // then reaching the default block should be impossible. In that scenario, we should not
    // report a compile-time error for non-exhaustiveness.
    std::function<bool(ast::Pattern*)> isCatchAll = [&](ast::Pattern* p) -> bool {
        if (!p) return false;
        if (dynamic_cast<ast::WildcardPattern*>(p) || dynamic_cast<ast::VariablePattern*>(p)) return true;
        if (auto* ap = dynamic_cast<ast::AsPattern*>(p)) return isCatchAll(ap->getPattern());
        return false;
    };
    bool hasExplicitCatchAll = false;
    for (const auto& c : cases) {
        if (!c) continue;
        if (!c->hasGuard() && isCatchAll(c->getPattern())) {
            hasExplicitCatchAll = true;
            break;
        }
    }
    // Record types: a RecordPattern with variable sub-patterns matches any record; treat as exhaustive
    if (!hasExplicitCatchAll && matchedValue->getType()->isPointerTy()) {
        for (const auto& c : cases) {
            if (c && dynamic_cast<ast::RecordPattern*>(c->getPattern())) {
                hasExplicitCatchAll = true;
                break;
            }
        }
    }
    // ADT: all cases are constructor patterns — type checker already ensured exhaustiveness
    if (!hasExplicitCatchAll) {
        bool allConstructorPatterns = true;
        for (const auto& c : cases) {
            if (!c || !dynamic_cast<ast::ConstructorPattern*>(c->getPattern())) {
                allConstructorPatterns = false;
                break;
            }
        }
        if (allConstructorPatterns && !cases.empty()) {
            hasExplicitCatchAll = true;
        }
    }
    
    // Start with first case
    builder_->CreateBr(caseBlocks[0]);
    
    // Process each case
    std::vector<llvm::Value*> caseResults;
    std::vector<llvm::BasicBlock*> resultBlocks; // Blocks where case results are generated
    for (size_t i = 0; i < cases.size(); ++i) {
        builder_->SetInsertPoint(caseBlocks[i]);
        
        // Determine next block (next case or default)
        llvm::BasicBlock* nextCaseBB = (i < cases.size() - 1) ? caseBlocks[i + 1] : defaultBB;
        
        // Check if pattern matches - this will branch to matchBB or nextCaseBB
        generatePatternMatch(cases[i]->getPattern(), matchedValue, matchBlocks[i], nextCaseBB);
        
        // Pattern matched - check guard if present
        builder_->SetInsertPoint(matchBlocks[i]);
        
        // Bind pattern variables (for variable patterns, constructor patterns, etc.)
        bindPatternVariables(cases[i]->getPattern(), matchedValue, scrutineeType);
        
        // Check guard condition if present
        llvm::BasicBlock* bodyBB = matchBlocks[i];
        if (cases[i]->hasGuard()) {
            llvm::BasicBlock* guardPassBB = llvm::BasicBlock::Create(context_, "guardpass" + std::to_string(i), currentFunction_);
            llvm::BasicBlock* guardFailBB = (i < cases.size() - 1) ? caseBlocks[i + 1] : defaultBB;
            
            llvm::Value* guardValue = evaluateExpr(cases[i]->getGuard());
            if (!guardValue) {
                return nullptr;
            }
            
            // Convert guard to boolean if needed
            if (!guardValue->getType()->isIntegerTy(1)) {
                // Assume non-zero means true
                llvm::Value* zero = llvm::ConstantInt::get(guardValue->getType(), 0);
                guardValue = builder_->CreateICmpNE(guardValue, zero, "guardbool");
            }
            
            builder_->CreateCondBr(guardValue, guardPassBB, guardFailBB);
            bodyBB = guardPassBB;
            builder_->SetInsertPoint(bodyBB);
        }
        
        // Evaluate case body (guard passed or no guard)
        llvm::Value* caseResult = evaluateExpr(cases[i]->getBody());
        if (!caseResult) {
            return nullptr;
        }
        caseResults.push_back(caseResult);
        // Use actual insert block (e.g. inner merge of ||) so PHI predecessors match
        resultBlocks.push_back(builder_->GetInsertBlock());
        builder_->CreateBr(mergeBB);
    }
    
    // Determine result type: use first case, or unify Int | null (i64 + null ptr) as i64
    if (caseResults.empty() || !caseResults[0]) {
        errorReporter_.error(
            expr->getLocation(),
            "Match expression: failed to generate case results"
        );
        return nullptr;
    }
    llvm::Type* resultType = caseResults[0]->getType();
    bool hasI64 = false, hasNullPtr = false;
    for (llvm::Value* v : caseResults) {
        if (v->getType()->isIntegerTy(64)) hasI64 = true;
        if (v->getType()->isPointerTy() && llvm::isa<llvm::ConstantPointerNull>(v)) hasNullPtr = true;
    }
    if (hasI64 && hasNullPtr)
        resultType = llvm::Type::getInt64Ty(context_);
    // LLVM PHI cannot have void type; use i64 0 for Unit/void match results
    if (resultType->isVoidTy())
        resultType = llvm::Type::getInt64Ty(context_);

    // Default case: no pattern matched
    builder_->SetInsertPoint(defaultBB);
    if (!hasExplicitCatchAll) {
        errorReporter_.error(
            expr->getLocation(),
            "Match expression: no pattern matched (non-exhaustive match)"
        );
    }
    llvm::Value* defaultResult = nullptr;
    if (resultType->isIntegerTy(64)) {
        defaultResult = llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
    } else if (resultType->isIntegerTy(1)) {
        defaultResult = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
    } else if (resultType->isDoubleTy()) {
        defaultResult = llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
    } else if (resultType->isPointerTy()) {
        defaultResult = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(resultType));
    } else {
        defaultResult = llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
    }
    caseResults.push_back(defaultResult);
    builder_->CreateBr(mergeBB);
    
    // Merge block: PHI node selects result
    // Normalize so all incomings match resultType: void/ptr (e.g. Unit) -> i64 0 when result is i64
    auto normalizeForPhi = [&](llvm::Value* val) -> llvm::Value* {
        if (!val) return llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
        if (val->getType()->isVoidTy()) return llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
        if (resultType->isIntegerTy(64) && val->getType()->isPointerTy())
            return llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
        if (resultType->isIntegerTy(64) && val->getType() != resultType)
            return llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true));
        return val;
    };

    builder_->SetInsertPoint(mergeBB);
    llvm::PHINode* phi = builder_->CreatePHI(resultType, caseResults.size() + 1, "matchresult");

    // Add incoming values from result blocks (match blocks or guard pass blocks)
    for (size_t i = 0; i < resultBlocks.size(); ++i) {
        llvm::Value* val = caseResults[i];
        if (!val || val->getType()->isVoidTy() || (val->getType() != resultType)) val = normalizeForPhi(caseResults[i]);
        phi->addIncoming(val, resultBlocks[i]);
    }
    // Add default result
    llvm::Value* defVal = (defaultResult->getType() != resultType) ? normalizeForPhi(defaultResult) : defaultResult;
    phi->addIncoming(defVal, defaultBB);

    return phi;
}

llvm::Value* IRGenerator::evaluateBlockExpr(ast::BlockExpr* expr) {
    if (!expr || !currentFunction_) return nullptr;
    for (const auto& stmt : expr->getStatements()) {
        generateStatement(stmt.get());
    }
    if (expr->hasValueExpr()) {
        return evaluateExpr(expr->getValueExpr());
    }
    return llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
}

llvm::Value* IRGenerator::evaluateIfExpr(ast::IfExpr* expr) {
    if (!expr || !currentFunction_) return nullptr;
    ast::Expr* thenBranch = expr->getThenBranch();
    ast::Expr* elseBranch = expr->getElseBranch();
    if (!thenBranch || !elseBranch) {
        errorReporter_.error(
            expr->getLocation(),
            "if expression requires both then and else branches");
        return nullptr;
    }
    llvm::Value* condValue = evaluateExpr(expr->getCondition());
    if (!condValue) return nullptr;
    llvm::Type* condType = condValue->getType();
    if (!condType->isIntegerTy(1)) {
        if (condType->isIntegerTy()) {
            condValue = builder_->CreateICmpNE(
                condValue,
                llvm::ConstantInt::get(condType, 0),
                "ifcond");
        } else if (condType->isFloatingPointTy()) {
            condValue = builder_->CreateFCmpONE(
                condValue,
                llvm::ConstantFP::get(condType, 0.0),
                "ifcond");
        } else {
            errorReporter_.error(
                expr->getLocation(),
                "If condition must be boolean, integer, or floating-point");
            return nullptr;
        }
    }
    llvm::Function* func = currentFunction_;
    llvm::BasicBlock* thenBB = llvm::BasicBlock::Create(context_, "if.then", func);
    llvm::BasicBlock* elseBB = llvm::BasicBlock::Create(context_, "if.else", func);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "if.cont", func);
    builder_->CreateCondBr(condValue, thenBB, elseBB);
    builder_->SetInsertPoint(thenBB);
    llvm::Value* thenVal = evaluateExpr(thenBranch);
    if (!thenVal) return nullptr;
    builder_->CreateBr(mergeBB);
    builder_->SetInsertPoint(elseBB);
    llvm::Value* elseVal = evaluateExpr(elseBranch);
    if (!elseVal) return nullptr;
    // When else branch is another if (else-if), the insert point is the inner merge block,
    // not elseBB. The phi must get the else value from the block that actually flows to mergeBB.
    llvm::BasicBlock* elseSourceBB = builder_->GetInsertBlock();
    if (!elseSourceBB->getTerminator()) {
        builder_->CreateBr(mergeBB);
    }
    builder_->SetInsertPoint(mergeBB);
    llvm::Type* resultType = thenVal->getType();
    if (resultType != elseVal->getType()) {
        errorReporter_.error(
            expr->getLocation(),
            "if expression branches must produce the same type");
        return nullptr;
    }
    llvm::PHINode* phi = builder_->CreatePHI(resultType, 2, "if.result");
    phi->addIncoming(thenVal, thenBB);
    phi->addIncoming(elseVal, elseSourceBB);
    return phi;
}

llvm::Value* IRGenerator::evaluateRangeExpr(ast::RangeExpr* expr) {
    // Range as standalone expression: return nullptr (used only in for-in)
    (void)expr;
    return nullptr;
}

bool IRGenerator::generatePatternMatch(ast::Pattern* pattern, llvm::Value* value,
                                       llvm::BasicBlock* matchBB, llvm::BasicBlock* nextBB) {
    if (!pattern || !value) {
        return false;
    }
    
    // If value is a pointer (i8*), we may need to handle it differently
    // For primitive types, value should be the actual value
    // For ADTs/records/arrays, value might be a pointer
    
    // Dispatch based on pattern type
    if (auto* varPattern = dynamic_cast<ast::VariablePattern*>(pattern)) {
        // "None" and similar are parsed as VariablePattern; if the name is a known constructor, match on tag
        const std::string& name = varPattern->getName();
        auto cit = constructorIndexMap_.find(name);
        if (cit != constructorIndexMap_.end() && value->getType()->isPointerTy()) {
            auto [adtAstType, tagIndex] = cit->second;
            (void)adtAstType;
            llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
            llvm::Value* adtPtr = builder_->CreateBitCast(value, llvm::PointerType::get(context_, 0), "adtptr");
            llvm::Value* isNull = builder_->CreateICmpEQ(
                adtPtr,
                llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(adtPtr->getType())),
                "adt.isnull");
            llvm::BasicBlock* tagCheckBB = llvm::BasicBlock::Create(context_, "tagcheck", currentFunction_);
            builder_->CreateCondBr(isNull, matchBB, tagCheckBB);
            builder_->SetInsertPoint(tagCheckBB);
            std::vector<llvm::Type*> structFields = {tagType, llvm::PointerType::get(context_, 0)};
            llvm::StructType* adtType = createStructTypeSafe(context_, structFields);
            llvm::Value* tagPtr = builder_->CreateStructGEP(adtType, adtPtr, 0, "tagptr");
            llvm::Value* tag = builder_->CreateAlignedLoad(tagType, tagPtr, llvm::Align(8), "tag");
            llvm::Value* tagMatch = builder_->CreateICmpEQ(tag, llvm::ConstantInt::get(context_, llvm::APInt(64, tagIndex)), "tagcmp");
            builder_->CreateCondBr(tagMatch, matchBB, nextBB);
            return true;
        }
        // Variable pattern always matches - bind the variable
        builder_->CreateBr(matchBB);
        return true;
    }
    else if (auto* wildcardPattern = dynamic_cast<ast::WildcardPattern*>(pattern)) {
        // Wildcard pattern always matches
        builder_->CreateBr(matchBB);
        return true;
    }
    else if (auto* literalPattern = dynamic_cast<ast::LiteralPattern*>(pattern)) {
        // Literal pattern: compare value with literal
        llvm::Value* literalValue = evaluateLiteral(literalPattern->getLiteral());
        if (!literalValue) {
            return false;
        }
        
        // Ensure types are compatible for comparison
        llvm::Type* valueType = value->getType();
        llvm::Type* literalType = literalValue->getType();
        
        // Handle type promotion (int to float)
        if (valueType->isIntegerTy(64) && literalType->isDoubleTy()) {
            value = builder_->CreateSIToFP(value, literalType, "valtofloat");
            valueType = literalType;
        } else if (valueType->isDoubleTy() && literalType->isIntegerTy(64)) {
            literalValue = builder_->CreateSIToFP(literalValue, valueType, "littofloat");
            literalType = valueType;
        }
        
        // Compare values
        llvm::Value* comparison = nullptr;
        if (valueType->isIntegerTy() && literalType->isIntegerTy()) {
            comparison = builder_->CreateICmpEQ(value, literalValue, "litcmp");
        } else if (valueType->isFloatingPointTy() && literalType->isFloatingPointTy()) {
            comparison = builder_->CreateFCmpOEQ(value, literalValue, "litcmp");
        } else {
            errorReporter_.error(
                pattern->getLocation(),
                "Pattern matching: cannot compare literal pattern with value type"
            );
            return false;
        }
        
        builder_->CreateCondBr(comparison, matchBB, nextBB);
        return true;
    }
    else if (auto* recordPattern = dynamic_cast<ast::RecordPattern*>(pattern)) {
        // Record pattern: extract specified fields and match subpatterns.
        // This relies on recordMetadata_ (same mechanism as field access).
        // Value may be a BitCast(alloca) when matching on a record variable, or the alloca itself.
        llvm::Value* recordKey = value;
        if (auto* cast = llvm::dyn_cast<llvm::CastInst>(value))
            recordKey = cast->getOperand(0);
        else if (auto* load = llvm::dyn_cast<llvm::LoadInst>(value))
            recordKey = load->getPointerOperand();
        auto it = recordMetadata_.find(recordKey);
        if (it == recordMetadata_.end()) {
            it = recordMetadata_.find(value);
        }
        if (it == recordMetadata_.end()) {
            for (const auto& varPair : localVars_) {
                llvm::Value* varAlloca = varPair.second;
                if (varAlloca == value || varAlloca == recordKey) {
                    auto varIt = recordMetadata_.find(varAlloca);
                    if (varIt != recordMetadata_.end()) {
                        it = varIt;
                        break;
                    }
                }
                if (recordKey != value) {
                    auto* cast = llvm::dyn_cast<llvm::CastInst>(value);
                    if (cast && cast->getOperand(0) == varAlloca) {
                        auto varIt = recordMetadata_.find(varAlloca);
                        if (varIt != recordMetadata_.end()) {
                            it = varIt;
                            break;
                        }
                    }
                }
            }
        }
        if (it == recordMetadata_.end()) {
            errorReporter_.error(pattern->getLocation(),
                                 "Pattern matching: record type information not available");
            builder_->CreateBr(nextBB);
            return false;
        }

        const RecordMetadata& meta = it->second;
        llvm::StructType* structType = meta.structType;
        if (!structType) {
            builder_->CreateBr(nextBB);
            return false;
        }

        // Use the metadata's alloca as the record pointer when available (canonical);
        // otherwise bitcast value (e.g. when value is the alloca or a cast of it).
        llvm::Value* recPtr = meta.alloca
            ? builder_->CreateBitCast(meta.alloca, llvm::PointerType::get(context_, 0), "recptr")
            : builder_->CreateBitCast(value, llvm::PointerType::get(context_, 0), "recptr");

        llvm::Function* func = currentFunction_;
        llvm::BasicBlock* curBB = builder_->GetInsertBlock();

        const auto& fields = recordPattern->getFields();
        if (fields.empty()) {
            builder_->CreateBr(matchBB);
            return true;
        }

        for (size_t i = 0; i < fields.size(); ++i) {
            const auto& fp = fields[i];
            auto nameIt = std::find(meta.fieldNames.begin(), meta.fieldNames.end(), fp.name);
            if (nameIt == meta.fieldNames.end()) {
                errorReporter_.error(pattern->getLocation(),
                                     "Pattern matching: record has no field named '" + fp.name + "'");
                builder_->CreateBr(nextBB);
                return false;
            }
            size_t fieldIndex = static_cast<size_t>(std::distance(meta.fieldNames.begin(), nameIt));
            llvm::Type* fieldTy = structType->getElementType(static_cast<unsigned>(fieldIndex));

            builder_->SetInsertPoint(curBB);
            llvm::Value* fieldPtr = builder_->CreateStructGEP(structType, recPtr, static_cast<unsigned>(fieldIndex), "recfieldptr");
            llvm::Value* fieldVal = builder_->CreateLoad(fieldTy, fieldPtr, "recfieldval");

            llvm::BasicBlock* nextFieldBB =
                (i == fields.size() - 1) ? matchBB : llvm::BasicBlock::Create(context_, "recpat_next", func);
            if (!generatePatternMatch(fp.pattern.get(), fieldVal, nextFieldBB, nextBB)) {
                return false;
            }
            curBB = nextFieldBB;
        }
        return true;
    }
    else if (auto* constructorPattern = dynamic_cast<ast::ConstructorPattern*>(pattern)) {
        // Constructor pattern: check ADT tag
        // Value should be a pointer to ADT struct {tag, payload}
        
        // Cast value to ADT struct type
        llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
        llvm::Type* payloadType = llvm::PointerType::get(context_, 0);
        std::vector<llvm::Type*> structFields = {tagType, payloadType};
        llvm::StructType* adtType = createStructTypeSafe(context_, structFields);
        
        // Cast value to a pointer (value might already be a pointer).
        // In LLVM opaque-pointer mode we avoid constructing typed pointers via getPointerTo().
        llvm::Value* adtPtr = nullptr;
        if (value->getType()->isPointerTy()) {
            adtPtr = builder_->CreateBitCast(
                value,
                llvm::PointerType::get(context_, 0),
                "adtptr"
            );
        } else {
            // Value is not a pointer - this shouldn't happen for ADTs
            errorReporter_.error(
                pattern->getLocation(),
                "Pattern matching: constructor pattern requires ADT value"
            );
            return false;
        }

        const std::string& constructorName = constructorPattern->getConstructorName();
        const auto& argPatterns = constructorPattern->getArguments();
        bool noArgConstructor = argPatterns.empty();
        auto [adtAstType, tagIndex] = getConstructorIndex(constructorName);
        llvm::Value* expectedTag = llvm::ConstantInt::get(context_, llvm::APInt(64, tagIndex));
        (void)adtAstType;

        // Never load from null: if scrutinee is null, match only no-arg constructor (e.g. Nil), else go to next case.
        llvm::Value* isNull = builder_->CreateICmpEQ(
            adtPtr,
            llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(adtPtr->getType())),
            "adt.isnull"
        );
        llvm::BasicBlock* tagCheckBB = llvm::BasicBlock::Create(context_, "tagcheck", currentFunction_);
        if (noArgConstructor) {
            builder_->CreateCondBr(isNull, matchBB, tagCheckBB);
        } else {
            builder_->CreateCondBr(isNull, nextBB, tagCheckBB);
        }
        builder_->SetInsertPoint(tagCheckBB);

        // Load tag only when value is non-null
        llvm::Value* tagPtr = builder_->CreateStructGEP(adtType, adtPtr, 0, "tagptr");
        llvm::Value* tag = builder_->CreateAlignedLoad(tagType, tagPtr, llvm::Align(8), "tag");
        llvm::Value* tagMatch = builder_->CreateICmpEQ(tag, expectedTag, "tagcmp");
        builder_->CreateCondBr(tagMatch, matchBB, nextBB);
        return true;
    }
    else if (auto* asPattern = dynamic_cast<ast::AsPattern*>(pattern)) {
        // As pattern: match inner pattern and bind to name
        // First match the inner pattern
        bool innerMatch = generatePatternMatch(asPattern->getPattern(), value, matchBB, nextBB);
        // The as-pattern name binding will be handled in bindPatternVariables
        return innerMatch;
    }
    
    // Unknown pattern type
    errorReporter_.error(
        pattern->getLocation(),
        "Pattern matching: unsupported pattern type: " + pattern->getNodeType()
    );
    return false;
}

void IRGenerator::bindPatternVariables(ast::Pattern* pattern, llvm::Value* value, ast::Type* scrutineeType) {
    if (!pattern || !value) {
        return;
    }
    
    // Dispatch based on pattern type
    if (auto* varPattern = dynamic_cast<ast::VariablePattern*>(pattern)) {
        // Bind variable to value
        std::string varName = varPattern->getName();
        
        // Allocate space for variable
        llvm::Type* varType = value->getType();
        llvm::AllocaInst* alloca = builder_->CreateAlloca(varType, nullptr, varName);
        builder_->CreateStore(value, alloca);
        setVariable(varName, alloca, varType);
        // If value is a record pointer (e.g. from Circle(c)), propagate metadata so c.radius works
        auto metaIt = recordMetadata_.find(value);
        if (metaIt == recordMetadata_.end() && value->getType()->isPointerTy()) {
            llvm::Value* castVal = builder_->CreateBitCast(value, llvm::PointerType::get(context_, 0), "cast");
            metaIt = recordMetadata_.find(castVal);
        }
        if (metaIt != recordMetadata_.end()) {
            recordMetadata_[alloca] = metaIt->second;
            recordMetadata_[builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0), "allocacast")] = metaIt->second;
        }
    }
    else if (auto* recordPattern = dynamic_cast<ast::RecordPattern*>(pattern)) {
        llvm::Value* recordKey = value;
        if (auto* cast = llvm::dyn_cast<llvm::CastInst>(value))
            recordKey = cast->getOperand(0);
        else if (auto* load = llvm::dyn_cast<llvm::LoadInst>(value))
            recordKey = load->getPointerOperand();
        auto it = recordMetadata_.find(recordKey);
        if (it == recordMetadata_.end()) {
            for (const auto& varPair : localVars_) {
                llvm::Value* varAlloca = varPair.second;
                auto varIt = recordMetadata_.find(varAlloca);
                if (varIt != recordMetadata_.end()) {
                    it = varIt;
                    break;
                }
            }
        }
        if (it == recordMetadata_.end()) {
            errorReporter_.error(pattern->getLocation(),
                                 "Pattern matching: record type information not available for bindings");
            return;
        }
        const RecordMetadata& meta = it->second;
        llvm::StructType* structType = meta.structType;
        if (!structType) return;

        llvm::Value* recPtr = builder_->CreateBitCast(value, llvm::PointerType::get(context_, 0), "recptr");

        for (const auto& fp : recordPattern->getFields()) {
            auto nameIt = std::find(meta.fieldNames.begin(), meta.fieldNames.end(), fp.name);
            if (nameIt == meta.fieldNames.end()) continue;
            size_t fieldIndex = static_cast<size_t>(std::distance(meta.fieldNames.begin(), nameIt));
            llvm::Type* fieldTy = structType->getElementType(static_cast<unsigned>(fieldIndex));
            llvm::Value* fieldPtr = builder_->CreateStructGEP(structType, recPtr, static_cast<unsigned>(fieldIndex), "recfieldptr");
            llvm::Value* fieldVal = builder_->CreateLoad(fieldTy, fieldPtr, "recfieldval");
            bindPatternVariables(fp.pattern.get(), fieldVal);
        }
    }
    else if (auto* constructorPattern = dynamic_cast<ast::ConstructorPattern*>(pattern)) {
        const auto& argPatterns = constructorPattern->getArguments();
        // No-arg constructor (e.g. Nil): nothing to bind; value may be null so do not dereference.
        if (argPatterns.empty()) return;

        llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
        llvm::Type* ptrTy = llvm::PointerType::get(context_, 0);

        llvm::Value* adtPtr = nullptr;
        if (value->getType()->isPointerTy()) {
            adtPtr = builder_->CreateBitCast(value, ptrTy, "adtptr");
        } else {
            errorReporter_.error(
                pattern->getLocation(),
                "Pattern matching: constructor pattern requires ADT pointer"
            );
            return;
        }

        auto [adtAstType, tagIndex] = getConstructorIndex(constructorPattern->getConstructorName());
        (void)tagIndex;
        ast::Constructor* constructor = nullptr;
        if (adtAstType && adtAstType->getConstructors().size() > 0) {
            for (const auto& c : adtAstType->getConstructors()) {
                if (!c) continue;
                if (c->getName() == constructorPattern->getConstructorName()) {
                    constructor = c.get();
                    break;
                }
            }
        }
        // Build substitution from scrutinee type (e.g. Option<Int> -> T=Int) for payload types
        std::map<std::string, ast::Type*> scrutineeSubst;
        ast::Type* effectiveScrutinee = scrutineeType;
        std::unique_ptr<ast::Type> paramScrutineeSubst;
        if (!effectiveScrutinee && currentTypeSubst_ && currentFunctionDecl_ &&
            !currentFunctionDecl_->getParameters().empty() && currentFunctionDecl_->getParameters()[0]->getType()) {
            paramScrutineeSubst = substituteType(currentFunctionDecl_->getParameters()[0]->getType(), *currentTypeSubst_);
            if (paramScrutineeSubst) effectiveScrutinee = paramScrutineeSubst.get();
        }
        if (effectiveScrutinee) {
            if (auto* param = dynamic_cast<ast::ParameterizedType*>(effectiveScrutinee)) {
                auto it = typeDeclParamsMap_.find(param->getBaseName());
                if (it != typeDeclParamsMap_.end() && it->second.size() == param->getTypeArgs().size()) {
                    for (size_t k = 0; k < it->second.size(); ++k)
                        if (param->getTypeArgs()[k])
                            scrutineeSubst[it->second[k]] = param->getTypeArgs()[k].get();
                }
            }
        }
        auto resolveArgType = [this, &scrutineeSubst](ast::Type* argType) -> ast::Type* {
            if (auto* gen = dynamic_cast<ast::GenericType*>(argType)) {
                auto it = scrutineeSubst.find(gen->getName());
                if (it != scrutineeSubst.end()) return it->second;
                auto typeIt = typeDeclsMap_.find(gen->getName());
                if (typeIt != typeDeclsMap_.end() && typeIt->second) return typeIt->second;
            }
            return argType;
        };
        // ADT layout: single block { tag, payload_struct } (inline payload)
        std::vector<llvm::Type*> fieldTypes;
        if (constructor && constructor->getArgumentTypes().size() == argPatterns.size()) {
            const auto& argTypes = constructor->getArgumentTypes();
            for (size_t j = 0; j < argPatterns.size(); ++j) {
                ast::Type* resolved = resolveArgType(argTypes[j].get());
                llvm::Type* ft = convertType(resolved);
                fieldTypes.push_back(ft ? ft : ptrTy);
            }
        } else {
            fieldTypes.resize(argPatterns.size(), ptrTy);
        }
        llvm::StructType* payloadStructType = createStructTypeSafe(context_, fieldTypes);
        llvm::StructType* fullType = createStructTypeSafe(context_, {tagType, payloadStructType});
        llvm::Value* structPtr = builder_->CreateBitCast(adtPtr, llvm::PointerType::get(context_, 0), "adtstruct");
        llvm::Value* payloadPtr = builder_->CreateStructGEP(fullType, structPtr, 1, "payloadptr");

        if (!constructor || constructor->getArgumentTypes().size() != argPatterns.size()) {
            if (argPatterns.size() == 1) {
                bindPatternVariables(argPatterns[0].get(), payloadPtr);
            } else {
                std::vector<llvm::Type*> fallbackTypes(argPatterns.size(), ptrTy);
                llvm::StructType* fallbackPayloadType = createStructTypeSafe(context_, fallbackTypes);
                for (size_t j = 0; j < argPatterns.size(); ++j) {
                    llvm::Value* fieldPtr = builder_->CreateStructGEP(fallbackPayloadType, payloadPtr, static_cast<unsigned>(j), "fieldptr");
                    llvm::Value* fieldValue = builder_->CreateLoad(ptrTy, fieldPtr, "fieldval");
                    bindPatternVariables(argPatterns[j].get(), fieldValue);
                }
            }
            return;
        }

        const auto& argTypes = constructor->getArgumentTypes();
        if (argPatterns.size() > 1) {
        // Use payloadPtr (inline payload) directly; no load
        for (size_t j = 0; j < argPatterns.size(); ++j) {
            llvm::Value* fieldPtr = builder_->CreateStructGEP(payloadStructType, payloadPtr, static_cast<unsigned>(j), "fieldptr");
            llvm::Value* fieldValue = builder_->CreateAlignedLoad(fieldTypes[j], fieldPtr, llvm::Align(8), "fieldval");
            ast::Type* argType = argTypes[j].get();
            if (auto* gen = dynamic_cast<ast::GenericType*>(argType)) {
                auto typeIt = typeDeclsMap_.find(gen->getName());
                if (typeIt != typeDeclsMap_.end() && typeIt->second) {
                    argType = typeIt->second;
                }
            }
            // Single-constructor ADT with one field (e.g. Operator(op: String)): extract payload so x.op works
            // fieldValue is the full ADT pointer (tag, payload); we need the payload pointer for field access
            auto* argAdt = dynamic_cast<ast::ADTType*>(argType);
            if (argAdt && argAdt->getConstructors().size() == 1) {
                const auto& ctorArgs = argAdt->getConstructors()[0]->getArgumentTypes();
                if (ctorArgs.size() == 1) {
                    ast::Type* payloadType = ctorArgs[0].get();
                    if (auto* genPayload = dynamic_cast<ast::GenericType*>(payloadType)) {
                        auto typeIt = typeDeclsMap_.find(genPayload->getName());
                        if (typeIt != typeDeclsMap_.end() && typeIt->second) {
                            payloadType = typeIt->second;
                        }
                    }
                    llvm::Type* payloadLlvmTy = convertType(payloadType);
                    if (payloadLlvmTy) {
                        llvm::StructType* singleFieldTy = createStructTypeSafe(context_, std::vector<llvm::Type*>{payloadLlvmTy});
                        std::string fieldName = "value";  // single-constructor ADT single field
                        if (auto* varPat = dynamic_cast<ast::VariablePattern*>(argPatterns[j].get())) {
                            std::string varName = varPat->getName();
                            // Extract payload from ADT: fieldValue points to { tag, payload }; load payload
                            llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
                            std::vector<llvm::Type*> adtFields = {tagType, ptrTy};
                            llvm::StructType* adtStructTy = createStructTypeSafe(context_, adtFields);
                            llvm::Value* adtPtr = builder_->CreateBitCast(fieldValue, llvm::PointerType::get(context_, 0), "adtptr");
                            llvm::Value* payloadSlotPtr = builder_->CreateStructGEP(adtStructTy, adtPtr, 1, "payloadslot");
                            llvm::Value* payloadPtr = builder_->CreateAlignedLoad(ptrTy, payloadSlotPtr, llvm::Align(8), "payload");
                            llvm::AllocaInst* alloca = builder_->CreateAlloca(ptrTy, nullptr, varName);
                            builder_->CreateStore(payloadPtr, alloca);
                            setVariable(varName, alloca, ptrTy);
                            RecordMetadata meta;
                            meta.structType = singleFieldTy;
                            meta.fieldNames.push_back(fieldName);
                            meta.alloca = nullptr;
                            recordMetadata_[alloca] = meta;
                            recordMetadata_[builder_->CreateBitCast(alloca, ptrTy, "allocacast")] = meta;
                            continue;
                        }
                    }
                }
            }
            bindPatternVariables(argPatterns[j].get(), fieldValue);
        }
        return;
        }
        if (argPatterns.size() == 1) {
            ast::Type* argType = resolveArgType(argTypes[0].get());
            llvm::Value* valueForBinding = payloadPtr;
            if (auto* recType = dynamic_cast<ast::RecordType*>(argType)) {
                // Payload is a pointer to wrapper struct { recordPtr }; load the record pointer.
                llvm::Type* ptrTy = llvm::PointerType::get(context_, 0);
                llvm::StructType* wrapperTy = createStructTypeSafe(context_, std::vector<llvm::Type*>{ptrTy});
                llvm::Value* wrapperPtr = builder_->CreateBitCast(payloadPtr, ptrTy, "wrapperptr");
                llvm::Value* recPtrGep = builder_->CreateStructGEP(wrapperTy, wrapperPtr, 0, "recptrgep");
                valueForBinding = builder_->CreateLoad(ptrTy, recPtrGep, "recptr");
                RecordMetadata meta;
                meta.structType = llvm::dyn_cast<llvm::StructType>(convertType(recType));
                meta.alloca = nullptr;
                for (const auto& f : recType->getFields()) {
                    meta.fieldNames.push_back(f->getName());
                }
                if (meta.structType) {
                    recordMetadata_[valueForBinding] = meta;
                    recordMetadata_[builder_->CreateBitCast(valueForBinding, ptrTy, "recptrcast")] = meta;
                    recordMetadata_[recPtrGep] = meta;  // RecordPattern may look up by load's pointer operand
                }
            } else {
                // Single scalar (e.g. Term(Int)): payload is pointer to struct { scalar }; load the scalar.
                llvm::Type* scalarTy = convertType(argType);
                if (scalarTy) {
                    llvm::StructType* singleFieldTy = createStructTypeSafe(context_, std::vector<llvm::Type*>{scalarTy});
                    llvm::Value* structPtr = builder_->CreateBitCast(payloadPtr, llvm::PointerType::get(context_, 0), "scalarstructptr");
                    llvm::Value* fieldPtr = builder_->CreateStructGEP(singleFieldTy, structPtr, 0, "scalarfieldptr");
                    valueForBinding = builder_->CreateAlignedLoad(scalarTy, fieldPtr, llvm::Align(8), "scalarval");
                }
            }
            bindPatternVariables(argPatterns[0].get(), valueForBinding);
        } else {
            // Payload layout must match evaluateConstructor (inline payload).
            llvm::Type* ptrTy = llvm::PointerType::get(context_, 0);
            std::vector<llvm::Type*> fieldTypes;
            for (size_t j = 0; j < argPatterns.size(); ++j) {
                llvm::Type* ft = convertType(argTypes[j].get());
                fieldTypes.push_back(ft ? ft : ptrTy);
            }
            llvm::StructType* payloadStructTypeInner = createStructTypeSafe(context_, fieldTypes);
            for (size_t j = 0; j < argPatterns.size(); ++j) {
                llvm::Value* fieldPtr = builder_->CreateStructGEP(payloadStructTypeInner, payloadPtr, static_cast<unsigned>(j), "fieldptr");
                llvm::Value* fieldValue = builder_->CreateLoad(fieldTypes[j], fieldPtr, "fieldval");
                bindPatternVariables(argPatterns[j].get(), fieldValue);
            }
        }
    }
    else if (auto* asPattern = dynamic_cast<ast::AsPattern*>(pattern)) {
        // Bind the as-pattern name to the value
        std::string varName = asPattern->getName();
        llvm::Type* varType = value->getType();
        llvm::AllocaInst* alloca = builder_->CreateAlloca(varType, nullptr, varName);
        builder_->CreateStore(value, alloca);
        setVariable(varName, alloca, varType);
        
        // Also bind inner pattern variables
        bindPatternVariables(asPattern->getPattern(), value);
    }
    // Wildcard and literal patterns don't bind variables
}

llvm::Value* IRGenerator::evaluateLambda(ast::LambdaExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Step 1: Analyze which variables need to be captured
    std::vector<std::string> captures = analyzeCaptures(expr);
    
    // Step 2: Generate closure function (with captured variables as parameters)
    llvm::Function* closureFunc = generateClosureFunction(expr, captures);
    if (!closureFunc) {
        return nullptr;
    }
    
    // Step 3: Allocate closure (function pointer + captured environment)
    llvm::Value* closure = allocateClosure(closureFunc, captures);
    
    return closure;
}

std::vector<std::string> IRGenerator::analyzeCaptures(ast::LambdaExpr* expr) {
    if (!expr) {
        return {};
    }
    
    // Simple capture analysis: find all variable references in the lambda body
    // that are not parameters or local variables
    std::vector<std::string> captures;
    std::set<std::string> paramNames;
    std::set<std::string> localVars;
    
    // Collect parameter names
    for (const auto& param : expr->getParameters()) {
        paramNames.insert(param->getName());
    }
    
    // TODO: Collect local variable names from body
    // For now, we'll do a simple analysis by traversing the body
    
    // Simple heuristic: any variable reference that's not a parameter
    // and not defined in the lambda body is a capture
    // This is a simplified version - full analysis would need symbol table
    
    return captures;
}

llvm::Function* IRGenerator::generateClosureFunction(ast::LambdaExpr* expr,
                                                     const std::vector<std::string>& captures) {
    if (!expr) {
        return nullptr;
    }
    
    // Generate a unique function name for this closure
    static size_t closureCounter = 0;
    std::string funcName = "closure_" + std::to_string(closureCounter++);
    
    // Build function type: (captures..., params...) -> returnType
    std::vector<llvm::Type*> paramTypes;
    
    // Add captured variable types as parameters
    for (const auto& captureName : captures) {
        llvm::Value* capturedValue = getVariable(captureName);
        if (capturedValue) {
            // Get type from the captured variable's alloca
            if (auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(capturedValue)) {
                paramTypes.push_back(alloca->getAllocatedType());
            } else {
                // Default to i64 if we can't determine type
                paramTypes.push_back(llvm::Type::getInt64Ty(context_));
            }
        } else {
            // Default to i64 if variable not found
            paramTypes.push_back(llvm::Type::getInt64Ty(context_));
        }
    }
    
    // Add lambda parameters
    for (const auto& param : expr->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (paramType) {
            paramTypes.push_back(paramType);
        } else {
            paramTypes.push_back(llvm::Type::getInt64Ty(context_));
        }
    }
    
    // Get return type
    llvm::Type* returnType = llvm::Type::getVoidTy(context_);
    if (expr->getReturnType()) {
        llvm::Type* rt = convertType(expr->getReturnType());
        if (rt) {
            returnType = rt;
        }
    }
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    
    // Create function
    llvm::Function* func = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        funcName,
        module_.get()
    );
    
    // Save caller's insert point so we can restore after generating the closure body
    llvm::BasicBlock* savedInsertBlock = builder_->GetInsertBlock();
    
    // Set up function body
    llvm::BasicBlock* entryBB = llvm::BasicBlock::Create(context_, "entry", func);
    builder_->SetInsertPoint(entryBB);
    
    // Save current function context
    llvm::Function* oldFunction = currentFunction_;
    currentFunction_ = func;
    
    // Allocate parameters on stack
    size_t paramIndex = 0;
    
    // Process captured variables
    for (const auto& captureName : captures) {
        llvm::Argument* arg = func->getArg(paramIndex++);
        arg->setName(captureName);
        
        // Allocate space for captured variable
        llvm::Type* captureType = arg->getType();
        llvm::AllocaInst* alloca = builder_->CreateAlloca(captureType, nullptr, captureName);
        builder_->CreateStore(arg, alloca);
        setVariable(captureName, alloca, captureType);
    }
    
    // Process lambda parameters
    for (const auto& param : expr->getParameters()) {
        llvm::Argument* arg = func->getArg(paramIndex++);
        arg->setName(param->getName());
        
        // Allocate space for parameter
        llvm::Type* paramType = convertType(param->getType());
        if (paramType) {
            llvm::AllocaInst* alloca = builder_->CreateAlloca(paramType, nullptr, param->getName());
            builder_->CreateStore(arg, alloca);
            setVariable(param->getName(), alloca, paramType);
        }
    }
    
    // Generate body
    for (const auto& stmt : expr->getBody()) {
        generateStatement(stmt.get());
    }
    
    // Add default return if function is non-void and no return statement
    if (!returnType->isVoidTy()) {
        llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
        if (currentBB && !currentBB->getTerminator()) {
            // No return statement - add default return
            llvm::Value* defaultRet = getDefaultValue(returnType);
            builder_->CreateRet(defaultRet);
        }
    }
    
    // Restore function context and builder insert point (so caller continues in its block)
    currentFunction_ = oldFunction;
    if (savedInsertBlock)
        builder_->SetInsertPoint(savedInsertBlock);
    
    return func;
}

llvm::Value* IRGenerator::allocateClosure(llvm::Function* func,
                                          const std::vector<std::string>& captures) {
    if (!func) {
        return nullptr;
    }
    
    // For simplicity, we'll represent closures as a struct containing:
    // - function pointer (i8*)
    // - captured variables (as a struct)
    
    // Build captured environment struct type
    std::vector<llvm::Type*> envTypes;
    for (const auto& captureName : captures) {
        llvm::Value* capturedValue = getVariable(captureName);
        if (capturedValue) {
            if (auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(capturedValue)) {
                envTypes.push_back(alloca->getAllocatedType());
            } else {
                envTypes.push_back(llvm::Type::getInt64Ty(context_));
            }
        } else {
            envTypes.push_back(llvm::Type::getInt64Ty(context_));
        }
    }
    
    // If no captures, use empty struct
    if (envTypes.empty()) {
        envTypes.push_back(llvm::Type::getInt8Ty(context_)); // Dummy field
    }
    
    llvm::StructType* envType = createStructTypeSafe(context_, envTypes);
    
    // Build closure struct: { function pointer, environment }
    std::vector<llvm::Type*> closureTypes = {
        llvm::PointerType::get(context_, 0), // Function pointer (opaque ptr)
        llvm::PointerType::get(context_, 0)  // Environment pointer (opaque ptr)
    };
    llvm::StructType* closureType = createStructTypeSafe(context_, closureTypes);
    
    // Allocate closure on stack
    llvm::AllocaInst* closureAlloca = builder_->CreateAlloca(closureType, nullptr, "closure");
    
    // Store function pointer (cast function to i8*)
    llvm::Value* funcPtr = builder_->CreateBitCast(
        func,
        llvm::PointerType::get(context_, 0),
        "funcptr"
    );
    
    llvm::Value* funcPtrPtr = builder_->CreateStructGEP(closureType, closureAlloca, 0, "funcptrptr");
    builder_->CreateStore(funcPtr, funcPtrPtr);
    
    // Allocate and store captured environment
    if (!captures.empty()) {
        llvm::AllocaInst* envAlloca = builder_->CreateAlloca(envType, nullptr, "env");
        
        // Store captured values
        for (size_t i = 0; i < captures.size(); ++i) {
            llvm::Value* capturedValue = getVariable(captures[i]);
            if (capturedValue) {
                // Load value if it's an alloca
                if (auto* alloca = llvm::dyn_cast<llvm::AllocaInst>(capturedValue)) {
                    llvm::Value* value = builder_->CreateLoad(alloca->getAllocatedType(), alloca, "captval");
                    
                    llvm::Value* fieldPtr = builder_->CreateStructGEP(envType, envAlloca, static_cast<unsigned>(i), "fieldptr");
                    builder_->CreateStore(value, fieldPtr);
                }
            }
        }
        
        // Store environment pointer in closure
        llvm::Value* envPtrPtr = builder_->CreateStructGEP(closureType, closureAlloca, 1, "envptrptr");
        builder_->CreateStore(envAlloca, envPtrPtr);
    } else {
        // No captures: store null so invokeClosure uses noenv path and calls with (args...) only
        llvm::Value* envPtrPtr = builder_->CreateStructGEP(closureType, closureAlloca, 1, "envptrptr");
        builder_->CreateStore(llvm::ConstantPointerNull::get(llvm::PointerType::get(context_, 0)), envPtrPtr);
    }
    
    // Return closure pointer (as i8* for compatibility)
    return builder_->CreateBitCast(
        closureAlloca,
        llvm::PointerType::get(context_, 0),
        "closureptr"
    );
}

llvm::Value* IRGenerator::invokeClosure(llvm::Value* closure,
                                        const std::vector<llvm::Value*>& args,
                                        llvm::Type* returnType) {
    if (!closure) {
        return nullptr;
    }
    
    // Closure structure: { function pointer (i8*), environment pointer }
    // Reconstruct closure struct type
    llvm::Type* funcPtrType = llvm::PointerType::get(context_, 0);
    llvm::Type* envPtrType = llvm::PointerType::get(context_, 0);
    std::vector<llvm::Type*> closureStructFields = {funcPtrType, envPtrType};
    llvm::StructType* closureType = createStructTypeSafe(context_, closureStructFields);
    
    // Cast closure to a pointer for GEPs (opaque pointer mode)
    llvm::Value* closurePtr = builder_->CreateBitCast(
        closure,
        llvm::PointerType::get(context_, 0),
        "closurestructptr"
    );
    
    // Load function pointer
    llvm::Value* funcPtrPtr = builder_->CreateStructGEP(closureType, closurePtr, 0, "funcptrptr");
    llvm::Value* funcPtr = builder_->CreateLoad(funcPtrType, funcPtrPtr, "funcptr");
    
    // Load environment pointer
    llvm::Value* envPtrPtr = builder_->CreateStructGEP(closureType, closurePtr, 1, "envptrptr");
    llvm::Value* envPtr = builder_->CreateLoad(envPtrType, envPtrPtr, "envptr");
    
    // Determine function type from arguments and return type
    std::vector<llvm::Type*> paramTypes;
    
    // First parameter is environment (if not null)
    // Check if environment pointer is null
    llvm::Value* envIsNull = builder_->CreateIsNull(envPtr, "envnull");
    llvm::BasicBlock* hasEnvBB = llvm::BasicBlock::Create(context_, "hasenv", currentFunction_);
    llvm::BasicBlock* noEnvBB = llvm::BasicBlock::Create(context_, "noenv", currentFunction_);
    llvm::BasicBlock* mergeBB = llvm::BasicBlock::Create(context_, "envmerge", currentFunction_);
    
    builder_->CreateCondBr(envIsNull, noEnvBB, hasEnvBB);
    
    // Has environment: add environment type to parameters
    builder_->SetInsertPoint(hasEnvBB);
    std::vector<llvm::Type*> paramTypesWithEnv;
    paramTypesWithEnv.push_back(envPtrType); // Environment pointer
    for (llvm::Value* arg : args) {
        if (arg) {
            paramTypesWithEnv.push_back(arg->getType());
        }
    }
    
    // No environment: just argument types
    builder_->SetInsertPoint(noEnvBB);
    std::vector<llvm::Type*> paramTypesNoEnv;
    for (llvm::Value* arg : args) {
        if (arg) {
            paramTypesNoEnv.push_back(arg->getType());
        }
    }
    
    // Create function types
    if (!returnType) {
        returnType = llvm::Type::getVoidTy(context_);
    }
    llvm::FunctionType* funcTypeWithEnv = llvm::FunctionType::get(returnType, paramTypesWithEnv, false);
    llvm::FunctionType* funcTypeNoEnv = llvm::FunctionType::get(returnType, paramTypesNoEnv, false);
    
    // Cast function pointer to correct function type
    builder_->SetInsertPoint(hasEnvBB);
    llvm::Value* typedFuncPtrWithEnv = builder_->CreateBitCast(funcPtr, llvm::PointerType::get(context_, 0), "typedfuncptrenv");
    
    // Prepare call arguments: environment first, then actual arguments
    std::vector<llvm::Value*> callArgsWithEnv;
    callArgsWithEnv.push_back(envPtr);
    callArgsWithEnv.insert(callArgsWithEnv.end(), args.begin(), args.end());
    
    // Call the closure function with environment
    llvm::Value* resultWithEnv = builder_->CreateCall(funcTypeWithEnv, typedFuncPtrWithEnv, callArgsWithEnv, "closurecallenv");
    builder_->CreateBr(mergeBB);
    
    // No environment branch
    builder_->SetInsertPoint(noEnvBB);
    llvm::Value* typedFuncPtrNoEnv = builder_->CreateBitCast(funcPtr, llvm::PointerType::get(context_, 0), "typedfuncptrnoenv");
    
    // Call the closure function without environment
    llvm::Value* resultNoEnv = builder_->CreateCall(funcTypeNoEnv, typedFuncPtrNoEnv, args, "closurecallnoenv");
    builder_->CreateBr(mergeBB);
    
    // Merge results
    builder_->SetInsertPoint(mergeBB);
    if (returnType->isVoidTy()) {
        return nullptr;
    } else {
        llvm::PHINode* phi = builder_->CreatePHI(returnType, 2, "closureresult");
        phi->addIncoming(resultWithEnv, hasEnvBB);
        phi->addIncoming(resultNoEnv, noEnvBB);
        return phi;
    }
}

llvm::Value* IRGenerator::evaluateArrayLiteral(ast::ArrayLiteralExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    const auto& elements = expr->getElements();
    
    // For empty arrays, we need element type from context
    // For now, if empty, we can't determine type - this will be handled by type checker
    if (elements.empty()) {
        errorReporter_.error(
            expr->getLocation(),
            "Empty array literals require explicit type annotation"
        );
        return nullptr;
    }
    
    // Evaluate all element expressions
    std::vector<llvm::Value*> elementValues;
    llvm::Type* elementType = nullptr;
    
    for (const auto& element : elements) {
        llvm::Value* elemValue = evaluateExpr(element.get());
        if (!elemValue) {
            return nullptr;
        }
        
        // Determine element type from first element
        if (!elementType) {
            elementType = elemValue->getType();
        }
        
        elementValues.push_back(elemValue);
    }
    
    if (!elementType) {
        errorReporter_.error(
            expr->getLocation(),
            "Failed to determine array element type"
        );
        return nullptr;
    }
    
    // Arrays are represented as pointers to runtime array objects (i8*)
    // For now, we'll create a simple implementation that:
    // 1. Allocates space for array elements on the stack (temporary solution)
    // 2. Stores elements into the allocated space
    // 3. Returns a pointer to the array
    
    // TODO: Proper implementation would:
    // 1. Declare runtime makeArray functions
    // 2. Call runtime allocation functions
    // 3. Store elements using runtime methods
    
    // Temporary: Create a constant array on the stack
    // This is a simplified approach - full implementation needs runtime integration
    size_t arraySize = elementValues.size();
    
    // Create array type: [N x elementType]
    llvm::ArrayType* arrayType = llvm::ArrayType::get(elementType, arraySize);
    
    // Allocate array on stack
    llvm::AllocaInst* arrayAlloca = builder_->CreateAlloca(arrayType, nullptr, "arrtmp");
    
    // Store each element
    for (size_t i = 0; i < arraySize; ++i) {
        llvm::Value* elemPtr = builder_->CreateGEP(arrayType, arrayAlloca,
            {builder_->getInt64(0), builder_->getInt64(static_cast<uint64_t>(i))}, "arrelem");
        
        // Store element value
        builder_->CreateStore(elementValues[i], elemPtr);
    }
    
    // Store metadata for this array (keyed by the alloca, not the bitcast)
    ArrayMetadata metadata;
    metadata.elementType = elementType;
    metadata.size = arraySize;
    metadata.alloca = arrayAlloca;
    
    // Store metadata keyed by alloca
    arrayMetadata_[arrayAlloca] = metadata;
    
    // Return pointer to array (cast to i8* for now)
    llvm::Value* arrayPtr = builder_->CreateBitCast(
        arrayAlloca,
        llvm::PointerType::get(context_, 0),
        "arrptr"
    );
    
    // Also store metadata keyed by the bitcast pointer for direct array literals
    arrayMetadata_[arrayPtr] = metadata;
    
    return arrayPtr;
}

llvm::Value* IRGenerator::evaluateArrayIndex(ast::ArrayIndexExpr* expr) {
    if (!expr) {
        return nullptr;
    }
    
    // Evaluate array expression
    llvm::Value* arrayValue = evaluateExpr(expr->getArray());
    if (!arrayValue) {
        return nullptr;
    }
    
    // Evaluate index expression
    llvm::Value* indexValue = evaluateExpr(expr->getIndex());
    if (!indexValue) {
        return nullptr;
    }
    
    // Ensure index is integer type
    if (!indexValue->getType()->isIntegerTy()) {
        // Convert to integer if needed
        if (indexValue->getType()->isFloatingPointTy()) {
            indexValue = builder_->CreateFPToSI(
                indexValue,
                llvm::Type::getInt64Ty(context_),
                "indextmp"
            );
        } else {
            errorReporter_.error(
                expr->getIndex()->getLocation(),
                "Array index must be an integer or floating-point number"
            );
            return nullptr;
        }
    }
    
    // Ensure index is 64-bit
    if (indexValue->getType()->getIntegerBitWidth() != 64) {
        indexValue = builder_->CreateSExt(
            indexValue,
            llvm::Type::getInt64Ty(context_),
            "indexext"
        );
    }
    
    // Look up array metadata
    // arrayValue might be a direct array pointer (i8*) or the result of loading from a variable
    auto it = arrayMetadata_.find(arrayValue);
    
    if (it == arrayMetadata_.end()) {
        // arrayValue may be a load from a variable (LoadInst); find the alloca we loaded from
        if (llvm::LoadInst* loadInst = llvm::dyn_cast<llvm::LoadInst>(arrayValue)) {
            llvm::Value* loadedFrom = loadInst->getPointerOperand();
            it = arrayMetadata_.find(loadedFrom);
        }
    }
    if (it == arrayMetadata_.end()) {
        for (const auto& varPair : localVars_) {
            llvm::Value* varAlloca = varPair.second;
            auto varIt = arrayMetadata_.find(varAlloca);
            if (varIt != arrayMetadata_.end()) {
                it = varIt;
                break;
            }
        }
    }
    
    if (it == arrayMetadata_.end()) {
        // Array not found in metadata
        errorReporter_.error(
            expr->getLocation(),
            "Array indexing: array type information not available"
        );
        return nullptr;
    }
    
    ArrayMetadata& metadata = it->second;
    llvm::Type* elementType = metadata.elementType;
    size_t arraySize = metadata.size;
    llvm::AllocaInst* arrayAlloca = metadata.alloca;
    
    // Get current basic block
    llvm::BasicBlock* currentBB = builder_->GetInsertBlock();
    
    // Bounds checking
    llvm::BasicBlock* inBoundsBB = llvm::BasicBlock::Create(context_, "inbounds", currentFunction_);
    llvm::BasicBlock* outOfBoundsBB = llvm::BasicBlock::Create(context_, "outofbounds", currentFunction_);
    llvm::BasicBlock* continueBB = llvm::BasicBlock::Create(context_, "boundscont", currentFunction_);
    
    // Check: index >= 0 && index < size
    llvm::Value* zero = llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    llvm::Value* sizeConst = llvm::ConstantInt::get(context_, llvm::APInt(64, arraySize));
    
    llvm::Value* geZero = builder_->CreateICmpSGE(indexValue, zero, "gezero");
    llvm::Value* ltSize = builder_->CreateICmpSLT(indexValue, sizeConst, "ltsize");
    llvm::Value* inBounds = builder_->CreateAnd(geZero, ltSize, "inbounds");
    
    builder_->CreateCondBr(inBounds, inBoundsBB, outOfBoundsBB);
    
    // In bounds: load element
    builder_->SetInsertPoint(inBoundsBB);
    
    // Cast array pointer back to array type
    llvm::ArrayType* arrayType = llvm::ArrayType::get(elementType, arraySize);
    // Opaque-pointer mode: keep element type explicit in the GEP.
    llvm::Value* typedPtr = arrayAlloca;
    
    llvm::Value* elemPtr = builder_->CreateGEP(arrayType, typedPtr,
        {builder_->getInt64(0), indexValue}, "arrelem");
    
    // Load element value
    llvm::Value* elemValue = builder_->CreateLoad(elementType, elemPtr, "arrelemval");
    builder_->CreateBr(continueBB);
    
    // Out of bounds: use default value (null/zero for the element type)
    builder_->SetInsertPoint(outOfBoundsBB);
    llvm::Value* defaultValue = nullptr;
    if (elementType->isIntegerTy(64)) {
        defaultValue = llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    } else if (elementType->isDoubleTy()) {
        defaultValue = llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
    } else if (elementType->isIntegerTy(1)) {
        defaultValue = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
    } else if (elementType->isPointerTy()) {
        defaultValue = llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(elementType));
    } else {
        errorReporter_.error(
            expr->getLocation(),
            "Array index out of bounds"
        );
        return nullptr;
    }
    builder_->CreateBr(continueBB);
    
    // Continue: phi node to merge results
    builder_->SetInsertPoint(continueBB);
    llvm::PHINode* phi = builder_->CreatePHI(elementType, 2, "arrindex");
    phi->addIncoming(elemValue, inBoundsBB);
    phi->addIncoming(defaultValue, outOfBoundsBB);
    
    return phi;
}

void IRGenerator::generateAssignmentStmt(ast::AssignmentStmt* stmt) {
    if (!stmt) {
        return;
    }
    
    // Evaluate the value expression
    llvm::Value* value = evaluateExpr(stmt->getValue());
    if (!value) {
        errorReporter_.error(
            stmt->getLocation(),
            "Failed to evaluate assignment value"
        );
        return;
    }
    
    // Get the target (must be a variable expression)
    ast::Expr* targetExpr = stmt->getTarget();
    if (auto* varExpr = dynamic_cast<ast::VariableExpr*>(targetExpr)) {
        std::string varName = varExpr->getName();
        llvm::Value* alloca = getVariable(varName);
        
        if (!alloca) {
            errorReporter_.error(
                stmt->getLocation(),
                "Undefined variable: " + varName
            );
            return;
        }
        
        // Store the value to the variable's alloca
        builder_->CreateStore(value, alloca);
    } else {
        errorReporter_.error(
            stmt->getLocation(),
            "Assignment target must be a variable"
        );
    }
}

void IRGenerator::generateForInStmt(ast::ForInStmt* stmt) {
    if (!stmt || !currentFunction_) return;

    llvm::Type* i64 = llvm::Type::getInt64Ty(context_);
    llvm::Type* i8ptr = llvm::PointerType::get(context_, 0);
    const std::string& varName = stmt->getVariableName();
    ast::Expr* iterable = stmt->getIterable();

    // Case 1: Range expression (Haskell-style: 1..5 step 1, or 1,3..9 step 2, or 10,8..2 step -2)
    if (auto* rangeExpr = dynamic_cast<ast::RangeExpr*>(iterable)) {
        llvm::Value* startVal = evaluateExpr(rangeExpr->getStart());
        llvm::Value* endVal = evaluateExpr(rangeExpr->getEnd());
        if (!startVal || !endVal) {
            errorReporter_.error(stmt->getLocation(), "Failed to evaluate range bounds");
            return;
        }
        if (startVal->getType() != i64)
            startVal = builder_->CreateSExtOrTrunc(startVal, i64, "start.i64");
        if (endVal->getType() != i64)
            endVal = builder_->CreateSExtOrTrunc(endVal, i64, "end.i64");

        llvm::Value* stepVal;
        if (rangeExpr->hasStepHint()) {
            llvm::Value* secondVal = evaluateExpr(rangeExpr->getStepHint());
            if (!secondVal) {
                errorReporter_.error(stmt->getLocation(), "Failed to evaluate range step hint");
                return;
            }
            if (secondVal->getType() != i64)
                secondVal = builder_->CreateSExtOrTrunc(secondVal, i64, "second.i64");
            stepVal = builder_->CreateSub(secondVal, startVal, "range.step");
        } else {
            stepVal = builder_->getInt64(1);
        }

        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context_, "for.cond", currentFunction_);
        llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context_, "for.body", currentFunction_);
        llvm::BasicBlock* exitBB = llvm::BasicBlock::Create(context_, "for.exit", currentFunction_);

        llvm::AllocaInst* iAlloca = builder_->CreateAlloca(i64, nullptr, "for.i");
        llvm::AllocaInst* stepAlloca = builder_->CreateAlloca(i64, nullptr, "for.step");
        builder_->CreateStore(startVal, iAlloca);
        builder_->CreateStore(stepVal, stepAlloca);
        builder_->CreateBr(condBB);

        builder_->SetInsertPoint(condBB);
        llvm::Value* i = builder_->CreateLoad(i64, iAlloca, "i");
        llvm::Value* step = builder_->CreateLoad(i64, stepAlloca, "step");
        llvm::Value* stepPos = builder_->CreateICmpSGT(step, builder_->getInt64(0), "step.pos");
        llvm::Value* le = builder_->CreateICmpSLE(i, endVal, "i.le.end");
        llvm::Value* ge = builder_->CreateICmpSGE(i, endVal, "i.ge.end");
        llvm::Value* lt = builder_->CreateICmpSLT(i, endVal, "i.lt.end");
        llvm::Value* gt = builder_->CreateICmpSGT(i, endVal, "i.gt.end");
        llvm::Value* cmp;
        if (rangeExpr->isInclusive()) {
            llvm::Value* posOk = builder_->CreateAnd(stepPos, le, "pos.incl");
            llvm::Value* negOk = builder_->CreateAnd(builder_->CreateNot(stepPos), ge, "neg.incl");
            cmp = builder_->CreateOr(posOk, negOk, "for.cmp");
        } else {
            llvm::Value* posOk = builder_->CreateAnd(stepPos, lt, "pos.excl");
            llvm::Value* negOk = builder_->CreateAnd(builder_->CreateNot(stepPos), gt, "neg.excl");
            cmp = builder_->CreateOr(posOk, negOk, "for.cmp");
        }
        builder_->CreateCondBr(cmp, bodyBB, exitBB);

        builder_->SetInsertPoint(bodyBB);
        setVariable(varName, iAlloca, i64);
        for (const auto& s : stmt->getBody()) {
            generateStatement(s.get());
        }
        llvm::Value* iCur = builder_->CreateLoad(i64, iAlloca, "i");
        llvm::Value* stepCur = builder_->CreateLoad(i64, stepAlloca, "step");
        llvm::Value* iNext = builder_->CreateAdd(iCur, stepCur, "i.next");
        builder_->CreateStore(iNext, iAlloca);
        builder_->CreateBr(condBB);

        builder_->SetInsertPoint(exitBB);
        return;
    }

    // Case 1b: ArrayBuf - iterate over bytes (0..length-1), element type Int
    if (stmt->getIterableKind() == ast::IterableKind::ArrayBuf) {
        llvm::Value* bufVal = evaluateExpr(iterable);
        if (!bufVal) {
            errorReporter_.error(stmt->getLocation(), "Failed to evaluate ArrayBuf iterable");
            return;
        }
        if (bufVal->getType() != i8ptr)
            bufVal = builder_->CreatePointerCast(bufVal, i8ptr, "buf.ptr");
        llvm::Function* lenFn = module_->getFunction("first_arraybuf_length");
        if (!lenFn)
            lenFn = llvm::Function::Create(
                llvm::FunctionType::get(i64, {i8ptr}, false),
                llvm::Function::ExternalLinkage, "first_arraybuf_length", module_.get());
        llvm::Function* getFn = module_->getFunction("first_arraybuf_get");
        if (!getFn)
            getFn = llvm::Function::Create(
                llvm::FunctionType::get(i64, {i8ptr, i64}, false),
                llvm::Function::ExternalLinkage, "first_arraybuf_get", module_.get());
        llvm::Value* len = builder_->CreateCall(lenFn, {bufVal}, "buf.len");
        llvm::AllocaInst* idxAlloca = builder_->CreateAlloca(i64, nullptr, "for.idx");
        builder_->CreateStore(builder_->getInt64(0), idxAlloca);
        llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context_, "for.cond", currentFunction_);
        llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context_, "for.body", currentFunction_);
        llvm::BasicBlock* exitBB = llvm::BasicBlock::Create(context_, "for.exit", currentFunction_);
        builder_->CreateBr(condBB);
        builder_->SetInsertPoint(condBB);
        llvm::Value* idx = builder_->CreateLoad(i64, idxAlloca, "idx");
        llvm::Value* cmp = builder_->CreateICmpSLT(idx, len, "for.cmp");
        builder_->CreateCondBr(cmp, bodyBB, exitBB);
        builder_->SetInsertPoint(bodyBB);
        llvm::Value* byteVal = builder_->CreateCall(getFn, {bufVal, idx}, "byte");
        llvm::AllocaInst* elemAlloca = builder_->CreateAlloca(i64, nullptr, varName);
        builder_->CreateStore(byteVal, elemAlloca);
        setVariable(varName, elemAlloca, i64);
        for (const auto& s : stmt->getBody()) {
            generateStatement(s.get());
        }
        llvm::Value* idxNext = builder_->CreateAdd(idx, builder_->getInt64(1), "idx.next");
        builder_->CreateStore(idxNext, idxAlloca);
        builder_->CreateBr(condBB);
        builder_->SetInsertPoint(exitBB);
        return;
    }

    // Case 2: Array - iterate over indices
    llvm::Value* arrayVal = evaluateExpr(iterable);
    if (!arrayVal) {
        errorReporter_.error(stmt->getLocation(), "Failed to evaluate iterable");
        return;
    }
    auto resolveArrayMeta = [this](llvm::Value* v) -> std::pair<llvm::AllocaInst*, int64_t> {
        auto it = arrayMetadata_.find(v);
        if (it != arrayMetadata_.end())
            return {it->second.alloca, static_cast<int64_t>(it->second.size)};
        if (auto* load = llvm::dyn_cast<llvm::LoadInst>(v)) {
            it = arrayMetadata_.find(load->getPointerOperand());
            if (it != arrayMetadata_.end())
                return {it->second.alloca, static_cast<int64_t>(it->second.size)};
        }
        for (const auto& p : localVars_) {
            it = arrayMetadata_.find(p.second);
            if (it != arrayMetadata_.end())
                return {it->second.alloca, static_cast<int64_t>(it->second.size)};
        }
        return {nullptr, 0};
    };
    auto [baseAlloca, len] = resolveArrayMeta(arrayVal);
    if (!baseAlloca || len <= 0) {
        errorReporter_.error(stmt->getLocation(), "for-in over non-array or array without metadata");
        return;
    }

    llvm::AllocaInst* idxAlloca = builder_->CreateAlloca(i64, nullptr, "for.idx");
    builder_->CreateStore(builder_->getInt64(0), idxAlloca);

    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context_, "for.cond", currentFunction_);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context_, "for.body", currentFunction_);
    llvm::BasicBlock* exitBB = llvm::BasicBlock::Create(context_, "for.exit", currentFunction_);

    builder_->CreateBr(condBB);
    builder_->SetInsertPoint(condBB);
    llvm::Value* idx = builder_->CreateLoad(i64, idxAlloca, "idx");
    llvm::Value* cmp = builder_->CreateICmpSLT(idx, builder_->getInt64(len), "for.cmp");
    builder_->CreateCondBr(cmp, bodyBB, exitBB);

    builder_->SetInsertPoint(bodyBB);
    llvm::Type* elemTy = arrayMetadata_[baseAlloca].elementType;
    llvm::Type* arrTy = baseAlloca->getAllocatedType();
    llvm::Value* elemPtr = builder_->CreateGEP(arrTy, baseAlloca,
        {builder_->getInt64(0), idx}, "elem.ptr");
    llvm::Value* elemVal = builder_->CreateLoad(elemTy, elemPtr, "elem");
    llvm::AllocaInst* elemAlloca = builder_->CreateAlloca(elemTy, nullptr, varName);
    builder_->CreateStore(elemVal, elemAlloca);
    setVariable(varName, elemAlloca, elemTy);

    for (const auto& s : stmt->getBody()) {
        generateStatement(s.get());
    }
    llvm::Value* idxNext = builder_->CreateAdd(idx, builder_->getInt64(1), "idx.next");
    builder_->CreateStore(idxNext, idxAlloca);
    builder_->CreateBr(condBB);

    builder_->SetInsertPoint(exitBB);
}

void IRGenerator::generateSelectStmt(ast::SelectStmt* stmt) {
    if (!stmt) {
        return;
    }
    // select { branches }: stub - run first branch's statement
    const auto& branches = stmt->getBranches();
    if (!branches.empty() && branches[0]->getStatement()) {
        generateStatement(branches[0]->getStatement());
    }
}

std::unique_ptr<ast::Type> IRGenerator::substituteType(ast::Type* type,
                                                       const std::map<std::string, ast::Type*>& substitutions) {
    if (!type) {
        return nullptr;
    }
    
    SourceLocation loc = type->getLocation();
    
    // If it's a generic type parameter, substitute it
    if (auto* genType = dynamic_cast<ast::GenericType*>(type)) {
        std::string paramName = genType->getName();
        auto it = substitutions.find(paramName);
        if (it != substitutions.end()) {
            // Return a copy of the substituted type
            return copyType(it->second);
        } else {
            // Type parameter not found in substitutions - return as-is
            // This might be an error, but we'll let the caller handle it
            return std::make_unique<ast::GenericType>(loc, paramName);
        }
    }
    
    // If it's a parameterized type, substitute type arguments
    if (auto* paramType = dynamic_cast<ast::ParameterizedType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> substitutedArgs;
        for (const auto& arg : paramType->getTypeArgs()) {
            auto subArg = substituteType(arg.get(), substitutions);
            if (subArg) {
                substitutedArgs.push_back(std::move(subArg));
            } else {
                return nullptr;
            }
        }
        return std::make_unique<ast::ParameterizedType>(
            loc, paramType->getBaseName(), std::move(substitutedArgs)
        );
    }
    
    // If it's an array type, substitute element type
    if (auto* arrType = dynamic_cast<ast::ArrayType*>(type)) {
        auto subElement = substituteType(arrType->getElementType(), substitutions);
        if (subElement) {
            return std::make_unique<ast::ArrayType>(loc, std::move(subElement));
        }
        return nullptr;
    }
    
    // If it's a record type, substitute field types
    if (auto* recType = dynamic_cast<ast::RecordType*>(type)) {
        std::vector<std::unique_ptr<ast::RecordField>> substitutedFields;
        for (const auto& field : recType->getFields()) {
            auto subFieldType = substituteType(field->getType(), substitutions);
            if (subFieldType) {
                substitutedFields.push_back(std::make_unique<ast::RecordField>(
                    field->getLocation(), field->getName(), std::move(subFieldType)
                ));
            } else {
                return nullptr;
            }
        }
        return std::make_unique<ast::RecordType>(loc, std::move(substitutedFields));
    }
    
    // If it's a function type, substitute parameter and return types
    if (auto* funcType = dynamic_cast<ast::FunctionType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> substitutedParams;
        for (const auto& paramType : funcType->getParamTypes()) {
            auto subParam = substituteType(paramType.get(), substitutions);
            if (subParam) {
                substitutedParams.push_back(std::move(subParam));
            } else {
                return nullptr;
            }
        }
        auto subReturn = substituteType(funcType->getReturnType(), substitutions);
        if (subReturn) {
            return std::make_unique<ast::FunctionType>(
                loc, std::move(substitutedParams), std::move(subReturn), funcType->isInteraction()
            );
        }
        return nullptr;
    }
    
    // If it's an ADT type, substitute in constructor argument types
    if (auto* adtType = dynamic_cast<ast::ADTType*>(type)) {
        std::vector<std::unique_ptr<ast::Constructor>> constructors;
        for (const auto& c : adtType->getConstructors()) {
            if (!c) continue;
            std::vector<std::unique_ptr<ast::Type>> argTypes;
            for (const auto& a : c->getArgumentTypes()) {
                auto at = substituteType(a.get(), substitutions);
                if (!at) return nullptr;
                argTypes.push_back(std::move(at));
            }
            constructors.push_back(std::make_unique<ast::Constructor>(
                c->getLocation(), c->getName(), std::move(argTypes)));
        }
        return std::make_unique<ast::ADTType>(loc, adtType->getName(), std::move(constructors));
    }

    // For primitive types and other types, return a copy
    return copyType(type);
}

std::unique_ptr<ast::Type> IRGenerator::copyType(ast::Type* type) {
    if (!type) {
        return nullptr;
    }
    
    SourceLocation loc = type->getLocation();
    
    if (auto* prim = dynamic_cast<ast::PrimitiveType*>(type)) {
        return std::make_unique<ast::PrimitiveType>(loc, prim->getKind());
    }
    
    if (auto* gen = dynamic_cast<ast::GenericType*>(type)) {
        return std::make_unique<ast::GenericType>(loc, gen->getName());
    }
    
    if (auto* arr = dynamic_cast<ast::ArrayType*>(type)) {
        auto elementCopy = copyType(arr->getElementType());
        if (!elementCopy) return nullptr;
        return std::make_unique<ast::ArrayType>(loc, std::move(elementCopy));
    }
    
    if (auto* rec = dynamic_cast<ast::RecordType*>(type)) {
        std::vector<std::unique_ptr<ast::RecordField>> fieldsCopy;
        for (const auto& field : rec->getFields()) {
            auto fieldTypeCopy = copyType(field->getType());
            if (!fieldTypeCopy) return nullptr;
            fieldsCopy.push_back(std::make_unique<ast::RecordField>(
                field->getLocation(), field->getName(), std::move(fieldTypeCopy)
            ));
        }
        return std::make_unique<ast::RecordType>(loc, std::move(fieldsCopy));
    }
    
    if (auto* param = dynamic_cast<ast::ParameterizedType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> argsCopy;
        for (const auto& arg : param->getTypeArgs()) {
            auto argCopy = copyType(arg.get());
            if (!argCopy) return nullptr;
            argsCopy.push_back(std::move(argCopy));
        }
        return std::make_unique<ast::ParameterizedType>(
            loc, param->getBaseName(), std::move(argsCopy)
        );
    }
    
    if (auto* func = dynamic_cast<ast::FunctionType*>(type)) {
        std::vector<std::unique_ptr<ast::Type>> paramTypesCopy;
        for (const auto& paramType : func->getParamTypes()) {
            auto paramCopy = copyType(paramType.get());
            if (!paramCopy) return nullptr;
            paramTypesCopy.push_back(std::move(paramCopy));
        }
        auto returnCopy = copyType(func->getReturnType());
        if (!returnCopy) return nullptr;
        return std::make_unique<ast::FunctionType>(
            loc, std::move(paramTypesCopy), std::move(returnCopy), func->isInteraction()
        );
    }
    
    return nullptr;
}

std::string IRGenerator::getMonomorphizedName(const std::string& baseName,
                                               const std::vector<ast::Type*>& typeArgs) {
    std::string name = baseName;
    if (!typeArgs.empty()) {
        name += "_";
        for (size_t i = 0; i < typeArgs.size(); ++i) {
            if (i > 0) name += "_";
            // Create a simple type name for the monomorphized function name
            if (auto* prim = dynamic_cast<ast::PrimitiveType*>(typeArgs[i])) {
                switch (prim->getKind()) {
                    case ast::PrimitiveType::Kind::Int: name += "Int"; break;
                    case ast::PrimitiveType::Kind::Float: name += "Float"; break;
                    case ast::PrimitiveType::Kind::Bool: name += "Bool"; break;
                    case ast::PrimitiveType::Kind::String: name += "String"; break;
                    case ast::PrimitiveType::Kind::Unit: name += "Unit"; break;
                    case ast::PrimitiveType::Kind::Null: name += "Null"; break;
                    case ast::PrimitiveType::Kind::ArrayBuf: name += "ArrayBuf"; break;
                }
            } else if (auto* arr = dynamic_cast<ast::ArrayType*>(typeArgs[i])) {
                name += "Array";
            } else if (auto* rec = dynamic_cast<ast::RecordType*>(typeArgs[i])) {
                name += "Record";
            } else if (auto* param = dynamic_cast<ast::ParameterizedType*>(typeArgs[i])) {
                name += param->getBaseName();
            } else {
                name += "T" + std::to_string(i);
            }
        }
    }
    return name;
}

llvm::Function* IRGenerator::monomorphizeFunction(ast::FunctionDecl* func,
                                                   const std::vector<ast::Type*>& typeArgs) {
    if (!func) {
        return nullptr;
    }
    
    // Check if we've already monomorphized this function with these type arguments
    std::string monoName = getMonomorphizedName(func->getName(), typeArgs);
    auto it = monomorphizedFunctions_.find(monoName);
    if (it != monomorphizedFunctions_.end()) {
        return it->second;
    }
    
    // Build substitution map from generic parameters to type arguments
    const auto& genericParams = func->getGenericParams();
    if (genericParams.size() != typeArgs.size()) {
        errorReporter_.error(
            func->getLocation(),
            "Generic function '" + func->getName() + "' expects " + 
            std::to_string(genericParams.size()) + " type arguments, got " +
            std::to_string(typeArgs.size())
        );
        return nullptr;
    }
    
    std::map<std::string, ast::Type*> substitutions;
    for (size_t i = 0; i < genericParams.size(); ++i) {
        substitutions[genericParams[i].name] = typeArgs[i];
    }
    
    // Substitute types in function signature
    std::vector<std::unique_ptr<ast::Parameter>> substitutedParams;
    for (const auto& param : func->getParameters()) {
        auto subParamType = substituteType(param->getType(), substitutions);
        if (!subParamType) {
            errorReporter_.error(
                param->getLocation(),
                "Failed to substitute type for parameter: " + param->getName()
            );
            return nullptr;
        }
        substitutedParams.push_back(std::make_unique<ast::Parameter>(
            param->getLocation(), param->getName(), std::move(subParamType)
        ));
    }
    
    auto subReturnType = substituteType(func->getReturnType(), substitutions);
    if (!subReturnType) {
        errorReporter_.error(
            func->getLocation(),
            "Failed to substitute return type"
        );
        return nullptr;
    }
    
    // Convert substituted types to LLVM types
    std::vector<llvm::Type*> llvmParamTypes;
    for (const auto& param : substitutedParams) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
            return nullptr;
        }
        llvmParamTypes.push_back(paramType);
    }
    
    llvm::Type* llvmReturnType = convertType(subReturnType.get());
    if (!llvmReturnType) {
        return nullptr;
    }
    
    // Create function type
    llvm::FunctionType* funcType = llvm::FunctionType::get(llvmReturnType, llvmParamTypes, false);
    
    // Create function
    llvm::Function* monoFunc = llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        monoName,
        module_.get()
    );
    
    // Cache the monomorphized function
    monomorphizedFunctions_[monoName] = monoFunc;
    
    return monoFunc;
}

void IRGenerator::generateMonomorphizedBody(ast::FunctionDecl* astFunc,
                                            const std::vector<ast::Type*>& typeArgs,
                                            llvm::Function* monoFunc) {
    if (!astFunc || !monoFunc || typeArgs.size() != astFunc->getGenericParams().size()) {
        return;
    }
    if (monoFunc->arg_size() != astFunc->getParameters().size()) {
        errorReporter_.error(
            astFunc->getLocation(),
            "Monomorphized function " + monoFunc->getName().str() + " has " +
            std::to_string(monoFunc->arg_size()) + " params but AST has " +
            std::to_string(astFunc->getParameters().size())
        );
        return;
    }
    generatingMonomorphized_.insert(monoFunc);
    std::map<std::string, ast::Type*> subst;
    for (size_t i = 0; i < astFunc->getGenericParams().size(); ++i) {
        subst[astFunc->getGenericParams()[i].name] = typeArgs[i];
    }
    std::map<std::string, ast::Type*>* savedSubst = currentTypeSubst_;
    currentTypeSubst_ = &subst;
    ast::FunctionDecl* savedDecl = currentFunctionDecl_;
    currentFunctionDecl_ = astFunc;

    // Save caller's context (enterFunction/exitFunction would corrupt it)
    llvm::Function* savedFunc = currentFunction_;
    llvm::BasicBlock* savedInsertBlock = builder_->GetInsertBlock();
    auto savedLocalVars = localVars_;
    auto savedLocalVarTypes = localVarTypes_;
    auto savedRefinementOverrides = refinementVarOverrides_;
    auto savedArrayMetadata = arrayMetadata_;
    auto savedRecordMetadata = recordMetadata_;

    enterFunction(monoFunc);
    llvm::BasicBlock* entryBlock = llvm::BasicBlock::Create(context_, "entry", monoFunc);
    builder_->SetInsertPoint(entryBlock);

    auto createAlloca = [&](llvm::Type* ty, const std::string& name) -> llvm::AllocaInst* {
        return new llvm::AllocaInst(ty, 0, nullptr, llvm::Align(8), name, entryBlock);
    };

    unsigned idx = 0;
    for (auto& arg : monoFunc->args()) {
        if (idx < astFunc->getParameters().size()) {
            std::string paramName = astFunc->getParameters()[idx]->getName();
            ast::Parameter* paramNode = astFunc->getParameters()[idx].get();
            auto subParamType = substituteType(paramNode->getType(), subst);
            llvm::Type* astLlvmType = subParamType ? convertType(subParamType.get()) : nullptr;
            llvm::Type* paramType = arg.getType();
            // Use AST type for scalar params so we load from pointer when caller passed a slot (erased generic).
            if (astLlvmType && astLlvmType->isIntegerTy(64) && paramType->isPointerTy()) {
                paramType = astLlvmType;
            } else if (astLlvmType && (astLlvmType->isDoubleTy() || astLlvmType->isIntegerTy(1)) && paramType->isPointerTy()) {
                paramType = astLlvmType;
            }
            llvm::AllocaInst* alloca = createAlloca(paramType, paramName);
            llvm::Value* valueToStore = &arg;
            if (arg.getType()->isPointerTy() && paramType->isIntegerTy(64)) {
                valueToStore = builder_->CreateAlignedLoad(llvm::Type::getInt64Ty(context_), &arg, llvm::Align(8), paramName + ".load");
            } else if (arg.getType()->isPointerTy() && paramType->isDoubleTy()) {
                valueToStore = builder_->CreateAlignedLoad(llvm::Type::getDoubleTy(context_), &arg, llvm::Align(8), paramName + ".load");
            } else if (arg.getType()->isPointerTy() && paramType->isIntegerTy(1)) {
                valueToStore = builder_->CreateAlignedLoad(llvm::Type::getInt1Ty(context_), &arg, llvm::Align(1), paramName + ".load");
            }
            builder_->CreateAlignedStore(valueToStore, alloca, llvm::Align(8));
            setVariable(paramName, alloca, paramType);
            if (subParamType) {
                if (auto* recType = dynamic_cast<ast::RecordType*>(subParamType.get())) {
                    RecordMetadata meta;
                    meta.structType = llvm::dyn_cast<llvm::StructType>(paramType);
                    meta.alloca = alloca;
                    for (const auto& f : recType->getFields()) {
                        meta.fieldNames.push_back(f->getName());
                    }
                    if (meta.structType) {
                        recordMetadata_[alloca] = meta;
                        recordMetadata_[builder_->CreateBitCast(alloca, llvm::PointerType::get(context_, 0))] = meta;
                    }
                }
            }
        }
        idx++;
    }

    std::vector<ast::Parameter*> paramPtrs;
    for (const auto& p : astFunc->getParameters()) {
        paramPtrs.push_back(p.get());
    }
    emitRefinementChecksForParams(paramPtrs, monoFunc, entryBlock);

    llvm::Type* returnType = monoFunc->getReturnType();
    bool hasReturn = false;
    const auto& bodyStmts = astFunc->getBody();
    for (size_t si = 0; si < bodyStmts.size(); ++si) {
        generateStatement(bodyStmts[si].get());
        if (dynamic_cast<ast::ReturnStmt*>(bodyStmts[si].get())) {
            hasReturn = true;
        }
    }

    if (!hasReturn) {
        if (returnType->isVoidTy()) {
            builder_->CreateRetVoid();
        } else {
            if (returnType->isIntegerTy(64)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(64, 0, true)));
            } else if (returnType->isDoubleTy()) {
                builder_->CreateRet(llvm::ConstantFP::get(context_, llvm::APFloat(0.0)));
            } else if (returnType->isIntegerTy(1)) {
                builder_->CreateRet(llvm::ConstantInt::get(context_, llvm::APInt(1, 0)));
            } else {
                builder_->CreateRet(llvm::Constant::getNullValue(returnType));
            }
        }
    }

    currentFunctionDecl_ = savedDecl;
    currentTypeSubst_ = savedSubst;
    generatingMonomorphized_.erase(monoFunc);

    // Restore caller's context (exitFunction would have cleared it)
    currentFunction_ = savedFunc;
    if (savedInsertBlock) {
        builder_->SetInsertPoint(savedInsertBlock);
    }
    localVars_ = std::move(savedLocalVars);
    localVarTypes_ = std::move(savedLocalVarTypes);
    refinementVarOverrides_ = std::move(savedRefinementOverrides);
    arrayMetadata_ = std::move(savedArrayMetadata);
    recordMetadata_ = std::move(savedRecordMetadata);
}

void IRGenerator::visitImportDecl(ast::ImportDecl* node) {
    if (!node) {
        return;
    }
    
    // Import declarations are handled during semantic analysis (module resolution)
    // During IR generation, we generate external declarations for imported symbols
    
    std::string moduleName = node->getModuleName();
    ast::ImportDecl::ImportKind kind = node->getKind();
    
    // If we have a module resolver, look up the module and generate external declarations
    if (moduleResolver_) {
        // Get the imported module
        ast::Program* importedModule = moduleResolver_->getModule(moduleName);
        
        if (importedModule) {
            // Generate external declarations based on import kind
            if (kind == ast::ImportDecl::ImportKind::All) {
                // Import all: declare every function and interaction so calls (e.g. optionMap, some) resolve
                for (const auto& func : importedModule->getFunctions()) {
                    if (func) generateExternalDeclaration(func->getName(), moduleName);
                }
                for (const auto& interaction : importedModule->getInteractions()) {
                    if (interaction) generateExternalDeclaration(interaction->getName(), moduleName);
                }
            } else if (kind == ast::ImportDecl::ImportKind::Specific) {
                // Import specific symbols
                for (const auto& symbolName : node->getSymbols()) {
                    generateExternalDeclaration(symbolName, moduleName);
                }
            } else {
                // Default import - import module namespace
                // This would typically import the module as a namespace
                // For now, we'll import all exported symbols
                std::vector<std::string> exportedSymbols = moduleResolver_->getExportedSymbols(moduleName);
                for (const auto& symbolName : exportedSymbols) {
                    generateExternalDeclaration(symbolName, moduleName);
                }
            }
        }
    }
    
    // TODO: Generate external declarations for imported symbols
    // This requires access to the module resolver's symbol information
}

void IRGenerator::generateExternalDeclaration(const std::string& symbolName, const std::string& moduleName) {
    // Math and Date are stdlib: calls resolve to first_* / first_date_* via getStdlibSig; no external decl needed.
    if (moduleName == "Math" || moduleName == "Date") {
        return;
    }
    // Generate an external function declaration
    // This allows the current module to call functions from the imported module
    if (module_->getFunction(symbolName)) {
        return; // Already declared
    }
    
    // Look up actual function signature from imported module
    if (!moduleResolver_) {
        // No module resolver, create placeholder
        std::vector<llvm::Type*> paramTypes;
        llvm::Type* returnType = llvm::Type::getInt64Ty(context_);
        llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
        llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            symbolName,
            module_.get()
        );
        return;
    }
    
    // Try to find function in imported module
    ast::FunctionDecl* funcDecl = moduleResolver_->getFunction(moduleName, symbolName);
    if (funcDecl) {
        // Convert parameter types
        std::vector<llvm::Type*> paramTypes;
        for (const auto& param : funcDecl->getParameters()) {
            llvm::Type* paramType = convertType(param->getType());
            if (!paramType) {
                errorReporter_.error(
                    SourceLocation(1, 1, "unknown"),
                    "Failed to convert parameter type for imported function: " + symbolName
                );
                return;
            }
            paramTypes.push_back(paramType);
        }
        
        // Convert return type
        llvm::Type* returnType = convertType(funcDecl->getReturnType());
        if (!returnType) {
            errorReporter_.error(
                SourceLocation(1, 1, "unknown"),
                "Failed to convert return type for imported function: " + symbolName
            );
            return;
        }
        
        // Create function type and external declaration
        llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
        llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            symbolName,
            module_.get()
        );
        return;
    }
    
    // Try to find interaction in imported module
    ast::InteractionDecl* interactionDecl = moduleResolver_->getInteraction(moduleName, symbolName);
    if (interactionDecl) {
        // Convert parameter types
        std::vector<llvm::Type*> paramTypes;
        for (const auto& param : interactionDecl->getParameters()) {
            llvm::Type* paramType = convertType(param->getType());
            if (!paramType) {
                errorReporter_.error(
                    SourceLocation(1, 1, "unknown"),
                    "Failed to convert parameter type for imported interaction: " + symbolName
                );
                return;
            }
            paramTypes.push_back(paramType);
        }
        
        // Convert return type
        llvm::Type* returnType = convertType(interactionDecl->getReturnType());
        if (!returnType) {
            errorReporter_.error(
                SourceLocation(1, 1, "unknown"),
                "Failed to convert return type for imported interaction: " + symbolName
            );
            return;
        }
        
        // Create function type and external declaration
        llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
        llvm::Function::Create(
            funcType,
            llvm::Function::ExternalLinkage,
            symbolName,
            module_.get()
        );
        return;
    }
    
    // Symbol not found, create placeholder
    std::vector<llvm::Type*> paramTypes;
    llvm::Type* returnType = llvm::Type::getInt64Ty(context_);
    llvm::FunctionType* funcType = llvm::FunctionType::get(returnType, paramTypes, false);
    llvm::Function::Create(
        funcType,
        llvm::Function::ExternalLinkage,
        symbolName,
        module_.get()
    );
}

void IRGenerator::visitTypeDecl(ast::TypeDecl* node) {
    if (!node) {
        return;
    }

    // Type declarations typically don't generate IR directly
    // They define types that are used by other declarations
    // The type information is used during IR generation for other nodes

    // For exported types, we may need to generate type metadata
    // For now, type declarations are handled during semantic analysis
}

void IRGenerator::buildConstructorIndexMap(ast::Program* program) {
    if (!program) return;
    for (const auto& typeDecl : program->getTypeDecls()) {
        if (!typeDecl || !typeDecl->getType()) continue;
        registerADTFromType(typeDecl->getType());
    }
    for (const auto& func : program->getFunctions()) {
        if (!func) continue;
        for (const auto& param : func->getParameters()) {
            if (param && param->getType()) registerADTFromType(param->getType());
        }
        if (func->getReturnType()) registerADTFromType(func->getReturnType());
        for (const auto& stmt : func->getBody()) {
            if (stmt) collectTypesFromStmt(stmt.get());
        }
    }
    for (const auto& interaction : program->getInteractions()) {
        if (!interaction) continue;
        for (const auto& param : interaction->getParameters()) {
            if (param && param->getType()) registerADTFromType(param->getType());
        }
        if (interaction->getReturnType()) registerADTFromType(interaction->getReturnType());
        for (const auto& stmt : interaction->getBody()) {
            if (stmt) collectTypesFromStmt(stmt.get());
        }
    }
}

void IRGenerator::registerADTFromType(ast::Type* type) {
    if (!type) return;
    if (auto* adt = dynamic_cast<ast::ADTType*>(type)) {
        const auto& constructors = adt->getConstructors();
        for (size_t i = 0; i < constructors.size(); ++i) {
            const std::string& name = constructors[i]->getName();
            if (constructorIndexMap_.find(name) == constructorIndexMap_.end()) {
                constructorIndexMap_[name] = std::make_pair(adt, nextConstructorTag_++);
            }
        }
        return;
    }
    if (auto* arr = dynamic_cast<ast::ArrayType*>(type)) {
        if (arr->getElementType()) registerADTFromType(arr->getElementType());
        return;
    }
    if (auto* rec = dynamic_cast<ast::RecordType*>(type)) {
        for (const auto& field : rec->getFields()) {
            if (field && field->getType()) registerADTFromType(field->getType());
        }
        return;
    }
    if (auto* param = dynamic_cast<ast::ParameterizedType*>(type)) {
        // Register the base type's ADT if it's a named type (e.g. List<Int> -> register List's Cons/Nil)
        auto it = typeDeclsMap_.find(param->getBaseName());
        if (it != typeDeclsMap_.end() && it->second)
            registerADTFromType(it->second);
        for (const auto& arg : param->getTypeArgs()) {
            if (arg) registerADTFromType(arg.get());
        }
        return;
    }
    if (auto* ref = dynamic_cast<ast::RefinementType*>(type)) {
        if (ref->getBaseType()) registerADTFromType(ref->getBaseType());
        return;
    }
    if (auto* idx = dynamic_cast<ast::IndexedType*>(type)) {
        if (idx->getBaseType()) registerADTFromType(idx->getBaseType());
        return;
    }
    if (auto* pi = dynamic_cast<ast::DependentFunctionType*>(type)) {
        if (pi->getParamType()) registerADTFromType(pi->getParamType());
        if (pi->getReturnType()) registerADTFromType(pi->getReturnType());
        return;
    }
    if (auto* sigma = dynamic_cast<ast::DependentPairType*>(type)) {
        if (sigma->getVarType()) registerADTFromType(sigma->getVarType());
        if (sigma->getBodyType()) registerADTFromType(sigma->getBodyType());
        return;
    }
    if (auto* forall = dynamic_cast<ast::ForallType*>(type)) {
        if (forall->getBodyType()) registerADTFromType(forall->getBodyType());
        return;
    }
    if (auto* ex = dynamic_cast<ast::ExistentialType*>(type)) {
        if (ex->getVarType()) registerADTFromType(ex->getVarType());
        if (ex->getBodyType()) registerADTFromType(ex->getBodyType());
        return;
    }
    if (auto* func = dynamic_cast<ast::FunctionType*>(type)) {
        if (func->getReturnType()) registerADTFromType(func->getReturnType());
        for (const auto& pt : func->getParamTypes()) {
            if (pt) registerADTFromType(pt.get());
        }
        return;
    }
}

void IRGenerator::collectTypesFromStmt(ast::Stmt* stmt) {
    if (!stmt) return;
    if (auto* varDecl = dynamic_cast<ast::VariableDecl*>(stmt)) {
        if (varDecl->getType()) registerADTFromType(varDecl->getType());
        return;
    }
    if (auto* exprStmt = dynamic_cast<ast::ExprStmt*>(stmt)) {
        (void)exprStmt;
        return;
    }
    if (auto* retStmt = dynamic_cast<ast::ReturnStmt*>(stmt)) {
        (void)retStmt;
        return;
    }
    if (auto* selectStmt = dynamic_cast<ast::SelectStmt*>(stmt)) {
        for (const auto& branch : selectStmt->getBranches()) {
            if (branch && branch->getStatement()) collectTypesFromStmt(branch->getStatement());
        }
        return;
    }
    if (auto* ifStmt = dynamic_cast<ast::IfStmt*>(stmt)) {
        if (ifStmt->getThenBranch()) collectTypesFromStmt(ifStmt->getThenBranch());
        if (ifStmt->getElseBranch()) collectTypesFromStmt(ifStmt->getElseBranch());
        return;
    }
    if (auto* forIn = dynamic_cast<ast::ForInStmt*>(stmt)) {
        for (const auto& s : forIn->getBody()) {
            if (s) collectTypesFromStmt(s.get());
        }
        return;
    }
}

std::pair<ast::ADTType*, size_t> IRGenerator::getConstructorIndex(const std::string& name) const {
    auto it = constructorIndexMap_.find(name);
    if (it != constructorIndexMap_.end()) return it->second;
    // Unknown constructor: deterministic tag per name so different names never collide
    size_t tag = std::hash<std::string>{}(name) % 1024;
    return {nullptr, tag};
}

} // namespace ir
} // namespace first
