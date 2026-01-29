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
#include <llvm/Support/Alignment.h>
#include <llvm/Support/raw_ostream.h>
#include <iostream>

namespace first {
namespace ir {

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
    buildConstructorIndexMap(node);

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
    
    // Convert parameter types
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : node->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
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
    
    // Create function
    llvm::Function* func = llvm::Function::Create(
        funcType,
        linkage,
        funcName,
        module_.get()
    );

    // Phase 6.2: Pure function optimization - mark as readonly (no writes; enables CSE, etc.)
    func->addFnAttr(llvm::Attribute::ReadOnly);
    // Phase 6.2: Immutability - mark pointer parameters as readonly
    for (auto& arg : func->args()) {
        if (arg.getType()->isPointerTy()) {
            arg.addAttr(llvm::Attribute::ReadOnly);
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
            // Allocate space for parameter
            llvm::Type* paramType = arg.getType();
            llvm::AllocaInst* alloca = createAlloca(paramType, paramName);
            // Avoid DataLayout-driven alignment computation (can hang with some LLVM builds).
            builder_->CreateAlignedStore(&arg, alloca, llvm::Align(8));
            setVariable(paramName, alloca, paramType);
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
                errorReporter_.error(
                    node->getLocation(),
                    "Function " + funcName + " missing return statement"
                );
            }
        }
    }
    
    exitFunction();
}

void IRGenerator::visitInteractionDecl(ast::InteractionDecl* node) {
    // Interactions are similar to functions but allow side effects
    // For now, generate IR the same way as functions
    // Get function name
    std::string funcName = node->getName();
    
    // Convert parameter types
    std::vector<llvm::Type*> paramTypes;
    for (const auto& param : node->getParameters()) {
        llvm::Type* paramType = convertType(param->getType());
        if (!paramType) {
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
            // Allocate space for parameter
            llvm::Type* paramType = arg.getType();
            llvm::AllocaInst* alloca = createAlloca(paramType, paramName);
            builder_->CreateAlignedStore(&arg, alloca, llvm::Align(8));
            setVariable(paramName, alloca, paramType);
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
                errorReporter_.error(
                    node->getLocation(),
                    "Interaction " + funcName + " missing return statement"
                );
            }
        }
    }
    
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
    currentValue_ = evaluateFunctionCall(node);
    
    // TODO: Add closure detection - if the function name refers to a closure variable,
    // use invokeClosure instead
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
    // async expr: evaluate operand (stub: pass through; full impl would wrap in Promise)
    currentValue_ = evaluateExpr(node->getOperand());
}

void IRGenerator::visitAwaitExpr(ast::AwaitExpr* node) {
    // await expr: evaluate operand (stub: pass through; full impl would unwrap Promise)
    currentValue_ = evaluateExpr(node->getOperand());
}

void IRGenerator::visitSpawnExpr(ast::SpawnExpr* node) {
    // spawn expr: evaluate operand (stub: pass through; full impl would run in task)
    currentValue_ = evaluateExpr(node->getOperand());
}

void IRGenerator::visitJoinExpr(ast::JoinExpr* node) {
    // join expr: evaluate operand (stub: pass through; full impl would wait for Task)
    currentValue_ = evaluateExpr(node->getOperand());
}

void IRGenerator::visitSelectExpr(ast::SelectExpr* node) {
    // select { branches }: stub - run first branch's statement, return unit
    const auto& branches = node->getBranches();
    if (!branches.empty() && branches[0]->getStatement()) {
        generateStatement(branches[0]->getStatement());
    }
    currentValue_ = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
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
        // Generic type parameter - this should have been substituted by now
        // If we encounter it, it means monomorphization didn't happen
        errorReporter_.error(
            type->getLocation(),
            "Generic type parameter '" + genType->getName() + "' not substituted"
        );
        return nullptr;
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
        return llvm::StructType::get(context_, {varTy, bodyTy});
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
        return llvm::StructType::get(context_, {varTy, bodyTy});
    }
    
    // TODO: Handle other types (FunctionType, etc.)
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
    
    // For other parameterized types (Option<T>, Result<T, E>, etc.),
    // we need to look up the type definition and instantiate it
    // TODO: Look up type definition from symbol table or type registry
    // For now, we'll treat them as ADTs (tagged unions)
    
    // Check if all type arguments are concrete (no generic parameters)
    bool allConcrete = true;
    for (const auto& arg : typeArgs) {
        if (dynamic_cast<ast::GenericType*>(arg.get())) {
            allConcrete = false;
            break;
        }
    }
    
    if (!allConcrete) {
        errorReporter_.error(
            type->getLocation(),
            "Cannot instantiate parameterized type '" + baseName + 
            "' with generic type parameters - type arguments must be concrete"
        );
        return nullptr;
    }
    
    // For now, treat parameterized types as ADTs (tagged unions)
    // This is a simplified approach - full implementation would:
    // 1. Look up the type definition
    // 2. Substitute type parameters in the definition
    // 3. Generate the appropriate LLVM type
    
    // Use the same structure as ADTType: { tag: i64, payload: i8* }
    llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
    llvm::Type* payloadType = llvm::PointerType::get(context_, 0);
    std::vector<llvm::Type*> structFields = {tagType, payloadType};
    
    // Create a unique struct type name based on the parameterized type
    // This ensures different instantiations get different types
    std::string structName = baseName;
    for (const auto& arg : typeArgs) {
        structName += "_";
        // Generate a simple name for the type argument
        if (auto* prim = dynamic_cast<ast::PrimitiveType*>(arg.get())) {
            switch (prim->getKind()) {
                case ast::PrimitiveType::Kind::Int: structName += "Int"; break;
                case ast::PrimitiveType::Kind::Float: structName += "Float"; break;
                case ast::PrimitiveType::Kind::Bool: structName += "Bool"; break;
                case ast::PrimitiveType::Kind::String: structName += "String"; break;
                case ast::PrimitiveType::Kind::Unit: structName += "Unit"; break;
            }
        } else {
            structName += "T";
        }
    }
    
    // Create or get the struct type
    llvm::StructType* structType = llvm::StructType::create(context_, structFields, structName);
    
    return structType;
}

llvm::Type* IRGenerator::convertRecordType(ast::RecordType* type) {
    // Records are represented as LLVM struct types
    // Convert each field type and create a struct
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
    
    // Create struct type
    return llvm::StructType::get(context_, fieldTypes);
}

llvm::Type* IRGenerator::convertADTType(ast::ADTType* type) {
    // ADTs are represented as tagged unions
    // Structure: { tag: i64, payload: union of all constructor argument types }
    // For simplicity, we'll use a struct with tag and a single payload pointer
    
    // Tag type (constructor index)
    llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
    
    // Payload type - for now, use i8* as opaque pointer
    // TODO: Properly compute union of all constructor argument types
    llvm::Type* payloadType = llvm::PointerType::get(context_, 0);
    
    // Create struct: { tag, payload }
    std::vector<llvm::Type*> structFields = {tagType, payloadType};
    return llvm::StructType::get(context_, structFields);
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
        case ast::BinaryExpr::Op::Eq:
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
            }
            break;
        case ast::BinaryExpr::Op::Ne:
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
            }
            break;
        case ast::BinaryExpr::Op::Lt:
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
            }
            break;
        case ast::BinaryExpr::Op::Le:
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
            }
            break;
        case ast::BinaryExpr::Op::Gt:
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
            }
            break;
        case ast::BinaryExpr::Op::Ge:
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
            }
            break;
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
        errorReporter_.error(
            expr->getLocation(),
            "Undefined variable: " + name
        );
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
        // This is a record variable - return the alloca directly
        // Cast to opaque ptr to match record representation
        return builder_->CreateBitCast(
            alloca,
            llvm::PointerType::get(context_, 0),
            name + "_ptr"
        );
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

llvm::Value* IRGenerator::evaluateFunctionCall(ast::FunctionCallExpr* expr, bool tailCall) {
    std::string funcName = expr->getName();

    // Evaluate arguments first (so we can synthesize an extern declaration if needed).
    std::vector<llvm::Value*> args;
    const auto& argExprs = expr->getArgs();
    for (const auto& argExpr : argExprs) {
        llvm::Value* argValue = evaluateExpr(argExpr.get());
        if (!argValue) {
            return nullptr;
        }
        args.push_back(argValue);
    }

    // Phase 7.3: arrayLength is compiler intrinsic (metadata lookup)
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

    // Look up function in module
    llvm::Function* func = module_->getFunction(funcName);

    // Stdlib (Phase 7.3): map First name -> C symbol and signature when not in module
    auto getStdlibSig = [&](const std::string& name) -> std::pair<std::string, llvm::FunctionType*> {
        if (name == "print" || name == "println")
            return {name, llvm::FunctionType::get(voidTy, {i8ptr}, false)};
        if (name == "readLine")
            return {"readLine", llvm::FunctionType::get(i8ptr, false)};
        if (name == "readFile")
            return {"readFile", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (name == "writeFile")
            return {"writeFile", llvm::FunctionType::get(voidTy, {i8ptr, i8ptr}, false)};
        if (name == "sin" || name == "cos" || name == "sqrt" || name == "abs" || name == "floor" || name == "ceil")
            return {"first_" + name, llvm::FunctionType::get(f64, {f64}, false)};
        if (name == "min" || name == "max")
            return {"first_" + name, llvm::FunctionType::get(f64, {f64, f64}, false)};
        if (name == "minInt" || name == "maxInt")
            return {"first_" + name, llvm::FunctionType::get(i64, {i64, i64}, false)};
        if (name == "stringLength") return {"first_string_length", llvm::FunctionType::get(i64, {i8ptr}, false)};
        if (name == "stringConcat") return {"first_string_concat", llvm::FunctionType::get(i8ptr, {i8ptr, i8ptr}, false)};
        if (name == "stringSlice") return {"first_string_slice", llvm::FunctionType::get(i8ptr, {i8ptr, i64, i64}, false)};
        if (name == "stringToInt") return {"first_string_to_int", llvm::FunctionType::get(i64, {i8ptr}, false)};
        if (name == "stringToFloat") return {"first_string_to_float", llvm::FunctionType::get(f64, {i8ptr}, false)};
        if (name == "intToString") return {"first_int_to_string", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (name == "floatToString") return {"first_float_to_string", llvm::FunctionType::get(i8ptr, {f64}, false)};
        if (name == "socketConnect") return {"first_socket_connect", llvm::FunctionType::get(i64, {i8ptr, i64}, false)};
        if (name == "socketSend") return {"first_socket_send", llvm::FunctionType::get(i64, {i64, i8ptr}, false)};
        if (name == "socketRecv") return {"first_socket_recv_str", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (name == "socketClose") return {"first_socket_close", llvm::FunctionType::get(voidTy, {i64}, false)};
        if (name == "httpGet") return {"first_http_get", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (name == "httpPost") return {"first_http_post", llvm::FunctionType::get(i8ptr, {i8ptr, i8ptr}, false)};
        if (name == "jsonPrettify") return {"first_json_prettify", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (name == "jsonStringifyString") return {"first_json_stringify_string", llvm::FunctionType::get(i8ptr, {i8ptr}, false)};
        if (name == "jsonStringifyInt") return {"first_json_stringify_int", llvm::FunctionType::get(i8ptr, {i64}, false)};
        if (name == "jsonStringifyFloat") return {"first_json_stringify_float", llvm::FunctionType::get(i8ptr, {f64}, false)};
        return {"", nullptr};
    };

    if (!func) {
        auto [cSymbol, funcType] = getStdlibSig(funcName);
        if (funcType) {
            func = module_->getFunction(cSymbol);
            if (!func)
                func = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, cSymbol, module_.get());
        } else {
            // Unknown: default external declaration from args
            std::vector<llvm::Type*> paramTypes;
            for (auto* v : args) paramTypes.push_back(v ? v->getType() : i64);
            llvm::FunctionType* defaultType = llvm::FunctionType::get(i64, paramTypes, false);
            func = llvm::Function::Create(defaultType, llvm::Function::ExternalLinkage, funcName, module_.get());
        }
    }

    if (!func) {
        errorReporter_.error(
            expr->getLocation(),
            "Undefined function: " + funcName
        );
        return nullptr;
    }
    
    // Create call instruction (Phase 6.2: tail call when requested for TCO)
    llvm::Value* callValue = builder_->CreateCall(func, args, "calltmp");
    if (tailCall) {
        if (auto* callInst = llvm::dyn_cast<llvm::CallInst>(callValue)) {
            callInst->setTailCall(true);
        }
    }
    return callValue;
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
    } else if (auto* whileStmt = dynamic_cast<ast::WhileStmt*>(stmt)) {
        generateWhileStmt(whileStmt);
    } else if (auto* assignStmt = dynamic_cast<ast::AssignmentStmt*>(stmt)) {
        generateAssignmentStmt(assignStmt);
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
    
    // If this is an array variable, preserve metadata
    // Check if initValue has array metadata
    auto arrayIt = arrayMetadata_.find(initValue);
    if (arrayIt != arrayMetadata_.end()) {
        // Copy metadata for the stored value
        ArrayMetadata metadata = arrayIt->second;
        metadata.alloca = alloca;
        arrayMetadata_[alloca] = metadata;
    }
    
    // If this is a record variable, preserve metadata
    auto recordIt = recordMetadata_.find(initValue);
    if (recordIt != recordMetadata_.end()) {
        RecordMetadata metadata = recordIt->second;
        metadata.alloca = alloca;
        recordMetadata_[alloca] = metadata;
    }
}

void IRGenerator::generateReturnStmt(ast::ReturnStmt* stmt) {
    ast::Expr* returnExpr = stmt->getValue();
    if (returnExpr) {
        // Phase 6.2: Tail call optimization - emit tail call when return value is a direct call
        llvm::Value* returnValue = nullptr;
        if (auto* callExpr = dynamic_cast<ast::FunctionCallExpr*>(returnExpr)) {
            returnValue = evaluateFunctionCall(callExpr, /*tailCall=*/ true);
        } else {
            returnValue = evaluateExpr(returnExpr);
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

void IRGenerator::visitWhileStmt(ast::WhileStmt* node) {
    generateWhileStmt(node);
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

void IRGenerator::generateWhileStmt(ast::WhileStmt* stmt) {
    if (!stmt || !currentFunction_) {
        return;
    }
    
    llvm::Function* func = currentFunction_;
    
    // Create basic blocks
    llvm::BasicBlock* condBB = llvm::BasicBlock::Create(context_, "whilecond", func);
    llvm::BasicBlock* bodyBB = llvm::BasicBlock::Create(context_, "whilebody", func);
    llvm::BasicBlock* afterBB = llvm::BasicBlock::Create(context_, "whileend", func);
    
    // Branch to condition block
    builder_->CreateBr(condBB);
    
    // Generate condition check
    builder_->SetInsertPoint(condBB);
    llvm::Value* condValue = evaluateExpr(stmt->getCondition());
    if (!condValue) {
        return;
    }
    
    // Convert condition to boolean if needed
    llvm::Type* condType = condValue->getType();
    if (!condType->isIntegerTy(1)) {
        if (condType->isIntegerTy()) {
            condValue = builder_->CreateICmpNE(
                condValue,
                llvm::ConstantInt::get(condType, 0),
                "whilecond"
            );
        } else if (condType->isFloatingPointTy()) {
            condValue = builder_->CreateFCmpONE(
                condValue,
                llvm::ConstantFP::get(condType, 0.0),
                "whilecond"
            );
        } else {
            errorReporter_.error(
                stmt->getLocation(),
                "While condition must be a boolean, integer, or floating-point value"
            );
            return;
        }
    }
    
    // Branch based on condition
    builder_->CreateCondBr(condValue, bodyBB, afterBB);
    
    // Generate loop body
    builder_->SetInsertPoint(bodyBB);
    generateStatement(stmt->getBody());
    // If body doesn't terminate, branch back to condition
    if (!bodyBB->getTerminator()) {
        builder_->CreateBr(condBB);
    }
    
    // Set insert point to after loop
    builder_->SetInsertPoint(afterBB);
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
    llvm::StructType* structType = llvm::StructType::get(context_, fieldTypes);
    
    // Allocate struct on stack
    llvm::AllocaInst* structAlloca = builder_->CreateAlloca(structType, nullptr, "rectmp");
    
    // Store each field value
    for (size_t i = 0; i < fields.size(); ++i) {
        // Get pointer to field i
        llvm::Value* indices[] = {
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
            llvm::ConstantInt::get(context_, llvm::APInt(64, i))
        };
        llvm::Value* fieldPtr = builder_->CreateGEP(structType, structAlloca, indices, "recfield");
        
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
    
    // If not found, try to find it by checking local variables
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
        errorReporter_.error(
            expr->getLocation(),
            "Field '" + fieldName + "' not found in record"
        );
        return nullptr;
    }
    
    // In LLVM opaque-pointer mode, `recordAlloca` is already a pointer to the alloca'd struct.
    // We keep the element type (`structType`) explicit when doing GEPs.
    llvm::Value* typedPtr = recordAlloca;
    
    // Get pointer to field
    llvm::Value* indices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        llvm::ConstantInt::get(context_, llvm::APInt(64, fieldIndex))
    };
    llvm::Value* fieldPtr = builder_->CreateGEP(structType, typedPtr, indices, "recfield");
    
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
    llvm::Type* payloadType = llvm::PointerType::get(context_, 0);
    std::vector<llvm::Type*> structFields = {tagType, payloadType};
    llvm::StructType* adtType = llvm::StructType::get(context_, structFields);
    llvm::AllocaInst* adtAlloca = builder_->CreateAlloca(adtType, nullptr, "adttmp");

    llvm::Value* tagIndices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0))
    };
    llvm::Value* tagPtr = builder_->CreateGEP(adtType, adtAlloca, tagIndices, "tagptr");
    builder_->CreateStore(
        llvm::ConstantInt::get(context_, llvm::APInt(64, tagIndex)),
        tagPtr
    );

    llvm::Value* payloadValue = nullptr;
    if (!argValues.empty()) {
        std::vector<llvm::Type*> fieldTypes;
        for (llvm::Value* v : argValues) fieldTypes.push_back(v->getType());
        llvm::StructType* payloadStructType = llvm::StructType::get(context_, fieldTypes);
        llvm::AllocaInst* payloadAlloca = builder_->CreateAlloca(payloadStructType, nullptr, "payload");
        for (size_t i = 0; i < argValues.size(); ++i) {
            llvm::Value* fieldIndices[] = {
                llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
                llvm::ConstantInt::get(context_, llvm::APInt(64, i))
            };
            llvm::Value* fieldPtr = builder_->CreateGEP(payloadStructType, payloadAlloca, fieldIndices, "fieldptr");
            builder_->CreateStore(argValues[i], fieldPtr);
        }
        payloadValue = builder_->CreateBitCast(
            payloadAlloca,
            llvm::PointerType::get(context_, 0),
            "payloadptr"
        );
    } else {
        payloadValue = llvm::ConstantPointerNull::get(llvm::PointerType::get(context_, 0));
    }

    llvm::Value* payloadIndices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        llvm::ConstantInt::get(context_, llvm::APInt(64, 1))
    };
    llvm::Value* payloadPtr = builder_->CreateGEP(adtType, adtAlloca, payloadIndices, "payloadptr");
    builder_->CreateStore(payloadValue, payloadPtr);

    return builder_->CreateBitCast(
        adtAlloca,
        llvm::PointerType::get(context_, 0),
        "adtptr"
    );
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
    
    // If matchedValue is a pointer (i8*), we may need to load it
    // For now, assume it's already the value we want to match against
    // TODO: Handle different value representations (primitives vs pointers)
    
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
        bindPatternVariables(cases[i]->getPattern(), matchedValue);
        
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
        resultBlocks.push_back(bodyBB); // Track the block where this result was generated
        builder_->CreateBr(mergeBB);
    }
    
    // Default case: no pattern matched
    builder_->SetInsertPoint(defaultBB);
    if (!hasExplicitCatchAll) {
        errorReporter_.error(
            expr->getLocation(),
            "Match expression: no pattern matched (non-exhaustive match)"
        );
    }
    // Return default value based on expected return type
    // TODO: Determine return type from context
    llvm::Value* defaultResult = llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    caseResults.push_back(defaultResult);
    builder_->CreateBr(mergeBB);
    
    // Merge block: PHI node selects result
    builder_->SetInsertPoint(mergeBB);
    
    // Determine result type from first case result
    if (caseResults.empty() || !caseResults[0]) {
        errorReporter_.error(
            expr->getLocation(),
            "Match expression: failed to generate case results"
        );
        return nullptr;
    }
    
    llvm::Type* resultType = caseResults[0]->getType();
    llvm::PHINode* phi = builder_->CreatePHI(resultType, caseResults.size() + 1, "matchresult");
    
    // Add incoming values from result blocks (match blocks or guard pass blocks)
    for (size_t i = 0; i < resultBlocks.size(); ++i) {
        phi->addIncoming(caseResults[i], resultBlocks[i]);
    }
    // Add default result
    phi->addIncoming(defaultResult, defaultBB);
    
    return phi;
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
        auto it = recordMetadata_.find(value);
        if (it == recordMetadata_.end()) {
            // Try to find metadata via local variable allocas (same fallback as evaluateFieldAccess)
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

        llvm::Value* recPtr = builder_->CreateBitCast(value, llvm::PointerType::get(context_, 0), "recptr");

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
            llvm::Value* idxs[] = {
                llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
                llvm::ConstantInt::get(context_, llvm::APInt(64, fieldIndex))
            };
            llvm::Value* fieldPtr = builder_->CreateGEP(structType, recPtr, idxs, "recfieldptr");
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
        llvm::StructType* adtType = llvm::StructType::get(context_, structFields);
        
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
        
        // Load tag
        llvm::Value* tagIndices[] = {
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0))
        };
        llvm::Value* tagPtr = builder_->CreateGEP(adtType, adtPtr, tagIndices, "tagptr");
        llvm::Value* tag = builder_->CreateLoad(tagType, tagPtr, "tag");

        const std::string& constructorName = constructorPattern->getConstructorName();
        auto [adtAstType, tagIndex] = getConstructorIndex(constructorName);
        llvm::Value* expectedTag = llvm::ConstantInt::get(context_, llvm::APInt(64, tagIndex));
        (void)adtAstType;
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

void IRGenerator::bindPatternVariables(ast::Pattern* pattern, llvm::Value* value) {
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
    }
    else if (auto* recordPattern = dynamic_cast<ast::RecordPattern*>(pattern)) {
        auto it = recordMetadata_.find(value);
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
            llvm::Value* idxs[] = {
                llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
                llvm::ConstantInt::get(context_, llvm::APInt(64, fieldIndex))
            };
            llvm::Value* fieldPtr = builder_->CreateGEP(structType, recPtr, idxs, "recfieldptr");
            llvm::Value* fieldVal = builder_->CreateLoad(fieldTy, fieldPtr, "recfieldval");
            bindPatternVariables(fp.pattern.get(), fieldVal);
        }
    }
    else if (auto* constructorPattern = dynamic_cast<ast::ConstructorPattern*>(pattern)) {
        const auto& argPatterns = constructorPattern->getArguments();
        if (argPatterns.empty()) return;

        llvm::Type* tagType = llvm::Type::getInt64Ty(context_);
        llvm::Type* payloadPtrType = llvm::PointerType::get(context_, 0);
        std::vector<llvm::Type*> structFields = {tagType, payloadPtrType};
        llvm::StructType* adtType = llvm::StructType::get(context_, structFields);

        llvm::Value* adtPtr = nullptr;
        if (value->getType()->isPointerTy()) {
            adtPtr = builder_->CreateBitCast(value, llvm::PointerType::get(context_, 0), "adtptr");
        } else {
            errorReporter_.error(
                pattern->getLocation(),
                "Pattern matching: constructor pattern requires ADT pointer"
            );
            return;
        }

        llvm::Value* payloadIndices[] = {
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
            llvm::ConstantInt::get(context_, llvm::APInt(64, 1))
        };
        llvm::Value* payloadPtr = builder_->CreateGEP(adtType, adtPtr, payloadIndices, "payloadptr");
        llvm::Value* payload = builder_->CreateLoad(payloadPtrType, payloadPtr, "payload");

        auto [adtAstType, tagIndex] = getConstructorIndex(constructorPattern->getConstructorName());
        (void)tagIndex;
        if (!adtAstType || adtAstType->getConstructors().size() <= 0) {
            bindPatternVariables(argPatterns[0].get(), payload);
            return;
        }
        ast::Constructor* constructor = nullptr;
        for (const auto& c : adtAstType->getConstructors()) {
            if (c->getName() == constructorPattern->getConstructorName()) {
                constructor = c.get();
                break;
            }
        }
        if (!constructor || constructor->getArgumentTypes().size() != argPatterns.size()) {
            if (argPatterns.size() == 1) {
                bindPatternVariables(argPatterns[0].get(), payload);
            } else {
                std::vector<llvm::Type*> fieldTypes;
                for (size_t j = 0; j < argPatterns.size(); ++j)
                    fieldTypes.push_back(llvm::PointerType::get(context_, 0));
                llvm::StructType* payloadStructType = llvm::StructType::get(context_, fieldTypes);
                llvm::Value* structPtr = builder_->CreateBitCast(payload, llvm::PointerType::get(context_, 0), "structptr");
                for (size_t j = 0; j < argPatterns.size(); ++j) {
                    llvm::Value* fieldIndices[] = {
                        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
                        llvm::ConstantInt::get(context_, llvm::APInt(64, j))
                    };
                    llvm::Value* fieldPtr = builder_->CreateGEP(payloadStructType, structPtr, fieldIndices, "fieldptr");
                    llvm::Value* fieldValue = builder_->CreateLoad(fieldTypes[j], fieldPtr, "fieldval");
                    bindPatternVariables(argPatterns[j].get(), fieldValue);
                }
            }
            return;
        }

        const auto& argTypes = constructor->getArgumentTypes();
        if (argPatterns.size() == 1) {
            llvm::Type* fieldType = convertType(argTypes[0].get());
            if (fieldType && payload->getType()->isPointerTy()) {
                llvm::Value* casted = builder_->CreateBitCast(payload, llvm::PointerType::get(context_, 0), "payloadcast");
                llvm::Value* loaded = builder_->CreateLoad(fieldType, casted, "payloadval");
                bindPatternVariables(argPatterns[0].get(), loaded);
            } else {
                bindPatternVariables(argPatterns[0].get(), payload);
            }
        } else {
            std::vector<llvm::Type*> fieldTypes;
            for (const auto& t : argTypes) {
                llvm::Type* lt = convertType(t.get());
                fieldTypes.push_back(lt ? lt : llvm::PointerType::get(context_, 0));
            }
            llvm::StructType* payloadStructType = llvm::StructType::get(context_, fieldTypes);
            llvm::Value* structPtr = builder_->CreateBitCast(payload, llvm::PointerType::get(context_, 0), "structptr");
            for (size_t j = 0; j < argPatterns.size(); ++j) {
                llvm::Value* fieldIndices[] = {
                    llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
                    llvm::ConstantInt::get(context_, llvm::APInt(64, j))
                };
                llvm::Value* fieldPtr = builder_->CreateGEP(payloadStructType, structPtr, fieldIndices, "fieldptr");
                llvm::Type* ft = fieldTypes[j];
                llvm::Value* fieldValue = builder_->CreateLoad(ft, fieldPtr, "fieldval");
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
    
    // Restore function context
    currentFunction_ = oldFunction;
    
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
    
    llvm::StructType* envType = llvm::StructType::get(context_, envTypes);
    
    // Build closure struct: { function pointer, environment }
    std::vector<llvm::Type*> closureTypes = {
        llvm::PointerType::get(context_, 0), // Function pointer (opaque ptr)
        llvm::PointerType::get(context_, 0)  // Environment pointer (opaque ptr)
    };
    llvm::StructType* closureType = llvm::StructType::get(context_, closureTypes);
    
    // Allocate closure on stack
    llvm::AllocaInst* closureAlloca = builder_->CreateAlloca(closureType, nullptr, "closure");
    
    // Store function pointer (cast function to i8*)
    llvm::Value* funcPtr = builder_->CreateBitCast(
        func,
        llvm::PointerType::get(context_, 0),
        "funcptr"
    );
    
    llvm::Value* funcPtrIndices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0))
    };
    llvm::Value* funcPtrPtr = builder_->CreateGEP(closureType, closureAlloca, funcPtrIndices, "funcptrptr");
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
                    
                    llvm::Value* fieldIndices[] = {
                        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
                        llvm::ConstantInt::get(context_, llvm::APInt(64, i))
                    };
                    llvm::Value* fieldPtr = builder_->CreateGEP(envType, envAlloca, fieldIndices, "fieldptr");
                    builder_->CreateStore(value, fieldPtr);
                }
            }
        }
        
        // Store environment pointer in closure
        llvm::Value* envPtrIndices[] = {
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
            llvm::ConstantInt::get(context_, llvm::APInt(64, 1))
        };
        llvm::Value* envPtrPtr = builder_->CreateGEP(closureType, closureAlloca, envPtrIndices, "envptrptr");
        builder_->CreateStore(envAlloca, envPtrPtr);
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
    llvm::StructType* closureType = llvm::StructType::get(context_, closureStructFields);
    
    // Cast closure to a pointer for GEPs (opaque pointer mode)
    llvm::Value* closurePtr = builder_->CreateBitCast(
        closure,
        llvm::PointerType::get(context_, 0),
        "closurestructptr"
    );
    
    // Load function pointer
    llvm::Value* funcPtrIndices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0))
    };
    llvm::Value* funcPtrPtr = builder_->CreateGEP(closureType, closurePtr, funcPtrIndices, "funcptrptr");
    llvm::Value* funcPtr = builder_->CreateLoad(funcPtrType, funcPtrPtr, "funcptr");
    
    // Load environment pointer
    llvm::Value* envPtrIndices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        llvm::ConstantInt::get(context_, llvm::APInt(64, 1))
    };
    llvm::Value* envPtrPtr = builder_->CreateGEP(closureType, closurePtr, envPtrIndices, "envptrptr");
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
        // Get pointer to element i
        llvm::Value* indices[] = {
            llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
            llvm::ConstantInt::get(context_, llvm::APInt(64, i))
        };
        llvm::Value* elemPtr = builder_->CreateGEP(arrayType, arrayAlloca, indices, "arrelem");
        
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
    // arrayValue might be a direct array pointer (i8*) or an alloca
    auto it = arrayMetadata_.find(arrayValue);
    
    // If not found, arrayValue might be a bitcast from an alloca
    // Try to find the original alloca by checking local variables
    if (it == arrayMetadata_.end()) {
        // Check if arrayValue is a bitcast that came from a variable
        // We need to trace back: if arrayValue is i8*, it might have been
        // created from an alloca stored in a variable
        for (const auto& varPair : localVars_) {
            llvm::Value* varAlloca = varPair.second;
            auto varIt = arrayMetadata_.find(varAlloca);
            if (varIt != arrayMetadata_.end()) {
                // Found array variable - use its metadata
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
    
    // Get pointer to element
    llvm::Value* indices[] = {
        llvm::ConstantInt::get(context_, llvm::APInt(64, 0)),
        indexValue
    };
    llvm::Value* elemPtr = builder_->CreateGEP(arrayType, typedPtr, indices, "arrelem");
    
    // Load element value
    llvm::Value* elemValue = builder_->CreateLoad(elementType, elemPtr, "arrelemval");
    builder_->CreateBr(continueBB);
    
    // Out of bounds: error (for now, return default value)
    builder_->SetInsertPoint(outOfBoundsBB);
    llvm::Value* defaultValue = nullptr;
    if (elementType->isIntegerTy(64)) {
        defaultValue = llvm::ConstantInt::get(context_, llvm::APInt(64, 0));
    } else if (elementType->isDoubleTy()) {
        defaultValue = llvm::ConstantFP::get(context_, llvm::APFloat(0.0));
    } else if (elementType->isIntegerTy(1)) {
        defaultValue = llvm::ConstantInt::get(context_, llvm::APInt(1, 0));
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
        substitutions[genericParams[i]] = typeArgs[i];
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
    
    // TODO: Generate function body with type substitutions
    // For now, we'll just create the function signature
    // Full implementation would need to:
    // 1. Clone function body
    // 2. Substitute types throughout the body
    // 3. Generate IR for the specialized body
    
    return monoFunc;
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
                // Import all exported symbols
                std::vector<std::string> exportedSymbols = moduleResolver_->getExportedSymbols(moduleName);
                for (const auto& symbolName : exportedSymbols) {
                    // Generate external function declaration
                    // TODO: Look up function signature from imported module
                    // For now, we'll generate a placeholder
                    generateExternalDeclaration(symbolName, moduleName);
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
    // Generate an external function declaration
    // This allows the current module to call functions from the imported module
    
    // Check if declaration already exists
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
                constructorIndexMap_[name] = std::make_pair(adt, i);
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
    if (auto* whileStmt = dynamic_cast<ast::WhileStmt*>(stmt)) {
        if (whileStmt->getBody()) collectTypesFromStmt(whileStmt->getBody());
        return;
    }
}

std::pair<ast::ADTType*, size_t> IRGenerator::getConstructorIndex(const std::string& name) const {
    auto it = constructorIndexMap_.find(name);
    if (it == constructorIndexMap_.end()) return {nullptr, 0};
    return it->second;
}

} // namespace ir
} // namespace first
