#pragma once

#include "first/ast/node.h"
#include "first/ast/visitor.h"
#include "first/ast/declarations.h"
#include <vector>
#include <memory>
#include <string>

namespace first {
namespace ast {

// Top-level program node
class Program : public ASTNode {
public:
    Program(const SourceLocation& location, const std::string& moduleName = "")
        : ASTNode(location), moduleName_(moduleName) {}

    void addFunction(std::unique_ptr<FunctionDecl> func) {
        functions_.push_back(std::move(func));
    }

    void addInteraction(std::unique_ptr<InteractionDecl> interaction) {
        interactions_.push_back(std::move(interaction));
    }

    void addTypeDecl(std::unique_ptr<TypeDecl> typeDecl) {
        typeDecls_.push_back(std::move(typeDecl));
    }

    void addImport(std::unique_ptr<ImportDecl> import) {
        imports_.push_back(std::move(import));
    }
    
    void setModuleName(const std::string& name) { moduleName_ = name; }
    const std::string& getModuleName() const { return moduleName_; }

    const std::vector<std::unique_ptr<FunctionDecl>>& getFunctions() const { return functions_; }
    const std::vector<std::unique_ptr<InteractionDecl>>& getInteractions() const { return interactions_; }
    const std::vector<std::unique_ptr<TypeDecl>>& getTypeDecls() const { return typeDecls_; }
    const std::vector<std::unique_ptr<ImportDecl>>& getImports() const { return imports_; }
    
    void accept(ASTVisitor& visitor) override {
        visitor.visitProgram(this);
    }
    
    std::string getNodeType() const override { return "Program"; }

private:
    std::string moduleName_;
    std::vector<std::unique_ptr<FunctionDecl>> functions_;
    std::vector<std::unique_ptr<InteractionDecl>> interactions_;
    std::vector<std::unique_ptr<TypeDecl>> typeDecls_;
    std::vector<std::unique_ptr<ImportDecl>> imports_;
};

} // namespace ast
} // namespace first
