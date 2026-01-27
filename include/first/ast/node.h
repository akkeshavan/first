#pragma once

#include "first/source_location.h"
#include <string>
#include <memory>

namespace first {
namespace ast {

// Forward declarations
class ASTVisitor;

// Base class for all AST nodes
class ASTNode {
public:
    ASTNode(const SourceLocation& location) : location_(location) {}
    virtual ~ASTNode() = default;

    // Get source location
    const SourceLocation& getLocation() const { return location_; }
    
    // Visitor pattern for AST traversal
    virtual void accept(ASTVisitor& visitor) = 0;
    
    // Get node type name for debugging
    virtual std::string getNodeType() const = 0;

protected:
    SourceLocation location_;
};

} // namespace ast
} // namespace first
