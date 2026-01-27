#include "first/ast/statements.h"
#include "first/ast/types.h"
#include "first/ast/expressions.h"
#include "first/ast/program.h"
#include "first/source_location.h"
#include "test_framework.h"

TEST(ast_variable_decl) {
    first::SourceLocation loc(1, 1);
    
    auto type = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Int
    );
    auto init = std::make_unique<first::ast::LiteralExpr>(
        loc, first::ast::LiteralExpr::LiteralType::Int, "42"
    );
    
    first::ast::VariableDecl decl(
        loc,
        "x",
        first::ast::VariableDecl::Mutability::Immutable,
        std::move(type),
        std::move(init)
    );
    
    ASSERT_EQ(decl.getName(), "x", "Variable name should be 'x'");
    bool isImmutable = (decl.getMutability() == first::ast::VariableDecl::Mutability::Immutable);
    ASSERT(isImmutable, "Should be immutable");
    ASSERT_NE(decl.getType(), nullptr, "Type should not be null");
    ASSERT_NE(decl.getInitializer(), nullptr, "Initializer should not be null");
}

TEST(ast_primitive_type) {
    first::SourceLocation loc(1, 1);
    
    first::ast::PrimitiveType intType(loc, first::ast::PrimitiveType::Kind::Int);
    bool isInt = (intType.getKind() == first::ast::PrimitiveType::Kind::Int);
    ASSERT(isInt, "Should be Int type");
    
    first::ast::PrimitiveType floatType(loc, first::ast::PrimitiveType::Kind::Float);
    bool isFloat = (floatType.getKind() == first::ast::PrimitiveType::Kind::Float);
    ASSERT(isFloat, "Should be Float type");
}

TEST(ast_array_type) {
    first::SourceLocation loc(1, 1);
    
    auto elementType = std::make_unique<first::ast::PrimitiveType>(
        loc, first::ast::PrimitiveType::Kind::Int
    );
    
    first::ast::ArrayType arrayType(loc, std::move(elementType));
    ASSERT_NE(arrayType.getElementType(), nullptr, "Element type should not be null");
}

TEST(ast_program_node) {
    first::SourceLocation loc(1, 1);
    
    first::ast::Program program(loc);
    
    ASSERT_EQ(program.getFunctions().size(), 0, "Should start with no functions");
    ASSERT_EQ(program.getInteractions().size(), 0, "Should start with no interactions");
    ASSERT_EQ(program.getTypeDecls().size(), 0, "Should start with no type declarations");
    ASSERT_EQ(program.getImports().size(), 0, "Should start with no imports");
    ASSERT_EQ(program.getNodeType(), "Program", "Node type should be Program");
}
