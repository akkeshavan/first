 #pragma once

 #include "first/source_location.h"
 #include <string>

 namespace first {
 namespace parser {

 // Token kinds used by the custom parser. This is a thin abstraction
 // over the underlying ANTLR token types but intentionally decoupled
 // from ANTLR headers so the parser logic does not depend on them.
 enum class TokenKind {
     EndOfFile,
     Identifier,
     IntLiteral,
     FloatLiteral,
     StringLiteral,

     // Keywords
     KwFunction,
     KwInteraction,
     KwIf,
     KwElse,
     KwWhile,
     KwReturn,
     KwLet,
     KwVar,
     KwMut,
     KwMatch,
     KwType,
     KwInterface,
     KwImplementation,
     KwImport,
     KwExport,
     KwExtends,
    KwModule,
    KwDo,
    KwWhere,
    KwForall,
    KwExists,
    KwAsync,
    KwAwait,
    KwSpawn,
    KwJoin,
    KwSelect,

    // Primitive types / builtins
     KwInt,
     KwFloat,
     KwBool,
     KwString,
     KwUnit,
     KwTrue,
     KwFalse,
     KwNull,

    // Operators
     OpLe,
     OpGe,
     OpEq,
     OpNe,
     OpLt,
     OpGt,
     OpAnd,
     OpOr,
     OpNot,
     OpPlus,
     OpMinus,
     OpMul,
     OpDiv,
     OpMod,
     OpAssign,
     OpPlusAssign,
     OpMinusAssign,
     OpMulAssign,
     OpDivAssign,
     OpArrow,
     OpFatArrow,
     OpRange,
     OpRangeInclusive,

    // Monadic / functional operators (for interactions)
    OpBind,   // >>=
    OpThen,   // >>
    OpFmap,   // <$>
    OpApply,  // <*>

    // Delimiters / punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LRefinement,  // {{
    RRefinement,  // }}
     LBracket,
     RBracket,
     Semicolon,
     Comma,
     Colon,
     Dot,
 };

 struct Token {
     TokenKind kind;
     std::string lexeme;
     first::SourceLocation loc;
 };

 } // namespace parser
 } // namespace first

