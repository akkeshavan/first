/**
 * ANTLR4 Grammar for the First Programming Language
 * 
 * Based on the First Language Specification
 * This grammar defines the complete syntax for the First language including:
 * - Function and interaction declarations
 * - Type system (refinement types, dependent types, etc.)
 * - Expressions and operators with proper precedence
 * - Control flow statements
 * - Pattern matching
 * - Module system
 * 
 * Operator Precedence (lowest to highest):
 * 1. || (logical or)
 * 2. && (logical and)
 * 3. == != (equality)
 * 4. < <= > >= (comparison)
 * 5. + - (addition/subtraction)
 * 6. * / % (multiplication/division/modulo)
 * 7. >>= >> <$> <*> (monadic operators)
 * 8. Postfix: . [] () (field access, array access, function call)
 */
grammar First;

// ============================================================================
// LEXER RULES
// ============================================================================

// Comments
LINE_COMMENT: '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' .*? '*/' -> skip;

// Whitespace
WS: [ \t\r\n]+ -> skip;

// Keywords
FUNCTION: 'function';
INTERACTION: 'interaction';
IF: 'if';
ELSE: 'else';
RETURN: 'return';
LET: 'let';
VAR: 'var';
MUT: 'mut';
MATCH: 'match';
TYPE: 'type';
INTERFACE: 'interface';
IMPLEMENTATION: 'implementation';
IMPORT: 'import';
EXPORT: 'export';
EXTENDS: 'extends';
MODULE: 'module';
DO: 'do';
ASYNC: 'async';
AWAIT: 'await';
SPAWN: 'spawn';
JOIN: 'join';
SELECT: 'select';
FORALL: 'forall';
KIND: 'kind';
WHERE: 'where';
WHEN: 'when';
CONSTRAINT: 'Constraint';
EXISTS: 'exists';
SINGLETON: 'Singleton';
FOR: 'for';
IN: 'in';
RANGE: 'range';

// Type keywords
INT: 'Int';
FLOAT: 'Float';
BOOL: 'Bool';
STRING: 'String';
UNIT: 'Unit';
ARRAY: 'Array';

// Boolean literals
TRUE: 'true';
FALSE: 'false';
NULL: 'null';

// Collection type keywords
HASHMAP: 'HashMap';
VECTOR: 'Vector';
SET: 'Set';
LINKEDLIST: 'LinkedList';
DEQUE: 'Deque';
BTREEMAP: 'BTreeMap';
BTREESET: 'BTreeSet';
PRIORITYQUEUE: 'PriorityQueue';

// Error handling keywords
TRY: 'try';
CATCH: 'catch';
FINALLY: 'finally';
THROW: 'throw';
// Note: Result, Ok, Err, Option, Some, None are regular identifiers, not keywords

// Concurrency keywords
PROMISE: 'Promise';
CHANNEL: 'Channel';
TASK: 'Task';

// Identifiers
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;

// Literals
INT_LITERAL: [0-9]+;
FLOAT_LITERAL: [0-9]+ '.' [0-9]+ ([eE] [+-]? [0-9]+)?;
STRING_LITERAL: '"' (~["\\\r\n] | '\\' .)* '"';

// Operators (multi-character first, then single character)
// Comparison
LE: '<=';
GE: '>=';
EQ: '==';
NE: '!=';
LT: '<';
GT: '>';

// Logical
AND: '&&';
OR: '||';
NOT: '!';

// Arithmetic
PLUS: '+';
MINUS: '-';
MUL: '*';
DIV: '/';
MOD: '%';

// Assignment
ASSIGN: '=';
PLUS_ASSIGN: '+=';
MINUS_ASSIGN: '-=';
MUL_ASSIGN: '*=';
DIV_ASSIGN: '/=';

// Arrows and type operators
ARROW: '->';
FAT_ARROW: '=>';
DOUBLE_COLON: '::';

// Monadic operators (for interactions)
BIND: '>>=';
THEN: '>>';
FMAP: '<$>';
APPLY: '<*>';

// Composition
PIPE: '|>';

// Range operators
RANGE_OP: '..';
RANGE_INCLUSIVE: '..=';

// Type operators
INTERSECTION: '&';
// Note: SIGMA (*) for dependent pairs is handled in parser, not lexer
// to avoid conflict with MUL operator

// Delimiters
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
LBRACKET: '[';
RBRACKET: ']';
SEMICOLON: ';';
COMMA: ',';
COLON: ':';
DOT: '.';
PIPE_DELIM: '|';
QUESTION: '?';

// Refinement type delimiters
LREFINEMENT: '{{';
RREFINEMENT: '}}';

// ============================================================================
// PARSER RULES
// ============================================================================

// Top-level program
program: (topLevel)* EOF;

topLevel
    : exportFunctionDecl
    | exportInteractionDecl
    | exportTypeDecl
    | functionDecl
    | interactionDecl
    | typeDecl
    | interfaceDecl
    | implementationDecl
    | importDecl
    | moduleDecl
    | varDecl
    ;

// Module declaration
moduleDecl: MODULE IDENTIFIER SEMICOLON;

// Import declaration
importDecl: IMPORT importSpec SEMICOLON;
importSpec
    : MUL STRING_LITERAL  // import * "module"
    | LBRACE importList? RBRACE STRING_LITERAL  // import { a, b, c } "module"
    | STRING_LITERAL  // import "module"
    ;
importList: IDENTIFIER (COMMA IDENTIFIER)*;

// Export declarations (modifiers on regular declarations)
exportFunctionDecl: EXPORT functionDecl;
exportInteractionDecl: EXPORT interactionDecl;
exportTypeDecl: EXPORT typeDecl;

// Function declaration
functionDecl: FUNCTION IDENTIFIER genericParams? LPAREN parameterList? RPAREN returnType? (functionBody | SEMICOLON);
functionBody: LBRACE statement* RBRACE;

// Interaction declaration
interactionDecl: INTERACTION IDENTIFIER genericParams? LPAREN parameterList? RPAREN returnType? functionBody;

// Generic type parameters (optional constraint: T : Eq)
genericParams: LT genericParam (COMMA genericParam)* GT;
genericParam: IDENTIFIER (COLON IDENTIFIER)?;

// Parameter list
parameterList: parameter (COMMA parameter)*;
parameter: IDENTIFIER (COLON type_)?;

// Return type
returnType: ARROW type_;

// Type declarations
typeDecl: TYPE IDENTIFIER genericParams? ASSIGN typeExpr SEMICOLON;
typeExpr
    : algebraicType
    | unionType
    | type_
    | genericType
    ;

// Interface declaration
interfaceDecl: INTERFACE IDENTIFIER genericParams? (EXTENDS type_ (COMMA type_)*)? LBRACE interfaceMember* RBRACE SEMICOLON;
interfaceMember: IDENTIFIER COLON type_ SEMICOLON;

// Implementation declaration
implementationDecl: IMPLEMENTATION IDENTIFIER LT typeList GT (WHERE typeConstraintList)? LBRACE implementationMember* RBRACE SEMICOLON;
implementationMember: IDENTIFIER ASSIGN (functionBody | expression) SEMICOLON;

// Union type
unionType: type_ (PIPE_DELIM type_)+;

// Algebraic data type (allows optional leading |)
algebraicType: (PIPE_DELIM)? constructor (PIPE_DELIM constructor)*;
constructor: IDENTIFIER (LPAREN constructorArgList RPAREN)?;
constructorArgList: constructorArg (COMMA constructorArg)*;
constructorArg: (IDENTIFIER COLON)? type_;

// Types
type_
    : refinementType
    | dependentFunctionType
    | dependentPairType
    | forallType
    | existentialType
    | intersectionType
    | unionTypeExpr
    | indexedType
    | singletonType
    | constrainedType
    | functionType
    | interactionType
    | primaryType
    | genericType
    ;

// Refinement type: {{variable: BaseType where predicate}}
refinementType: LREFINEMENT IDENTIFIER COLON type_ WHERE refinementPredicate RREFINEMENT;

// Refinement predicates
refinementPredicate
    : refinementPredicate AND refinementPredicate
    | refinementPredicate OR refinementPredicate
    | NOT refinementPredicate
    | refinementPredicate FAT_ARROW refinementPredicate
    | equalsPredicate
    | notEqualsPredicate
    | lessThanPredicate
    | lessEqualPredicate
    | greaterThanPredicate
    | greaterEqualPredicate
    | inRangePredicate
    | memberPredicate
    | expression  // Arbitrary boolean expression
    ;

equalsPredicate: IDENTIFIER EQ expression;
notEqualsPredicate: IDENTIFIER NE expression;
lessThanPredicate: IDENTIFIER LT expression;
lessEqualPredicate: IDENTIFIER LE expression;
greaterThanPredicate: IDENTIFIER GT expression;
greaterEqualPredicate: IDENTIFIER GE expression;
inRangePredicate: expression LE IDENTIFIER AND IDENTIFIER LE expression;
memberPredicate: IDENTIFIER IN LBRACE expressionList RBRACE;

// Dependent function type (Pi type): (paramName: ParamType) -> ReturnType
dependentFunctionType: LPAREN IDENTIFIER COLON type_ RPAREN ARROW type_;

// Dependent pair type (Sigma type): (varName: VarType) * BodyType
// Note: Using MUL token for * in dependent pair context
dependentPairType: LPAREN IDENTIFIER COLON type_ RPAREN MUL type_;

// Forall type: forall typeVars. Type
forallType: FORALL IDENTIFIER+ DOT type_;

// Existential type: exists varName: VarType. BodyType
existentialType: EXISTS IDENTIFIER COLON type_ DOT type_;

// Intersection type: T1 & T2 & ...
// Right-associative to avoid left-recursion
intersectionType: primaryType (INTERSECTION primaryType)+;

// Union type expression: T1 | T2 | ...
// Right-associative to avoid left-recursion  
unionTypeExpr: primaryType (PIPE_DELIM primaryType)+;

// Indexed type: BaseType[index1, index2, ...]
indexedType: primaryType LBRACKET indexList RBRACKET;
indexList: index (COMMA index)*;
index
    : IDENTIFIER
    | INT_LITERAL
    | expression
    | index PLUS index
    | index MINUS index
    | index MUL index
    | index DIV index
    ;

// Singleton type: Singleton(expression)
singletonType: SINGLETON LPAREN expression RPAREN;

// Constrained type: (constraints) => Type
constrainedType: LPAREN typeConstraintList RPAREN FAT_ARROW type_;

// Function type: function(ParamTypes) -> ReturnType
functionType: FUNCTION LPAREN typeList? RPAREN ARROW type_;

// Interaction type: interaction(ParamTypes) -> ReturnType
interactionType: INTERACTION LPAREN typeList? RPAREN ARROW type_;

// Primary types
primaryType
    : builtinType
    | arrayType
    | recordType
    | genericType
    | collectionType
    | concurrencyType
    | typeVar
    | parenthesizedType
    ;

builtinType
    : INT
    | FLOAT
    | BOOL
    | STRING
    | UNIT
    ;

arrayType: ARRAY LT type_ GT;

recordType: LBRACE recordFieldList? RBRACE;
recordFieldList: recordField (COMMA recordField)*;
recordField: IDENTIFIER COLON type_;

genericType: IDENTIFIER (LT typeList GT)?;

collectionType
    : vectorType
    | hashMapType
    | setType
    | linkedListType
    ;

vectorType: VECTOR LT type_ GT;
hashMapType: HASHMAP LT type_ COMMA type_ GT;
setType: SET LT type_ GT;
linkedListType: LINKEDLIST LT type_ GT;

concurrencyType
    : promiseType
    | channelType
    | taskType
    ;

promiseType: PROMISE LT type_ COMMA type_ GT;
channelType: CHANNEL LT type_ GT;
taskType: TASK LT type_ GT;

typeVar: IDENTIFIER;

parenthesizedType: LPAREN type_ RPAREN;

// Type lists
typeList: type_ (COMMA type_)*;

// Type constraints
typeConstraintList: typeConstraint (COMMA typeConstraint)*;
typeConstraint: IDENTIFIER COLON type_;

// Statements (if as statement kept for ANTLR builder; hand-written parser uses if-expr)
statement
    : forStmt
    | varDecl
    | assignment
    | compoundAssignment
    | ifStmt
    | returnStmt
    | matchStmt
    | blockStmt
    | doBlockStmt
    | exprStmt
    | selectStmt
    ;

forStmt: FOR IDENTIFIER IN expression LBRACE statement* RBRACE;

varDecl: (LET | VAR) IDENTIFIER (COLON type_)? (ASSIGN expression)? SEMICOLON;

assignment: expression ASSIGN expression SEMICOLON;

compoundAssignment: expression (PLUS_ASSIGN | MINUS_ASSIGN | MUL_ASSIGN | DIV_ASSIGN) expression SEMICOLON;

ifStmt: IF LPAREN expression RPAREN statement (ELSE statement)?;

returnStmt: RETURN expression? SEMICOLON;

matchStmt: MATCH expression LBRACE matchCase* RBRACE;
matchCase: pattern (WHEN expression)? FAT_ARROW (statement | expression);

blockStmt: LBRACE statement* RBRACE;

doBlockStmt: DO LBRACE doStmt* RBRACE;
doStmt
    : LET IDENTIFIER ASSIGN expression SEMICOLON  // do let x = expr;
    | IDENTIFIER LT MINUS expression SEMICOLON  // do x <- expr;
    | expression SEMICOLON  // do expr;
    ;

exprStmt: expression SEMICOLON;

selectStmt: SELECT LBRACE selectBranch* RBRACE;
selectBranch
    : LT MINUS expression FAT_ARROW IDENTIFIER COLON statement  // receive: <-channel => x: statements
    | expression LT MINUS expression COLON statement  // send: channel <- value: statements
    | ELSE COLON statement  // default branch uses 'else'
    ;

// Patterns
pattern
    : IDENTIFIER
    | literal
    | recordPattern
    | arrayPattern
    | constructorPattern
    | wildcardPattern
    | asPattern
    ;

recordPattern: LBRACE patternFieldList? RBRACE;
patternFieldList: patternField (COMMA patternField)*;
patternField: IDENTIFIER COLON pattern;

arrayPattern: LBRACKET patternList? RBRACKET;
patternList: pattern (COMMA pattern)*;

constructorPattern: IDENTIFIER (LPAREN patternList? RPAREN)?;

wildcardPattern: '_';

asPattern: (IDENTIFIER | literal | recordPattern | arrayPattern | constructorPattern | wildcardPattern) '@' IDENTIFIER;

// Expressions (operator precedence from lowest to highest)
// Precedence: Or(1) < And(2) < Eq/Ne(3) < Lt/Le/Gt/Ge(4) < Add/Sub(5) < Mul/Div/Mod(6) < Monadic(7) < Postfix
expression
    : logicalOrExpr
    ;

logicalOrExpr: logicalAndExpr (OR logicalAndExpr)*;

logicalAndExpr: equalityExpr (AND equalityExpr)*;

equalityExpr: comparisonExpr ((EQ | NE) comparisonExpr)*;

comparisonExpr: rangeExpr ((LT | LE | GT | GE) rangeExpr)*;

additiveExpr: multiplicativeExpr ((PLUS | MINUS) multiplicativeExpr)*;

multiplicativeExpr: monadicExpr ((MUL | DIV | MOD) monadicExpr)*;

monadicExpr: unaryExpr ((BIND | THEN | FMAP | APPLY) unaryExpr)*;

unaryExpr
    : (NOT | MINUS | PLUS) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : primaryExpr
    | postfixExpr DOT IDENTIFIER (genericParams? LPAREN expressionList? RPAREN)?
    | postfixExpr LBRACKET expression RBRACKET
    | postfixExpr LPAREN expressionList? RPAREN
    ;

primaryExpr
    : literal
    | IDENTIFIER
    | parenthesizedExpr
    | recordLiteral
    | arrayLiteral
    | constructorCall
    | lambdaExpr
    | conditionalExpr
    | blockExpr
    | asyncExpr
    | awaitExpr
    | spawnExpr
    | joinExpr
    | selectExpr
    ;

lambdaExpr: LPAREN parameterList? RPAREN FAT_ARROW (expression | functionBody)
    | FUNCTION LPAREN parameterList? RPAREN returnType? functionBody;

// Rust-style if expression: value is last expression in the chosen block
blockExpr: LBRACE statement* expression? RBRACE;
conditionalExpr: IF LPAREN expression RPAREN (blockExpr | expression) ELSE (conditionalExpr | blockExpr | expression);

asyncExpr: ASYNC primaryExpr;

awaitExpr: AWAIT primaryExpr;

spawnExpr: SPAWN primaryExpr;

joinExpr: JOIN primaryExpr;

selectExpr: SELECT LBRACE selectBranch* RBRACE;

parenthesizedExpr: LPAREN expression RPAREN;

recordLiteral: LBRACE recordLiteralFieldList? RBRACE;
recordLiteralFieldList: recordLiteralField (COMMA recordLiteralField)*;
recordLiteralField: IDENTIFIER COLON expression;

arrayLiteral: LBRACKET expressionList? RBRACKET;

// Range expression (Haskell-style): start..end (step 1) or start, second..end (step = second - first)
// .. = exclusive end, ..= = inclusive end
rangeExpr: additiveExpr (COMMA additiveExpr (RANGE_OP | RANGE_INCLUSIVE) additiveExpr | (RANGE_OP | RANGE_INCLUSIVE) additiveExpr)?;

constructorCall: IDENTIFIER (LPAREN expressionList? RPAREN)?;

expressionList: expression (COMMA expression)*;

// Literals
literal
    : INT_LITERAL
    | FLOAT_LITERAL
    | STRING_LITERAL
    | TRUE
    | FALSE
    | NULL
    | unitLiteral
    ;

unitLiteral: LPAREN RPAREN;
