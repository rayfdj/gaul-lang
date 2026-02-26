grammar Gaul;

/*
 * ============================================================================
 * GAUL LANGUAGE GRAMMAR
 * ============================================================================
 *
 * Gaul (from Indonesian "bahasa gaul" meaning "slang language").
 *
 * KEY FEATURES:
 *   - Spaces in identifiers ("My Variable Name", "Calculate Risk Factor")
 *   - Configurable keywords via JSON (supports internationalization)
 *   - Expression-based (everything returns a value)
 *   - Immutable by default (let/var)
 *   - Jam karet operator for approximate equality (~=)
 *   - Number separators for readability (1_000_000)
 *   - Modules with import/export
 *   - Lambda expressions
 *   - Maps and arrays with subscript indexing
 *   - Bitwise operators
 *   - Compound assignment operators (+=, -=, *=, /=)
 *   - Tail call optimization
 *
 * LEXER NOTE:
 *   This grammar defines PARSER RULES ONLY.
 *   Gaul uses a custom Rust scanner (crates/gaul-core/src/scanner.rs) because
 *   ANTLR cannot natively handle:
 *     - Spaces within identifiers
 *     - Runtime-configurable keywords
 *     - Number separators (1_000_000)
 *
 *   See crates/gaul-core/src/scanner.rs for lexer implementation.
 *   See crates/gaul-core/src/scanner/token.rs for token type definitions.
 *
 * EXPRESSION-BASED DESIGN:
 *   Everything in Gaul is an expression that returns a value.
 *   - If/Else returns the value of the taken branch
 *   - Blocks return their last expression
 *   - While/For return Null (executed for side effects)
 *   - Return exits early and returns Null to the enclosing expression
 *
 * PARENTHESES REQUIREMENT:
 *   Control flow keywords (If, While, For) require parentheses around their
 *   conditions. This is necessary to disambiguate where multi-word identifiers
 *   end.
 *
 * ============================================================================
 */


/* ============================================================================
 * PROGRAM STRUCTURE
 * ============================================================================
 * A program is a sequence of declarations, ending with EOF.
 * Newlines are significant at the top level (they separate declarations).
 * Inside parentheses () and braces {}, newlines are ignored by the parser.
 */

program
    : declaration* EOF
    ;

declaration
    : letDecl
    | varDecl
    | fnDecl
    | importDecl
    | exportDecl
    | expression NEWLINE
    ;


/* ============================================================================
 * VARIABLE DECLARATIONS
 * ============================================================================
 * Gaul uses let/var for immutable/mutable bindings (like Swift).
 *
 * Examples:
 *   let My Constant = 100
 *   var My Counter = 0
 *   let Tax Rate = 0.15
 */

letDecl
    : LET IDENTIFIER ASSIGN expression NEWLINE
    ;

varDecl
    : VAR IDENTIFIER ASSIGN expression NEWLINE
    ;


/* ============================================================================
 * FUNCTION DECLARATIONS
 * ============================================================================
 * Functions are defined with the 'fn' keyword (configurable).
 * Parameters and function names can contain spaces.
 * The last expression in the body is the return value (implicit return).
 * Use 'return' for early exit.
 *
 * Examples:
 *   fn Add(A, B) { A + B }
 *
 *   fn Calculate Risk Factor(Portfolio, Risk Model) {
 *       let Raw Risk = Compute Raw(Portfolio)
 *       Apply Model(Raw Risk, Risk Model)
 *   }
 */

fnDecl
    : FUNCTION IDENTIFIER LPAREN parameters? RPAREN block
    ;

parameters
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;


/* ============================================================================
 * IMPORT / EXPORT DECLARATIONS
 * ============================================================================
 * Modules use import/export for code sharing across files.
 * Stdlib modules (math, fs, sys) are imported by bare name.
 *
 * Examples:
 *   import { sin, cos, PI } from "math"
 *   import { read_file } from "fs"
 *   import { My Helper } from "./utils.gaul"
 *
 *   export fn Add(A, B) { A + B }
 *   export let Version = "1.0"
 */

importDecl
    : IMPORT LBRACE IDENTIFIER (COMMA IDENTIFIER)* RBRACE FROM STRING NEWLINE
    ;

exportDecl
    : EXPORT (letDecl | varDecl | fnDecl)
    ;


/* ============================================================================
 * EXPRESSIONS
 * ============================================================================
 * Everything in Gaul is an expression (returns a value).
 *
 * Expression forms (checked first):
 *   - ifExpr      : conditional branching, returns branch value
 *   - whileExpr   : loop, returns Null
 *   - forExpr     : iteration, returns Null
 *   - returnExpr  : early exit, returns Null
 *
 * Operator precedence (lowest to highest):
 *    1. Assignment       (=, +=, -=, *=, /=)
 *    2. Logical OR       (||)
 *    3. Logical AND      (&&)
 *    4. Equality         (==, !=, ~=)
 *    5. Comparison       (<, >, <=, >=)
 *    6. Bitwise OR       (|)
 *    7. Bitwise XOR      (^)
 *    8. Bitwise AND      (&)
 *    9. Shift            (<<, >>)
 *   10. Range            (..)
 *   11. Additive         (+, -)
 *   12. Multiplicative   (*, /, %)
 *   13. Unary            (!, -, ~)
 *   14. Call and access   ((), ., [])
 */

expression
    : ifExpr
    | whileExpr
    | forExpr
    | returnExpr
    | assignment
    ;


/* ============================================================================
 * CONTROL FLOW EXPRESSIONS
 * ============================================================================
 */

// If expression - returns the value of the taken branch
// Examples:
//   if(X > 0) { "positive" } else { "non-positive" }
//
//   if(A) {
//       One
//   } else if(B) {
//       Two
//   } else {
//       Three
//   }
ifExpr
    : IF LPAREN expression RPAREN block elseClause?
    ;

elseClause
    : ELSE block
    | ELSE ifExpr
    ;

// While expression - returns Null, executed for side effects
// Example: while(X < 10) { X = X + 1 }
whileExpr
    : WHILE LPAREN expression RPAREN block
    ;

// For expression - returns Null, executed for side effects
// Examples:
//   for(I : 1..10) { println(I) }
//   for(Item : My Array) { Process(Item) }
forExpr
    : FOR LPAREN IDENTIFIER COLON expression RPAREN block
    ;

// Return expression - exits function early
// Example: return Compute Result()
returnExpr
    : RETURN expression?
    ;

// Break - exits the innermost loop
// Continue - skips to next iteration
breakExpr
    : BREAK
    ;

continueExpr
    : CONTINUE
    ;


/* ============================================================================
 * OPERATOR EXPRESSIONS (by precedence, lowest to highest)
 * ============================================================================
 */

// Assignment (right-associative)
// Targets can be identifiers, field access, or subscript indexing.
// Compound assignment (+=, -=, *=, /=) is desugared to binary op + assign.
//
// Examples:
//   X = 5
//   My Array[0] = "hello"
//   Obj.field = 42
//   Counter += 1
assignment
    : (IDENTIFIER | call DOT IDENTIFIER | call LBRACKET expression RBRACKET)
        (ASSIGN | PLUS_EQUAL | MINUS_EQUAL | STAR_EQUAL | SLASH_EQUAL) assignment
    | logicOr
    ;

// Logical OR (||) - short-circuit evaluation
logicOr
    : logicAnd (OR logicAnd)*
    ;

// Logical AND (&&) - short-circuit evaluation
logicAnd
    : equality (AND equality)*
    ;

// Equality operators
//   == : strict equality
//   != : strict inequality
//   ~= : approximate equality ("jam karet")
equality
    : comparison ((EQUALS | NOT_EQUAL | APPROX_EQUAL) comparison)*
    ;

// Comparison operators
comparison
    : bitwiseOr ((LESS | GREATER | LESS_EQUAL | GREATER_EQUAL) bitwiseOr)*
    ;

// Bitwise OR
bitwiseOr
    : bitwiseXor (PIPE bitwiseXor)*
    ;

// Bitwise XOR
bitwiseXor
    : bitwiseAnd (CARET bitwiseAnd)*
    ;

// Bitwise AND
bitwiseAnd
    : shift (AMPERSAND shift)*
    ;

// Bit shift operators
shift
    : range ((LEFT_SHIFT | RIGHT_SHIFT) range)*
    ;

// Range operator - creates a range from start to end (exclusive)
// Example: 1..10 creates [1, 2, 3, ..., 9]
range
    : term (RANGE term)?
    ;

// Addition and subtraction
term
    : factor ((PLUS | MINUS) factor)*
    ;

// Multiplication, division, and modulo
factor
    : unary ((STAR | SLASH | PERCENT) unary)*
    ;

// Unary operators: logical not (!), negation (-), bitwise not (~)
unary
    : (BANG | MINUS | TILDE) unary
    | call
    ;

// Function calls, property access, method calls, and subscript indexing
// All of these chain left-to-right as postfix operations.
//
// Examples:
//   My Func(A, B)                 // function call
//   My Array.len()                // method call
//   My Array[0]                   // subscript indexing
//   My Array[0].to_str().len()    // chained
call
    : primary (LPAREN arguments? RPAREN | DOT IDENTIFIER | LBRACKET expression RBRACKET)*
    ;

arguments
    : expression (COMMA expression)*
    ;


/* ============================================================================
 * PRIMARY EXPRESSIONS
 * ============================================================================
 * The atomic building blocks: literals, identifiers, grouping, blocks,
 * arrays, maps, and lambdas.
 */

primary
    : NUMBER
    | STRING
    | TRUE
    | FALSE
    | NULL
    | IDENTIFIER
    | BREAK
    | CONTINUE
    | LPAREN expression RPAREN
    | block
    | array
    | map
    | lambda
    ;


/* ============================================================================
 * BLOCKS
 * ============================================================================
 * Blocks are expressions that contain declarations and return the last
 * expression's value. If empty or ends with a declaration, returns Null.
 *
 * Examples:
 *   { 5 }                              // returns 5
 *   { let X = 5; X + 1 }              // returns 6
 *   { println("hi") }                  // returns result of println
 */

block
    : LBRACE declaration* expression? RBRACE
    ;


/* ============================================================================
 * ARRAYS AND MAPS
 * ============================================================================
 * Arrays and maps share bracket syntax. If the first element is followed
 * by a colon, it's a map. Otherwise it's an array.
 *
 * Arrays:
 *   [1, 2, 3]
 *   ["hello", "world"]
 *   []                                  // empty array
 *
 * Maps:
 *   ["name": "Alice", "age": 30]
 *   [1: "one", 2: "two"]
 *   [:]                                 // empty map
 */

array
    : LBRACKET (expression (COMMA expression)*)? RBRACKET
    ;

map
    : LBRACKET COLON RBRACKET
    | LBRACKET expression COLON expression (COMMA expression COLON expression)* RBRACKET
    ;


/* ============================================================================
 * LAMBDA EXPRESSIONS
 * ============================================================================
 * Anonymous functions, using the same keyword as fn declarations.
 * The result is a function value that can be stored or passed around.
 *
 * Examples:
 *   let Double = fn(X) { X * 2 }
 *   My Array.map(fn(Item) { Item + 1 })
 *   let Add = fn(A, B) { A + B }
 */

lambda
    : FUNCTION LPAREN parameters? RPAREN block
    ;


/* ============================================================================
 * TOKEN DECLARATIONS (Documentation Only)
 * ============================================================================
 * These tokens are defined in crates/gaul-core/src/scanner/token.rs and
 * handled by the custom Rust scanner. Listed here for documentation.
 *
 * CONFIGURABLE KEYWORDS (via JSON file):
 *   LET, VAR, FUNCTION, RETURN, IF, ELSE, WHILE, FOR,
 *   TRUE, FALSE, NULL, BREAK, CONTINUE, IMPORT, EXPORT, FROM
 *
 * OPERATORS:
 *   LPAREN '('        RPAREN ')'        LBRACE '{'        RBRACE '}'
 *   LBRACKET '['      RBRACKET ']'      COMMA ','         DOT '.'
 *   COLON ':'         RANGE '..'
 *
 *   PLUS '+'          MINUS '-'         STAR '*'          SLASH '/'
 *   PERCENT '%'       BANG '!'          TILDE '~'
 *
 *   ASSIGN '='        EQUALS '=='       NOT_EQUAL '!='    APPROX_EQUAL '~='
 *   GREATER '>'       GREATER_EQUAL '>='
 *   LESS '<'          LESS_EQUAL '<='
 *
 *   PLUS_EQUAL '+='   MINUS_EQUAL '-='  STAR_EQUAL '*='   SLASH_EQUAL '/='
 *
 *   AND '&&'          OR '||'
 *   AMPERSAND '&'     PIPE '|'          CARET '^'
 *   LEFT_SHIFT '<<'   RIGHT_SHIFT '>>'
 *
 * LITERALS:
 *   NUMBER      - integers, floats, hex (0x), binary (0b), octal (0o)
 *               - supports separators: 1_000_000
 *   STRING      - double-quoted: "hello"
 *   IDENTIFIER  - may contain spaces: "My Variable Name"
 *               - terminated by operators, punctuation, or NEWLINE
 *
 * CONTROL:
 *   NEWLINE     - significant for statement separation
 *               - ignored inside () and {} by parser
 *   EOF         - end of file
 * ============================================================================
 */
