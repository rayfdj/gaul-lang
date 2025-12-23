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
 *   - Pipe operator for data flow (|>)
 *   - Jam karet operator for approximate equality (~=)
 *   - Number separators for readability (1_000_000)
 *
 * LEXER NOTE:
 *   This grammar defines PARSER RULES ONLY.
 *   Gaul uses a custom Rust scanner (src/scanner/) because ANTLR cannot
 *   natively handle:
 *     - Spaces within identifiers
 *     - Runtime-configurable keywords
 *     - Number separators (1_000_000)
 *   
 *   See src/scanner/scanner.rs for lexer implementation.
 *   See src/scanner/token.rs for token type definitions.
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
 *   1. Assignment (=)
 *   2. Pipe (|>)
 *   3. Logical OR (or)
 *   4. Logical AND (and)
 *   5. Equality (==, !=, ~=)
 *   6. Comparison (<, >, <=, >=)
 *   7. Range (..)
 *   8. Additive (+, -)
 *   9. Multiplicative (*, /)
 *  10. Unary (!, -)
 *  11. Call and access ((), .)
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
//   If(X > 0) { "positive" } Else { "non-positive" }
//
//   If(A) {
//       One
//   } Else If(B) {
//       Two
//   } Else {
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
// Example: While(X < 10) { X = X + 1 }
whileExpr
    : WHILE LPAREN expression RPAREN block
    ;

// For expression - returns Null, executed for side effects
// Examples:
//   For(I : 1..10) { Print(I) }
//   For(Item : My Array) { Process(Item) }
forExpr
    : FOR LPAREN IDENTIFIER COLON expression RPAREN block
    ;

// Return expression - exits function early, returns Null as expression value
// The returned value goes to the function, not to the enclosing expression
// Example: return Compute Result()
returnExpr
    : RETURN expression?
    ;


/* ============================================================================
 * OPERATOR EXPRESSIONS (by precedence, lowest to highest)
 * ============================================================================
 */

// Assignment is right-associative: a = b = c means a = (b = c)
assignment
    : IDENTIFIER ASSIGN assignment
    | pipe
    ;

// Pipe operator - chains function calls left-to-right
// "A |> B(X)" becomes "B(A, X)"
pipe
    : logicOr (PIPE logicOr)*
    ;

// Logical OR - short-circuit evaluation
logicOr
    : logicAnd (OR logicAnd)*
    ;

// Logical AND - short-circuit evaluation
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
    : range ((LESS | GREATER | LESS_EQUAL | GREATER_EQUAL) range)*
    ;

// Range operator - creates a range from start to end
// Example: 1..10
range
    : term (RANGE term)?
    ;

// Addition and subtraction
term
    : factor ((PLUS | MINUS) factor)*
    ;

// Multiplication and division
factor
    : unary ((STAR | SLASH) unary)*
    ;

// Unary operators: logical not (!), negation (-)
unary
    : (BANG | MINUS) unary
    | call
    ;

// Function calls and property access
call
    : primary (LPAREN arguments? RPAREN | DOT IDENTIFIER)*
    ;

arguments
    : expression (COMMA expression)*
    ;


/* ============================================================================
 * PRIMARY EXPRESSIONS
 * ============================================================================
 * The atomic building blocks - literals, identifiers, grouping, and blocks.
 */

primary
    : NUMBER
    | STRING
    | TRUE
    | FALSE
    | NULL
    | IDENTIFIER
    | LPAREN expression RPAREN
    | block
    | array
    ;


/* ============================================================================
 * BLOCKS
 * ============================================================================
 * Blocks are expressions that contain declarations and return the last
 * expression's value. If empty or ends with declaration, returns Null.
 *
 * Examples:
 *   { 5 }                              // returns 5
 *   { let X = 5; X + 1 }               // returns 6
 *   { Print("hi") }                    // returns result of Print
 */

block
    : LBRACE declaration* expression? RBRACE
    ;


/* ============================================================================
 * ARRAYS
 * ============================================================================
 * Arrays are expressions that create, er, arrays. They are sandwiched by
 * square brackets, just like in many other languages.
 *
 * Examples:
 *   [ 1, 2, 3 ]                        // array of number 1, 2, 3
 *   [ Func 1, Func 2, Func 3 ]         // array of functions
 */

array
    : LBRACKET (expression (COMMA expression)*)? RBRACKET
    ;

/* ============================================================================
 * TOKEN DECLARATIONS (Documentation Only)
 * ============================================================================
 * These tokens are defined in src/scanner/token.rs and handled by the
 * custom Rust scanner. They are listed here for documentation purposes.
 *
 * CONFIGURABLE KEYWORDS (via JSON file):
 *   LET, VAR, FUNCTION, RETURN, IF, ELSE, WHILE, FOR, IN,
 *   AND, OR, TRUE, FALSE, NULL
 *
 * OPERATORS:
 *   LPAREN '('    RPAREN ')'    LBRACE '{'    RBRACE '}'
 *   COMMA ','     DOT '.'       MINUS '-'     PLUS '+'
 *   SLASH '/'     STAR '*'      BANG '!'
 *   ASSIGN '='    EQUALS '=='   NOT_EQUAL '!='
 *   GREATER '>'   GREATER_EQUAL '>='
 *   LESS '<'      LESS_EQUAL '<='
 *   PIPE '|>'     APPROX_EQUAL '~='   RANGE '..'
 *   LBRACKET '['  RBRACKET ']'  COLON ':'
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
