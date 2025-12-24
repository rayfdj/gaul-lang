use gaul_lang::interpreter::environment::Environment;
use gaul_lang::interpreter::interpreter::Interpreter;
use gaul_lang::interpreter::value::Value;
use gaul_lang::keywords::load_keywords;
use gaul_lang::parser::parser::Parser;
use gaul_lang::resolver::Resolver;
use gaul_lang::scanner::scanner::Scanner;
use gaul_lang::scanner::token::TokenType;

// Mimic what the Gaul interpreter is doing
fn eval(source: &str) -> Result<Value, String> {
    let keywords = load_keywords(None).map_err(|e| e.to_string())?;
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new(Environment::new());

    let scanner = Scanner::new(source, &keywords);
    let tokens = scanner.scan_tokens().map_err(|e| format!("{:?}", e))?;

    let parser = Parser::new(tokens);
    let mut program = parser.parse().map_err(|e| format!("{:?}", e))?;

    resolver.resolve(&mut program).map_err(|e| format!("{:?}", e))?;

    interpreter.interpret(program).map_err(|e| format!("{:?}", e))
}

#[test]
fn test_greedy_if_bug() {
    let code = r#"
    {
        if(true) { 0 }
        let x = 1
        x
    }
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected Number(1.0), got {:?}", result),
    }
}

#[test]
fn test_stmt_at_eof_no_newline() {
    // BUG: This used to fail with "Expected newline, got Eof"
    // We are testing a single expression statement with NO trailing \n
    let code = "true";

    let result = eval(code);

    // Assert it parsed successfully and returned true
    match result {
        Ok(Value::Bool(b)) => assert_eq!(b, true),
        Ok(val) => panic!("Expected Value::Bool(true), got {:?}", val),
        Err(e) => panic!("Parser failed at EOF: {}", e),
    }
}

#[test]
fn test_let_decl_at_eof_no_newline() {
    // BUG: "let" declarations also demanded a newline
    let code = "let x = 10";

    let result = eval(code);

    
    
    // The important part is result.is_ok().
    assert!(result.is_ok(), "Failed to parse let decl at EOF: {:?}", result.err());
}

#[test]
fn test_if_no_else_at_eof() {
    // BUG: The greedy skip_newlines() inside if_expr would eat the EOF
    // and cause the statement parser to crash.
    let code = "if(true) { 1 }";

    let result = eval(code);

    assert!(result.is_ok(), "Failed to parse if-no-else at EOF: {:?}", result.err());
}

// --- EDGE CASE TESTS ---

#[test]
fn test_identifier_keyword_overlap() {
    // Edge Case: Can a keyword be part of a variable name?
    // "my if var" contains "if", but since it's not the start,
    // the scanner should treat "my if var" as a single Identifier.
    let code = r#"
    let my if var = 10
    my if var
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected identifier containing keyword to work, got {:?}", result),
    }
}

#[test]
fn test_identifier_starts_with_keyword_fails() {
    // Edge Case: Identifier starting with a keyword.
    // The scanner should immediately match "if" as a Keyword, not start an Identifier.
    // Parser expects Identifier after 'let', gets 'If' token -> Error.
    let code = "let if var = 10";
    let result = eval(code);
    assert!(result.is_err(), "Should strictly forbid identifiers starting with keywords");
}

#[test]
fn test_identifier_split_by_newline() {
    // Edge Case: Multi-word identifier split by newline.
    // Should NOT be joined. "my" is ident, "\n" terminates statement.
    // Parser expects '=' after "my", gets Newline.
    let code = "let my \n var = 10";
    let result = eval(code);
    assert!(result.is_err(), "Identifiers must not span newlines");
}

#[test]
fn test_operator_terminates_identifier() {
    // Edge Case: Spaces in identifier vs spaces around operators.
    // "a b" should be one var, "+" terminates it.
    let code = r#"
    let a b = 5
    let c = 2
    a b + c
    "#;
    // Should parse as (a b) + c = 7
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 7.0),
        _ => panic!("Expected operator to cleanly terminate multi-word identifier"),
    }
}

#[test]
fn test_newline_breaks_function_call() {
    // Edge Case: Newline between function and arguments.
    // Should NOT be a call.
    // 1. "ident" -> evaluates to null (variable declared but not set, or just identifier lookup)
    // 2. "(10)" -> grouping expression, evaluates to 10.
    // Result of block is last stmt -> 10.
    let code = r#"
    let ident = 999
    ident
    (10)
    "#;

    // If it WAS a call, it would try to call 999() and runtime error "Can only call functions".
    // Since it is NOT a call, it evaluates 10.
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        Err(e) => panic!("Newline should have separated call from args, but got error: {}", e),
        _ => panic!("Unexpected result: {:?}", result),
    }
}

#[test]
fn test_trailing_spaces_before_newline() {
    // Edge Case: Identifier with trailing spaces before the line ends.
    // Scanner should trim the trailing spaces or parser should handle them?
    // Actually, scanner eats spaces inside identifiers.
    // "let x   = 10" -> "x" (ident), "=" (assign).
    // "let x y   \n = 10" -> "x y" (ident), Newline, = (assign). -> Parse Error expected.
    let code = "let x y   \n = 10";
    let result = eval(code);
    assert!(result.is_err(), "Newline after identifier (even with spaces) should break assignment");
}

#[test]
fn test_newlines_inside_parens_and_braces() {
    // BUG: "Recursive Nesting Logic"
    // Scenario: An if-expression wrapped in parentheses.
    // The outer parens (...) normally suppress newlines (like for function args).
    // BUT, the block { ... } inside the if MUST allow newlines for statements.

    let code = r#"
    let val = (if(true) {
        let inner = 10
        inner
    } else {
        0
    })
    val
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        // Without the fix, this will fail with "Expected newline... got Identifier"
        _ => panic!("Expected block inside parens to handle newlines correctly, got {:?}", result),
    }
}

#[test]
fn test_multiline_comment_is_ignored() {
    // Scenario: A comment spanning multiple lines shouldn't affect the code around it.
    let code = r#"
    let x = 10
    /* This is a comment
       that spans lines
    */
    x
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected multiline comment to be ignored, got {:?}", result),
    }
}

#[test]
fn test_multiline_comment_counts_lines_correctly() {
    // Scenario: If we have an error AFTER a multiline comment,
    // the line number must be correct (meaning the scanner tracked \n inside the comment).
    let code = r#"
    /* Line 2
       Line 3
    */
    @ // This is invalid syntax on Line 5 (assuming file starts at 1)
    "#;

    // Note: The 'r#' raw string starts the content on the NEXT line.
    // Line 1 is empty. Line 2 is '/*', Line 3 is 'Line 2', Line 4 is 'Line 3', Line 5 is '*/', Line 6 is '@'.
    // Expect the error message to contain "Line 5".

    let result = eval(code);

    match result {
        Err(msg) => {
            // We check if the error message reports the correct line number.
            
            assert!(msg.contains("Line 5"), "Expected error on Line 6, but got: {}", msg);
        }
        _ => panic!("Expected scanner error for invalid character"),
    }
}

#[test]
fn test_unterminated_multiline_comment() {
    // Scenario: File ends while inside a comment.
    let code = "/* This comment never ends...";
    let result = eval(code);

    match result {
        Err(msg) => assert!(msg.contains("Unterminated multi-line comment")),
        _ => panic!("Expected error for unterminated comment"),
    }
}