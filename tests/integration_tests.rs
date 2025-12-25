use gaul_lang::interpreter::environment::Environment;
use gaul_lang::interpreter::interpreter::Interpreter;
use gaul_lang::interpreter::value::Value;
use gaul_lang::keywords::load_keywords;
use gaul_lang::parser::parser::Parser;
use gaul_lang::resolver::Resolver;
use gaul_lang::scanner::scanner::Scanner;

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

#[test]
fn test_multiline_expressions_trailing_ops() {
    // Scenario: Splitting a long expression across lines.
    // Convention: The operator must be at the END of the line (trailing).
    let code = r#"
    let result = 10 +
                 20 *
                 3

    let logic = true and
                false or
                true

    if (result == 70 and
        logic == true) {
        1
    } else {
        0
    }
    "#;

    // Math: 10 + (20 * 3) = 10 + 60 = 70
    // Logic: (true and false) or true -> false or true -> true
    // If (70 == 70 and true == true) -> Returns 1

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected multiline expression to parse and evaluate to 1, got {:?}", result),
    }
}

#[test]
fn test_binary_precedence_basic() {
    // Multiplication (*) is tighter than Addition (+)
    // 1 + 2 * 3 should be 1 + (2 * 3) = 7
    // NOT (1 + 2) * 3 = 9
    let code = "1 + 2 * 3";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 7.0),
        _ => panic!("Precedence failed: 1 + 2 * 3 should be 7.0, got {:?}", result),
    }
}

#[test]
fn test_binary_precedence_mixed() {
    // Division (/) tighter than Subtraction (-)
    // 10 - 8 / 2 should be 10 - (8 / 2) = 6
    // NOT (10 - 8) / 2 = 1
    let code = "10 - 8 / 2";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Precedence failed: 10 - 8 / 2 should be 6.0, got {:?}", result),
    }
}

#[test]
fn test_binary_associativity_left() {
    // Subtraction is Left-Associative
    // 10 - 5 - 2 should be (10 - 5) - 2 = 3
    // NOT 10 - (5 - 2) = 7
    let code = "10 - 5 - 2";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Associativity failed: 10 - 5 - 2 should be 3.0, got {:?}", result),
    }
}

#[test]
fn test_binary_grouping_overrides_precedence() {
    // Parentheses should override precedence
    // (1 + 2) * 3 = 9
    let code = "(1 + 2) * 3";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 9.0),
        _ => panic!("Grouping failed: (1 + 2) * 3 should be 9.0, got {:?}", result),
    }
}

#[test]
fn test_logic_precedence() {
    // 'and' is tighter than 'or'
    // true or false and false
    // Should be: true or (false and false) -> true or false -> true
    // NOT: (true or false) and false -> true and false -> false
    let code = "true or false and false";
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert_eq!(b, true),
        _ => panic!("Logic precedence failed: expected true, got {:?}", result),
    }
}

#[test]
fn test_comparison_precedence() {
    // Comparisons (<) are tighter than Equality (==)
    // 1 < 2 == true
    // Should be: (1 < 2) == true -> true == true -> true
    // NOT: 1 < (2 == true) -> Runtime Error or logic fail
    let code = "1 < 2 == true";
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert_eq!(b, true),
        _ => panic!("Comparison precedence failed: expected true, got {:?}", result),
    }
}

#[test]
fn test_break_in_while_loop() {
    // Scenario: Loop from 0 to 10, but break at 5.
    // Result should be 0+1+2+3+4 = 10.
    let code = r#"
    var i = 0
    var sum = 0
    while (i < 10) {
        if (i == 5) {
            break
        }
        sum = sum + i
        i = i + 1
    }
    sum
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected loop to break at 5 (sum=10), got {:?}", result),
    }
}

#[test]
fn test_continue_in_for_loop() {
    // Scenario: Loop array [1, 2, 3, 4, 5].
    // Skip even numbers (2, 4).
    // Sum: 1 + 3 + 5 = 9.
    let code = r#"
    var sum = 0
    let list = [1, 2, 3, 4, 5]

    for (n : list) {
        if (n.mod(2) == 0) {
            continue
        }
        sum = sum + n
    }
    sum
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 9.0),
        _ => panic!("Expected loop to skip evens (sum=9), got {:?}", result),
    }
}

#[test]
fn test_nested_loop_break() {
    // Scenario: Break should only exit the INNER loop.
    // Outer loop runs 3 times (i=0, 1, 2).
    // Inner loop runs until j=2, then breaks.
    // Inner loop adds j=0, j=1 (Total +1 per inner run).
    // Total sum = 1 + 1 + 1 = 3.
    let code = r#"
    var sum = 0
    var i = 0
    while (i < 3) {
        var j = 0
        while (j < 5) {
            if (j == 2) {
                break
            }
            sum = sum + 1
            j = j + 1
        }
        i = i + 1
    }
    sum
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0), // 0,1 for each i(0,1,2) -> 2*3 = 6
        _ => panic!("Expected nested break to work correctly, got {:?}", result),
    }
}

#[test]
fn test_return_early() {
    // Scenario: Return immediately stops execution.
    // The "unreachable" assignment should never happen.
    let code = r#"
    fn check(n) {
        if (n < 5) {
            return "low"
        }
        return "high"

        // This should never run
        return "unreachable"
    }

    check(3)
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "low"),
        _ => panic!("Expected early return 'low', got {:?}", result),
    }
}

#[test]
fn test_return_inside_loop() {
    // Scenario: Return inside a loop should kill the loop AND the function.
    let code = r#"
    fn find_first_even(list) {
        for (n : list) {
            if (n.mod(2) == 0) {
                return n
            }
        }
        return -1
    }

    find_first_even([1, 3, 4, 5])
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected return from loop to give 4, got {:?}", result),
    }
}

#[test]
fn test_void_return() {
    // Scenario: 'return' without value should return Null.
    let code = r#"
    fn do_nothing() {
        return
    }
    do_nothing()
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Null) => {}, // Pass
        _ => panic!("Expected void return to be Null, got {:?}", result),
    }
}

#[test]
fn test_control_flow_as_expression() {
    // Scenario: Using 'break' as the branch of an 'if' expression.
    // logic: x is assigned the result of the if.
    // If true, we break (so x is never assigned, loop ends).
    // If false, x becomes 10.
    let code = r#"
    var result = 0
    var i = 0
    while (i < 2) {
        let x = if (i == 1) {
            break
        } else {
            10
        }
        result = result + x
        i = i + 1
    }
    result
    "#;

    // Iteration 0: i=0. if(false). x = 10. result = 10.
    // Iteration 1: i=1. if(true). break. Loop ends.
    // Final result should be 10.

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected break in expression to halt loop safely, got {:?}", result),
    }
}

#[test]
fn test_return_without_newline_at_block_end() {
    // Scenario: 'return' is the very last thing in the block.
    // There is no newline between 'return' and '}'.
    // The parser must realize '}' ends the statement and the block.
    let code = r#"
    fn nothing() { return }
    nothing()
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Null) => {}, // Success! It parsed and returned Null.
        Err(e) => panic!("Parser failed to handle 'return }}': {:?}", e),
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_return_eof_edge_case() {
    let code = "return";
    let result = eval(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("outside of function"));
}

#[test]
fn test_break_outside_loop_rejected() {
    let code = "break";
    let result = eval(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("outside of loop"));
}

#[test]
fn test_continue_outside_loop_rejected() {
    let code = "continue";
    let result = eval(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("outside of loop"));
}

#[test]
fn test_return_outside_function_rejected() {
    let code = "return 5";
    let result = eval(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("outside of function"));
}

#[test]
fn test_break_in_function_outside_loop_rejected() {
    let code = r#"
    fn bad() {
        break
    }
    "#;
    let result = eval(code);
    assert!(result.is_err());
    assert!(result.unwrap_err().contains("outside of loop"));
}

#[test]
fn test_continue_in_while_loop() {
    let code = r#"
    var sum = 0
    var i = 0
    while (i < 5) {
        i = i + 1
        if (i == 3) {
            continue
        }
        sum = sum + i
    }
    sum
    "#;
    // i goes 1,2,3,4,5. Skip when i==3.
    // sum = 1 + 2 + 4 + 5 = 12
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 12.0),
        _ => panic!("Expected 12, got {:?}", result),
    }
}

#[test]
fn test_break_in_for_loop() {
    let code = r#"
    var sum = 0
    for (i : 0..10) {
        if (i == 5) {
            break
        }
        sum = sum + i
    }
    sum
    "#;
    // 0+1+2+3+4 = 10
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_nested_loop_continue() {
    let code = r#"
    var sum = 0
    for (i : 0..3) {
        for (j : 0..3) {
            if (j == 1) {
                continue
            }
            sum = sum + 1
        }
    }
    sum
    "#;
    // Inner loop: j=0,1,2. Skip j==1. So 2 adds per outer iteration.
    // Outer loop: 3 iterations. Total = 3 * 2 = 6
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}

#[test]
fn test_return_with_expression() {
    let code = r#"
    fn double(x) {
        return x * 2
    }
    double(21)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 42.0),
        _ => panic!("Expected 42, got {:?}", result),
    }
}