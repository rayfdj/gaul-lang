use gaul_lang::interpreter::Interpreter;
use gaul_lang::interpreter::environment::Environment;
use gaul_lang::interpreter::value::Value;
use gaul_lang::keywords::load_keywords;
use gaul_lang::parser::Parser;
use gaul_lang::resolver::Resolver;
use gaul_lang::scanner::Scanner;

// Mimic what the Gaul interpreter is doing
fn eval(source: &str) -> Result<Value, String> {
    let keywords = load_keywords(None).map_err(|e| e.to_string())?;
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new(Environment::new());

    let scanner = Scanner::new(source, &keywords);
    let tokens = scanner.scan_tokens().map_err(|e| format!("{:?}", e))?;

    let parser = Parser::new(tokens);
    let mut program = parser.parse().map_err(|e| format!("{:?}", e))?;

    resolver
        .resolve(&mut program)
        .map_err(|e| format!("{:?}", e))?;

    interpreter
        .interpret(program)
        .map_err(|e| format!("{:?}", e))
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
    assert!(
        result.is_ok(),
        "Failed to parse let decl at EOF: {:?}",
        result.err()
    );
}

#[test]
fn test_if_no_else_at_eof() {
    // BUG: The greedy skip_newlines() inside if_expr would eat the EOF
    // and cause the statement parser to crash.
    let code = "if(true) { 1 }";

    let result = eval(code);

    assert!(
        result.is_ok(),
        "Failed to parse if-no-else at EOF: {:?}",
        result.err()
    );
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
        _ => panic!(
            "Expected identifier containing keyword to work, got {:?}",
            result
        ),
    }
}

#[test]
fn test_identifier_starts_with_keyword_fails() {
    // Edge Case: Identifier starting with a keyword.
    // The scanner should immediately match "if" as a Keyword, not start an Identifier.
    // Parser expects Identifier after 'let', gets 'If' token -> Error.
    let code = "let if var = 10";
    let result = eval(code);
    assert!(
        result.is_err(),
        "Should strictly forbid identifiers starting with keywords"
    );
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
        Err(e) => panic!(
            "Newline should have separated call from args, but got error: {}",
            e
        ),
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
    assert!(
        result.is_err(),
        "Newline after identifier (even with spaces) should break assignment"
    );
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
        _ => panic!(
            "Expected block inside parens to handle newlines correctly, got {:?}",
            result
        ),
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
            
            assert!(
                msg.contains("Line 5"),
                "Expected error on Line 6, but got: {}",
                msg
            );
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
        _ => panic!(
            "Expected multiline expression to parse and evaluate to 1, got {:?}",
            result
        ),
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
        _ => panic!(
            "Precedence failed: 1 + 2 * 3 should be 7.0, got {:?}",
            result
        ),
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
        _ => panic!(
            "Precedence failed: 10 - 8 / 2 should be 6.0, got {:?}",
            result
        ),
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
        _ => panic!(
            "Associativity failed: 10 - 5 - 2 should be 3.0, got {:?}",
            result
        ),
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
        _ => panic!(
            "Grouping failed: (1 + 2) * 3 should be 9.0, got {:?}",
            result
        ),
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
        _ => panic!(
            "Comparison precedence failed: expected true, got {:?}",
            result
        ),
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
        Ok(Value::Null) => {} // Pass
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
        _ => panic!(
            "Expected break in expression to halt loop safely, got {:?}",
            result
        ),
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
        Ok(Value::Null) => {} // Success! It parsed and returned Null.
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

// ==================== NATIVE METHOD TESTS ====================

// --- String Methods ---

#[test]
fn test_string_len() {
    let result = eval(r#""hello".len()"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_string_len_unicode() {
    let result = eval(r#""héllo".len()"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5 (char count, not bytes), got {:?}", result),
    }
}

#[test]
fn test_string_char_at() {
    let result = eval(r#""hello".char_at(1)"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "e"),
        _ => panic!("Expected 'e', got {:?}", result),
    }
}

#[test]
fn test_string_char_at_out_of_bounds() {
    let result = eval(r#""hello".char_at(10)"#);
    assert!(result.is_err());
}

#[test]
fn test_string_substring() {
    let result = eval(r#""hello world".substring(0, 5)"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

#[test]
fn test_string_substring_to_end() {
    let result = eval(r#""hello world".substring(6)"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "world"),
        _ => panic!("Expected 'world', got {:?}", result),
    }
}

#[test]
fn test_string_split() {
    let code = r#"
    let parts = "a,b,c".split(",")
    parts.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3 parts, got {:?}", result),
    }
}

#[test]
fn test_string_split_get_element() {
    let code = r#"
    let parts = "a,b,c".split(",")
    parts.get(1)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "b"),
        _ => panic!("Expected 'b', got {:?}", result),
    }
}

#[test]
fn test_string_lines() {
    let code = r#"
    let text = "line1
line2
line3"
    text.lines().len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3 lines, got {:?}", result),
    }
}

#[test]
fn test_string_trim() {
    let result = eval(r#""  hello  ".trim()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

#[test]
fn test_string_contains_true() {
    let result = eval(r#""hello world".contains("world")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_string_contains_false() {
    let result = eval(r#""hello world".contains("foo")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_string_starts_with_true() {
    let result = eval(r#""hello world".starts_with("hello")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_string_starts_with_false() {
    let result = eval(r#""hello world".starts_with("world")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_string_ends_with_true() {
    let result = eval(r#""hello world".ends_with("world")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_string_ends_with_false() {
    let result = eval(r#""hello world".ends_with("hello")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_string_to_num() {
    let result = eval(r#""42".to_num()"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 42.0),
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_string_to_num_float() {
    let result = eval(r#""1.234".to_num()"#);
    match result {
        Ok(Value::Num(n)) => assert!((n - 1.234).abs() < 0.001),
        _ => panic!("Expected 3.14, got {:?}", result),
    }
}

#[test]
fn test_string_to_num_invalid() {
    let result = eval(r#""not a number".to_num()"#);
    assert!(result.is_err());
}

#[test]
fn test_string_chars() {
    let code = r#"
    let chars = "hello".chars()
    chars.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_string_chars_iterate() {
    let code = r#"
    var result = ""
    for(c : "abc".chars()) {
        result = result + c + "-"
    }
    result
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "a-b-c-"),
        _ => panic!("Expected 'a-b-c-', got {:?}", result),
    }
}

#[test]
fn test_string_chars_get_element() {
    let code = r#""hello".chars().get(1)"#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "e"),
        _ => panic!("Expected 'e', got {:?}", result),
    }
}

// --- Number Methods ---

#[test]
fn test_number_to_str() {
    let result = eval("42.to_str()");
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "42"),
        _ => panic!("Expected '42', got {:?}", result),
    }
}

#[test]
fn test_number_abs_positive() {
    let result = eval("42.abs()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 42.0),
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_number_abs_negative() {
    let code = "(-42).abs()";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 42.0),
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_number_floor() {
    let result = eval("3.7.floor()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_number_ceil() {
    let result = eval("3.2.ceil()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected 4, got {:?}", result),
    }
}

#[test]
fn test_number_round() {
    let result = eval("3.5.round()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected 4, got {:?}", result),
    }
}

#[test]
fn test_number_pow() {
    let result = eval("2.pow(10)");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1024.0),
        _ => panic!("Expected 1024, got {:?}", result),
    }
}

#[test]
fn test_number_sqrt() {
    let result = eval("16.sqrt()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected 4, got {:?}", result),
    }
}

#[test]
fn test_number_sqrt_negative() {
    let code = "(-1).sqrt()";
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_number_mod() {
    let result = eval("17.mod(5)");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_number_mod_negative() {
    let code = "(-7).mod(3)";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0), // rem_euclid gives positive result
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_number_mod_by_zero() {
    let result = eval("10.mod(0)");
    assert!(result.is_err());
}

#[test]
fn test_number_floor_div() {
    let result = eval("17.floor_div(5)");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_number_floor_div_negative() {
    let code = "(-7).floor_div(3)";
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, -3.0), // div_euclid floors toward negative infinity
        _ => panic!("Expected -3, got {:?}", result),
    }
}

#[test]
fn test_number_floor_div_by_zero() {
    let result = eval("10.floor_div(0)");
    assert!(result.is_err());
}

// --- Bool Methods ---

#[test]
fn test_bool_to_str_true() {
    let result = eval("true.to_str()");
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "true"),
        _ => panic!("Expected 'true', got {:?}", result),
    }
}

#[test]
fn test_bool_to_str_false() {
    let result = eval("false.to_str()");
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "false"),
        _ => panic!("Expected 'false', got {:?}", result),
    }
}

// --- Array Methods ---

#[test]
fn test_array_len() {
    let result = eval("[1, 2, 3, 4, 5].len()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_array_len_empty() {
    let result = eval("[].len()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_array_get() {
    let result = eval("[10, 20, 30].get(1)");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 20.0),
        _ => panic!("Expected 20, got {:?}", result),
    }
}

#[test]
fn test_array_get_out_of_bounds() {
    let result = eval("[1, 2, 3].get(10)");
    assert!(result.is_err());
}

#[test]
fn test_array_first() {
    let result = eval("[10, 20, 30].first()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_array_first_empty() {
    let result = eval("[].first()");
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_array_last() {
    let result = eval("[10, 20, 30].last()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 30.0),
        _ => panic!("Expected 30, got {:?}", result),
    }
}

#[test]
fn test_array_last_empty() {
    let result = eval("[].last()");
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_array_push() {
    let code = r#"
    let arr = [1, 2]
    arr.push(3)
    arr.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_array_push_returns_null() {
    let code = r#"
    let arr = [1, 2]
    arr.push(3)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_array_pop() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.pop()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_array_pop_modifies_array() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.pop()
    arr.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_array_pop_empty() {
    let result = eval("[].pop()");
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_array_set() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.set(1, 99)
    arr.get(1)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 99.0),
        _ => panic!("Expected 99, got {:?}", result),
    }
}

#[test]
fn test_array_set_out_of_bounds() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.set(10, 99)
    "#;
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_array_remove() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.remove(1)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_array_remove_shifts_elements() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.remove(0)
    arr.get(0)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_array_remove_out_of_bounds() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.remove(10)
    "#;
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_array_contains_true() {
    let result = eval("[1, 2, 3].contains(2)");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_array_contains_false() {
    let result = eval("[1, 2, 3].contains(99)");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_array_contains_string() {
    let result = eval(r#"["a", "b", "c"].contains("b")"#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_array_reverse() {
    let code = r#"
    let arr = [1, 2, 3]
    let rev = arr.reverse()
    rev.get(0)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_array_reverse_does_not_mutate() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.reverse()
    arr.get(0)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1 (original unchanged), got {:?}", result),
    }
}

#[test]
fn test_array_is_empty_true() {
    let result = eval("[].is_empty()");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_array_is_empty_false() {
    let result = eval("[1].is_empty()");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_array_join() {
    let result = eval(r#"["a", "b", "c"].join("-")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "a-b-c"),
        _ => panic!("Expected 'a-b-c', got {:?}", result),
    }
}

#[test]
fn test_array_join_no_separator() {
    let result = eval(r#"["a", "b", "c"].join()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "abc"),
        _ => panic!("Expected 'abc', got {:?}", result),
    }
}

#[test]
fn test_array_join_numbers() {
    let result = eval(r#"[1, 2, 3].join(", ")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "1, 2, 3"),
        _ => panic!("Expected '1, 2, 3', got {:?}", result),
    }
}

#[test]
fn test_array_sum() {
    let result = eval("[1, 2, 3, 4, 5].sum()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0),
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_array_sum_empty() {
    let result = eval("[].sum()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_array_sum_non_numbers() {
    let result = eval(r#"[1, "two", 3].sum()"#);
    assert!(result.is_err());
}

#[test]
fn test_array_min() {
    let result = eval("[3, 1, 4, 1, 5].min()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1, got {:?}", result),
    }
}

#[test]
fn test_array_min_empty() {
    let result = eval("[].min()");
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_array_min_non_numbers() {
    let result = eval(r#"[1, "two", 3].min()"#);
    assert!(result.is_err());
}

#[test]
fn test_array_max() {
    let result = eval("[3, 1, 4, 1, 5].max()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_array_max_empty() {
    let result = eval("[].max()");
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected Null, got {:?}", result),
    }
}

#[test]
fn test_array_max_non_numbers() {
    let result = eval(r#"[1, "two", 3].max()"#);
    assert!(result.is_err());
}

#[test]
fn test_array_max_negative() {
    let result = eval("[-5, -2, -10].max()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, -2.0),
        _ => panic!("Expected -2, got {:?}", result),
    }
}

#[test]
fn test_array_sort_numbers() {
    let code = "[3, 1, 4, 1, 5, 9, 2, 6].sort()";
    let result = eval(code);
    match result {
        Ok(Value::Array(arr)) => {
            let nums: Vec<f64> = arr
                .borrow()
                .iter()
                .map(|v| match v {
                    Value::Num(n) => *n,
                    _ => panic!("Expected number"),
                })
                .collect();
            assert_eq!(nums, vec![1.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 9.0]);
        }
        _ => panic!("Expected sorted array, got {:?}", result),
    }
}

#[test]
fn test_array_sort_strings() {
    let code = r#"["banana", "apple", "cherry"].sort()"#;
    let result = eval(code);
    match result {
        Ok(Value::Array(arr)) => {
            let strs: Vec<String> = arr
                .borrow()
                .iter()
                .map(|v| match v {
                    Value::Str(s) => s.to_string(),
                    _ => panic!("Expected string"),
                })
                .collect();
            assert_eq!(strs, vec!["apple", "banana", "cherry"]);
        }
        _ => panic!("Expected sorted array, got {:?}", result),
    }
}

#[test]
fn test_array_sort_empty() {
    let result = eval("[].sort()");
    match result {
        Ok(Value::Array(arr)) => assert!(arr.borrow().is_empty()),
        _ => panic!("Expected empty array, got {:?}", result),
    }
}

#[test]
fn test_array_sort_mixed_types_error() {
    let result = eval(r#"[1, "two", 3].sort()"#);
    assert!(result.is_err());
}

#[test]
fn test_array_sort_does_not_mutate() {
    let code = r#"
    let arr = [3, 1, 2]
    arr.sort()
    arr.get(0)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0), // Original unchanged
        _ => panic!("Expected 3, got {:?}", result),
    }
}

// --- Range Methods ---

#[test]
fn test_range_from() {
    let result = eval("(5..10).from()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_range_until() {
    let result = eval("(5..10).until()");
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

// --- Invalid Method Tests ---

#[test]
fn test_invalid_method_on_string() {
    let result = eval(r#""hello".foo()"#);
    assert!(result.is_err());
}

#[test]
fn test_invalid_method_on_number() {
    let result = eval("42.foo()");
    assert!(result.is_err());
}

#[test]
fn test_invalid_method_on_array() {
    let result = eval("[1, 2, 3].foo()");
    assert!(result.is_err());
}

// ==================== CLOSURE TESTS ====================

// --- Basic Closure Behavior ---

#[test]
fn test_closure_captures_variable() {
    // Basic test: inner function captures outer variable
    let code = r#"
    fn outer() {
        let x = 10
        fn inner() {
            return x
        }
        return inner()
    }
    outer()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_closure_captures_multiple_variables() {
    let code = r#"
    fn outer() {
        let a = 1
        let b = 2
        let c = 3
        fn inner() {
            return a + b + c
        }
        return inner()
    }
    outer()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}

#[test]
fn test_closure_returns_function() {
    // Return a function that closes over a variable
    let code = r#"
    fn makeAdder(x) {
        fn add(y) {
            return x + y
        }
        return add
    }
    let addFive = makeAdder(5)
    addFive(3)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 8.0),
        _ => panic!("Expected 8, got {:?}", result),
    }
}

// --- Closure State Mutation (THE CRITICAL TEST) ---

#[test]
fn test_closure_mutates_captured_variable() {
    // This is the counter example that was failing!
    let code = r#"
    fn makeCounter() {
        var count = 0
        fn increment() {
            count = count + 1
            return count
        }
        return increment
    }
    let counter = makeCounter()
    let first = counter()
    let second = counter()
    first + second * 10
    "#;
    // first = 1, second = 2, result = 1 + 20 = 21
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 21.0),
        _ => panic!("Expected 21 (first=1, second=2), got {:?}", result),
    }
}

#[test]
fn test_closure_counter_multiple_calls() {
    let code = r#"
    fn makeCounter() {
        var count = 0
        fn increment() {
            count = count + 1
            return count
        }
        return increment
    }
    let counter = makeCounter()
    counter()
    counter()
    counter()
    counter()
    counter()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_independent_closures_have_separate_state() {
    // Two counters should have independent state
    let code = r#"
    fn makeCounter() {
        var count = 0
        fn increment() {
            count = count + 1
            return count
        }
        return increment
    }
    let counter1 = makeCounter()
    let counter2 = makeCounter()
    counter1()
    counter1()
    counter1()
    counter2()
    counter1() + counter2() * 10
    "#;
    // counter1: 1, 2, 3, 4
    // counter2: 1, 2
    // result = 4 + 2*10 = 24
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 24.0),
        _ => panic!("Expected 24, got {:?}", result),
    }
}

// --- Nested Closures ---

#[test]
fn test_deeply_nested_closure() {
    let code = r#"
    fn level1() {
        let a = 1
        fn level2() {
            let b = 2
            fn level3() {
                let c = 3
                fn level4() {
                    return a + b + c
                }
                return level4
            }
            return level3
        }
        return level2
    }
    let l2 = level1()
    let l3 = l2()
    let l4 = l3()
    l4()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}

#[test]
fn test_nested_closure_mutation() {
    let code = r#"
    fn outer() {
        var x = 0
        fn middle() {
            fn inner() {
                x = x + 1
                return x
            }
            return inner
        }
        return middle()
    }
    let increment = outer()
    let a = increment()
    let b = increment()
    let c = increment()
    a + b * 10 + c * 100
    "#;
    // a=1, b=2, c=3 -> 1 + 20 + 300 = 321
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 321.0),
        _ => panic!("Expected 321, got {:?}", result),
    }
}

// --- Closure Over Loop Variables ---

#[test]
fn test_closure_captures_loop_variable_value() {
    // Each closure should capture the value at the time of creation
    let code = r#"
    let funcs = []
    for (i : 0..3) {
        let captured = i
        fn f() {
            return captured
        }
        funcs.push(f)
    }
    let a = funcs.get(0)()
    let b = funcs.get(1)()
    let c = funcs.get(2)()
    a + b * 10 + c * 100
    "#;
    // a=0, b=1, c=2 -> 0 + 10 + 200 = 210
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 210.0),
        _ => panic!("Expected 210, got {:?}", result),
    }
}

// --- Closure with Multiple Functions Sharing State ---

#[test]
fn test_multiple_closures_share_state() {
    // Two functions that share and mutate the same variable
    let code = r#"
    fn makePair() {
        var shared = 0
        fn increment() {
            shared = shared + 1
            return shared
        }
        fn decrement() {
            shared = shared - 1
            return shared
        }
        return [increment, decrement]
    }
    let pair = makePair()
    let inc = pair.get(0)
    let dec = pair.get(1)
    inc()
    inc()
    inc()
    dec()
    inc()
    "#;
    // shared: 0 -> 1 -> 2 -> 3 -> 2 -> 3
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_closure_getter_setter() {
    let code = r#"
    fn makeCell(initial) {
        var value = initial
        fn get() {
            return value
        }
        fn set(new value) {
            value = new value
        }
        return [get, set]
    }
    let cell = makeCell(42)
    let get = cell.get(0)
    let set = cell.get(1)
    let before = get()
    set(100)
    let after = get()
    before + after * 10
    "#;
    // before=42, after=100 -> 42 + 1000 = 1042
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1042.0),
        _ => panic!("Expected 1042, got {:?}", result),
    }
}

// --- Closure and Recursion ---

#[test]
fn test_recursive_function_in_closure() {
    let code = r#"
    fn makeFactorial() {
        fn factorial(n) {
            if (n <= 1) {
                return 1
            }
            return n * factorial(n - 1)
        }
        return factorial
    }
    let fact = makeFactorial()
    fact(5)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 120.0),
        _ => panic!("Expected 120, got {:?}", result),
    }
}

#[test]
fn test_closure_with_recursive_helper() {
    let code = r#"
    fn makeFibonacci() {
        var calls = 0
        fn fib(n) {
            calls = calls + 1
            if (n <= 1) {
                return n
            }
            return fib(n - 1) + fib(n - 2)
        }
        fn get calls() {
            return calls
        }
        return [fib, get calls]
    }
    let pair = makeFibonacci()
    let fib = pair.get(0)
    let get calls = pair.get(1)
    let result = fib(6)
    let total calls = get calls()
    result + total calls * 1000
    "#;
    // fib(6) = 8, calls = 25 (for naive fib)
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 25008.0),
        _ => panic!("Expected 25008, got {:?}", result),
    }
}

// --- Edge Cases ---

#[test]
fn test_closure_captures_parameter() {
    let code = r#"
    fn outer(x) {
        fn inner() {
            return x * 2
        }
        return inner
    }
    let double five = outer(5)
    double five()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_closure_shadows_outer_variable() {
    let code = r#"
    fn outer() {
        let x = 10
        fn inner() {
            let x = 20
            return x
        }
        return inner() + x
    }
    outer()
    "#;
    // inner returns 20, outer's x is still 10, so 20 + 10 = 30
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 30.0),
        _ => panic!("Expected 30, got {:?}", result),
    }
}

#[test]
fn test_closure_does_not_affect_outer_immutable() {
    // Shadowing in closure doesn't affect outer scope
    let code = r#"
    fn test() {
        let x = 5
        fn inner() {
            let x = 100
            return x
        }
        let inner result = inner()
        return x + inner result
    }
    test()
    "#;
    // x in outer is still 5, inner returns 100, so 5 + 100 = 105
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 105.0),
        _ => panic!("Expected 105, got {:?}", result),
    }
}

#[test]
fn test_closure_returned_from_if() {
    let code = r#"
    fn choose(condition) {
        var x = 0
        if (condition) {
            fn yes() {
                x = x + 1
                return x
            }
            return yes
        } else {
            fn no() {
                x = x - 1
                return x
            }
            return no
        }
    }
    let inc = choose(true)
    let dec = choose(false)
    let a = inc()
    let b = inc()
    let c = dec()
    let d = dec()
    a + b * 10 + c * 100 + d * 1000
    "#;
    // inc: 1, 2 (separate x from dec)
    // dec: -1, -2
    // 1 + 20 + (-100) + (-2000) = -2079
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, -2079.0),
        _ => panic!("Expected -2079, got {:?}", result),
    }
}

#[test]
fn test_immediate_closure_invocation() {
    // Define and immediately call
    let code = r#"
    fn outer() {
        let x = 42
        fn inner() {
            return x
        }
        return inner
    }
    outer()()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 42.0),
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_closure_with_array_mutation() {
    let code = r#"
    fn makeAccumulator() {
        let items = []
        fn add(item) {
            items.push(item)
            return items.len()
        }
        fn get all() {
            return items
        }
        return [add, get all]
    }
    let acc = makeAccumulator()
    let add = acc.get(0)
    let get all = acc.get(1)
    add(1)
    add(2)
    add(3)
    get all().sum()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}

// --- Stress Tests ---

#[test]
fn test_many_closures() {
    let code = r#"
    fn make adder(n) {
        fn add(x) {
            return x + n
        }
        return add
    }
    let add1 = make adder(1)
    let add2 = make adder(2)
    let add3 = make adder(3)
    let add4 = make adder(4)
    let add5 = make adder(5)
    add1(0) + add2(0) + add3(0) + add4(0) + add5(0)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0),
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_closure_chain() {
    let code = r#"
    fn a(x) {
        fn b(y) {
            fn c(z) {
                return x + y + z
            }
            return c
        }
        return b
    }
    a(1)(2)(3)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}
