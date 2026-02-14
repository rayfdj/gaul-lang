use gaul_lang::config::RuntimeConfig;
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
    let mut interpreter = Interpreter::new(Environment::new(), RuntimeConfig::default());

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
                "Expected error on Line 5, but got: {}",
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

#[test]
fn test_range_to_array_basic() {
    let code = r#"
    let arr = (0..5).to_array()
    arr
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 5);
            assert_eq!(arr[0], Value::Num(0.0));
            assert_eq!(arr[4], Value::Num(4.0));
        }
        _ => panic!("Expected array [0,1,2,3,4], got {:?}", result),
    }
}

#[test]
fn test_range_to_array_sum() {
    // Convert range to array, then use array methods
    let code = r#"
    (1..6).to_array().sum()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0), // 1+2+3+4+5
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_range_to_array_chained() {
    // Range -> array -> reverse -> join
    let code = r#"
    (0..3).to_array().reverse().join("-")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(&*s, "2-1-0"),
        _ => panic!("Expected '2-1-0', got {:?}", result),
    }
}

#[test]
fn test_range_to_array_empty() {
    // Empty range (start == end)
    let code = r#"
    (5..5).to_array().len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_range_to_array_negative() {
    // Range with negative numbers
    let code = r#"
    let arr = (-2..2).to_array()
    arr.join(",")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(&*s, "-2,-1,0,1"),
        _ => panic!("Expected '-2,-1,0,1', got {:?}", result),
    }
}

#[test]
fn test_range_to_array_single_element() {
    let code = r#"
    (0..1).to_array().first()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
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

// ==================== STYLE & FORMATTING TESTS ====================

#[test]
fn test_function_allman_style() {
    // Scenario: Newline before function body brace
    let code = r#"
    fn add(a, b)
    {
        return a + b
    }
    add(10, 5)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0),
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_if_allman_style() {
    // Scenario: Newline before 'if' block brace
    let code = r#"
    let x = if (true)
    {
        100
    } else {
        0
    }
    x
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 100.0),
        _ => panic!("Expected 100, got {:?}", result),
    }
}

#[test]
fn test_else_allman_style() {
    // Scenario: Newline before 'else' block brace
    // (And mixed style: 'if' is K&R, 'else' is Allman)
    let code = r#"
    let x = if (false) {
        0
    } else
    {
        200
    }
    x
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 200.0),
        _ => panic!("Expected 200, got {:?}", result),
    }
}

#[test]
fn test_while_allman_style() {
    // Scenario: Newline before 'while' body brace
    let code = r#"
    var i = 0
    while (i < 3)
    {
        i = i + 1
    }
    i
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_for_allman_style() {
    // Scenario: Newline before 'for' body brace
    let code = r#"
    var sum = 0
    for (i : 0..3)
    {
        sum = sum + i
    }
    sum
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0), // 0+1+2
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_nested_allman_style() {
    // Scenario: Nested structures with excessive newlines
    let code = r#"
    fn check(n)

    {
        if (n > 0)

        {
            return true
        }

        return false
    }
    check(5)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_if_else_full_allman_style() {
    let code = r#"
    let result = if (false)
    {
        10
    }
    else
    {
        20
    }
    result
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 20.0),
        _ => panic!("Expected 20, got {:?}", result),
    }
}

// ==================== ANONYMOUS FUNCTION AMBIGUITY & EDGE CASES ====================

#[test]
fn test_anon_fn_in_expression_position() {
    // 'fn' usually starts a declaration.
    // Here it MUST be parsed as a primary expression because it's after '='.
    let code = r#"
    let adder = fn(a, b) {
        return a + b
    }
    adder(2, 3)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5, got {:?}", result),
    }
}

#[test]
fn test_anon_fn_call_ambiguity() {
    // Does the parser see 'fn' and think "Declaration"?
    // It should parse 'fn(x){...}' as a value, then see '(' and parse a Call.
    let code = r#"
    let result = fn(x) { return x * 2 }(10)
    result
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 20.0),
        _ => panic!("Expected 20 (IIFE), got {:?}", result),
    }
}

#[test]
fn test_anon_fn_natural_allman_style() {
    // Scenario: 'fn(x)' is on one line, but the body '{' starts on the next.
    // We do NOT support fn and then (x) on a newline, that's just too weird.
    let code = r#"
    let f = fn(x)
    {
        return x
    }
    f(10)
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_anon_fn_in_math_expression() {
    // Ensure 'fn' doesn't break the expression parser precedence.
    // (10 + <function>) should be valid (though adding a function to a number errors at runtime).
    // We test returning a number to be safe.
    let code = r#"
    let res = 10 + (fn() { return 5 })()
    res
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0),
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_anon_fn_trailing_newline_terminator() {
    // The closing '}' of the function body acts as a delimiter.
    // Does the parser realize the 'let' statement is over?
    // Or does it mistakenly try to consume the next line as part of the function?
    let code = r#"
    let f = fn() { 1 }
    f()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1, got {:?}", result),
    }
}

#[test]
fn test_anon_fn_vs_call_newline_separation() {
    // We declare f.
    // If the parser is correct, f is a function, and (10) is a separate statement.
    // If the parser is greedy, f becomes the result of the call (10).
    let code = r#"
    let f = fn(x) { return x }
    (10)
    f
    "#;

    let result = eval(code);
    match result {
        Ok(Value::Fn(_)) => {} // Success! f is a function.
        Ok(Value::Num(_)) => {
            panic!("Ambiguity error: Parser greedily consumed (10) as an argument!")
        }
        _ => panic!("Unexpected result: {:?}", result),
    }
}

#[test]
fn test_anon_fn_nested_in_call_args() {
    // Parsing nested structures
    let code = r#"
    fn apply(f, val) { return f(val) }

    apply(fn(x) {
        return x + 1
    }, 10)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 11.0),
        _ => panic!("Expected 11, got {:?}", result),
    }
}

#[test]
fn test_anon_fn_missing_braces_fail() {
    // Edge Case: Ensure we don't accidentally support implicit blocks like 'fn(x) x+1'
    let code = "let f = fn(x) x + 1";
    let result = eval(code);
    assert!(result.is_err(), "Expected error for missing braces body");
}

#[test]
fn test_anon_fn_missing_parens_fail() {
    // Edge Case: Ensure 'fn { ... }' is rejected (params required even if empty)
    let code = "let f = fn { return 1 }";
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for missing parameter parentheses"
    );
}

// ==================== MAP TESTS ====================

#[test]
fn test_map_basic() {
    let code = r#"
    let arr = [1, 2, 3]
    arr.map(fn(x) { return x * 2 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(2.0));
            assert_eq!(arr[1], Value::Num(4.0));
            assert_eq!(arr[2], Value::Num(6.0));
        }
        _ => panic!("Expected [2,4,6], got {:?}", result),
    }
}

#[test]
fn test_map_empty_array() {
    let code = r#"
    [].map(fn(x) { return x * 2 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            assert_eq!(elements.borrow().len(), 0);
        }
        _ => panic!("Expected empty array, got {:?}", result),
    }
}

#[test]
fn test_map_with_named_function() {
    let code = r#"
    fn double(x) { return x * 2 }
    [1, 2, 3].map(double)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Num(2.0));
            assert_eq!(arr[1], Value::Num(4.0));
            assert_eq!(arr[2], Value::Num(6.0));
        }
        _ => panic!("Expected [2,4,6], got {:?}", result),
    }
}

#[test]
fn test_map_with_multi_word_identifier() {
    // Gaul spirit: spaces in identifiers
    let code = r#"
    fn times two(x) { return x * 2 }
    let my numbers = [1, 2, 3]
    my numbers.map(times two)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Num(2.0));
            assert_eq!(arr[1], Value::Num(4.0));
            assert_eq!(arr[2], Value::Num(6.0));
        }
        _ => panic!("Expected [2,4,6], got {:?}", result),
    }
}

#[test]
fn test_map_closure_captures_variable() {
    let code = r#"
    let multiplier = 10
    [1, 2, 3].map(fn(x) { return x * multiplier })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Num(10.0));
            assert_eq!(arr[1], Value::Num(20.0));
            assert_eq!(arr[2], Value::Num(30.0));
        }
        _ => panic!("Expected [10,20,30], got {:?}", result),
    }
}

#[test]
fn test_map_chained() {
    let code = r#"
    [1, 2, 3].map(fn(x) { return x + 1 }).map(fn(x) { return x * 2 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            // [1,2,3] -> [2,3,4] -> [4,6,8]
            assert_eq!(arr[0], Value::Num(4.0));
            assert_eq!(arr[1], Value::Num(6.0));
            assert_eq!(arr[2], Value::Num(8.0));
        }
        _ => panic!("Expected [4,6,8], got {:?}", result),
    }
}

#[test]
fn test_map_returns_different_type() {
    // Map numbers to strings
    let code = r#"
    [1, 2, 3].map(fn(x) { return x.to_str() }).join("-")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(&*s, "1-2-3"),
        _ => panic!("Expected '1-2-3', got {:?}", result),
    }
}

#[test]
fn test_map_with_index_via_closure() {
    // Simulate index by capturing a mutable variable
    let code = r#"
    var idx = 0
    let result = ["a", "b", "c"].map(fn(x) {
        let current = idx
        idx = idx + 1
        return current.to_str() + ":" + x
    })
    result.join(",")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(&*s, "0:a,1:b,2:c"),
        _ => panic!("Expected '0:a,1:b,2:c', got {:?}", result),
    }
}

#[test]
fn test_map_allman_style() {
    let code = r#"
    [1, 2, 3].map(fn(x)
    {
        return x * 2
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Num(2.0));
        }
        _ => panic!("Expected array, got {:?}", result),
    }
}

#[test]
fn test_map_multiline_trailing_op() {
    let code = r#"
    ([1, 2, 3].
        map(fn(x) { return x * 2 }))
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Num(2.0));
        }
        _ => panic!("Expected array, got {:?}", result),
    }
}

#[test]
fn test_map_nested_arrays() {
    let code = r#"
    [[1, 2], [3, 4]].map(fn(inner) { return inner.sum() })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Num(3.0));
            assert_eq!(arr[1], Value::Num(7.0));
        }
        _ => panic!("Expected [3,7], got {:?}", result),
    }
}

// ==================== FILTER TESTS ====================

#[test]
fn test_filter_basic() {
    let code = r#"
    [1, 2, 3, 4, 5].filter(fn(x) { return x > 2 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(3.0));
            assert_eq!(arr[1], Value::Num(4.0));
            assert_eq!(arr[2], Value::Num(5.0));
        }
        _ => panic!("Expected [3,4,5], got {:?}", result),
    }
}

#[test]
fn test_filter_empty_array() {
    let code = r#"
    [].filter(fn(x) { return x > 0 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            assert_eq!(elements.borrow().len(), 0);
        }
        _ => panic!("Expected empty array, got {:?}", result),
    }
}

#[test]
fn test_filter_none_match() {
    let code = r#"
    [1, 2, 3].filter(fn(x) { return x > 100 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            assert_eq!(elements.borrow().len(), 0);
        }
        _ => panic!("Expected empty array, got {:?}", result),
    }
}

#[test]
fn test_filter_all_match() {
    let code = r#"
    [1, 2, 3].filter(fn(x) { return x > 0 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            assert_eq!(elements.borrow().len(), 3);
        }
        _ => panic!("Expected [1,2,3], got {:?}", result),
    }
}

#[test]
fn test_filter_with_named_function() {
    let code = r#"
    fn is even(x) { return x.mod(2) == 0 }
    [1, 2, 3, 4, 5, 6].filter(is even)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(2.0));
            assert_eq!(arr[1], Value::Num(4.0));
            assert_eq!(arr[2], Value::Num(6.0));
        }
        _ => panic!("Expected [2,4,6], got {:?}", result),
    }
}

#[test]
fn test_filter_closure_captures_variable() {
    let code = r#"
    let threshold = 3
    [1, 2, 3, 4, 5].filter(fn(x) { return x >= threshold })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(3.0));
        }
        _ => panic!("Expected [3,4,5], got {:?}", result),
    }
}

#[test]
fn test_filter_strings() {
    let code = r#"
    ["apple", "banana", "apricot", "cherry"].filter(fn(s) {
        return s.starts_with("a")
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 2);
            assert_eq!(arr[0], Value::Str("apple".into()));
            assert_eq!(arr[1], Value::Str("apricot".into()));
        }
        _ => panic!("Expected ['apple','apricot'], got {:?}", result),
    }
}

#[test]
fn test_filter_must_return_bool() {
    // Filter callback returns non-boolean - should error
    let code = r#"
    [1, 2, 3].filter(fn(x) { return x })
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for non-boolean filter callback, got {:?}",
        result
    );
}

#[test]
fn test_filter_chained_with_map() {
    let code = r#"
    ([1, 2, 3, 4, 5, 6]
        .filter(fn(x) { return x.mod(2) == 0 })
        .map(fn(x) { return x * 10 }))
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            // [2,4,6] -> [20,40,60]
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(20.0));
            assert_eq!(arr[1], Value::Num(40.0));
            assert_eq!(arr[2], Value::Num(60.0));
        }
        _ => panic!("Expected [20,40,60], got {:?}", result),
    }
}

#[test]
fn test_filter_allman_style() {
    let code = r#"
    [1, 2, 3, 4, 5].filter(fn(x)
    {
        return x > 2
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            assert_eq!(elements.borrow().len(), 3);
        }
        _ => panic!("Expected array with 3 elements, got {:?}", result),
    }
}

#[test]
fn test_filter_with_multi_word_identifier() {
    let code = r#"
    fn greater than two(x) { return x > 2 }
    let my list = [1, 2, 3, 4, 5]
    my list.filter(greater than two)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(3.0));
        }
        _ => panic!("Expected [3,4,5], got {:?}", result),
    }
}

// ==================== REDUCE TESTS ====================

#[test]
fn test_reduce_sum() {
    let code = r#"
    [1, 2, 3, 4, 5].reduce(0, fn(acc, x) { return acc + x })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0),
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_reduce_product() {
    let code = r#"
    [1, 2, 3, 4].reduce(1, fn(acc, x) { return acc * x })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 24.0),
        _ => panic!("Expected 24, got {:?}", result),
    }
}

#[test]
fn test_reduce_empty_array() {
    // With empty array, should return initial value
    let code = r#"
    [].reduce(42, fn(acc, x) { return acc + x })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 42.0),
        _ => panic!("Expected 42, got {:?}", result),
    }
}

#[test]
fn test_reduce_single_element() {
    let code = r#"
    [10].reduce(5, fn(acc, x) { return acc + x })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 15.0),
        _ => panic!("Expected 15, got {:?}", result),
    }
}

#[test]
fn test_reduce_string_concat() {
    let code = r#"
    ["a", "b", "c"].reduce("", fn(acc, x) { return acc + x })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(&*s, "abc"),
        _ => panic!("Expected 'abc', got {:?}", result),
    }
}

#[test]
fn test_reduce_build_array() {
    // Using reduce to build an array (doubling each element)
    let code = r#"
    [1, 2, 3].reduce([], fn(acc, x) {
        acc.push(x * 2)
        return acc
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr.len(), 3);
            assert_eq!(arr[0], Value::Num(2.0));
            assert_eq!(arr[1], Value::Num(4.0));
            assert_eq!(arr[2], Value::Num(6.0));
        }
        _ => panic!("Expected [2,4,6], got {:?}", result),
    }
}

#[test]
fn test_reduce_find_max() {
    let code = r#"
    [3, 1, 4, 1, 5, 9, 2, 6].reduce(0, fn(max, x) {
        if (x > max) { return x }
        return max
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 9.0),
        _ => panic!("Expected 9, got {:?}", result),
    }
}

#[test]
fn test_reduce_with_named_function() {
    let code = r#"
    fn add(a, b) { return a + b }
    [1, 2, 3, 4].reduce(0, add)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_reduce_with_multi_word_identifier() {
    let code = r#"
    fn add together(a, b) { return a + b }
    let my numbers = [1, 2, 3, 4]
    my numbers.reduce(0, add together)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 10.0),
        _ => panic!("Expected 10, got {:?}", result),
    }
}

#[test]
fn test_reduce_closure_captures_variable() {
    let code = r#"
    let multiplier = 2
    [1, 2, 3].reduce(0, fn(acc, x) { return acc + (x * multiplier) })
    "#;
    let result = eval(code);
    match result {
        // (1*2) + (2*2) + (3*2) = 2 + 4 + 6 = 12
        Ok(Value::Num(n)) => assert_eq!(n, 12.0),
        _ => panic!("Expected 12, got {:?}", result),
    }
}

#[test]
fn test_reduce_allman_style() {
    let code = r#"
    [1, 2, 3].reduce(0, fn(acc, x)
    {
        return acc + x
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}

#[test]
fn test_reduce_multiline_trailing_op() {
    let code = r#"
    ([1, 2, 3, 4, 5].
        filter(fn(x) { return x.mod(2) == 1 }).
        reduce(0, fn(acc, x) { return acc + x }))
    "#;
    let result = eval(code);
    match result {
        // odd numbers: 1, 3, 5 -> sum = 9
        Ok(Value::Num(n)) => assert_eq!(n, 9.0),
        _ => panic!("Expected 9, got {:?}", result),
    }
}

// ==================== COMBINED / PIPELINE TESTS ====================

#[test]
fn test_map_filter_reduce_pipeline() {
    let code = r#"
    ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        .filter(fn(x) { return x.mod(2) == 0 })
        .map(fn(x) { return x * x })
        .reduce(0, fn(acc, x) { return acc + x }))
    "#;
    let result = eval(code);
    match result {
        // evens: 2,4,6,8,10 -> squares: 4,16,36,64,100 -> sum: 220
        Ok(Value::Num(n)) => assert_eq!(n, 220.0),
        _ => panic!("Expected 220, got {:?}", result),
    }
}

#[test]
fn test_pipeline_with_multi_word_identifiers() {
    let code = r#"
    let is even = fn(x) { return x.mod(2) == 0 }
    let square it = fn(x) { return x * x }
    let add up = fn(acc, x) { return acc + x }

    let my numbers = [1, 2, 3, 4, 5, 6]
    my numbers.filter(is even).map(square it).reduce(0, add up)
    "#;
    let result = eval(code);
    match result {
        // evens: 2,4,6 -> squares: 4,16,36 -> sum: 56
        Ok(Value::Num(n)) => assert_eq!(n, 56.0),
        _ => panic!("Expected 56, got {:?}", result),
    }
}

#[test]
fn test_pipeline_preserves_closure_context() {
    let code = r#"
    let min val = 2
    let max val = 5
    let scale factor = 10

    ([1, 2, 3, 4, 5, 6]
        .filter(fn(x) { return (x >= min val) and (x <= max val) })
        .map(fn(x) { return x * scale factor })
        .reduce(0, fn(acc, x) { return acc + x }))
    "#;
    let result = eval(code);
    match result {
        // filter: 2,3,4,5 -> map: 20,30,40,50 -> sum: 140
        Ok(Value::Num(n)) => assert_eq!(n, 140.0),
        _ => panic!("Expected 140, got {:?}", result),
    }
}

#[test]
fn test_nested_map_operations() {
    // Map over array of arrays
    let code = r#"
    [[1, 2], [3, 4], [5, 6]].map(fn(inner) {
        return inner.map(fn(x) { return x * 2 }).sum()
    })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            // [1,2] -> [2,4] -> 6
            // [3,4] -> [6,8] -> 14
            // [5,6] -> [10,12] -> 22
            assert_eq!(arr[0], Value::Num(6.0));
            assert_eq!(arr[1], Value::Num(14.0));
            assert_eq!(arr[2], Value::Num(22.0));
        }
        _ => panic!("Expected [6,14,22], got {:?}", result),
    }
}

#[test]
fn test_filter_then_method_chain() {
    let code = r#"
    ([5, 2, 8, 1, 9, 3]
        .filter(fn(x) { return x > 3 })
        .sort()
        .reverse()
        .first())
    "#;
    let result = eval(code);
    match result {
        // filter: 5,8,9 -> sort: 5,8,9 -> reverse: 9,8,5 -> first: 9
        Ok(Value::Num(n)) => assert_eq!(n, 9.0),
        _ => panic!("Expected 9, got {:?}", result),
    }
}

#[test]
fn test_reduce_returns_function() {
    // Reduce can return any type, including functions
    let code = r#"
    let composers = [
        fn(x) { return x + 1 },
        fn(x) { return x * 2 },
        fn(x) { return x + 10 }
    ]

    let composed = composers.reduce(fn(x) { return x }, fn(acc, f) {
        return fn(x) { return f(acc(x)) }
    })

    composed(5)
    "#;
    let result = eval(code);
    match result {
        // ((5 + 1) * 2) + 10 = 22
        Ok(Value::Num(n)) => assert_eq!(n, 22.0),
        _ => panic!("Expected 22, got {:?}", result),
    }
}

// ==================== ERROR CASES ====================

#[test]
fn test_map_no_argument_fails() {
    let code = r#"
    [1, 2, 3].map()
    "#;
    let result = eval(code);
    assert!(result.is_err(), "Expected error for map with no arguments");
}

#[test]
fn test_filter_no_argument_fails() {
    let code = r#"
    [1, 2, 3].filter()
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for filter with no arguments"
    );
}

#[test]
fn test_reduce_missing_callback_fails() {
    let code = r#"
    [1, 2, 3].reduce(0)
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for reduce with only initial value"
    );
}

#[test]
fn test_reduce_no_arguments_fails() {
    let code = r#"
    [1, 2, 3].reduce()
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for reduce with no arguments"
    );
}

#[test]
fn test_map_non_function_fails() {
    let code = r#"
    [1, 2, 3].map(42)
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for map with non-function argument"
    );
}

#[test]
fn test_filter_non_function_fails() {
    let code = r#"
    [1, 2, 3].filter("not a function")
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for filter with non-function argument"
    );
}

#[test]
fn test_reduce_non_function_callback_fails() {
    let code = r#"
    [1, 2, 3].reduce(0, "not a function")
    "#;
    let result = eval(code);
    assert!(
        result.is_err(),
        "Expected error for reduce with non-function callback"
    );
}

// ==================== JAM KARET (~=) OPERATOR TESTS ====================

// --- Numeric Approximate Equality ---

#[test]
fn test_jam_karet_numbers_equal() {
    let result = eval("100 ~= 100");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_numbers_within_tolerance() {
    // 100 vs 104 is 4% difference, within default 5%
    let result = eval("100 ~= 104");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!(
            "Expected true (4% diff within 5% tolerance), got {:?}",
            result
        ),
    }
}

#[test]
fn test_jam_karet_numbers_outside_tolerance() {
    // 100 vs 110 is 10% difference, outside default 5%
    let result = eval("100 ~= 110");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!(
            "Expected false (10% diff outside 5% tolerance), got {:?}",
            result
        ),
    }
}

#[test]
fn test_jam_karet_numbers_boundary_exactly_5_percent() {
    // 100 vs 105 is exactly 5%, should be true (<=)
    let result = eval("100 ~= 105");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true (exactly 5% tolerance), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_numbers_just_over_tolerance() {
    // 100 vs 105.5 -> diff = 5.5, max = 105.5, relative = 5.21% > 5%
    let result = eval("100 ~= 105.5");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false (just over 5% tolerance), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_numbers_symmetric() {
    // Order shouldn't matter
    let result1 = eval("100 ~= 104");
    let result2 = eval("104 ~= 100");
    match (result1, result2) {
        (Ok(Value::Bool(b1)), Ok(Value::Bool(b2))) => {
            assert_eq!(b1, b2, "~= should be symmetric");
            assert!(b1);
        }
        _ => panic!("Expected both to be true"),
    }
}

#[test]
fn test_jam_karet_small_numbers() {
    // 1 vs 1.04 is 4% difference
    let result = eval("1 ~= 1.04");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_small_numbers_outside_tolerance() {
    // 1 vs 2 is 100% difference
    let result = eval("1 ~= 2");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false (100% diff), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_large_numbers() {
    // 1000000 vs 1040000 is 4% difference
    let result = eval("1000000 ~= 1040000");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_negative_numbers() {
    // -100 vs -104 is 4% difference
    let result = eval("(-100) ~= (-104)");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_negative_outside_tolerance() {
    // -100 vs -110 is 10% difference
    let result = eval("(-100) ~= (-110)");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_mixed_sign_numbers() {
    // -1 vs 1 should be way outside tolerance
    let result = eval("(-1) ~= 1");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false (opposite signs), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_zero_vs_zero() {
    let result = eval("0 ~= 0");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_zero_vs_small() {
    // 0 vs 0.001 - relative diff is infinite/undefined, should be false
    let result = eval("0 ~= 0.001");
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false (0 vs non-zero), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_floats() {
    // 3.14159 vs 3.14 is ~0.05% difference
    let result = eval("3.14159 ~= 3.14");
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_in_expression() {
    let code = r#"
    let a = 100
    let b = 103
    if (a ~= b) { "close enough" } else { "not close" }
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "close enough"),
        _ => panic!("Expected 'close enough', got {:?}", result),
    }
}

#[test]
fn test_jam_karet_in_while_condition() {
    let code = r#"
    var x = 100
    var iterations = 0
    while (x ~= 100) {
        x = x + 1
        iterations = iterations + 1
        if (iterations > 20) { break }
    }
    iterations
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => {
            // Should iterate while within 5% of 100 (up to 105)
            assert!(n >= 5.0 && n <= 6.0, "Expected ~5-6 iterations, got {}", n);
        }
        _ => panic!("Expected number, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_chained_comparisons() {
    // a ~= b and b ~= c doesn't mean a ~= c (not transitive)
    let code = r#"
    let a = 100
    let b = 104
    let c = 108
    let ab = a ~= b
    let bc = b ~= c
    let ac = a ~= c
    [ab, bc, ac]
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Bool(true)); // 100 ~= 104 (4%)
            assert_eq!(arr[1], Value::Bool(true)); // 104 ~= 108 (~3.8%)
            assert_eq!(arr[2], Value::Bool(false)); // 100 ~= 108 (8%)
        }
        _ => panic!("Expected array of bools, got {:?}", result),
    }
}

// --- String Approximate Equality (Levenshtein) ---

#[test]
fn test_jam_karet_strings_equal() {
    let result = eval(r#""hello" ~= "hello""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_one_typo() {
    // "hello" vs "helo" - distance 1, threshold for 5 chars is 1
    let result = eval(r#""hello" ~= "helo""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true (1 edit), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_one_substitution() {
    // "hello" vs "hallo" - distance 1
    let result = eval(r#""hello" ~= "hallo""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true (1 substitution), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_one_insertion() {
    // "hello" vs "helloo" - distance 1
    let result = eval(r#""hello" ~= "helloo""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true (1 insertion), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_too_different() {
    // "hello" vs "world" - distance 4, threshold for 5 chars is 1
    let result = eval(r#""hello" ~= "world""#);
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false (too many edits), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_longer_more_tolerance() {
    // "programming" (11 chars) vs "programing" (10 chars) - distance 1
    // threshold = max(11, 10) / 5 = 2
    let result = eval(r#""programming" ~= "programing""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_longer_two_typos() {
    // "programming" vs "programing" with extra typo
    // "programming" vs "progrming" - distance 2, threshold 2
    let result = eval(r#""programming" ~= "progrming""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true (2 edits within threshold), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_case_sensitive() {
    // "Hello" vs "hello" - distance 1
    let result = eval(r#""Hello" ~= "hello""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true (case counts as 1 edit), got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_empty_vs_empty() {
    let result = eval(r#""" ~= """#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_empty_vs_short() {
    // "" vs "a" - distance 1, threshold max(0,1)/5 = 0, but min 1
    let result = eval(r#""" ~= "a""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!(
            "Expected true (within min threshold of 1), got {:?}",
            result
        ),
    }
}

#[test]
fn test_jam_karet_strings_empty_vs_longer() {
    // "" vs "hello" - distance 5, threshold 1
    let result = eval(r#""" ~= "hello""#);
    match result {
        Ok(Value::Bool(b)) => assert!(!b),
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_symmetric() {
    let result1 = eval(r#""hello" ~= "helo""#);
    let result2 = eval(r#""helo" ~= "hello""#);
    match (result1, result2) {
        (Ok(Value::Bool(b1)), Ok(Value::Bool(b2))) => {
            assert_eq!(b1, b2, "String ~= should be symmetric");
        }
        _ => panic!("Expected both to be bools"),
    }
}

#[test]
fn test_jam_karet_indonesian_typos() {
    // Common Indonesian misspellings
    // "terima kasih" vs "trima kasih" - 1 deletion
    let result = eval(r#""terima kasih" ~= "trima kasih""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_indonesian_typos_2() {
    // "selamat" vs "slamat" - 1 deletion, threshold 7/5 = 1
    let result = eval(r#""selamat" ~= "slamat""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_strings_transposition() {
    // "receive" vs "recieve" - distance 2
    // threshold = ceil(7 * 0.2) = ceil(1.4) = 2, so this is within tolerance
    let result = eval(r#""receive" ~= "recieve""#);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!(
            "Expected true (distance 2 within threshold 2), got {:?}",
            result
        ),
    }
}

#[test]
fn test_jam_karet_strings_in_filter() {
    let code = r#"
    let names = ["John", "Jon", "Jane", "Joan"]
    names.filter(fn(name) { return name ~= "John" })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            // "John" ~= "John" (0), "Jon" ~= "John" (1), "Jane" ~= "John" (2), "Joan" ~= "John" (1)
            // threshold for 4 chars = ceil(4 * 0.2) = 1, so John, Jon, and Joan match
            assert_eq!(arr.len(), 3);
        }
        _ => panic!("Expected array with 3 elements, got {:?}", result),
    }
}

// --- Precedence and Associativity ---

#[test]
fn test_jam_karet_precedence_with_and() {
    // ~= should have same precedence as ==
    let code = "100 ~= 103 and 200 ~= 205";
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert!(b),
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_precedence_with_or() {
    let code = "100 ~= 150 or 200 ~= 205";
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert!(b), // first false, second true
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_precedence_with_comparison() {
    // Comparison (<) should be tighter than ~=
    // This parses as: (5 < 10) ~= true, which would be type error
    // OR: 5 < (10 ~= true) which is also weird
    // Actually ~= is at equality level, so: (5 < 10) ~= true
    // But ~= on bool vs bool... depends on implementation
    // Let's test a clearer case
    let code = "(5 < 10) == (100 ~= 103)";
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert!(b), // true == true
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_not_operator() {
    let code = "!(100 ~= 200)";
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert!(b), // 100 ~= 200 is false, !false = true
        _ => panic!("Expected true, got {:?}", result),
    }
}

// --- Type Errors ---

#[test]
fn test_jam_karet_number_vs_string_error() {
    let result = eval(r#"100 ~= "100""#);
    assert!(result.is_err(), "Expected error for number ~= string");
}

#[test]
fn test_jam_karet_bool_error() {
    let result = eval("true ~= false");
    assert!(result.is_err(), "Expected error for bool ~= bool");
}

#[test]
fn test_jam_karet_null_error() {
    let result = eval("null ~= null");
    assert!(result.is_err(), "Expected error for null ~= null");
}

#[test]
fn test_jam_karet_array_error() {
    let result = eval("[1, 2] ~= [1, 2]");
    assert!(result.is_err(), "Expected error for array ~= array");
}

#[test]
fn test_jam_karet_function_error() {
    let code = r#"
    fn a() { return 1 }
    fn b() { return 1 }
    a ~= b
    "#;
    let result = eval(code);
    assert!(result.is_err(), "Expected error for function ~= function");
}

// --- Real-world Use Cases ---

#[test]
fn test_jam_karet_financial_comparison() {
    // Comparing prices that might have rounding differences
    let code = r#"
    let expected price = 99.99
    let actual price = 100.02
    if (expected price ~= actual price) {
        "price acceptable"
    } else {
        "price mismatch"
    }
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "price acceptable"),
        _ => panic!("Expected 'price acceptable', got {:?}", result),
    }
}

#[test]
fn test_jam_karet_measurement_tolerance() {
    // Scientific measurements with tolerance
    let code = r#"
    let expected = 9.81
    let measured = 9.78
    expected ~= measured
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(b)) => assert!(b), // ~0.3% difference
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_fuzzy_search() {
    // Finding approximate matches in a list
    let code = r#"
    let products = ["iPhone", "iPad", "iPod", "iMac", "MacBook"]
    let search = "ifone"
    products.filter(fn(p) { return p ~= search })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            // "iPhone" vs "ifone" - distance 2 (P->f, h->deleted? Let's see)
            // Actually: iPhone -> ifone needs: P->f (1), h deleted (1) = 2
            // threshold = 6/5 = 1, so might not match
            // This test documents actual behavior
            assert!(arr.len() <= 1);
        }
        _ => panic!("Expected array, got {:?}", result),
    }
}

#[test]
fn test_jam_karet_grade_boundaries() {
    // Flexible grade boundaries
    let code = r#"
    fn is passing(score, threshold) {
        return (score ~= threshold) or (score > threshold)
    }
    let results = [
        is passing(58, 60),
        is passing(55, 60),
        is passing(62, 60)
    ]
    results
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Array(elements)) => {
            let arr = elements.borrow();
            assert_eq!(arr[0], Value::Bool(true)); // 58 ~= 60 (3.3%)
            assert_eq!(arr[1], Value::Bool(false)); // 55 ~= 60 (8.3%)
            assert_eq!(arr[2], Value::Bool(true)); // 62 > 60
        }
        _ => panic!("Expected array, got {:?}", result),
    }
}

// =============================================================================
// Map tests
// =============================================================================

#[test]
fn test_map_literal_empty() {
    let result = eval("[:]");
    match result {
        Ok(Value::Map(m)) => assert!(m.borrow().is_empty()),
        _ => panic!("Expected empty map, got {:?}", result),
    }
}

#[test]
fn test_map_literal_string_keys() {
    let code = r#"["name": "Alice", "age": 30]"#;
    let result = eval(code);
    match result {
        Ok(Value::Map(m)) => assert_eq!(m.borrow().len(), 2),
        _ => panic!("Expected map with 2 entries, got {:?}", result),
    }
}

#[test]
fn test_map_literal_numeric_keys() {
    let code = "[1: \"one\", 2: \"two\", 3: \"three\"]";
    let result = eval(code);
    match result {
        Ok(Value::Map(m)) => assert_eq!(m.borrow().len(), 3),
        _ => panic!("Expected map with 3 entries, got {:?}", result),
    }
}

#[test]
fn test_map_literal_bool_keys() {
    let code = "[true: \"yes\", false: \"no\"]";
    let result = eval(code);
    match result {
        Ok(Value::Map(m)) => assert_eq!(m.borrow().len(), 2),
        _ => panic!("Expected map with 2 entries, got {:?}", result),
    }
}

#[test]
fn test_map_literal_mixed_keys() {
    let code = r#"["name": "Alice", 1: "one", true: "yes"]"#;
    let result = eval(code);
    match result {
        Ok(Value::Map(m)) => assert_eq!(m.borrow().len(), 3),
        _ => panic!("Expected map with 3 entries, got {:?}", result),
    }
}

#[test]
fn test_map_literal_single_entry() {
    let code = r#"["key": "value"]"#;
    let result = eval(code);
    match result {
        Ok(Value::Map(m)) => assert_eq!(m.borrow().len(), 1),
        _ => panic!("Expected map with 1 entry, got {:?}", result),
    }
}

#[test]
fn test_map_literal_duplicate_key_last_wins() {
    let code = r#"
    let m = ["a": 1, "a": 2]
    m.get("a")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2 (last wins), got {:?}", result),
    }
}

#[test]
fn test_array_single_element_regression() {
    // [42] must still parse as a single-element array, not a map
    let result = eval("[42]");
    match result {
        Ok(Value::Array(a)) => {
            let arr = a.borrow();
            assert_eq!(arr.len(), 1);
            assert_eq!(arr[0], Value::Num(42.0));
        }
        _ => panic!("Expected [42] as array, got {:?}", result),
    }
}

#[test]
fn test_array_empty_regression() {
    let result = eval("[]");
    match result {
        Ok(Value::Array(a)) => assert!(a.borrow().is_empty()),
        _ => panic!("Expected empty array, got {:?}", result),
    }
}

#[test]
fn test_map_get_existing_key() {
    let code = r#"
    let m = ["name": "Alice", "age": 30]
    m.get("name")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "Alice"),
        _ => panic!("Expected 'Alice', got {:?}", result),
    }
}

#[test]
fn test_map_get_missing_key() {
    let code = r#"
    let m = ["name": "Alice"]
    m.get("missing")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected null for missing key, got {:?}", result),
    }
}

#[test]
fn test_map_get_or_existing() {
    let code = r#"
    let m = ["name": "Alice"]
    m.get_or("name", "default")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "Alice"),
        _ => panic!("Expected 'Alice', got {:?}", result),
    }
}

#[test]
fn test_map_get_or_missing() {
    let code = r#"
    let m = ["name": "Alice"]
    m.get_or("missing", "default")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "default"),
        _ => panic!("Expected 'default', got {:?}", result),
    }
}

#[test]
fn test_map_set_new_key() {
    let code = r#"
    let m = [:]
    m.set("key", "value")
    m.get("key")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "value"),
        _ => panic!("Expected 'value', got {:?}", result),
    }
}

#[test]
fn test_map_set_overwrite() {
    let code = r#"
    let m = ["key": "old"]
    m.set("key", "new")
    m.get("key")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "new"),
        _ => panic!("Expected 'new', got {:?}", result),
    }
}

#[test]
fn test_map_set_returns_null() {
    let code = r#"
    let m = [:]
    m.set("key", "value")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected null from set, got {:?}", result),
    }
}

#[test]
fn test_map_has_true() {
    let code = r#"
    let m = ["key": "value"]
    m.has("key")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(true)) => {}
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_map_has_false() {
    let code = r#"
    let m = ["key": "value"]
    m.has("missing")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(false)) => {}
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_map_remove_existing() {
    let code = r#"
    let m = ["key": "value"]
    m.remove("key")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "value"),
        _ => panic!("Expected removed value 'value', got {:?}", result),
    }
}

#[test]
fn test_map_remove_missing() {
    let code = r#"
    let m = ["key": "value"]
    m.remove("missing")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected null for missing remove, got {:?}", result),
    }
}

#[test]
fn test_map_remove_actually_removes() {
    let code = r#"
    let m = ["key": "value"]
    m.remove("key")
    m.has("key")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(false)) => {}
        _ => panic!("Expected false after remove, got {:?}", result),
    }
}

#[test]
fn test_map_keys() {
    let code = r#"
    let m = ["a": 1, "b": 2]
    m.keys().len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2 keys, got {:?}", result),
    }
}

#[test]
fn test_map_values() {
    let code = r#"
    let m = ["a": 1, "b": 2]
    m.values().len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2 values, got {:?}", result),
    }
}

#[test]
fn test_map_entries() {
    let code = r#"
    let m = ["a": 1]
    let entries = m.entries()
    entries.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1 entry, got {:?}", result),
    }
}

#[test]
fn test_map_entries_structure() {
    let code = r#"
    let m = ["key": "val"]
    let entry = m.entries().get(0)
    entry.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected entry of length 2, got {:?}", result),
    }
}

#[test]
fn test_map_len() {
    let code = r#"
    let m = ["a": 1, "b": 2, "c": 3]
    m.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_map_len_empty() {
    let code = r#"
    let m = [:]
    m.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_map_is_empty_true() {
    let code = r#"
    let m = [:]
    m.is_empty()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(true)) => {}
        _ => panic!("Expected true, got {:?}", result),
    }
}

#[test]
fn test_map_is_empty_false() {
    let code = r#"
    let m = ["a": 1]
    m.is_empty()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(false)) => {}
        _ => panic!("Expected false, got {:?}", result),
    }
}

#[test]
fn test_map_for_loop() {
    let code = r#"
    let m = ["a": 1, "b": 2, "c": 3]
    var count = 0
    for(k : m) {
        count = count + 1
    }
    count
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_map_for_loop_empty() {
    let code = r#"
    var count = 0
    for(k : [:]) {
        count = count + 1
    }
    count
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_map_for_loop_break() {
    let code = r#"
    let m = ["a": 1, "b": 2, "c": 3]
    var count = 0
    for(k : m) {
        count = count + 1
        if(count == 2) { break }
    }
    count
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 2.0),
        _ => panic!("Expected 2, got {:?}", result),
    }
}

#[test]
fn test_map_equality() {
    let code = r#"
    let a = ["x": 1, "y": 2]
    let b = ["x": 1, "y": 2]
    a == b
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(true)) => {}
        _ => panic!("Expected true for equal maps, got {:?}", result),
    }
}

#[test]
fn test_map_inequality() {
    let code = r#"
    let a = ["x": 1]
    let b = ["x": 2]
    a != b
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Bool(true)) => {}
        _ => panic!("Expected true for unequal maps, got {:?}", result),
    }
}

#[test]
fn test_map_empty_equality() {
    let code = "[:]  == [:]";
    let result = eval(code);
    match result {
        Ok(Value::Bool(true)) => {}
        _ => panic!("Expected true for empty maps, got {:?}", result),
    }
}

#[test]
fn test_map_error_function_as_key() {
    let code = r#"
    let f = fn(x) { x }
    [f: "value"]
    "#;
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_map_error_array_as_key() {
    let code = r#"
    let a = [1, 2, 3]
    [a: "value"]
    "#;
    let result = eval(code);
    assert!(result.is_err());
}

#[test]
fn test_map_computed_keys() {
    let code = r#"
    let key = "name"
    let m = [key: "Alice"]
    m.get("name")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "Alice"),
        _ => panic!("Expected 'Alice', got {:?}", result),
    }
}

#[test]
fn test_map_display_empty() {
    let code = r#"
    let m = [:]
    m
    "#;
    let result = eval(code);
    match &result {
        Ok(val @ Value::Map(_)) => {
            assert_eq!(format!("{}", val), "[:]");
        }
        _ => panic!("Expected map, got {:?}", result),
    }
}

#[test]
fn test_map_frequency_counting() {
    // Classic AOC pattern: frequency counting
    let code = r#"
    let words = ["apple", "banana", "apple", "cherry", "banana", "apple"]
    let counts = [:]
    for(word : words) {
        let current = counts.get_or(word, 0)
        counts.set(word, current + 1)
    }
    counts.get("apple")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 3.0),
        _ => panic!("Expected 3, got {:?}", result),
    }
}

#[test]
fn test_map_as_visited_set() {
    // Classic AOC pattern: visited set
    let code = r#"
    let visited = [:]
    let items = [1, 2, 3, 2, 1, 4]
    var unique count = 0
    for(item : items) {
        if(!visited.has(item)) {
            visited.set(item, true)
            unique count = unique count + 1
        }
    }
    unique count
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected 4 unique items, got {:?}", result),
    }
}

#[test]
fn test_map_null_key() {
    let code = r#"
    let m = [null: "nothing"]
    m.get(null)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "nothing"),
        _ => panic!("Expected 'nothing', got {:?}", result),
    }
}

#[test]
fn test_map_get_numeric_key() {
    let code = r#"
    let m = [42: "answer"]
    m.get(42)
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "answer"),
        _ => panic!("Expected 'answer', got {:?}", result),
    }
}

#[test]
fn test_map_for_loop_uses_keys() {
    // Verify that for-loop iterates over keys, and we can use them to look up values
    let code = r#"
    let m = ["a": 10, "b": 20]
    var total = 0
    for(k : m) {
        total = total + m.get(k)
    }
    total
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 30.0),
        _ => panic!("Expected 30, got {:?}", result),
    }
}

// --- String replace ---

#[test]
fn test_string_replace_basic() {
    let result = eval(r#""hello world".replace("world", "gaul")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello gaul"),
        _ => panic!("Expected 'hello gaul', got {:?}", result),
    }
}

#[test]
fn test_string_replace_multiple_occurrences() {
    let result = eval(r#""aaa".replace("a", "bb")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "bbbbbb"),
        _ => panic!("Expected 'bbbbbb', got {:?}", result),
    }
}

#[test]
fn test_string_replace_no_match() {
    let result = eval(r#""hello".replace("xyz", "abc")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

#[test]
fn test_string_replace_with_empty() {
    let result = eval(r#""hello world".replace(" world", "")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

#[test]
fn test_string_replace_empty_source() {
    let result = eval(r#""".replace("a", "b")"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), ""),
        _ => panic!("Expected '', got {:?}", result),
    }
}

// --- String index_of ---

#[test]
fn test_string_index_of_found() {
    let result = eval(r#""hello world".index_of("world")"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 6.0),
        _ => panic!("Expected 6, got {:?}", result),
    }
}

#[test]
fn test_string_index_of_not_found() {
    let result = eval(r#""hello".index_of("xyz")"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, -1.0),
        _ => panic!("Expected -1, got {:?}", result),
    }
}

#[test]
fn test_string_index_of_at_start() {
    let result = eval(r#""hello".index_of("hel")"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_string_index_of_first_occurrence() {
    let result = eval(r#""abcabc".index_of("bc")"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1, got {:?}", result),
    }
}

#[test]
fn test_string_index_of_unicode() {
    // "café" — the 'é' is multi-byte in UTF-8
    let result = eval(r#""café latte".index_of("latte")"#);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 5.0),
        _ => panic!("Expected 5 (char index, not byte index), got {:?}", result),
    }
}

// --- String to_upper / to_lower ---

#[test]
fn test_string_to_upper_basic() {
    let result = eval(r#""hello".to_upper()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "HELLO"),
        _ => panic!("Expected 'HELLO', got {:?}", result),
    }
}

#[test]
fn test_string_to_upper_already_upper() {
    let result = eval(r#""HELLO".to_upper()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "HELLO"),
        _ => panic!("Expected 'HELLO', got {:?}", result),
    }
}

#[test]
fn test_string_to_upper_mixed() {
    let result = eval(r#""Hello World".to_upper()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "HELLO WORLD"),
        _ => panic!("Expected 'HELLO WORLD', got {:?}", result),
    }
}

#[test]
fn test_string_to_upper_numbers_unchanged() {
    let result = eval(r#""abc123!".to_upper()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "ABC123!"),
        _ => panic!("Expected 'ABC123!', got {:?}", result),
    }
}

#[test]
fn test_string_to_lower_basic() {
    let result = eval(r#""HELLO".to_lower()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

#[test]
fn test_string_to_lower_already_lower() {
    let result = eval(r#""hello".to_lower()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

#[test]
fn test_string_to_lower_mixed() {
    let result = eval(r#""Hello World".to_lower()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello world"),
        _ => panic!("Expected 'hello world', got {:?}", result),
    }
}

#[test]
fn test_string_to_lower_numbers_unchanged() {
    let result = eval(r#""ABC123!".to_lower()"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "abc123!"),
        _ => panic!("Expected 'abc123!', got {:?}", result),
    }
}

// --- String repeat ---

#[test]
fn test_string_repeat_basic() {
    let result = eval(r#""ab".repeat(3)"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "ababab"),
        _ => panic!("Expected 'ababab', got {:?}", result),
    }
}

#[test]
fn test_string_repeat_zero() {
    let result = eval(r#""hello".repeat(0)"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), ""),
        _ => panic!("Expected '', got {:?}", result),
    }
}

#[test]
fn test_string_repeat_one() {
    let result = eval(r#""hello".repeat(1)"#);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "hello"),
        _ => panic!("Expected 'hello', got {:?}", result),
    }
}

// --- Array slice ---

#[test]
fn test_array_slice_basic() {
    let code = r#"
    let arr = [10, 20, 30, 40, 50]
    arr.slice(1, 3).join(",")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "20,30"),
        _ => panic!("Expected '20,30', got {:?}", result),
    }
}

#[test]
fn test_array_slice_to_end() {
    let code = r#"
    let arr = [10, 20, 30, 40, 50]
    arr.slice(2).join(",")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "30,40,50"),
        _ => panic!("Expected '30,40,50', got {:?}", result),
    }
}

#[test]
fn test_array_slice_empty_result() {
    let code = r#"
    let arr = [10, 20, 30]
    arr.slice(2, 2).len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 0.0),
        _ => panic!("Expected 0, got {:?}", result),
    }
}

#[test]
fn test_array_slice_full() {
    let code = r#"
    let arr = [10, 20, 30]
    arr.slice(0, 3).join(",")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "10,20,30"),
        _ => panic!("Expected '10,20,30', got {:?}", result),
    }
}

#[test]
fn test_array_slice_out_of_bounds() {
    let code = r#"
    let arr = [10, 20, 30]
    arr.slice(1, 100).join(",")
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Str(s)) => assert_eq!(s.as_ref(), "20,30"),
        _ => panic!("Expected '20,30', got {:?}", result),
    }
}

#[test]
fn test_array_slice_does_not_mutate() {
    let code = r#"
    var arr = [10, 20, 30, 40]
    let sliced = arr.slice(1, 3)
    arr.len()
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected 4 (original unchanged), got {:?}", result),
    }
}

// --- Array index_of ---

#[test]
fn test_array_index_of_found() {
    let code = r#"[10, 20, 30].index_of(20)"#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1, got {:?}", result),
    }
}

#[test]
fn test_array_index_of_not_found() {
    let code = r#"[10, 20, 30].index_of(99)"#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, -1.0),
        _ => panic!("Expected -1, got {:?}", result),
    }
}

#[test]
fn test_array_index_of_first_duplicate() {
    let code = r#"[10, 20, 20, 30].index_of(20)"#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1 (first occurrence), got {:?}", result),
    }
}

#[test]
fn test_array_index_of_string() {
    let code = r#"["a", "b", "c"].index_of("b")"#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 1.0),
        _ => panic!("Expected 1, got {:?}", result),
    }
}

// --- Array find ---

#[test]
fn test_array_find_found() {
    let code = r#"
    [1, 2, 3, 4, 5].find(fn(x) { x > 3 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 4.0),
        _ => panic!("Expected 4, got {:?}", result),
    }
}

#[test]
fn test_array_find_not_found() {
    let code = r#"
    [1, 2, 3].find(fn(x) { x > 10 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Null) => {}
        _ => panic!("Expected null, got {:?}", result),
    }
}

#[test]
fn test_array_find_returns_first_match() {
    let code = r#"
    [10, 20, 30, 40].find(fn(x) { x > 15 })
    "#;
    let result = eval(code);
    match result {
        Ok(Value::Num(n)) => assert_eq!(n, 20.0),
        _ => panic!("Expected 20 (first match), got {:?}", result),
    }
}

#[test]
fn test_array_find_predicate_must_return_bool() {
    let code = r#"
    [1, 2, 3].find(fn(x) { x })
    "#;
    let result = eval(code);
    assert!(result.is_err(), "Expected error, got {:?}", result);
}
