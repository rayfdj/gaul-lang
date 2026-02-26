pub mod token;

use crate::scanner::token::{Token, TokenType};
use crate::span::Span;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Nesting {
    Paren,   // (...) - Suppresses newlines
    Brace,   // {...} - Allows newlines (statements)
    Bracket, // [...] - Suppresses newlines
}

#[derive(Debug, Clone)]
pub struct ScanError {
    pub span: Span,
    pub message: String,
}

pub struct ScanResult {
    pub tokens: Vec<Token>,
    pub errors: Vec<ScanError>,
}

impl ScanResult {
    pub fn tokens_without_comments(&self) -> Vec<Token> {
        self.tokens
            .iter()
            .filter(|t| !matches!(t.token_type, TokenType::Comment))
            .cloned()
            .collect()
    }
}

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    errors: Vec<ScanError>,
    start: usize,
    current: usize,
    line: usize,
    line_start: usize,
    keywords: HashMap<String, TokenType>,
    nesting: Vec<Nesting>, // turn out we need to track nesting... not only depths
}

impl Scanner {
    pub fn new(source: impl Into<String>, keywords: &HashMap<String, TokenType>) -> Self {
        Scanner {
            source: source.into().chars().collect(),
            tokens: Vec::new(),
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            line_start: 0,
            keywords: keywords.clone(),
            nesting: Vec::new(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn scan_tokens(mut self) -> ScanResult {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        let eof_span = Span {
            line: self.line,
            col: self.current - self.line_start + 1,
            length: 0,
        };
        self.tokens.push(Token::new(TokenType::Eof, "", eof_span));
        ScanResult {
            tokens: self.tokens,
            errors: self.errors,
        }
    }

    fn scan_token(&mut self) {
        let c = self.advance();
        match c {
            '(' => {
                self.nesting.push(Nesting::Paren);
                self.add_token(TokenType::LeftParen)
            }
            ')' => {
                if let Some(Nesting::Paren) = self.nesting.last() {
                    self.nesting.pop();
                } else {
                    self.report_error("Unmatched ')'");
                }
                self.add_token(TokenType::RightParen)
            }
            '{' => {
                self.nesting.push(Nesting::Brace); // Pushing Brace allows newlines again!
                self.add_token(TokenType::LeftBrace)
            }
            '}' => {
                if let Some(Nesting::Brace) = self.nesting.last() {
                    self.nesting.pop();
                }
                // We don't error here to be lenient (hmmm or should we? TBD)
                self.add_token(TokenType::RightBrace)
            }
            '[' => {
                self.nesting.push(Nesting::Bracket);
                self.add_token(TokenType::LeftBracket)
            }
            ']' => {
                if let Some(Nesting::Bracket) = self.nesting.last() {
                    self.nesting.pop();
                } else {
                    self.report_error("Unmatched ']'");
                }
                self.add_token(TokenType::RightBracket)
            }
            ',' => self.add_token(TokenType::Comma),
            '-' => {
                let t = if self.match_char('=') {
                    TokenType::MinusEqual
                } else {
                    TokenType::Minus
                };
                self.add_token(t);
            }
            '+' => {
                let t = if self.match_char('=') {
                    TokenType::PlusEqual
                } else {
                    TokenType::Plus
                };
                self.add_token(t);
            }
            ':' => self.add_token(TokenType::Colon),
            '*' => {
                let t = if self.match_char('=') {
                    TokenType::StarEqual
                } else {
                    TokenType::Star
                };
                self.add_token(t);
            }
            '%' => self.add_token(TokenType::Percent),

            // Dot or Range
            '.' => {
                if self.match_char('.') {
                    self.add_token(TokenType::Range)
                } else {
                    self.add_token(TokenType::Dot)
                }
            }

            // Bitwise and logical operators
            '&' => {
                let token_type = if self.match_char('&') {
                    TokenType::And
                } else {
                    TokenType::Ampersand
                };
                self.add_token(token_type);
            }
            '|' => {
                let token_type = if self.match_char('|') {
                    TokenType::Or
                } else {
                    TokenType::Pipe
                };
                self.add_token(token_type);
            }
            '^' => self.add_token(TokenType::Caret),

            // Tilde: ~= is approximate equality, bare ~ is bitwise NOT
            '~' => {
                if self.match_char('=') {
                    self.add_token(TokenType::ApproxEqual)
                } else {
                    self.add_token(TokenType::Tilde)
                }
            }

            // One or two character tokens
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::NotEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type);
            }

            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::Equal
                } else {
                    TokenType::Assign
                };
                self.add_token(token_type);
            }

            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else if self.match_char('>') {
                    TokenType::RightShift
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type);
            }

            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else if self.match_char('<') {
                    TokenType::LeftShift
                } else {
                    TokenType::Less
                };
                self.add_token(token_type);
            }

            '/' => {
                // Handle comments or division
                if self.match_char('/') {
                    // Comment goes until end of line
                    while self.peek() != Some('\n') && !self.is_at_end() {
                        self.advance();
                    }
                    self.add_token(TokenType::Comment);
                } else if self.match_char('=') {
                    self.add_token(TokenType::SlashEqual);
                } else if self.match_char('*') {
                    // Multi-line comment
                    loop {
                        if self.is_at_end() {
                            self.report_error("Unterminated multi-line comment");
                            break;
                        }

                        if self.peek() == Some('\n') {
                            self.line += 1;
                            self.advance();
                            self.line_start = self.current;
                            continue;
                        }

                        if self.peek() == Some('*') && self.peek_next() == Some('/') {
                            self.advance(); // consume '*'
                            self.advance(); // consume '/'
                            break;
                        }

                        self.advance();
                    }
                    self.add_token(TokenType::Comment);
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            // Whitespace (not newlines)
            ' ' | '\r' | '\t' => {}

            // Newlines - significant in Gaul Lang!
            '\n' => {
                // Capture span before updating line tracking (the \n belongs to the old line)
                let newline_span = Span {
                    line: self.line,
                    col: self.start - self.line_start + 1,
                    length: 1,
                };

                self.line += 1;
                self.line_start = self.current;

                // Only suppress if the TOP of the stack is Paren or Bracket.
                // If top is Brace (or stack empty), we emit newline.
                let should_suppress = matches!(
                    self.nesting.last(),
                    Some(Nesting::Paren) | Some(Nesting::Bracket)
                );

                if should_suppress {
                    return;
                }

                let should_emit = self
                    .tokens
                    .last()
                    .map(|t| t.token_type != TokenType::Newline)
                    .unwrap_or(true);
                if should_emit {
                    let text = self.source[self.start..self.current]
                        .iter()
                        .collect::<String>();
                    self.tokens
                        .push(Token::new(TokenType::Newline, text, newline_span));
                }
            }

            // strings
            '"' => self.handle_string(),

            // numbers
            c if c.is_ascii_digit() => match c {
                '0' => match self.peek() {
                    Some('x') | Some('X') => self.handle_non_base10(16),
                    Some('b') | Some('B') => self.handle_non_base10(2),
                    Some('o') | Some('O') => self.handle_non_base10(8),
                    _ => self.handle_number(),
                },
                _ => self.handle_number(),
            },

            // identifiers
            c if c.is_alphabetic() || c == '_' => self.handle_identifier(),

            _ => self.report_error(format!("Unexpected character: '{}'", c)),
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.current_char().expect("Unexpected EOF");
        self.current += 1;
        ch
    }

    fn current_char(&self) -> Option<char> {
        self.source.get(self.current).copied()
    }

    fn peek(&self) -> Option<char> {
        match self.is_at_end() {
            true => Some('\0'),
            false => self.current_char(),
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            Some('\0')
        } else {
            self.source.get(self.current + 1).copied()
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        match self.current_char() {
            Some(ch) if ch == expected => {
                self.current += 1;
                true
            }
            _ => false,
        }
    }

    fn handle_string(&mut self) {
        let mut value = String::new();

        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
                self.advance();
                value.push('\n');
                self.line_start = self.current;
                continue;
            }
            if self.peek() == Some('\\') {
                self.advance(); // consume the backslash
                if self.is_at_end() {
                    self.report_error(String::from("Unterminated string"));
                    return;
                }
                let escaped = self.advance();
                match escaped {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    _ => {
                        self.report_error(format!("Invalid escape sequence: \\{}", escaped));
                        return;
                    }
                }
                continue;
            }
            value.push(self.advance());
        }

        if self.is_at_end() {
            self.report_error(String::from("Unterminated string"));
            return;
        }

        self.advance(); // closing quote
        self.add_token(TokenType::String(value));
    }

    fn handle_non_base10(&mut self, radix: u32) {
        // consume the x/b/o
        self.advance();

        // Check for leading underscore
        if self.peek() == Some('_') {
            let prefix: String = self.source[self.start..self.current].iter().collect();
            self.report_error(format!(
                "Invalid number - leading underscore after '{}'",
                prefix
            ));
            while self.peek().is_some_and(|c| c.is_alphanumeric() || c == '_') {
                self.advance();
            }
            return;
        }

        let valid: fn(char) -> bool = match radix {
            16 => |c| c.is_ascii_hexdigit() || c == '_',
            2 => |c| c == '0' || c == '1' || c == '_',
            8 => |c| ('0'..='7').contains(&c) || c == '_',
            _ => unreachable!(),
        };

        while self.peek().is_some_and(valid) {
            self.advance();
        }

        // Check for invalid suffix (letter immediately after number)
        if self.peek().is_some_and(|c| c.is_alphabetic()) {
            let text: String = self.source[self.start..self.current].iter().collect();
            self.report_error(format!(
                "Invalid number '{}' - unexpected suffix '{}'",
                text,
                self.peek().unwrap()
            ));
            while self.peek().is_some_and(|c| c.is_alphanumeric() || c == '_') {
                self.advance();
            }
            return;
        }

        let raw: String = self.source[self.start..self.current].iter().collect();

        // Check for trailing underscore
        if raw.ends_with('_') {
            self.report_error(format!(
                "Invalid number '{}' - trailing underscore not allowed",
                raw
            ));
            return;
        }

        // skip the 0x/0b/0o prefix, filter out underscores
        let text: String = self.source[self.start + 2..self.current]
            .iter()
            .filter(|&&c| c != '_')
            .collect();

        if text.is_empty() {
            self.report_error(format!(
                "Expected digits after '{}'",
                self.source[self.start..self.start + 2]
                    .iter()
                    .collect::<String>()
            ));
            return;
        }

        match i64::from_str_radix(&text, radix) {
            Ok(value) => self.add_token(TokenType::Number(value as f64)),
            Err(_) => self.report_error(format!("Invalid literal: '{}'", raw)),
        }
    }

    fn handle_number(&mut self) {
        // First character is already consumed and is a digit

        while self.peek().is_some_and(|c| c.is_ascii_digit() || c == '_') {
            self.advance();
        }

        if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            self.advance(); // consume '.'

            while self.peek().is_some_and(|c| c.is_ascii_digit() || c == '_') {
                self.advance();
            }
        }

        // Check for invalid suffix (letter immediately after number)
        if self.peek().is_some_and(|c| c.is_alphabetic()) {
            let text: String = self.source[self.start..self.current].iter().collect();
            self.report_error(format!(
                "Invalid number '{}' - unexpected suffix '{}'",
                text,
                self.peek().unwrap()
            ));
            while self.peek().is_some_and(|c| c.is_alphanumeric() || c == '_') {
                self.advance();
            }
            return;
        }

        let raw: String = self.source[self.start..self.current].iter().collect();

        // Check for trailing underscore
        if raw.ends_with('_') {
            self.report_error(format!(
                "Invalid number '{}' - trailing underscore not allowed",
                raw
            ));
            return;
        }

        // Check for underscore next to decimal point
        if raw.contains("_.") || raw.contains("._") {
            self.report_error(format!(
                "Invalid number '{}' - underscore cannot be adjacent to decimal point",
                raw
            ));
            return;
        }

        // Filter out underscores before parsing
        let text: String = raw.chars().filter(|&c| c != '_').collect();

        match text.parse::<f64>() {
            Ok(num) => self.add_token(TokenType::Number(num)),
            Err(_) => self.report_error(format!("Invalid number: '{}'", raw)),
        }
    }

    fn handle_identifier(&mut self) {
        loop {
            // Consume word characters (alphanumeric + underscore)
            while self.peek().is_some_and(|c| c.is_alphanumeric() || c == '_') {
                self.advance();
            }

            let text: String = self.source[self.start..self.current].iter().collect();

            // If current word is a keyword, stop here — don't absorb more words
            if self.keywords.contains_key(&text) {
                let token_type = self.keywords.get(&text).cloned().unwrap();
                self.add_token(token_type);
                return;
            }

            // Space followed by alphabetic char? Continue the identifier!
            if self.peek() == Some(' ')
                && self
                    .peek_next()
                    .is_some_and(|c| c.is_alphanumeric() || c == '_')
            {
                self.advance(); // consume the space
                continue;
            }

            // Anything else ends the identifier
            break;
        }

        let text: String = self.source[self.start..self.current].iter().collect();

        // Reserved words only match if single word (no spaces)
        let token_type = if !text.contains(' ') {
            self.keywords
                .get(&text)
                .cloned()
                .unwrap_or(TokenType::Identifier)
        } else {
            TokenType::Identifier
        };

        self.add_token(token_type);
    }

    fn add_token(&mut self, t: TokenType) {
        let text = self.source[self.start..self.current]
            .iter()
            .collect::<String>();
        // For multi-line tokens (strings, comments), start may be on a previous line.
        // In that case, col relative to line_start doesn't make sense, so use 1.
        let col = if self.start >= self.line_start {
            self.start - self.line_start + 1
        } else {
            1
        };
        let span = Span {
            line: self.line,
            col,
            length: self.current - self.start,
        };
        self.tokens.push(Token::new(t, text, span));
    }

    fn report_error(&mut self, message: impl Into<String>) {
        let col = if self.start >= self.line_start {
            self.start - self.line_start + 1
        } else {
            1
        };
        let span = Span {
            line: self.line,
            col,
            length: if self.current > self.start {
                self.current - self.start
            } else {
                1
            },
        };
        self.errors.push(ScanError {
            span,
            message: message.into(),
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::keywords::load_keywords;

    fn scan(source: &str) -> ScanResult {
        let keywords = load_keywords(None).unwrap();
        Scanner::new(source, &keywords).scan_tokens()
    }

    fn token_types(result: &ScanResult) -> Vec<&TokenType> {
        result.tokens.iter().map(|t| &t.token_type).collect()
    }

    // --- Baseline tests (current behavior) ---

    #[test]
    fn scan_basic_tokens() {
        let result = scan("let X = 5");
        assert!(result.errors.is_empty());
        assert_eq!(
            token_types(&result),
            vec![
                &TokenType::Let,
                &TokenType::Identifier,
                &TokenType::Assign,
                &TokenType::Number(5.0),
                &TokenType::Eof,
            ]
        );
    }

    #[test]
    fn scan_single_line_comment_ignored() {
        let result = scan("// hello\n5");
        assert!(result.errors.is_empty());
        let filtered = result.tokens_without_comments();
        let types: Vec<_> = filtered.iter().map(|t| &t.token_type).collect();
        assert_eq!(
            types,
            vec![
                &TokenType::Newline,
                &TokenType::Number(5.0),
                &TokenType::Eof
            ]
        );
    }

    #[test]
    fn scan_multi_line_comment_ignored() {
        let result = scan("/* hi */ 5");
        assert!(result.errors.is_empty());
        let filtered = result.tokens_without_comments();
        let types: Vec<_> = filtered.iter().map(|t| &t.token_type).collect();
        assert_eq!(types, vec![&TokenType::Number(5.0), &TokenType::Eof]);
    }

    #[test]
    fn scan_error_on_unterminated_string() {
        let result = scan("\"unterminated");
        assert!(!result.errors.is_empty());
        assert!(result.errors[0].message.contains("Unterminated string"));
    }

    // --- New behavior tests ---

    #[test]
    fn scan_always_returns_tokens_and_errors() {
        let result = scan("let X = 5\n\"unterminated");
        // Has tokens from the valid part
        let types: Vec<_> = result.tokens.iter().map(|t| &t.token_type).collect();
        assert!(types.contains(&&TokenType::Let));
        assert!(types.contains(&&TokenType::Identifier));
        assert!(types.contains(&&TokenType::Assign));
        assert!(types.contains(&&TokenType::Number(5.0)));
        assert!(types.contains(&&TokenType::Newline));
        assert!(types.contains(&&TokenType::Eof));
        // AND has errors
        assert!(!result.errors.is_empty());
        assert!(result.errors[0].message.contains("Unterminated string"));
    }

    #[test]
    fn scan_emits_single_line_comment() {
        let result = scan("// hello\n5");
        assert!(result.errors.is_empty());
        let types = token_types(&result);
        assert!(types.contains(&&TokenType::Comment));
    }

    #[test]
    fn scan_emits_multi_line_comment() {
        let result = scan("/* hi */ 5");
        assert!(result.errors.is_empty());
        let types = token_types(&result);
        assert!(types.contains(&&TokenType::Comment));
    }

    #[test]
    fn scan_comment_span_covers_full_text() {
        let result = scan("// hello world");
        let comment = result
            .tokens
            .iter()
            .find(|t| t.token_type == TokenType::Comment)
            .expect("should have a comment token");
        assert_eq!(comment.lexeme, "// hello world");
        assert_eq!(comment.span.col, 1);
        assert_eq!(comment.span.length, 14);
    }

    #[test]
    fn tokens_without_comments_filters() {
        let result = scan("// hello\n5 /* inline */ + 3");
        let filtered = result.tokens_without_comments();
        let types: Vec<_> = filtered.iter().map(|t| &t.token_type).collect();
        // No Comment tokens in filtered output
        assert!(!types.contains(&&TokenType::Comment));
        // But the actual tokens should be there
        assert!(types.contains(&&TokenType::Number(5.0)));
        assert!(types.contains(&&TokenType::Plus));
        assert!(types.contains(&&TokenType::Number(3.0)));
    }
}
