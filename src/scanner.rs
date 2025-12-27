pub mod token;

use crate::scanner::token::{Token, TokenType};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum Nesting {
    Paren,   // (...) - Suppresses newlines
    Brace,   // {...} - Allows newlines (statements)
    Bracket, // [...] - Suppresses newlines
}

pub struct Scanner {
    source: Vec<char>,
    tokens: Vec<Token>,
    errors: Vec<String>,
    start: usize,
    current: usize,
    line: usize,
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
            keywords: keywords.clone(),
            nesting: Vec::new(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, Vec<String>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new(TokenType::Eof, "", self.line));
        if self.errors.is_empty() {
            Ok(self.tokens)
        } else {
            Err(self.errors)
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
                    self.errors
                        .push(format!("Unmatched ')' at line {}", self.line));
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
                    self.errors
                        .push(format!("Unmatched ']' at line {}", self.line));
                }
                self.add_token(TokenType::RightBracket)
            }
            ',' => self.add_token(TokenType::Comma),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ':' => self.add_token(TokenType::Colon),
            '*' => self.add_token(TokenType::Star),

            // Dot or Range
            '.' => {
                if self.match_char('.') {
                    self.add_token(TokenType::Range)
                } else {
                    self.add_token(TokenType::Dot)
                }
            }

            // Approximate equality (jam karet!)
            '~' => {
                if self.match_char('=') {
                    self.add_token(TokenType::ApproxEqual)
                } else {
                    self.report_error("Expected '=' after '~' for approximate equality")
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
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type);
            }

            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
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
                } else if self.match_char('*') {
                    // Multi-line comment
                    loop {
                        if self.is_at_end() {
                            self.report_error("Unterminated multi-line comment");
                            break;
                        }

                        if self.peek() == Some('\n') {
                            self.line += 1;
                        }

                        if self.peek() == Some('*') && self.peek_next() == Some('/') {
                            self.advance(); // consume '*'
                            self.advance(); // consume '/'
                            break;
                        }

                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }

            // Whitespace (not newlines)
            ' ' | '\r' | '\t' => {}

            // Newlines - significant in Gaul Lang!
            '\n' => {
                self.line += 1;

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
                    self.add_token(TokenType::Newline);
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
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.report_error(String::from("Unterminated string"));
            return;
        }

        self.advance();

        // remember this is the string value, not the lexeme, so we're excluding the double quotation marks
        let value = self.source[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>();
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
        self.tokens.push(Token::new(t, text, self.line));
    }

    fn report_error(&mut self, message: impl Into<String>) {
        let error = format!("Line {}: {}", self.line, message.into());
        self.errors.push(error);
    }
}
