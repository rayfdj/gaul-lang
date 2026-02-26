use crate::span::Span;

#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub span: Span,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: impl Into<String>, span: Span) -> Self {
        Token {
            token_type,
            lexeme: lexeme.into(),
            span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,    // (
    RightParen,   // )
    LeftBrace,    // {
    RightBrace,   // }
    LeftBracket,  // [
    RightBracket, // ]
    Comma,        // ,
    Dot,          // .
    Minus,        // -
    Plus,         // +
    Colon,        // :
    Slash,        // /
    Star,         // *

    // One or two character tokens
    Bang,         // !
    NotEqual,     // !=
    Assign,       // =
    Equal,        // ==
    Greater,      // >
    GreaterEqual, // >=
    Less,         // <
    LessEqual,    // <=
    PlusEqual,    // +=
    MinusEqual,   // -=
    StarEqual,    // *=
    SlashEqual,   // /=
    Percent,      // %

    // Bitwise operators
    Ampersand,  // &
    Pipe,       // |
    Caret,      // ^
    Tilde,      // ~
    LeftShift,  // <<
    RightShift, // >>

    // Logical operators
    And, // &&
    Or,  // ||

    // Funky operators
    ApproxEqual, // ~= (jam karet!)
    Range,       // ..

    // Literals
    Identifier,     // variable names, function names
    String(String), // "hello world"
    Number(f64),    // 123, 45.67

    // Keywords
    Else,     // else
    False,    // false
    Function, // fn
    For,      // for
    If,       // if
    Null,     // null
    Return,   // return
    True,     // true
    Let,      // let (immutable binding)
    Var,      // var (mutable binding)
    While,    // while
    Break,    // break
    Continue, // continue
    Import,   // import
    Export,   // export
    From,     // from

    // Control
    Newline, // significant for ending identifier
    Eof,     // end of file

    // LSP support
    Comment, // single-line (//) and multi-line (/* */) comments
}
