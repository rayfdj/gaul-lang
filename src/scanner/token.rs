#[derive(Clone, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: impl Into<String>, line: usize) -> Self {
        Token {
            token_type,
            lexeme: lexeme.into(),
            line,
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

    // Funky operators
    ApproxEqual, // ~= (jam karet!)
    Range,       // ..

    // Literals
    Identifier,     // variable names, function names
    String(String), // "hello world"
    Number(f64),    // 123, 45.67

    // Keywords
    And,      // and
    Class,    // class
    Else,     // else
    False,    // false
    Function, // fn
    For,      // for
    If,       // if
    Null,     // null
    Or,       // or
    Return,   // return
    Super,    // super
    This,     // this
    True,     // true
    Let,      // let (immutable binding)
    Var,      // var (mutable binding)
    While,    // while
    Break,    // break
    Continue, // continue

    // Control
    Newline, // significant for ending identifier
    Eof,     // end of file
}
