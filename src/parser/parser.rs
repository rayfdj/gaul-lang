use crate::parser::ast::Expr;
use crate::scanner::token::Token;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    pub fn parse(mut self) -> Result<Expr, Vec<ParseError>> {
        match self.expression() {
            Ok(expr) if self.errors.is_empty() => Ok(expr),
            _ => Err(self.errors),
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        todo!("implement expression parsing")
    }

}