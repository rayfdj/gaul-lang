use crate::parser::ast::DeclarationKind::{ExprStmt, Fn, Let, Var};
use crate::parser::ast::ExprKind::{
    Assign, Binary, Block, Bool, Call, For, Get, Identifier, If, Null, Num, Pipe, Range, Return,
    Str, Unary, While,
};
use crate::parser::ast::{ExprKind, Program};
use crate::parser::ast::{Declaration, Expr};
use crate::scanner::token::{Token, TokenType};

#[derive(Debug, Clone)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    // utility methods
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn match_any(&mut self, types: &[TokenType]) -> bool {
        for t in types {
            if self.check(t.clone()) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn error_expected(&self, expected: &str) -> ParseError {
        let current = self.peek();
        let context = if self.current > 0 {
            format!(" after '{}'", self.previous().lexeme)
        } else {
            String::new()
        };
        ParseError {
            line: current.line,
            message: format!(
                "Expected {}{}, got {:?}",
                expected, context, current.token_type
            ),
        }
    }

    fn consume(&mut self, token_type: TokenType, expected: &str) -> Result<&Token, ParseError> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(self.error_expected(expected))
        }
    }

    pub fn parse(mut self) -> Result<Program, Vec<ParseError>> {
        self.program()
    }

    fn program(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut declarations = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            // Skip newlines between declarations
            while self.check(TokenType::Newline) {
                self.advance();
            }

            if self.is_at_end() {
                break;
            }

            match self.declaration() {
                Ok(decl) => declarations.push(decl),
                Err(e) => {
                    errors.push(e);
                    self.synchronize(); // skip to next statement
                }
            }
        }

        if errors.is_empty() {
            Ok(Program { declarations })
        } else {
            Err(errors)
        }
    }

    fn synchronize(&mut self) {
        self.advance(); // Skip the token that caused the error

        while !self.is_at_end() {
            // Check 1: Did we just pass a newline?
            // In Gaul, newline ends a statement, so we're at a fresh start
            if self.previous().token_type == TokenType::Newline {
                return;
            }

            // Check 2: Are we looking at a keyword that starts a declaration?
            // If so, we're at a safe place to resume
            match self.peek().token_type {
                TokenType::Let
                | TokenType::Var
                | TokenType::Function
                | TokenType::If
                | TokenType::While
                | TokenType::For
                | TokenType::Return => return,
                _ => {}
            }

            self.advance(); // Keep skipping
        }
    }

    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        if self.check(TokenType::Let) {
            self.let_decl()
        } else if self.check(TokenType::Var) {
            self.var_decl()
        } else if self.check(TokenType::Function) {
            self.fn_decl()
        } else {
            self.expr_stmt()
        }
    }

    fn let_decl(&mut self) -> Result<Declaration, ParseError> {
        self.binding_decl(false)
    }

    fn var_decl(&mut self) -> Result<Declaration, ParseError> {
        self.binding_decl(true)
    }

    fn binding_decl(&mut self, is_mutable: bool) -> Result<Declaration, ParseError> {
        let line = self.peek().line; // capture before consuming
        self.advance(); // consume let/var
        let name = self
            .consume(TokenType::Identifier, "identifier")?
            .lexeme
            .clone();
        self.consume(TokenType::Assign, "=")?;
        let initializer = self.expression()?;
        self.consume(TokenType::Newline, "newline")?;

        let kind = if is_mutable {
            Var { name, initializer }
        } else {
            Let { name, initializer }
        };

        Ok(Declaration { kind, line })
    }

    fn fn_decl(&mut self) -> Result<Declaration, ParseError> {
        let line = self.peek().line;
        self.advance(); // consume fn

        let name = self
            .consume(TokenType::Identifier, "identifier")?
            .lexeme
            .clone();

        self.consume(TokenType::LeftParen, "(")?;
        let mut params = Vec::new();
        if !self.check(TokenType::RightParen) {
            params.push(
                self.consume(TokenType::Identifier, "identifier")?
                    .lexeme
                    .clone(),
            );

            while self.check(TokenType::Comma) {
                self.advance();
                params.push(
                    self.consume(TokenType::Identifier, "identifier")?
                        .lexeme
                        .clone(),
                );
            }
        }
        self.consume(TokenType::RightParen, ")")?;

        let body = self.block()?;

        Ok(Declaration {
            kind: Fn { name, params, body },
            line,
        })
    }

    fn expr_stmt(&mut self) -> Result<Declaration, ParseError> {
        let line = self.peek().line;
        let expr = self.expression()?;
        self.consume(TokenType::Newline, "newline")?;

        Ok(Declaration {
            kind: ExprStmt(expr),
            line,
        })
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        if self.check(TokenType::If) {
            self.if_expr()
        } else if self.check(TokenType::While) {
            self.while_expr()
        } else if self.check(TokenType::For) {
            self.for_expr()
        } else if self.check(TokenType::Return) {
            self.return_expr()
        } else {
            self.assignment()
        }
    }

    fn if_expr(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek().line;
        self.advance(); // consume if
        self.consume(TokenType::LeftParen, "(")?;
        let condition = Box::new(self.expression()?);
        self.consume(TokenType::RightParen, ")")?;
        let then_branch = Box::new(self.block()?);
        let else_branch = if self.check(TokenType::Else) {
            self.advance(); // consume else
            if self.check(TokenType::If) {
                Some(Box::new(self.if_expr()?))
            } else {
                Some(Box::new(self.block()?))
            }
        } else {
            None
        };

        Ok(Expr {
            kind: If {
                condition,
                then_branch,
                else_branch,
            },
            line,
        })
    }

    fn while_expr(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek().line;
        self.advance(); // consume while
        self.consume(TokenType::LeftParen, "(")?;
        let condition = Box::new(self.expression()?);
        self.consume(TokenType::RightParen, ")")?;
        let body = Box::new(self.block()?);

        Ok(Expr {
            kind: While { condition, body },
            line,
        })
    }

    fn for_expr(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek().line;
        self.advance(); // consume for
        self.consume(TokenType::LeftParen, "(")?;
        let variable = self
            .consume(TokenType::Identifier, "identifier")?
            .lexeme
            .clone();
        self.consume(TokenType::Colon, ":")?;
        let iterable = Box::new(self.expression()?);
        self.consume(TokenType::RightParen, ")")?;
        let body = Box::new(self.block()?);

        Ok(Expr {
            kind: For {
                variable,
                iterable,
                body,
            },
            line,
        })
    }

    fn return_expr(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek().line;
        self.advance(); // consume return
        let returnee = if self.check(TokenType::Newline) {
            None
        } else {
            Some(Box::new(self.expression()?))
        };

        Ok(Expr {
            kind: Return(returnee),
            line,
        })
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.pipe()?;

        if self.check(TokenType::Assign) {
            let line = self.peek().line;
            self.advance();
            let value = Box::new(self.assignment()?); // right-associative

            match &expr.kind {
                Identifier { .. } | Get { .. } => Ok(Expr {
                    kind: Assign {
                        target: Box::new(expr),
                        value,
                    },
                    line,
                }),
                _ => Err(ParseError {
                    line,
                    message: "Invalid assignment target".to_string(),
                }),
            }
        } else {
            Ok(expr)
        }
    }

    fn pipe(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.logic_or()?;

        while self.check(TokenType::Pipe) {
            let line = self.peek().line;
            self.advance();
            let right = self.logic_or()?;
            left = Expr {
                kind: Pipe {
                    left: Box::new(left),
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.logic_and()?;

        while self.check(TokenType::Or) {
            let operator = self.advance().clone();
            let line = operator.line;
            let right = self.logic_and()?;
            left = Expr {
                kind: Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.equality()?;

        while self.check(TokenType::And) {
            let operator = self.advance().clone();
            let line = operator.line;
            let right = self.equality()?;
            left = Expr {
                kind: Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.comparison()?;

        while self.match_any(&[
            TokenType::Equal,
            TokenType::NotEqual,
            TokenType::ApproxEqual,
        ]) {
            let operator = self.previous().clone();
            let line = operator.line;
            let right = self.comparison()?;
            left = Expr {
                kind: Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.range()?;

        while self.match_any(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
            let operator = self.previous().clone();
            let line = operator.line;
            let right = self.range()?;
            left = Expr {
                kind: Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn range(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.term()?;

        if self.check(TokenType::Range) {
            let line = self.peek().line;
            self.advance();
            let right = self.term()?;
            left = Expr {
                kind: Range {
                    start: Box::new(left),
                    end: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.factor()?;

        while self.match_any(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let line = operator.line;
            let right = self.factor()?;
            left = Expr {
                kind: Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.unary()?;

        while self.match_any(&[TokenType::Star, TokenType::Slash]) {
            let operator = self.previous().clone();
            let line = operator.line;
            let right = self.unary()?;
            left = Expr {
                kind: Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                line,
            };
        }

        Ok(left)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_any(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let line = operator.line;
            let operand = self.unary()?; // recursive for chained unary: --x
            Ok(Expr {
                kind: Unary {
                    operator,
                    operand: Box::new(operand),
                },
                line,
            })
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.check(TokenType::LeftParen) {
                self.advance();
                let mut arguments = Vec::new();

                if !self.check(TokenType::RightParen) {
                    arguments.push(self.expression()?);
                    while self.check(TokenType::Comma) {
                        self.advance();
                        arguments.push(self.expression()?);
                    }
                }

                let line = self.peek().line;
                self.consume(TokenType::RightParen, ")")?;

                expr = Expr {
                    kind: Call {
                        callee: Box::new(expr),
                        arguments,
                    },
                    line,
                };
            } else if self.check(TokenType::Dot) {
                self.advance();
                let name = self
                    .consume(TokenType::Identifier, "property name")?
                    .lexeme
                    .clone();
                let line = self.previous().line;

                expr = Expr {
                    kind: Get {
                        object: Box::new(expr),
                        name,
                    },
                    line,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();
        let line = token.line;

        match &token.token_type {
            TokenType::Number(n) => {
                let value = *n;
                self.advance();
                Ok(Expr {
                    kind: Num(value),
                    line,
                })
            }
            TokenType::String(s) => {
                let value = s.clone();
                self.advance();
                Ok(Expr {
                    kind: Str(value),
                    line,
                })
            }
            TokenType::True => {
                self.advance();
                Ok(Expr {
                    kind: Bool(true),
                    line,
                })
            }
            TokenType::False => {
                self.advance();
                Ok(Expr {
                    kind: Bool(false),
                    line,
                })
            }
            TokenType::Null => {
                self.advance();
                Ok(Expr { kind: Null, line })
            }
            TokenType::Identifier => {
                let name = token.lexeme.clone();
                self.advance();
                Ok(Expr {
                    kind: Identifier {
                        name,
                        resolved: None,
                    },
                    line,
                })
            }
            TokenType::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume(TokenType::RightParen, ")")?;
                Ok(expr)
            }
            TokenType::LeftBrace => self.block(),
            TokenType::LeftBracket => self.array(),
            _ => Err(ParseError {
                line,
                message: format!("Unexpected token: {:?}", token.token_type),
            }),
        }
    }

    fn array(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek().line;
        self.consume(TokenType::LeftBracket, "[")?;

        let mut elements = Vec::new();

        while !self.check(TokenType::RightBracket) && !self.is_at_end() {
            while self.check(TokenType::Newline) {
                self.advance();
            }

            if self.check(TokenType::RightBracket) {
                break;
            }

            elements.push(self.expression()?);

            while self.check(TokenType::Newline) {
                self.advance();
            }

            if !self.check(TokenType::RightBracket) {
                self.consume(TokenType::Comma, ",")?;
            }
        }

        self.consume(TokenType::RightBracket, "]")?;

        Ok(Expr {
            kind: ExprKind::Array {
                elements,
            },
            line,
        })
    }

    fn block(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek().line;
        self.consume(TokenType::LeftBrace, "{")?;

        let mut declarations = Vec::new();
        let mut final_expr = None;

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            // Skip any leading/extra newlines
            while self.check(TokenType::Newline) {
                self.advance();
            }

            // After skipping newlines, we might be at }
            if self.check(TokenType::RightBrace) {
                break;
            }

            if self.check(TokenType::Let)
                || self.check(TokenType::Var)
                || self.check(TokenType::Function)
            {
                declarations.push(self.declaration()?);
            } else {
                let expr = self.expression()?;

                // Skip newlines, then check if we're at }
                if self.check(TokenType::Newline) {
                    self.advance();
                    // skip any extra newlines
                    while self.check(TokenType::Newline) {
                        self.advance();
                    }
                    let expr_line = expr.line;
                    if self.check(TokenType::RightBrace) {
                        final_expr = Some(Box::new(expr));
                    } else {
                        // there's more stuff, so this was a statement
                        declarations.push(Declaration { kind: ExprStmt(expr), line: expr_line });
                    }
                } else if self.check(TokenType::RightBrace) {
                    final_expr = Some(Box::new(expr));
                } else {
                    return Err(self.error_expected("newline or '}' after expression"));
                }
            }
        }

        self.consume(TokenType::RightBrace, "}")?;

        Ok(Expr {
            kind: Block {
                declarations,
                expr: final_expr,
            },
            line,
        })
    }
}
