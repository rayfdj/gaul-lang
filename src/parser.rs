pub mod ast;

use crate::parser::ast::DeclarationKind::{ExprStmt, Fn, Let, Var};
use crate::parser::ast::ExprKind::{
    Assign, Block, Bool, Call, For, Get, Identifier, If, Index, Null, Num, Range, Return, Str,
    Unary, While,
};
use crate::parser::ast::{Declaration, Expr};
use crate::parser::ast::{ExprKind, Program};
use crate::scanner::token::{Token, TokenType};
use crate::span::Span;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
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
            return token_type == TokenType::Eof;
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
            span: current.span,
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
            self.skip_newlines();

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
        let span = self.peek().span; // capture before consuming
        self.advance(); // consume let/var
        let name = self
            .consume(TokenType::Identifier, "identifier")?
            .lexeme
            .clone();
        self.consume(TokenType::Assign, "=")?;
        let initializer = self.expression()?;
        if !self.match_any(&[TokenType::Newline, TokenType::Eof]) {
            return Err(self.error_expected("newline or eof"));
        }

        let kind = if is_mutable {
            Var { name, initializer }
        } else {
            Let { name, initializer }
        };

        Ok(Declaration { kind, span })
    }

    fn fn_decl(&mut self) -> Result<Declaration, ParseError> {
        let span = self.peek().span;
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

        let body = Rc::new(self.block()?);

        Ok(Declaration {
            kind: Fn { name, params, body },
            span,
        })
    }

    fn expr_stmt(&mut self) -> Result<Declaration, ParseError> {
        let span = self.peek().span;
        let expr = self.expression()?;
        if !self.match_any(&[TokenType::Newline, TokenType::Eof]) {
            return Err(self.error_expected("newline or eof"));
        }

        Ok(Declaration {
            kind: ExprStmt(expr),
            span,
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
        let span = self.peek().span;
        self.advance(); // consume if

        self.consume(TokenType::LeftParen, "(")?;
        let condition = Box::new(self.expression()?);
        self.consume(TokenType::RightParen, ")")?;

        let then_branch = Box::new(self.block()?);

        let mut lookahead = self.current;
        while lookahead < self.tokens.len()
            && self.tokens[lookahead].token_type == TokenType::Newline
        {
            lookahead += 1;
        }

        let else_branch = if lookahead < self.tokens.len()
            && self.tokens[lookahead].token_type == TokenType::Else
        {
            self.skip_newlines(); // NOW it is safe to eat them
            self.advance(); // consume else
            self.skip_newlines(); // allow newlines after else

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
            span,
        })
    }

    fn while_expr(&mut self) -> Result<Expr, ParseError> {
        let span = self.peek().span;
        self.advance(); // consume while
        self.consume(TokenType::LeftParen, "(")?;
        let condition = Box::new(self.expression()?);
        self.consume(TokenType::RightParen, ")")?;
        let body = Box::new(self.block()?);

        Ok(Expr {
            kind: While { condition, body },
            span,
        })
    }

    fn for_expr(&mut self) -> Result<Expr, ParseError> {
        let span = self.peek().span;
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
            span,
        })
    }

    fn return_expr(&mut self) -> Result<Expr, ParseError> {
        let span = self.peek().span;
        self.advance(); // consume return
        let returnee = if self.check(TokenType::Newline)
            || self.check(TokenType::RightBrace)
            || self.check(TokenType::Eof)
        {
            None
        } else {
            Some(Box::new(self.expression()?))
        };

        Ok(Expr {
            kind: Return(returnee),
            span,
        })
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logic_or()?;

        if self.check(TokenType::Assign) {
            let span = self.peek().span;
            self.advance();
            let value = Box::new(self.expression()?); // right-associative

            match &expr.kind {
                Identifier { .. } | Get { .. } | Index { .. } => Ok(Expr {
                    kind: Assign {
                        target: Box::new(expr),
                        value,
                    },
                    span,
                }),
                _ => Err(ParseError {
                    span,
                    message: "Invalid assignment target".to_string(),
                }),
            }
        } else if self.match_any(&[
            TokenType::PlusEqual,
            TokenType::MinusEqual,
            TokenType::StarEqual,
            TokenType::SlashEqual,
        ]) {
            let op_token = self.previous().clone();
            let span = op_token.span;
            let rhs = self.expression()?;

            match &expr.kind {
                Identifier { .. } | Get { .. } | Index { .. } => {}
                _ => {
                    return Err(ParseError {
                        span,
                        message: "Invalid assignment target".to_string(),
                    })
                }
            }

            let bin_op_type = match op_token.token_type {
                TokenType::PlusEqual => TokenType::Plus,
                TokenType::MinusEqual => TokenType::Minus,
                TokenType::StarEqual => TokenType::Star,
                TokenType::SlashEqual => TokenType::Slash,
                _ => unreachable!(),
            };
            let bin_op_token = Token {
                token_type: bin_op_type,
                lexeme: op_token.lexeme.clone(),
                span,
            };

            let expanded_rhs = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr.clone()),
                    operator: bin_op_token,
                    right: Box::new(rhs),
                },
                span,
            };

            Ok(Expr {
                kind: Assign {
                    target: Box::new(expr),
                    value: Box::new(expanded_rhs),
                },
                span,
            })
        } else {
            Ok(expr)
        }
    }

    // This method is created because pretty much all binary
    fn binary_expression<F>(
        &mut self,
        tokens: &[TokenType],
        mut next_precedence: F, // We pass a closure here
    ) -> Result<Expr, ParseError>
    where
        F: FnMut(&mut Self) -> Result<Expr, ParseError>,
    {
        // Parse the left side (e.g., the first number) -- remember we are left associative
        let mut left = next_precedence(self)?;

        // Loop as long as we see one of our operators
        while self.match_any(tokens) {
            let operator = self.previous().clone();
            let span = operator.span;

            // Skip newlines after the operator
            self.skip_newlines();

            // Parse the right side using the SAME precedence level
            let right = next_precedence(self)?;

            // Build the new tree node and reassign 'left' (Accumulate)
            left = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn logic_or(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::Or], |p| p.logic_and())
    }

    fn logic_and(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::And], |p| p.equality())
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(
            &[
                TokenType::Equal,
                TokenType::NotEqual,
                TokenType::ApproxEqual,
            ],
            |p| p.comparison(),
        )
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(
            &[
                TokenType::Less,
                TokenType::LessEqual,
                TokenType::Greater,
                TokenType::GreaterEqual,
            ],
            |p| p.bitwise_or(),
        )
    }

    fn bitwise_or(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::Pipe], |p| p.bitwise_xor())
    }

    fn bitwise_xor(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::Caret], |p| p.bitwise_and())
    }

    fn bitwise_and(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::Ampersand], |p| p.shift())
    }

    fn shift(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::LeftShift, TokenType::RightShift], |p| {
            p.range()
        })
    }

    fn range(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.term()?;

        if self.check(TokenType::Range) {
            let span = self.peek().span;
            self.advance();
            let right = self.term()?;
            left = Expr {
                kind: Range {
                    start: Box::new(left),
                    end: Box::new(right),
                },
                span,
            };
        }

        Ok(left)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::Plus, TokenType::Minus], |p| p.factor())
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        self.binary_expression(&[TokenType::Star, TokenType::Slash, TokenType::Percent], |p| p.unary())
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_any(&[TokenType::Bang, TokenType::Minus, TokenType::Tilde]) {
            let operator = self.previous().clone();
            let span = operator.span;
            let operand = self.unary()?; // recursive for chained unary: --x
            Ok(Expr {
                kind: Unary {
                    operator,
                    operand: Box::new(operand),
                },
                span,
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

                let span = self.peek().span;
                self.consume(TokenType::RightParen, ")")?;

                expr = Expr {
                    kind: Call {
                        callee: Box::new(expr),
                        arguments,
                        is_tail_call: false,
                    },
                    span,
                };
            } else if self.check(TokenType::Dot) {
                self.advance();
                let name = self
                    .consume(TokenType::Identifier, "property name")?
                    .lexeme
                    .clone();
                let span = self.previous().span;

                expr = Expr {
                    kind: Get {
                        object: Box::new(expr),
                        name,
                    },
                    span,
                };
            } else if self.check(TokenType::LeftBracket) {
                self.advance();
                let index = self.expression()?;
                let span = self.peek().span;
                self.consume(TokenType::RightBracket, "]")?;

                expr = Expr {
                    kind: Index {
                        object: Box::new(expr),
                        index: Box::new(index),
                    },
                    span,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();
        let span = token.span;

        match &token.token_type {
            TokenType::Number(n) => {
                let value = *n;
                self.advance();
                Ok(Expr {
                    kind: Num(value),
                    span,
                })
            }
            TokenType::String(s) => {
                let value = s.clone();
                self.advance();
                Ok(Expr {
                    kind: Str(value),
                    span,
                })
            }
            TokenType::True => {
                self.advance();
                Ok(Expr {
                    kind: Bool(true),
                    span,
                })
            }
            TokenType::False => {
                self.advance();
                Ok(Expr {
                    kind: Bool(false),
                    span,
                })
            }
            TokenType::Null => {
                self.advance();
                Ok(Expr { kind: Null, span })
            }
            TokenType::Identifier => {
                let name = token.lexeme.clone();
                self.advance();
                Ok(Expr {
                    kind: Identifier {
                        name,
                        resolved: None,
                    },
                    span,
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
            TokenType::Break => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Break,
                    span,
                })
            }
            TokenType::Continue => {
                self.advance();
                Ok(Expr {
                    kind: ExprKind::Continue,
                    span,
                })
            }
            // Remember that lambda (or anonymous function) is NOT a declaration, and not a call
            // so that's why we're putting this in the primary expression handling.
            // In other words, fn(x) {} is an expression that evaluates to a Function!
            TokenType::Function => {
                self.advance(); // consume fn
                self.consume(TokenType::LeftParen, "(")?;
                let mut arguments = Vec::new();
                if !self.check(TokenType::RightParen) {
                    arguments.push(
                        self.consume(TokenType::Identifier, "identifier")?
                            .lexeme
                            .clone(),
                    );
                    while self.check(TokenType::Comma) {
                        self.advance();
                        arguments.push(
                            self.consume(TokenType::Identifier, "identifier")?
                                .lexeme
                                .clone(),
                        );
                    }
                }
                self.consume(TokenType::RightParen, ")")?;

                let body = Rc::new(self.block()?);

                Ok(Expr {
                    kind: ExprKind::Lambda {
                        params: arguments,
                        body,
                    },
                    span,
                })
            }
            _ => Err(ParseError {
                span,
                message: format!("Unexpected token: {:?}", token.token_type),
            }),
        }
    }

    fn array(&mut self) -> Result<Expr, ParseError> {
        let span = self.peek().span;
        self.consume(TokenType::LeftBracket, "[")?;

        // [] → empty array
        if self.check(TokenType::RightBracket) {
            self.advance();
            return Ok(Expr {
                kind: ExprKind::Array { elements: vec![] },
                span,
            });
        }

        // [:] → empty map
        if self.check(TokenType::Colon) {
            self.advance(); // consume ':'
            self.consume(TokenType::RightBracket, "]")?;
            return Ok(Expr {
                kind: ExprKind::Map { pairs: vec![] },
                span,
            });
        }

        // Parse first expression, then decide: map or array?
        let first = self.expression()?;

        if self.check(TokenType::Colon) {
            // It's a map: [key: value, ...]
            self.advance(); // consume ':'
            let value = self.expression()?;
            let mut pairs = vec![(first, value)];

            while self.check(TokenType::Comma) {
                self.advance(); // consume ','
                if self.check(TokenType::RightBracket) {
                    break; // trailing comma
                }
                let key = self.expression()?;
                self.consume(TokenType::Colon, ":")?;
                let val = self.expression()?;
                pairs.push((key, val));
            }

            self.consume(TokenType::RightBracket, "]")?;
            Ok(Expr {
                kind: ExprKind::Map { pairs },
                span,
            })
        } else {
            // It's an array: [expr, ...]
            let mut elements = vec![first];

            while self.check(TokenType::Comma) {
                self.advance(); // consume ','
                if self.check(TokenType::RightBracket) {
                    break; // trailing comma
                }
                elements.push(self.expression()?);
            }

            self.consume(TokenType::RightBracket, "]")?;
            Ok(Expr {
                kind: ExprKind::Array { elements },
                span,
            })
        }
    }

    fn block(&mut self) -> Result<Expr, ParseError> {
        // Skip newlines to support Allman style (braces on next line)
        self.skip_newlines();

        let span = self.peek().span;
        self.consume(TokenType::LeftBrace, "{")?;

        let mut declarations = Vec::new();
        let mut final_expr = None;

        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            // Skip any leading/extra newlines
            self.skip_newlines();

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
                    self.skip_newlines();
                    let expr_span = expr.span;
                    if self.check(TokenType::RightBrace) {
                        final_expr = Some(Box::new(expr));
                    } else {
                        // there's more stuff, so this was a statement
                        declarations.push(Declaration {
                            kind: ExprStmt(expr),
                            span: expr_span,
                        });
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
            span,
        })
    }

    fn skip_newlines(&mut self) {
        while self.check(TokenType::Newline) {
            self.advance();
        }
    }
}
