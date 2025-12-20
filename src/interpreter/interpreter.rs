use crate::interpreter::environment::Environment;
use crate::interpreter::value::Value;
use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use crate::scanner::token::TokenType;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub line: usize,
    pub message: String,
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new(env: Environment) -> Interpreter {
        Interpreter { env }
    }

    pub fn interpret(&mut self, program: Program) -> Result<Value, RuntimeError> {
        let mut last_value: Value = Value::Null;
        for declaration in program.declarations {
            last_value = self.execute_declaration(declaration)?;
        }
        Ok(last_value)
    }

    fn execute_declaration(&mut self, declaration: Declaration) -> Result<Value, RuntimeError> {
        match declaration.kind {
            DeclarationKind::Let { name, initializer } => {
                let value = self.evaluate_expression(initializer)?;
                self.env
                    .define(name, value, false)
                    .map(|()| Value::Null)
                    .map_err(|msg| RuntimeError {
                        line: declaration.line,
                        message: msg,
                    })
            }
            DeclarationKind::Var { name, initializer } => {
                let value = self.evaluate_expression(initializer)?;
                self.env
                    .define(name, value, true)
                    .map(|()| Value::Null)
                    .map_err(|msg| RuntimeError {
                        line: declaration.line,
                        message: msg,
                    })
            }
            DeclarationKind::Fn { name, params, body } => {
                let function = Value::Fn(name.clone(), params, body);
                self.env
                    .define(name, function, false)
                    .map(|()| Value::Null)
                    .map_err(|msg| RuntimeError {
                        line: declaration.line,
                        message: msg,
                    })
            }
            DeclarationKind::ExprStmt(expr) => self.evaluate_expression(expr),
        }
    }

    fn evaluate_expression(&mut self, expression: Expr) -> Result<Value, RuntimeError> {
        match expression.kind {
            // primary
            ExprKind::Number(n) => Ok(Value::Num(n)),
            ExprKind::Str(s) => Ok(Value::Str(s)),
            ExprKind::Bool(b) => Ok(Value::Bool(b)),
            ExprKind::Null => Ok(Value::Null),
            ExprKind::Identifier(id) => self.env.get(&id).map_err(|msg| RuntimeError {
                line: expression.line,
                message: msg,
            }),

            // unary
            ExprKind::Unary { operator, operand } => {
                let operand_value = self.evaluate_expression(*operand)?;
                match (operator.token_type, operand_value) {
                    (TokenType::Bang, Value::Bool(b)) => Ok(Value::Bool(!b)),
                    (TokenType::Minus, Value::Num(n)) => Ok(Value::Num(-n)),
                    (op_type, v) => Err(RuntimeError {
                        line: expression.line,
                        message: format!(
                            "cannot apply operator '{:?}' to operand '{:?}'",
                            op_type, v
                        ),
                    }),
                }
            }

            // binary
            ExprKind::Binary {
                operator,
                left,
                right,
            } => {
                let left_value = self.evaluate_expression(*left)?;

                match (&operator.token_type, &left_value) {
                    // handle the short-circuit
                    (TokenType::And, Value::Bool(false)) => Ok(left_value),
                    (TokenType::Or, Value::Bool(true)) => Ok(left_value),
                    (TokenType::And | TokenType::Or, Value::Bool(_)) => {
                        let right_value = self.evaluate_expression(*right)?;
                        match right_value {
                            Value::Bool(_) => Ok(right_value),
                            _ => Err(RuntimeError {
                                line: expression.line,
                                message: format!(
                                    "right value '{:?}' is not of boolean type",
                                    right_value
                                ),
                            }),
                        }
                    }
                    (TokenType::And | TokenType::Or, _) => Err(RuntimeError {
                        line: expression.line,
                        message: format!(
                            "logical operators require boolean operands, got {:?}",
                            left_value
                        ),
                    }),
                    _ => {
                        let right_value = self.evaluate_expression(*right)?;
                        match (operator.token_type, left_value, right_value) {
                            // arithmetic
                            (TokenType::Plus, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 + n2))
                            }
                            (TokenType::Minus, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 - n2))
                            }
                            (TokenType::Slash, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 / n2))
                            }
                            (TokenType::Star, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 * n2))
                            }

                            // weird String concat
                            (TokenType::Plus, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Str(s1 + &s2))
                            }

                            // comparison
                            (TokenType::Equal, val1, val2) => Ok(Value::Bool(val1 == val2)),
                            (TokenType::NotEqual, val1, val2) => Ok(Value::Bool(val1 != val2)),
                            (TokenType::Greater, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 > n2))
                            }
                            (TokenType::Greater, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 > s2))
                            }
                            (TokenType::GreaterEqual, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 >= n2))
                            }
                            (TokenType::GreaterEqual, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 >= s2))
                            }
                            (TokenType::Less, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 < n2))
                            }
                            (TokenType::Less, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 < s2))
                            }
                            (TokenType::LessEqual, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 <= n2))
                            }
                            (TokenType::LessEqual, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 <= s2))
                            }
                            (TokenType::ApproxEqual, _, _) => todo!("jam karet!"),
                            (op, l, r) => Err(RuntimeError {
                                line: expression.line,
                                message: format!("cannot apply {:?} to {:?} and {:?}", op, l, r),
                            }),
                        }
                    }
                }
            }

            _ => Ok(Value::Null),
        }
    }
}
