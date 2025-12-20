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
        let mut interpreter = Interpreter { env };
        interpreter.define_native_functions();
        interpreter
    }

    fn define_native_functions(&mut self) {
        self.env
            .define(
                "println",
                Value::NativeFn {
                    name: "println".to_string(),
                    arity: None,
                    func: |args| {
                        for arg in &args {
                            print!("{}", arg); // need Display impl for Value
                        }
                        println!(); // newline at end
                        Ok(Value::Null)
                    },
                },
                false,
            )
            .unwrap();
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

            // assignment
            ExprKind::Assign { target, value } => {
                let value = self.evaluate_expression(*value)?;
                match (*target).kind {
                    ExprKind::Identifier(id) => self
                        .env
                        .assign(&id, value)
                        .map(|()| Value::Null)
                        .map_err(|msg| RuntimeError {
                            line: expression.line,
                            message: msg,
                        }),
                    other => Err(RuntimeError {
                        line: expression.line,
                        message: format!(
                            "Only identifier can be target of an assignment, but found: '{:?}'",
                            other
                        ),
                    }),
                }
            }

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
                left,
                operator,
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

            ExprKind::Block { declarations, expr } => {
                self.env.push_scope();

                let result = (|| {
                    for declaration in declarations {
                        self.execute_declaration(declaration)?;
                    }
                    match expr {
                        Some(e) => self.evaluate_expression(*e),
                        None => Ok(Value::Null),
                    }
                })();

                self.env.pop_scope();
                result
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.evaluate_expression(*condition)?;
                match condition {
                    Value::Bool(true) => self.evaluate_expression(*then_branch),
                    Value::Bool(false) => match else_branch {
                        Some(else_expr) => self.evaluate_expression(*else_expr),
                        None => Ok(Value::Null),
                    },
                    _ => Err(RuntimeError {
                        line: expression.line,
                        message: format!(
                            "if condition must evaluate to bool, but it evaluates to {:?}",
                            condition
                        ),
                    }),
                }
            }

            ExprKind::While { condition, body } => {
                loop {
                    let cond_value = self.evaluate_expression((*condition).clone())?;
                    match cond_value {
                        Value::Bool(true) => {
                            self.evaluate_expression((*body).clone())?;
                        }
                        Value::Bool(false) => break,
                        _ => {
                            return Err(RuntimeError {
                                line: expression.line,
                                message: format!(
                                    "while condition must be bool, got {:?}",
                                    cond_value
                                ),
                            });
                        }
                    }
                }
                Ok(Value::Null)
            }

            ExprKind::Range { start, end } => {
                let start_val = self.evaluate_expression(*start)?;
                let end_val = self.evaluate_expression(*end)?;
                match (&start_val, &end_val) {
                    (Value::Num(n1), Value::Num(n2)) => {
                        if n1.fract() != 0.0 || n2.fract() != 0.0 {
                            Err(RuntimeError {
                                line: expression.line,
                                message: format!(
                                    "range requires integers for start: '{}' and end: '{}'",
                                    n1, n2
                                ),
                            })
                        } else {
                            Ok(Value::Range(*n1, *n2))
                        }
                    }
                    _ => Err(RuntimeError {
                        line: expression.line,
                        message: format!(
                            "range start: '{:?}' and end: '{:?}' must be numbers",
                            start_val, end_val
                        ),
                    }),
                }
            }

            ExprKind::For {
                variable,
                iterable,
                body,
            } => {
                let iterable = self.evaluate_expression(*iterable)?;
                match iterable {
                    Value::Range(start, end) => {
                        self.env.push_scope();
                        let result = (|| {
                            self.env
                                .define(&variable, Value::Num(0.0), true)
                                .map_err(|msg| RuntimeError {
                                    line: expression.line,
                                    message: msg,
                                })?;
                            for i in (start as i64)..(end as i64) {
                                self.env.assign(&variable, Value::Num(i as f64)).map_err(
                                    |msg| RuntimeError {
                                        line: expression.line,
                                        message: msg,
                                    },
                                )?;
                                self.evaluate_expression((*body).clone())?;
                            }
                            Ok(Value::Null)
                        })();
                        self.env.pop_scope();
                        result
                    }
                    _ => Err(RuntimeError {
                        line: expression.line,
                        message: format!("iterable: '{:?}' must be a range", iterable),
                    }),
                }
            }

            ExprKind::Call { callee, arguments } => {
                let callee_value = self.evaluate_expression(*callee)?;

                let argument_values = arguments
                    .into_iter()
                    .map(|expr| self.evaluate_expression(expr))
                    .collect::<Result<Vec<_>, _>>()?;

                match callee_value {
                    Value::Fn(name, params, body) => {
                        if params.len() != argument_values.len() {
                            return Err(RuntimeError {
                                line: expression.line,
                                message: format!(
                                    "function '{}' expects {} arguments, but got {}",
                                    name,
                                    params.len(),
                                    argument_values.len()
                                ),
                            });
                        };
                        self.env.push_scope();
                        let result = (|| {
                            for (param, arg) in params.into_iter().zip(argument_values) {
                                self.env
                                    .define(param, arg, false)
                                    .map_err(|msg| RuntimeError {
                                        line: expression.line,
                                        message: msg,
                                    })?;
                            }
                            self.evaluate_expression(body)
                        })();
                        self.env.pop_scope();
                        result
                    }
                    Value::NativeFn { name, arity, func } => {
                        func(argument_values).map_err(|msg| RuntimeError {
                            line: expression.line,
                            message: msg,
                        })
                    }
                    _ => Err(RuntimeError {
                        line: expression.line,
                        message: format!("value: '{:?}' is not a function", callee_value),
                    }),
                }
            }

            _ => Ok(Value::Null),
        }
    }
}
