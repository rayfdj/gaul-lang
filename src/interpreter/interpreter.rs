use crate::interpreter::environment::Environment;
use crate::interpreter::native_function::all_native_functions;
use crate::interpreter::native_method::call_native_method;
use crate::interpreter::value::{Function, Value};
use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use crate::scanner::token::TokenType;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub line: usize,
    pub message: String,
}

pub struct Interpreter {
    env: Environment,
}

impl Interpreter {
    pub fn new(env: Environment) -> Self {
        let mut interpreter = Self { env };
        interpreter.define_native_functions();
        interpreter
    }

    fn define_native_functions(&mut self) {
        for (name, native_function) in all_native_functions() {
            self.env.define(native_function, false);
        }
    }

    pub fn interpret(&mut self, program: Program) -> Result<Value, RuntimeError> {
        let mut last_value: Value = Value::Null;
        for declaration in program.declarations.iter() {
            last_value = self.execute_declaration(declaration)?;
        }
        Ok(last_value)
    }

    fn execute_declaration(&mut self, declaration: &Declaration) -> Result<Value, RuntimeError> {
        match &declaration.kind {
            DeclarationKind::Let { name, initializer } => {
                let value = self.evaluate_expression(&initializer)?;
                self.env.define(value, false);
                Ok(Value::Null)
            }
            DeclarationKind::Var { name, initializer } => {
                let value = self.evaluate_expression(&initializer)?;
                self.env.define(value, true);
                Ok(Value::Null)
            }
            DeclarationKind::Fn { name, params, body } => {
                // 1) Reserve slot in current env
                let slot = self.env.current_scope_len(); 
                self.env.define(Value::Null, false);      // placeholder so slot exists

                // 2) Capture closure AFTER reserving slot
                let closure = Rc::new(RefCell::new(self.env.clone()));

                // 3) Build function
                let fun_rc = Rc::new(Function {
                    name: Rc::from(name.as_str()),
                    params: params.iter().map(|p| Rc::<str>::from(p.as_str())).collect(),
                    body: Rc::new(body.clone()),
                    closure: closure.clone(),
                });

                let fun_val = Value::Fn(fun_rc);

                // 4) Patch function into BOTH:
                //   - current env
                //   - closure env
                self.env.set_at(0, slot, fun_val.clone());
                closure.borrow_mut().set_at(0, slot, fun_val);

                Ok(Value::Null)
            }
            DeclarationKind::ExprStmt(expr) => self.evaluate_expression(&expr),
        }
    }

    fn evaluate_expression(&mut self, expression: &Expr) -> Result<Value, RuntimeError> {
        match &expression.kind {
            // primary
            ExprKind::Num(n) => Ok(Value::Num(*n)),
            ExprKind::Str(s) => Ok(Value::Str(Rc::<str>::from(s.as_str()))),
            ExprKind::Bool(b) => Ok(Value::Bool(*b)),
            ExprKind::Null => Ok(Value::Null),
            ExprKind::Identifier { name, resolved } => {
                match resolved {
                    Some((depth, slot)) => Ok(self.env.get_at(*depth, *slot)),
                    None => panic!("Unresolved variable '{}'", name),  // should never happen
                }
            }

            // assignment
            ExprKind::Assign { target, value } => {
                let value = self.evaluate_expression(&*value)?;
                match &target.kind {
                    ExprKind::Identifier { name, resolved } => {
                        match resolved {
                            Some((depth, slot)) => {
                                self.env.assign_at(*depth, *slot, value).map_err(|msg| RuntimeError {
                                    line: expression.line,
                                    message: msg,
                                })?;
                                Ok(Value::Null)
                            },
                            None => panic!("Unresolved variable '{}'", name),  // should never happen
                        }
                    }
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
                let operand_value = self.evaluate_expression(&*operand)?;
                match (&operator.token_type, operand_value) {
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
                let left_value = self.evaluate_expression(&*left)?;

                match (&operator.token_type, &left_value) {
                    // handle the short-circuit
                    (TokenType::And, Value::Bool(false)) => Ok(left_value),
                    (TokenType::Or, Value::Bool(true)) => Ok(left_value),
                    (TokenType::And | TokenType::Or, Value::Bool(_)) => {
                        let right_value = self.evaluate_expression(&*right)?;
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
                        let right_value = self.evaluate_expression(&*right)?;
                        match (&operator.token_type, left_value, right_value) {
                            // arithmetic
                            (TokenType::Plus, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 + n2))
                            }
                            (TokenType::Minus, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 - n2))
                            }
                            (TokenType::Slash, Value::Num(n1), Value::Num(n2)) => {
                                if n2 == 0.0 {
                                    Err(RuntimeError {
                                        line: expression.line,
                                        message: format!(
                                            "cannot divide '{}' by '{}'", n1, n2
                                        ),
                                    })
                                } else {
                                    Ok(Value::Num(n1 / n2))
                                }
                            }
                            (TokenType::Star, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 * n2))
                            }

                            // weird String concat
                            (TokenType::Plus, Value::Str(s1), Value::Str(s2)) => {
                                let mut s = String::with_capacity(s1.len() + s2.len());
                                s.push_str(s1.as_ref());
                                s.push_str(s2.as_ref());
                                Ok(Value::Str(Rc::from(s)))
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
                        Some(e) => self.evaluate_expression(e.as_ref()),
                        None => Ok(Value::Null),
                    }
                })();

                self.env.pop_scope();
                result
            }

            ExprKind::Array { elements } => {
                let mut array = Vec::with_capacity(elements.len());
                for element in elements {
                    array.push(self.evaluate_expression(element)?);
                }
                Ok(Value::Array(Rc::new(RefCell::new(array))))
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.evaluate_expression(condition.as_ref())?;
                match condition {
                    Value::Bool(true) => self.evaluate_expression(then_branch.as_ref()),
                    Value::Bool(false) => match else_branch {
                        Some(else_expr) => self.evaluate_expression(else_expr.as_ref()),
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
                    let cond_value = self.evaluate_expression(condition.as_ref())?;
                    match cond_value {
                        Value::Bool(true) => {
                            self.evaluate_expression(body.as_ref())?;
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
                let start_val = self.evaluate_expression(start.as_ref())?;
                let end_val = self.evaluate_expression(end.as_ref())?;
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
                let iterable = self.evaluate_expression(&*iterable)?;
                match iterable {
                    Value::Range(start, end) => {
                        self.env.push_scope();
                        self.env.define(Value::Num(0.0), false);  // slot 0
                        let result = (|| {
                            for i in (start as i64)..(end as i64) {
                                self.env.set_at(0, 0, Value::Num(i as f64));
                                self.evaluate_expression(body.as_ref())?;
                            }
                            Ok(Value::Null)
                        })();
                        self.env.pop_scope();
                        result
                    },
                    Value::Array(elements) => {
                        self.env.push_scope();
                        self.env.define(Value::Null, false);  // slot 0
                        let result = (|| {
                            for element in elements.borrow().iter() {
                                self.env.set_at(0, 0, element.clone());
                                self.evaluate_expression(body.as_ref())?;
                            }
                            Ok(Value::Null)
                        })();
                        self.env.pop_scope();
                        result
                    },
                    _ => Err(RuntimeError {
                        line: expression.line,
                        message: format!("iterable: '{:?}' must be a range or an array", iterable),
                    }),
                }
            }

            ExprKind::Call { callee, arguments } => {
                let argument_values = arguments
                    .iter()
                    .map(|expr| self.evaluate_expression(expr))
                    .collect::<Result<Vec<_>, _>>()?;

                if let ExprKind::Get { object, name } = &callee.kind {
                    let receiver = self.evaluate_expression(object)?;
                    return call_native_method(&receiver, name, &argument_values).map_err(|message| RuntimeError {
                        line: expression.line,
                        message,
                    });
                }

                let callee_value = self.evaluate_expression(&*callee)?;
                match callee_value {
                    Value::Fn(fun) => {
                        if fun.params.len() != argument_values.len() {
                            return Err(RuntimeError {
                                line: expression.line,
                                message: format!(
                                    "function '{}' expects {} arguments, but got {}",
                                    fun.name.as_ref(),
                                    fun.params.len(),
                                    argument_values.len()
                                ),
                            });
                        };
                        // Save current env, switch to closure's env
                        let saved_env = std::mem::replace(&mut self.env, fun.closure.borrow().clone());

                        self.env.push_scope();
                        let result = (|| {
                            for (param, arg) in fun.params.iter().zip(argument_values.into_iter()) {
                                self.env.define(arg, false); // or define_at_slot; depending on your slot env API
                            }
                            self.evaluate_expression(fun.body.as_ref())
                        })();
                        self.env.pop_scope();

                        self.env = saved_env;
                        result
                    }
                    Value::NativeFn(native_fun) => {
                        (native_fun.func)(&argument_values).map_err(|msg| RuntimeError {
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
