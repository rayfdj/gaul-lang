use crate::interpreter::environment::Environment;
use crate::interpreter::native_function::all_native_functions;
use crate::interpreter::native_method::call_native_method;
use crate::interpreter::value::{Function, Value};
use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use crate::scanner::token::TokenType;
use std::cell::RefCell;
use std::rc::Rc;

// Uhhhhh needed to support break, continue, return. Wish I'd done this earlier!
// NOT public because we don't want this seen by the outside world outside the interpreter.
#[derive(Debug, Clone)]
enum ControlFlow {
    Value(Value),
    Break,
    Continue,
    Return(Value),
}

// prevent having to wrap all those values with ControlFlow::Value! Annoying. With this, it's
// just an extra .into()
impl From<Value> for ControlFlow {
    fn from(v: Value) -> Self {
        ControlFlow::Value(v)
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub line: usize,
    pub message: String,
}

pub struct Interpreter {
    env: Environment,
}

// Propagate control flow, discard value
macro_rules! prop {
    ($expr:expr) => {
        match $expr? {
            ControlFlow::Value(_) => {}
            other => return Ok(other),
        }
    };
}

// Propagate control flow, extract value
macro_rules! prop_val {
    ($expr:expr) => {
        match $expr? {
            ControlFlow::Value(v) => v,
            other => return Ok(other),
        }
    };
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
        let mut last = Value::Null;
        for decl in program.declarations.iter() {
            match self.execute_declaration(decl)? {
                ControlFlow::Value(v) => last = v,
                ControlFlow::Break | ControlFlow::Continue => {
                    return Err(RuntimeError {
                        line: 0,
                        message: "break/continue outside loop".into(),
                    });
                }
                ControlFlow::Return(v) => return Ok(v),
            }
        }
        Ok(last)
    }

    fn execute_declaration(&mut self, declaration: &Declaration) -> Result<ControlFlow, RuntimeError> {
        match &declaration.kind {
            DeclarationKind::Let { name, initializer } => {
                let value = prop_val!(self.evaluate_expression(&initializer));
                self.env.define(value, false);
                Ok(Value::Null.into())
            }
            DeclarationKind::Var { name, initializer } => {
                let value = prop_val!(self.evaluate_expression(&initializer));
                self.env.define(value, true);
                Ok(Value::Null.into())
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

                Ok(Value::Null.into())
            }
            DeclarationKind::ExprStmt(expr) => self.evaluate_expression(&expr),
        }
    }

    fn evaluate_expression(&mut self, expression: &Expr) -> Result<ControlFlow, RuntimeError> {
        match &expression.kind {
            // primary
            ExprKind::Num(n) => Ok(Value::Num(*n).into()),
            ExprKind::Str(s) => Ok(Value::Str(Rc::<str>::from(s.as_str())).into()),
            ExprKind::Bool(b) => Ok(Value::Bool(*b).into()),
            ExprKind::Null => Ok(Value::Null.into()),
            ExprKind::Identifier { name, resolved } => {
                match resolved {
                    Some((depth, slot)) => Ok(self.env.get_at(*depth, *slot).into()),
                    None => panic!("Unresolved variable '{}'", name),  // should never happen
                }
            }

            // assignment
            ExprKind::Assign { target, value } => {
                let value = prop_val!(self.evaluate_expression(&*value));
                match &target.kind {
                    ExprKind::Identifier { name, resolved } => {
                        match resolved {
                            Some((depth, slot)) => {
                                self.env.assign_at(*depth, *slot, value).map_err(|msg| RuntimeError {
                                    line: expression.line,
                                    message: msg,
                                })?;
                                Ok(Value::Null.into())
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
                let operand_value = prop_val!(self.evaluate_expression(&*operand));
                match (&operator.token_type, operand_value) {
                    (TokenType::Bang, Value::Bool(b)) => Ok(Value::Bool(!b).into()),
                    (TokenType::Minus, Value::Num(n)) => Ok(Value::Num(-n).into()),
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
                let left_value = prop_val!(self.evaluate_expression(&*left));

                match (&operator.token_type, &left_value) {
                    // handle the short-circuit
                    (TokenType::And, Value::Bool(false)) => Ok(left_value.into()),
                    (TokenType::Or, Value::Bool(true)) => Ok(left_value.into()),
                    (TokenType::And | TokenType::Or, Value::Bool(_)) => {
                        let right_value = prop_val!(self.evaluate_expression(&*right));
                        match right_value {
                            Value::Bool(_) => Ok(right_value.into()),
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
                        let right_value = prop_val!(self.evaluate_expression(&*right));
                        match (&operator.token_type, left_value, right_value) {
                            // arithmetic
                            (TokenType::Plus, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 + n2).into())
                            }
                            (TokenType::Minus, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 - n2).into())
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
                                    Ok(Value::Num(n1 / n2).into())
                                }
                            }
                            (TokenType::Star, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 * n2).into())
                            }

                            // weird String concat
                            (TokenType::Plus, Value::Str(s1), Value::Str(s2)) => {
                                let mut s = String::with_capacity(s1.len() + s2.len());
                                s.push_str(s1.as_ref());
                                s.push_str(s2.as_ref());
                                Ok(Value::Str(Rc::from(s)).into())
                            }

                            // comparison
                            (TokenType::Equal, val1, val2) => Ok(Value::Bool(val1 == val2).into()),
                            (TokenType::NotEqual, val1, val2) => Ok(Value::Bool(val1 != val2).into()),
                            (TokenType::Greater, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 > n2).into())
                            }
                            (TokenType::Greater, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 > s2).into())
                            }
                            (TokenType::GreaterEqual, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 >= n2).into())
                            }
                            (TokenType::GreaterEqual, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 >= s2).into())
                            }
                            (TokenType::Less, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 < n2).into())
                            }
                            (TokenType::Less, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 < s2).into())
                            }
                            (TokenType::LessEqual, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Bool(n1 <= n2).into())
                            }
                            (TokenType::LessEqual, Value::Str(s1), Value::Str(s2)) => {
                                Ok(Value::Bool(s1 <= s2).into())
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
                        prop!(self.execute_declaration(declaration));
                    }
                    match expr {
                        Some(e) => self.evaluate_expression(e.as_ref()),
                        None => Ok(Value::Null.into()),
                    }
                })();

                self.env.pop_scope();
                result
            }

            ExprKind::Array { elements } => {
                let mut array = Vec::with_capacity(elements.len());
                for element in elements {
                    array.push(prop_val!(self.evaluate_expression(element)));
                }
                Ok(Value::Array(Rc::new(RefCell::new(array))).into())
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = prop_val!(self.evaluate_expression(condition.as_ref()));
                match condition {
                    Value::Bool(true) => self.evaluate_expression(then_branch.as_ref()),
                    Value::Bool(false) => match else_branch {
                        Some(else_expr) => self.evaluate_expression(else_expr.as_ref()),
                        None => Ok(Value::Null.into()),
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
                    let cond_value = prop_val!(self.evaluate_expression(condition.as_ref()));
                    match cond_value {
                        Value::Bool(true) => {
                            match self.evaluate_expression(body.as_ref())? {
                                ControlFlow::Value(_) => {}
                                ControlFlow::Continue => continue,
                                ControlFlow::Break => break,
                                ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                            }                        }
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
                Ok(Value::Null.into())
            }

            ExprKind::Range { start, end } => {
                let start_val = prop_val!(self.evaluate_expression(start.as_ref()));
                let end_val = prop_val!(self.evaluate_expression(end.as_ref()));
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
                            Ok(Value::Range(*n1, *n2).into())
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
                let iterable = prop_val!(self.evaluate_expression(&*iterable));
                match iterable {
                    Value::Range(start, end) => {
                        self.env.push_scope();
                        self.env.define(Value::Num(0.0), false);
                        let result: Result<ControlFlow, RuntimeError> = (|| {
                            for i in (start as i64)..(end as i64) {
                                self.env.set_at(0, 0, Value::Num(i as f64));
                                match self.evaluate_expression(body.as_ref())? {
                                    ControlFlow::Value(_) => {}
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Break => break,
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                }
                            }
                            Ok(Value::Null.into())
                        })();
                        self.env.pop_scope();
                        result
                    },
                    Value::Array(elements) => {
                        self.env.push_scope();
                        self.env.define(Value::Null, false);  // slot 0
                        let result: Result<ControlFlow, RuntimeError> = (|| {
                            for element in elements.borrow().iter() {
                                self.env.set_at(0, 0, element.clone());
                                match self.evaluate_expression(body.as_ref())? {
                                    ControlFlow::Value(_) => {}
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Break => break,
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                }
                            }
                            Ok(Value::Null.into())
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
                let mut argument_values = Vec::with_capacity(arguments.len());
                for arg in arguments {
                    argument_values.push(prop_val!(self.evaluate_expression(arg)));
                }

                if let ExprKind::Get { object, name } = &callee.kind {
                    let receiver = prop_val!(self.evaluate_expression(object));
                    return call_native_method(&receiver, name, &argument_values)
                        .map(|v| v.into())
                        .map_err(|message| RuntimeError {
                            line: expression.line,
                            message,
                        });
                }

                let callee_value = prop_val!(self.evaluate_expression(&*callee));
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
                                self.env.define(arg, false);
                            }
                            self.evaluate_expression(fun.body.as_ref())
                        })();
                        self.env.pop_scope();

                        self.env = saved_env;

                        // Unwrap Return at function boundary
                        match result? {
                            ControlFlow::Return(v) => Ok(v.into()),
                            ControlFlow::Break | ControlFlow::Continue => {
                                Err(RuntimeError {
                                    line: expression.line,
                                    message: "break/continue outside loop".into(),
                                })
                            }
                            other => Ok(other),
                        }
                    }
                    Value::NativeFn(native_fun) => {
                        (native_fun.func)(&argument_values)
                            .map(|v| v.into())
                            .map_err(|msg| RuntimeError {
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

            ExprKind::Break => Ok(ControlFlow::Break),
            ExprKind::Continue => Ok(ControlFlow::Continue),
            ExprKind::Return(value) => {
                match value {
                    Some(expr) => {
                        let v = prop_val!(self.evaluate_expression(expr));
                        Ok(ControlFlow::Return(v))
                    }
                    None => Ok(ControlFlow::Return(Value::Null)),
                }
            }
            ExprKind::Get { .. } => {
                // Get is handled in Call for method calls
                // Standalone property access not yet implemented
                Err(RuntimeError {
                    line: expression.line,
                    message: "standalone property access not yet implemented".into(),
                })
            }
            ExprKind::Pipe { .. } => {
                todo!("pipe operator not yet implemented")
            }
        }
    }
}
