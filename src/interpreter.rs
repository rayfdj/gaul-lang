pub mod environment;
pub mod native_function;
pub mod native_method;
pub mod value;

use crate::config::RuntimeConfig;
use crate::interpreter::environment::Environment;
use crate::interpreter::native_function::all_native_functions;
use crate::interpreter::native_method::call_native_method;
use crate::interpreter::value::{Function, MapKey, Value};
use crate::parser::ast::{Declaration, DeclarationKind, Expr, ExprKind, Program};
use crate::scanner::token::TokenType;
use crate::span::Span;
use smallvec::{SmallVec, smallvec};
use std::cell::RefCell;
use std::rc::Rc;

type Args = SmallVec<[Value; 4]>;

// Uhhhhh needed to support break, continue, return. Wish I'd done this earlier!
// NOT public because we don't want this seen by the outside world outside the interpreter.
#[derive(Debug, Clone)]
enum ControlFlow {
    Value(Value),
    Break,
    Continue,
    Return(Value),
    TailCall { callee: Value, args: Args },
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
    pub span: Span,
    pub message: String,
}

pub struct Interpreter {
    // 3rd try for closure. So we're holding a pointer to the "head" environment, rather than
    // mucking around with stack of scopes.
    env: Rc<Environment>,
    env_pool: Vec<Rc<Environment>>,
    runtime_config: RuntimeConfig,
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
    pub fn new(env: Environment, runtime_config: RuntimeConfig) -> Self {
        let mut interpreter = Self {
            env: Rc::new(env),
            env_pool: Vec::new(),
            runtime_config,
        };
        interpreter.define_native_functions();
        interpreter
    }

    fn define_native_functions(&mut self) {
        for (_name, native_function) in all_native_functions() {
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
                        span: Span::default(),
                        message: "break/continue outside loop".into(),
                    });
                }
                ControlFlow::Return(v) => return Ok(v),
                ControlFlow::TailCall { .. } => unreachable!("TailCall escaped past call_function"),
            }
        }
        Ok(last)
    }

    fn execute_declaration(
        &mut self,
        declaration: &Declaration,
    ) -> Result<ControlFlow, RuntimeError> {
        match &declaration.kind {
            DeclarationKind::Let {
                name: _name,
                initializer,
            } => {
                let slot = self.env.current_scope_len();
                self.env.define(Value::Null, false);
                let value = prop_val!(self.evaluate_expression(initializer));
                self.env.set_at(0, slot, value);
                Ok(Value::Null.into())
            }
            DeclarationKind::Var {
                name: _name,
                initializer,
            } => {
                let slot = self.env.current_scope_len();
                self.env.define(Value::Null, true);
                let value = prop_val!(self.evaluate_expression(initializer));
                self.env.set_at(0, slot, value);
                Ok(Value::Null.into())
            }
            DeclarationKind::Fn { name, params, body } => {
                // Reserve slot in current env
                let slot = self.env.current_scope_len();
                self.env.define(Value::Null, false); // placeholder so slot exists

                // Capture closure AFTER reserving slot
                let closure = Rc::clone(&self.env);

                // Build function
                let fun_rc = Rc::new(Function {
                    name: Rc::from(name.as_str()),
                    params: params.iter().map(|p| Rc::<str>::from(p.as_str())).collect(),
                    body: Rc::clone(body),
                    closure,
                });

                let fun_val = Value::Fn(fun_rc);

                // now we no longer need to patch the closure, since it's a reference to the same
                // environment
                self.env.set_at(0, slot, fun_val.clone());

                Ok(Value::Null.into())
            }
            DeclarationKind::ExprStmt(expr) => self.evaluate_expression(expr),
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
                    None => panic!("Unresolved variable '{}'", name), // should never happen
                }
            }

            // assignment
            ExprKind::Assign { target, value } => {
                self.evaluate_assign(target, value, expression.span)
            }

            // unary
            ExprKind::Unary { operator, operand } => {
                let operand_value = prop_val!(self.evaluate_expression(operand));
                match (&operator.token_type, operand_value) {
                    (TokenType::Bang, Value::Bool(b)) => Ok(Value::Bool(!b).into()),
                    (TokenType::Minus, Value::Num(n)) => Ok(Value::Num(-n).into()),
                    (TokenType::Tilde, Value::Num(n)) => Ok(Value::Num(!(n as i64) as f64).into()),
                    (op_type, v) => Err(RuntimeError {
                        span: expression.span,
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
                let left_value = prop_val!(self.evaluate_expression(left));

                match (&operator.token_type, &left_value) {
                    // handle the short-circuit
                    (TokenType::And, Value::Bool(false)) => Ok(left_value.into()),
                    (TokenType::Or, Value::Bool(true)) => Ok(left_value.into()),
                    (TokenType::And | TokenType::Or, Value::Bool(_)) => {
                        let right_value = prop_val!(self.evaluate_expression(right));
                        match right_value {
                            Value::Bool(_) => Ok(right_value.into()),
                            _ => Err(RuntimeError {
                                span: expression.span,
                                message: format!(
                                    "right value '{:?}' is not of boolean type",
                                    right_value
                                ),
                            }),
                        }
                    }
                    (TokenType::And | TokenType::Or, _) => Err(RuntimeError {
                        span: expression.span,
                        message: format!(
                            "logical operators require boolean operands, got {:?}",
                            left_value
                        ),
                    }),
                    _ => {
                        let right_value = prop_val!(self.evaluate_expression(right));
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
                                        span: expression.span,
                                        message: format!("cannot divide '{}' by '{}'", n1, n2),
                                    })
                                } else {
                                    Ok(Value::Num(n1 / n2).into())
                                }
                            }
                            (TokenType::Star, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(n1 * n2).into())
                            }
                            (TokenType::Percent, Value::Num(n1), Value::Num(n2)) => {
                                if n2 == 0.0 {
                                    Err(RuntimeError {
                                        span: expression.span,
                                        message: format!("cannot modulo '{}' by zero", n1),
                                    })
                                } else {
                                    Ok(Value::Num(n1.rem_euclid(n2)).into())
                                }
                            }

                            // bitwise
                            (TokenType::Ampersand, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(((n1 as i64) & (n2 as i64)) as f64).into())
                            }
                            (TokenType::Pipe, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(((n1 as i64) | (n2 as i64)) as f64).into())
                            }
                            (TokenType::Caret, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(((n1 as i64) ^ (n2 as i64)) as f64).into())
                            }
                            (TokenType::LeftShift, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(((n1 as i64) << (n2 as i64)) as f64).into())
                            }
                            (TokenType::RightShift, Value::Num(n1), Value::Num(n2)) => {
                                Ok(Value::Num(((n1 as i64) >> (n2 as i64)) as f64).into())
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
                            (TokenType::NotEqual, val1, val2) => {
                                Ok(Value::Bool(val1 != val2).into())
                            }
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
                            (TokenType::ApproxEqual, Value::Num(n1), Value::Num(n2)) => {
                                let diff = (n1 - n2).abs();
                                let max_val = n1.abs().max(n2.abs());
                                let is_close = if max_val == 0.0 {
                                    diff == 0.0
                                } else {
                                    (diff / max_val) <= self.runtime_config.jam_karet_num
                                };
                                Ok(Value::Bool(is_close).into())
                            }
                            (TokenType::ApproxEqual, Value::Str(s1), Value::Str(s2)) => {
                                let max_len = s1.chars().count().max(s2.chars().count());
                                let threshold =
                                    ((max_len as f64) * self.runtime_config.jam_karet_str).ceil()
                                        as usize;
                                let threshold = threshold.max(1); // minimum 1 edit allowed
                                let distance = strsim::levenshtein(&s1, &s2);
                                Ok(Value::Bool(distance <= threshold).into())
                            }
                            (op, l, r) => Err(RuntimeError {
                                span: expression.span,
                                message: format!("cannot apply {:?} to {:?} and {:?}", op, l, r),
                            }),
                        }
                    }
                }
            }

            ExprKind::Block { declarations, expr } => {
                if declarations.is_empty() {
                    // No declarations means no new bindings, so skip the Environment allocation
                    match expr {
                        Some(e) => self.evaluate_expression(e.as_ref()),
                        None => Ok(Value::Null.into()),
                    }
                } else {
                    let previous = self.env.clone();
                    self.env = Rc::new(Environment::new_with_enclosing(previous.clone()));

                    let result = (|| {
                        for declaration in declarations {
                            prop!(self.execute_declaration(declaration));
                        }
                        match expr {
                            Some(e) => self.evaluate_expression(e.as_ref()),
                            None => Ok(Value::Null.into()),
                        }
                    })();

                    self.env = previous;
                    result
                }
            }

            ExprKind::Array { elements } => {
                let mut array = Vec::with_capacity(elements.len());
                for element in elements {
                    array.push(prop_val!(self.evaluate_expression(element)));
                }
                Ok(Value::Array(Rc::new(RefCell::new(array))).into())
            }

            ExprKind::Map { pairs } => {
                let mut map = std::collections::HashMap::new();
                for (key_expr, val_expr) in pairs {
                    let key_val = prop_val!(self.evaluate_expression(key_expr));
                    let map_key = MapKey::from_value(&key_val).map_err(|msg| RuntimeError {
                        span: expression.span,
                        message: msg,
                    })?;
                    let value = prop_val!(self.evaluate_expression(val_expr));
                    map.insert(map_key, value);
                }
                Ok(Value::Map(Rc::new(RefCell::new(map))).into())
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
                        span: expression.span,
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
                        Value::Bool(true) => match self.evaluate_expression(body.as_ref())? {
                            ControlFlow::Value(_) => {}
                            ControlFlow::Continue => continue,
                            ControlFlow::Break => break,
                            ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                            cf @ ControlFlow::TailCall { .. } => return Ok(cf),
                        },
                        Value::Bool(false) => break,
                        _ => {
                            return Err(RuntimeError {
                                span: expression.span,
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
                                span: expression.span,
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
                        span: expression.span,
                        message: format!(
                            "range start: '{:?}' and end: '{:?}' must be numbers",
                            start_val, end_val
                        ),
                    }),
                }
            }

            ExprKind::For {
                variable: _variable,
                iterable,
                body,
            } => {
                let iterable = prop_val!(self.evaluate_expression(iterable));
                match iterable {
                    Value::Range(start, end) => {
                        let previous = self.env.clone();
                        self.env = Rc::new(Environment::new_with_enclosing(previous.clone()));
                        self.env.define(Value::Num(0.0), false);
                        let result: Result<ControlFlow, RuntimeError> = (|| {
                            for i in (start as i64)..(end as i64) {
                                self.env.set_at(0, 0, Value::Num(i as f64));
                                match self.evaluate_expression(body.as_ref())? {
                                    ControlFlow::Value(_) => {}
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Break => break,
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                    cf @ ControlFlow::TailCall { .. } => return Ok(cf),
                                }
                            }
                            Ok(Value::Null.into())
                        })(
                        );
                        self.env = previous;
                        result
                    }
                    Value::Array(elements) => {
                        let previous = self.env.clone();
                        self.env = Rc::new(Environment::new_with_enclosing(previous.clone()));
                        self.env.define(Value::Null, false); // slot 0
                        let result: Result<ControlFlow, RuntimeError> = (|| {
                            for element in elements.borrow().iter() {
                                self.env.set_at(0, 0, element.clone());
                                match self.evaluate_expression(body.as_ref())? {
                                    ControlFlow::Value(_) => {}
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Break => break,
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                    cf @ ControlFlow::TailCall { .. } => return Ok(cf),
                                }
                            }
                            Ok(Value::Null.into())
                        })(
                        );
                        self.env = previous;
                        result
                    }
                    Value::Map(map) => {
                        // Collect keys first to avoid holding RefCell borrow during body execution
                        let keys: Vec<Value> = map.borrow().keys().map(|k| k.to_value()).collect();
                        let previous = self.env.clone();
                        self.env = Rc::new(Environment::new_with_enclosing(previous.clone()));
                        self.env.define(Value::Null, false); // slot 0
                        let result: Result<ControlFlow, RuntimeError> = (|| {
                            for key in &keys {
                                self.env.set_at(0, 0, key.clone());
                                match self.evaluate_expression(body.as_ref())? {
                                    ControlFlow::Value(_) => {}
                                    ControlFlow::Continue => continue,
                                    ControlFlow::Break => break,
                                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                                    cf @ ControlFlow::TailCall { .. } => return Ok(cf),
                                }
                            }
                            Ok(Value::Null.into())
                        })(
                        );
                        self.env = previous;
                        result
                    }
                    _ => Err(RuntimeError {
                        span: expression.span,
                        message: format!(
                            "iterable: '{:?}' must be a range, array, or map",
                            iterable
                        ),
                    }),
                }
            }

            ExprKind::Call {
                callee,
                arguments,
                is_tail_call,
            } => {
                let mut argument_values = Args::with_capacity(arguments.len());
                for arg in arguments {
                    argument_values.push(prop_val!(self.evaluate_expression(arg)));
                }

                // This feels a bit hacky, but basically we're doing this because unlike "normal"
                // functions or native functions, the callee here is NOT a function.
                //
                // In normal functions, when we call fibonacci(10), fibonacci the callee resolves
                // to a (Gaul) function, that we then call with the argument 10.
                //
                // But in "method calls", e.g.: arr.len(), the callee is an array (so obviously it
                // does NOT resolve to a function). Rather, it gets resolved to an array, and
                // then we call the Rust len() method implemented in native_methods.rs, passing the
                // array as an argument to that function.
                //
                // Now, map() is a bit different, because it still takes the array as an argument,
                // but they need to call a different function (in Gaul, not Rust) on each of the
                // array's elements. So they again have to be handled a bit differently from
                // the native methods like len().
                if let ExprKind::Get { object, name } = &callee.kind {
                    let receiver = prop_val!(self.evaluate_expression(object));

                    if let Value::Array(elements) = &receiver {
                        match &name[..] {
                            "map" => {
                                let callable =
                                    argument_values.first().ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "map expects a function".into(),
                                    })?;
                                let result: Result<Vec<Value>, RuntimeError> = elements
                                    .borrow()
                                    .iter()
                                    .map(|e| {
                                        self.call_function(
                                            callable,
                                            smallvec![e.clone()],
                                            expression.span,
                                        )
                                    })
                                    .collect();
                                return Ok(Value::Array(Rc::new(RefCell::new(result?))).into());
                            }
                            "filter" => {
                                let predicate =
                                    argument_values.first().ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "filter expects a predicate".into(),
                                    })?;
                                let arr = elements.borrow();
                                let mut result = Vec::new();

                                for item in arr.iter() {
                                    let keep = self.call_function(
                                        predicate,
                                        smallvec![item.clone()],
                                        expression.span,
                                    )?;
                                    match keep {
                                        Value::Bool(true) => result.push(item.clone()),
                                        Value::Bool(false) => {}
                                        _ => {
                                            return Err(RuntimeError {
                                                span: expression.span,
                                                message: "filter predicate must return a boolean"
                                                    .into(),
                                            });
                                        }
                                    }
                                }

                                return Ok(Value::Array(Rc::new(RefCell::new(result))).into());
                            }
                            "reduce" => {
                                let initial_value =
                                    argument_values.first().ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "reduce expects an initial value".into(),
                                    })?;
                                let callback =
                                    argument_values.get(1).ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "reduce expects a function".into(),
                                    })?;

                                let arr = elements.borrow();
                                let mut acc = initial_value.clone();

                                for item in arr.iter() {
                                    acc = self.call_function(
                                        callback,
                                        smallvec![acc, item.clone()],
                                        expression.span,
                                    )?;
                                }

                                return Ok(acc.into());
                            }
                            "find" => {
                                let predicate =
                                    argument_values.first().ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "find expects a predicate".into(),
                                    })?;
                                let arr = elements.borrow();

                                for item in arr.iter() {
                                    let result = self.call_function(
                                        predicate,
                                        smallvec![item.clone()],
                                        expression.span,
                                    )?;
                                    match result {
                                        Value::Bool(true) => return Ok(item.clone().into()),
                                        Value::Bool(false) => {}
                                        _ => {
                                            return Err(RuntimeError {
                                                span: expression.span,
                                                message: "find predicate must return a boolean"
                                                    .into(),
                                            });
                                        }
                                    }
                                }

                                return Ok(Value::Null.into());
                            }
                            "sort_by" => {
                                let comparator =
                                    argument_values.first().ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "sort_by expects a comparator function".into(),
                                    })?;
                                let items: Vec<Value> = elements.borrow().clone();
                                let sorted = self.merge_sort(items, comparator, expression.span)?;
                                return Ok(Value::Array(Rc::new(RefCell::new(sorted))).into());
                            }
                            "sort_by_key" => {
                                let key_fn =
                                    argument_values.first().ok_or_else(|| RuntimeError {
                                        span: expression.span,
                                        message: "sort_by_key expects a key function".into(),
                                    })?;
                                let arr = elements.borrow();

                                // Schwartzian transform: extract keys first
                                let mut pairs: Vec<(Value, Value)> = Vec::with_capacity(arr.len());
                                for item in arr.iter() {
                                    let key = self.call_function(
                                        key_fn,
                                        smallvec![item.clone()],
                                        expression.span,
                                    )?;
                                    pairs.push((key, item.clone()));
                                }

                                if pairs.is_empty() {
                                    return Ok(Value::Array(Rc::new(RefCell::new(vec![]))).into());
                                }

                                // Validate all keys are the same sortable type
                                let first_is_num = matches!(&pairs[0].0, Value::Num(_));
                                let first_is_str = matches!(&pairs[0].0, Value::Str(_));
                                if !first_is_num && !first_is_str {
                                    return Err(RuntimeError {
                                        span: expression.span,
                                        message: "sort_by_key: key function must return a number or string".into(),
                                    });
                                }
                                for (key, _) in &pairs[1..] {
                                    let ok = if first_is_num {
                                        matches!(key, Value::Num(_))
                                    } else {
                                        matches!(key, Value::Str(_))
                                    };
                                    if !ok {
                                        return Err(RuntimeError {
                                            span: expression.span,
                                            message: "sort_by_key: all keys must be the same type"
                                                .into(),
                                        });
                                    }
                                }

                                // Sort by extracted keys
                                pairs.sort_by(|(a, _), (b, _)| match (a, b) {
                                    (Value::Num(x), Value::Num(y)) => {
                                        x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                                    }
                                    (Value::Str(x), Value::Str(y)) => x.cmp(y),
                                    _ => std::cmp::Ordering::Equal,
                                });

                                let sorted: Vec<Value> =
                                    pairs.into_iter().map(|(_, v)| v).collect();
                                return Ok(Value::Array(Rc::new(RefCell::new(sorted))).into());
                            }
                            _ => {}
                        }
                    }

                    return call_native_method(&receiver, name, &argument_values)
                        .map(|v| v.into())
                        .map_err(|message| RuntimeError {
                            span: expression.span,
                            message,
                        });
                }

                let callee_value = prop_val!(self.evaluate_expression(callee));

                if *is_tail_call {
                    return Ok(ControlFlow::TailCall {
                        callee: callee_value,
                        args: argument_values,
                    });
                }

                // refactored this whole thing out to call_function
                self.call_function(&callee_value, argument_values, expression.span)
                    .map(|v| v.into())
            }

            ExprKind::Break => Ok(ControlFlow::Break),
            ExprKind::Continue => Ok(ControlFlow::Continue),
            ExprKind::Return(value) => match value {
                Some(expr) => {
                    let v = prop_val!(self.evaluate_expression(expr));
                    Ok(ControlFlow::Return(v))
                }
                None => Ok(ControlFlow::Return(Value::Null)),
            },
            ExprKind::Get { .. } => {
                // Get is handled in Call for method calls
                // Standalone property access not yet implemented
                Err(RuntimeError {
                    span: expression.span,
                    message: "standalone property access not yet implemented".into(),
                })
            }
            ExprKind::Index { object, index } => {
                let obj = prop_val!(self.evaluate_expression(object));
                let idx = prop_val!(self.evaluate_expression(index));
                self.evaluate_index_read(obj, idx, expression.span)
            }
            ExprKind::Lambda { params, body } => {
                // We capture the *current* environment. Because we use the Linked Chain
                // architecture (Rc<Environment>), this is a cheap pointer clone that
                // keeps the parent scope alive.
                let closure = Rc::clone(&self.env);

                // Create the anonymous function object
                let function = Function {
                    name: Rc::from("<lambda>"),
                    params: params.iter().map(|p| Rc::from(p.as_str())).collect(),
                    body: Rc::clone(body),
                    closure,
                };

                Ok(Value::Fn(Rc::new(function)).into())
            }
        }
    }

    #[inline(never)]
    fn evaluate_assign(
        &mut self,
        target: &Expr,
        value_expr: &Expr,
        span: Span,
    ) -> Result<ControlFlow, RuntimeError> {
        let value = prop_val!(self.evaluate_expression(value_expr));
        match &target.kind {
            ExprKind::Identifier { name, resolved } => match resolved {
                Some((depth, slot)) => {
                    self.env
                        .assign_at(*depth, *slot, value)
                        .map_err(|msg| RuntimeError { span, message: msg })?;
                    Ok(Value::Null.into())
                }
                None => panic!("Unresolved variable '{}'", name),
            },
            ExprKind::Index { object, index } => {
                let obj = prop_val!(self.evaluate_expression(object));
                let idx = prop_val!(self.evaluate_expression(index));
                self.evaluate_index_write(obj, idx, value, span)?;
                Ok(Value::Null.into())
            }
            other => Err(RuntimeError {
                span,
                message: format!(
                    "Only identifier can be target of an assignment, but found: '{:?}'",
                    other
                ),
            }),
        }
    }

    #[inline(never)]
    fn evaluate_index_read(
        &mut self,
        obj: Value,
        idx: Value,
        span: Span,
    ) -> Result<ControlFlow, RuntimeError> {
        match (obj, &idx) {
            (Value::Array(arr), Value::Num(n)) => {
                if *n < 0.0 {
                    return Err(RuntimeError {
                        span,
                        message: format!("index {} is negative", n),
                    });
                }
                let i = *n as usize;
                let arr = arr.borrow();
                arr.get(i).cloned().ok_or_else(|| RuntimeError {
                    span,
                    message: format!("index {} out of bounds (len {})", i, arr.len()),
                }).map(ControlFlow::Value)
            }
            (Value::Map(map), _) => {
                let key = MapKey::from_value(&idx).map_err(|msg| RuntimeError { span, message: msg })?;
                Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Null).into())
            }
            (Value::Str(s), Value::Num(n)) => {
                if *n < 0.0 {
                    return Err(RuntimeError {
                        span,
                        message: format!("index {} is negative", n),
                    });
                }
                let i = *n as usize;
                match s.chars().nth(i) {
                    Some(c) => Ok(Value::Str(c.to_string().into()).into()),
                    None => Err(RuntimeError {
                        span,
                        message: format!("index {} out of bounds (len {})", i, s.chars().count()),
                    }),
                }
            }
            (obj, idx) => Err(RuntimeError {
                span,
                message: format!("cannot index '{}' with '{}'", obj, idx),
            }),
        }
    }

    #[inline(never)]
    fn evaluate_index_write(
        &mut self,
        obj: Value,
        idx: Value,
        value: Value,
        span: Span,
    ) -> Result<(), RuntimeError> {
        match (obj, &idx) {
            (Value::Array(arr), Value::Num(n)) => {
                let i = *n as usize;
                let mut arr = arr.borrow_mut();
                if i < arr.len() {
                    arr[i] = value;
                    Ok(())
                } else {
                    Err(RuntimeError {
                        span,
                        message: format!("index {} out of bounds (len {})", i, arr.len()),
                    })
                }
            }
            (Value::Map(map), _) => {
                let key = MapKey::from_value(&idx).map_err(|msg| RuntimeError { span, message: msg })?;
                map.borrow_mut().insert(key, value);
                Ok(())
            }
            (Value::Str(_), _) => Err(RuntimeError {
                span,
                message: "strings are immutable — cannot assign via subscript".into(),
            }),
            (obj, _) => Err(RuntimeError {
                span,
                message: format!("cannot subscript-assign into '{}'", obj),
            }),
        }
    }

    // Adding this because we will need to repeat the same code to call functions that will be
    // passed to map/filter/reduce. Then it can be a lot cleaner
    fn call_function(
        &mut self,
        callee: &Value,
        args: Args,
        span: Span,
    ) -> Result<Value, RuntimeError> {
        let mut current_callee = callee.clone();
        let mut current_args = args;

        loop {
            match &current_callee {
                Value::Fn(fun) => {
                    if fun.params.len() != current_args.len() {
                        return Err(RuntimeError {
                            span,
                            message: format!(
                                "function '{}' expects {} arguments, but got {}",
                                fun.name,
                                fun.params.len(),
                                current_args.len()
                            ),
                        });
                    }

                    // Try to reuse a recycled environment to avoid heap allocation
                    let function_env = if let Some(mut recycled) = self.env_pool.pop() {
                        Rc::get_mut(&mut recycled)
                            .expect("recycled env has refcount > 1")
                            .reinit(fun.closure.clone(), current_args);
                        recycled
                    } else {
                        Rc::new(Environment::new_for_call(fun.closure.clone(), current_args))
                    };

                    let previous = std::mem::replace(&mut self.env, function_env);
                    let result = self.evaluate_expression(fun.body.as_ref());
                    let used_env = std::mem::replace(&mut self.env, previous);

                    // Recycle if nobody else holds a reference (no closure captured it)
                    if Rc::strong_count(&used_env) == 1 && self.env_pool.len() < 8 {
                        self.env_pool.push(used_env);
                    }

                    match result? {
                        ControlFlow::TailCall { callee, args } => {
                            current_callee = callee;
                            current_args = args;
                            continue;
                        }
                        ControlFlow::Return(v) | ControlFlow::Value(v) => return Ok(v),
                        ControlFlow::Break | ControlFlow::Continue => {
                            return Err(RuntimeError {
                                span,
                                message: "break/continue outside loop".into(),
                            });
                        }
                    }
                }
                Value::NativeFn(native_fun) => {
                    if let Some(expected) = native_fun.arity {
                        if current_args.len() != expected {
                            return Err(RuntimeError {
                                span,
                                message: format!(
                                    "'{}' expects {} argument{}, got {}",
                                    native_fun.name,
                                    expected,
                                    if expected == 1 { "" } else { "s" },
                                    current_args.len()
                                ),
                            });
                        }
                    }
                    return (native_fun.func)(&current_args)
                        .map_err(|msg| RuntimeError { span, message: msg });
                }
                _ => {
                    return Err(RuntimeError {
                        span,
                        message: format!("'{}' is not callable", current_callee),
                    });
                }
            }
        }
    }

    fn merge_sort(
        &mut self,
        items: Vec<Value>,
        comparator: &Value,
        span: Span,
    ) -> Result<Vec<Value>, RuntimeError> {
        let len = items.len();
        if len <= 1 {
            return Ok(items);
        }
        let mid = len / 2;
        let left = self.merge_sort(items[..mid].to_vec(), comparator, span)?;
        let right = self.merge_sort(items[mid..].to_vec(), comparator, span)?;
        self.merge(left, right, comparator, span)
    }

    fn merge(
        &mut self,
        left: Vec<Value>,
        right: Vec<Value>,
        comparator: &Value,
        span: Span,
    ) -> Result<Vec<Value>, RuntimeError> {
        let mut result = Vec::with_capacity(left.len() + right.len());
        let mut li = 0;
        let mut ri = 0;

        while li < left.len() && ri < right.len() {
            let cmp_val = self.call_function(
                comparator,
                smallvec![left[li].clone(), right[ri].clone()],
                span,
            )?;
            match cmp_val {
                Value::Num(n) => {
                    if n <= 0.0 {
                        result.push(left[li].clone());
                        li += 1;
                    } else {
                        result.push(right[ri].clone());
                        ri += 1;
                    }
                }
                _ => {
                    return Err(RuntimeError {
                        span,
                        message: "sort_by comparator must return a number".into(),
                    });
                }
            }
        }

        result.extend_from_slice(&left[li..]);
        result.extend_from_slice(&right[ri..]);
        Ok(result)
    }
}
