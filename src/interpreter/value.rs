use crate::interpreter::environment::Environment;
use crate::parser::ast::Expr;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Value {
    Str(Rc<str>),
    Num(f64),
    Bool(bool),
    Null,
    Range(f64, f64),
    Array(Rc<RefCell<Vec<Value>>>),
    Fn(Rc<Function>),
    NativeFn(Rc<NativeFunction>),
}

pub struct Function {
    pub name: Rc<str>,
    pub params: Vec<Rc<str>>,
    pub body: Rc<Expr>,
    pub closure: Rc<Environment>,
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Function")
            .field("name", &self.name)
            .field("params", &self.params)
            .field("body", &"<expr>")
            .field("closure", &"<env>")
            .finish()
    }
}

#[derive(Debug)]
pub struct NativeFunction {
    pub name: Rc<str>,
    pub arity: Option<usize>,
    pub func: fn(&[Value]) -> Result<Value, String>,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Str(s1), Self::Str(s2)) => s1 == s2,
            (Self::Num(n1), Self::Num(n2)) => n1 == n2,
            (Self::Bool(b1), Self::Bool(b2)) => b1 == b2,
            (Self::Null, Self::Null) => true,
            (Self::Range(s1, s2), Self::Range(s3, s4)) => (s1 == s3) && (s2 == s4),
            (Self::Array(a1), Self::Array(a2)) => *a1.borrow() == *a2.borrow(),
            (Self::Fn(_), Self::Fn(_)) => panic!("Cannot compare two functions"),
            (Self::NativeFn { .. }, Self::NativeFn { .. }) => {
                panic!("Cannot compare two functions")
            }
            (_, _) => false,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "null"),
            Value::Range(a, b) => write!(f, "[{}..{})", *a as i64, *b as i64),
            Value::Array(elements) => {
                write!(f, "[")?;
                for (i, val) in elements.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    match val {
                        // If the element is a string, print it with quotes (and escapes)
                        Value::Str(s) => write!(f, "{:?}", s)?,
                        // Otherwise, print normally
                        _ => write!(f, "{}", val)?,
                    }
                }
                write!(f, "]")
            }
            Value::Fn(fun) => write!(f, "<fn {}>", fun.name.as_ref()),
            Value::NativeFn(native_fun) => write!(f, "<native fn {}>", native_fun.name.as_ref()),
        }
    }
}
