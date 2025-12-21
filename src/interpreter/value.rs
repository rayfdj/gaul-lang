use std::rc::Rc;
use crate::parser::ast::Expr;

#[derive(Debug, Clone)]
pub enum Value {
    Str(Rc<str>),
    Num(f64),
    Bool(bool),
    Null,
    Range(f64, f64),
    Fn(Rc<Function>),
    NativeFn(Rc<NativeFunction>),
}

#[derive(Debug)]
pub struct Function {
    pub name: Rc<str>,
    pub params: Vec<Rc<str>>,
    pub body: Rc<Expr>,
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
            Value::Fn(fun) => write!(f, "<fn {}>", fun.name.as_ref()),
            Value::NativeFn(native_fun) => write!(f, "<native fn {}>", native_fun.name.as_ref()),
        }
    }
}
