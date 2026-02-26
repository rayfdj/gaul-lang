use super::environment::Environment;
use gaul_core::parser::ast::Expr;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum MapKey {
    Str(Rc<str>),
    Num(f64),
    Bool(bool),
    Null,
}

impl Hash for MapKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            MapKey::Str(s) => s.hash(state),
            MapKey::Num(n) => n.to_bits().hash(state),
            MapKey::Bool(b) => b.hash(state),
            MapKey::Null => {}
        }
    }
}

impl PartialEq for MapKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MapKey::Str(a), MapKey::Str(b)) => a == b,
            (MapKey::Num(a), MapKey::Num(b)) => a.to_bits() == b.to_bits(),
            (MapKey::Bool(a), MapKey::Bool(b)) => a == b,
            (MapKey::Null, MapKey::Null) => true,
            _ => false,
        }
    }
}

impl Eq for MapKey {}

impl fmt::Display for MapKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MapKey::Str(s) => write!(f, "{:?}", s),
            MapKey::Num(n) => write!(f, "{}", n),
            MapKey::Bool(b) => write!(f, "{}", b),
            MapKey::Null => write!(f, "null"),
        }
    }
}

impl MapKey {
    pub fn from_value(value: &Value) -> Result<MapKey, String> {
        match value {
            Value::Str(s) => Ok(MapKey::Str(s.clone())),
            Value::Num(n) => Ok(MapKey::Num(*n)),
            Value::Bool(b) => Ok(MapKey::Bool(*b)),
            Value::Null => Ok(MapKey::Null),
            Value::Array(_) => Err("cannot use array as map key".into()),
            Value::Map(_) => Err("cannot use map as map key".into()),
            Value::Fn(_) => Err("cannot use function as map key".into()),
            Value::NativeFn(_) => Err("cannot use function as map key".into()),
            Value::Range(_, _) => Err("cannot use range as map key".into()),
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            MapKey::Str(s) => Value::Str(s.clone()),
            MapKey::Num(n) => Value::Num(*n),
            MapKey::Bool(b) => Value::Bool(*b),
            MapKey::Null => Value::Null,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Str(Rc<str>),
    Num(f64),
    Bool(bool),
    Null,
    Range(f64, f64),
    Array(Rc<RefCell<Vec<Value>>>),
    Map(Rc<RefCell<HashMap<MapKey, Value>>>),
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
            (Self::Map(m1), Self::Map(m2)) => *m1.borrow() == *m2.borrow(),
            (Self::Fn(f1), Self::Fn(f2)) => Rc::ptr_eq(f1, f2),
            (Self::NativeFn(f1), Self::NativeFn(f2)) => Rc::ptr_eq(f1, f2),
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
            Value::Map(map) => {
                let map = map.borrow();
                if map.is_empty() {
                    return write!(f, "[:]");
                }
                write!(f, "[")?;
                for (i, (key, val)) in map.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: ", key)?;
                    match val {
                        Value::Str(s) => write!(f, "{:?}", s)?,
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
